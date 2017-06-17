;;; go-mode.el --- Major mode for the Go programming language

;;; Commentary:

;; Copyright 2013 The go-mode Authors.  All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

;; Author: The go-mode Authors
;; Version: 1.5.0
;; Keywords: languages go
;; URL: https://github.com/dominikh/go-mode.el
;;
;; This file is not part of GNU Emacs.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'etags)
(require 'ffap)
(require 'find-file)
(require 'ring)
(require 'url)
(require 'xref nil :noerror)  ; xref is new in Emacs 25.1


(eval-when-compile
  (defmacro go--forward-word (&optional arg)
   (if (fboundp 'forward-word-strictly)
       `(forward-word-strictly ,arg)
     `(forward-word ,arg))))

(defun go--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun go-goto-opening-parenthesis (&optional _legacy-unused)
  "Move up one level of parentheses."
  ;; The old implementation of go-goto-opening-parenthesis had an
  ;; optional argument to speed up the function.  It didn't change the
  ;; function's outcome.

  ;; Silently fail if there's no matching opening parenthesis.
  (condition-case nil
      (backward-up-list)
    (scan-error nil)))


(defconst go-dangling-operators-regexp "[^-]-\\|[^+]\\+\\|[/*&><.=|^]")
(defconst go--max-dangling-operator-length 2
  "The maximum length of dangling operators.
This must be at least the length of the longest string matched by
‘go-dangling-operators-regexp.’, and must be updated whenever
that constant is changed.")

(defconst go-identifier-regexp "[[:word:][:multibyte:]]+")
(defconst go-type-name-no-prefix-regexp "\\(?:[[:word:][:multibyte:]]+\\.\\)?[[:word:][:multibyte:]]+")
(defconst go-qualified-identifier-regexp (concat go-identifier-regexp "\\." go-identifier-regexp))
(defconst go-label-regexp go-identifier-regexp)
(defconst go-type-regexp "[[:word:][:multibyte:]*]+")
(defconst go-func-regexp (concat "\\_<func\\_>\\s *\\(" go-identifier-regexp "\\)"))
(defconst go-func-meth-regexp (concat
                               "\\_<func\\_>\\s *\\(?:(\\s *"
                               "\\(" go-identifier-regexp "\\s +\\)?" go-type-regexp
                               "\\s *)\\s *\\)?\\("
                               go-identifier-regexp
                               "\\)("))

(defconst go-builtins
  '("append" "cap"   "close"   "complex" "copy"
    "delete" "imag"  "len"     "make"    "new"
    "panic"  "print" "println" "real"    "recover")
  "All built-in functions in the Go language.  Used for font locking.")

(defconst go-mode-keywords
  '("break"    "default"     "func"   "interface" "select"
    "case"     "defer"       "go"     "map"       "struct"
    "chan"     "else"        "goto"   "package"   "switch"
    "const"    "fallthrough" "if"     "range"     "type"
    "continue" "for"         "import" "return"    "var")
  "All keywords in the Go language.  Used for font locking.")

(defconst go-constants '("nil" "true" "false" "iota"))
(defconst go-type-name-regexp (concat "\\(?:[*(]\\)*\\(\\(?:" go-identifier-regexp "\\.\\)?" go-identifier-regexp "\\)"))

;; Maximum number of identifiers that can be highlighted as type names
;; in one function type/declaration.
(defconst go--font-lock-func-param-num-groups 16)

(defvar go-dangling-cache)
(defvar go-godoc-history nil)
(defvar go--coverage-current-file-name)

(defgroup go nil
  "Major mode for editing Go code."
  :link '(url-link "https://github.com/dominikh/go-mode.el")
  :group 'languages)

(defgroup go-cover nil
  "Options specific to `cover`."
  :group 'go)

(defgroup godoc nil
  "Options specific to `godoc'."
  :group 'go)

(defcustom go-fontify-function-calls t
  "Fontify function and method calls if this is non-nil."
  :type 'boolean
  :group 'go)

(defcustom go-mode-hook nil
  "Hook called by `go-mode'."
  :type 'hook
  :group 'go)

(defcustom go-command "go"
  "The 'go' command.
Some users have multiple Go development trees and invoke the 'go'
tool via a wrapper that sets GOROOT and GOPATH based on the
current directory.  Such users should customize this variable to
point to the wrapper script."
  :type 'string
  :group 'go)

(defcustom gofmt-command "gofmt"
  "The 'gofmt' command.
Some users may replace this with 'goimports'
from https://golang.org/x/tools/cmd/goimports."
  :type 'string
  :group 'go)

(defcustom gofmt-args nil
  "Additional arguments to pass to gofmt."
  :type '(repeat string)
  :group 'go)

(defcustom gofmt-show-errors 'buffer
  "Where to display gofmt error output.
It can either be displayed in its own buffer, in the echo area, or not at all.

Please note that Emacs outputs to the echo area when writing
files and will overwrite gofmt's echo output if used from inside
a `before-save-hook'."
  :type '(choice
          (const :tag "Own buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'go)

(defcustom godef-command "godef"
  "The 'godef' command."
  :type 'string
  :group 'go)

(defcustom go-other-file-alist
  '(("_test\\.go\\'" (".go"))
    ("\\.go\\'" ("_test.go")))
  "See the documentation of `ff-other-file-alist' for details."
  :type '(repeat (list regexp (choice (repeat string) function)))
  :group 'go)

(defcustom go-packages-function 'go-packages-native
  "Function called by `go-packages' to determine the list of available packages.
This is used in e.g. tab completion in `go-import-add'.

This package provides two functions: `go-packages-native' uses
elisp to find all .a files in all /pkg/ directories.
`go-packages-go-list' uses 'go list all' to determine all Go
packages.  `go-packages-go-list' generally produces more accurate
results, but can be slower than `go-packages-native'."
  :type 'function
  :package-version '(go-mode . 1.4.0)
  :group 'go)

(defcustom go-guess-gopath-functions (list #'go-godep-gopath
                                           #'go-wgo-gopath
                                           #'go-gb-gopath
                                           #'go-plain-gopath)
  "Functions to call in sequence to detect a project's GOPATH.

The functions in this list will be called one after another,
until a function returns non-nil.  The order of the functions in
this list is important, as some project layouts may superficially
look like others.  For example, a subset of wgo projects look like
gb projects.  That's why we need to detect wgo first, to avoid
mis-identifying them as gb projects."
  :type '(repeat function)
  :group 'go)

(defcustom godoc-command "go doc"
  "Which executable to use for `godoc'.
This can either be 'godoc' or 'go doc', both as an absolute path
or an executable in PATH."
  :type 'string
  :group 'go)

(defcustom godoc-and-godef-command "godoc"
  "Which executable to use for `godoc' in `godoc-and-godef-command'.
Must be 'godoc' and not 'go doc' and can be an absolute path or
an executable in PATH."
  :type 'string
  :group 'go)

(defcustom godoc-use-completing-read nil
  "Provide auto-completion for godoc.
Only really desirable when using `godoc' instead of `go doc'."
  :type 'boolean
  :group 'godoc)

(defcustom godoc-at-point-function #'godoc-and-godef
  "Function to call to display the documentation for an
identifier at a given position.

This package provides two functions: `godoc-and-godef' uses a
combination of godef and godoc to find the documentation.  This
approach has several caveats.  See its documentation for more
information.  The second function, `godoc-gogetdoc' uses an
additional tool that correctly determines the documentation for
any identifier.  It provides better results than
`godoc-and-godef'."
  :type 'function
  :group 'godoc)

(defun godoc-and-godef (point)
  "Use a combination of godef and godoc to guess the documentation at POINT.

Due to a limitation in godoc, it is not possible to differentiate
between functions and methods, which may cause `godoc-at-point'
to display more documentation than desired.  Furthermore, it
doesn't work on package names or variables.

Consider using ‘godoc-gogetdoc’ instead for more accurate results."
  (condition-case nil
      (let* ((output (godef--call point))
             (file (car output))
             (name-parts (split-string (cadr output) " "))
             (first (car name-parts)))
        (if (not (godef--successful-p file))
            (message "%s" (godef--error file))
          (go--godoc (format "%s %s"
                         (file-name-directory file)
                         (if (or (string= first "type") (string= first "const"))
                             (cadr name-parts)
                           (car name-parts)))
                    godoc-and-godef-command)))
    (file-error (message "Could not run godef binary"))))

(defun godoc-gogetdoc (point)
  "Use the gogetdoc tool to find the documentation for an identifier at POINT.

You can install gogetdoc with 'go get -u github.com/zmb3/gogetdoc'."
  (if (not (buffer-file-name (go--coverage-origin-buffer)))
      ;; TODO: gogetdoc supports unsaved files, but not introducing
      ;; new artifical files, so this limitation will stay for now.
      (error "Cannot use gogetdoc on a buffer without a file name"))
  (let ((posn (format "%s:#%d" (shell-quote-argument (file-truename buffer-file-name)) (1- (position-bytes point))))
        (out (godoc--get-buffer "<at point>")))
  (with-current-buffer (get-buffer-create "*go-gogetdoc-input*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (go--insert-modified-files)
    (call-process-region (point-min) (point-max) "gogetdoc" nil out nil
                         "-modified"
                         (format "-pos=%s" posn)))
  (with-current-buffer out
    (goto-char (point-min))
    (godoc-mode)
    (display-buffer (current-buffer) t))))

(defun go--kill-new-message (url)
  "Make URL the latest kill and print a message."
  (kill-new url)
  (message "%s" url))

(defcustom go-play-browse-function 'go--kill-new-message
  "Function to call with the Playground URL.
See `go-play-region' for more details."
  :type '(choice
          (const :tag "Nothing" nil)
          (const :tag "Kill + Message" go--kill-new-message)
          (const :tag "Browse URL" browse-url)
          (function :tag "Call function"))
  :group 'go)

(defcustom go-coverage-display-buffer-func 'display-buffer-reuse-window
  "How `go-coverage' should display the coverage buffer.
See `display-buffer' for a list of possible functions."
  :type 'function
  :group 'go-cover)

(defface go-coverage-untracked
  '((t (:foreground "#505050")))
  "Coverage color of untracked code."
  :group 'go-cover)

(defface go-coverage-0
  '((t (:foreground "#c00000")))
  "Coverage color for uncovered code."
  :group 'go-cover)
(defface go-coverage-1
  '((t (:foreground "#808080")))
  "Coverage color for covered code with weight 1."
  :group 'go-cover)
(defface go-coverage-2
  '((t (:foreground "#748c83")))
  "Coverage color for covered code with weight 2."
  :group 'go-cover)
(defface go-coverage-3
  '((t (:foreground "#689886")))
  "Coverage color for covered code with weight 3."
  :group 'go-cover)
(defface go-coverage-4
  '((t (:foreground "#5ca489")))
  "Coverage color for covered code with weight 4."
  :group 'go-cover)
(defface go-coverage-5
  '((t (:foreground "#50b08c")))
  "Coverage color for covered code with weight 5."
  :group 'go-cover)
(defface go-coverage-6
  '((t (:foreground "#44bc8f")))
  "Coverage color for covered code with weight 6."
  :group 'go-cover)
(defface go-coverage-7
  '((t (:foreground "#38c892")))
  "Coverage color for covered code with weight 7."
  :group 'go-cover)
(defface go-coverage-8
  '((t (:foreground "#2cd495")))
  "Coverage color for covered code with weight 8.
For mode=set, all covered lines will have this weight."
  :group 'go-cover)
(defface go-coverage-9
  '((t (:foreground "#20e098")))
  "Coverage color for covered code with weight 9."
  :group 'go-cover)
(defface go-coverage-10
  '((t (:foreground "#14ec9b")))
  "Coverage color for covered code with weight 10."
  :group 'go-cover)
(defface go-coverage-covered
  '((t (:foreground "#2cd495")))
  "Coverage color of covered code."
  :group 'go-cover)

(defvar go-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?%  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?|  "." st)
    (modify-syntax-entry ?^  "." st)
    (modify-syntax-entry ?!  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?`  "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; TODO make _ a symbol constituent now that xemacs is gone
    (modify-syntax-entry ?_  "w" st)

    st)
  "Syntax table for Go mode.")

(defun go--build-font-lock-keywords ()
  ;; we cannot use 'symbols in regexp-opt because GNU Emacs <24
  ;; doesn't understand that
  (append
   `((go--match-func
      ,@(mapcar (lambda (x) `(,x font-lock-type-face))
                (number-sequence 1 go--font-lock-func-param-num-groups)))
     (,(concat "\\_<" (regexp-opt go-mode-keywords t) "\\_>") . font-lock-keyword-face)
     (,(concat "\\(\\_<" (regexp-opt go-builtins t) "\\_>\\)[[:space:]]*(") 1 font-lock-builtin-face)
     (,(concat "\\_<" (regexp-opt go-constants t) "\\_>") . font-lock-constant-face)
     (,go-func-regexp 1 font-lock-function-name-face)) ;; function (not method) name

   (if go-fontify-function-calls
       `((,(concat "\\(" go-identifier-regexp "\\)[[:space:]]*(") 1 font-lock-function-name-face) ;; function call/method name
         (,(concat "[^[:word:][:multibyte:]](\\(" go-identifier-regexp "\\))[[:space:]]*(") 1 font-lock-function-name-face)) ;; bracketed function call
     `((,go-func-meth-regexp 2 font-lock-function-name-face))) ;; method name

   `(
     ("\\(`[^`]*`\\)" 1 font-lock-multiline) ;; raw string literal, needed for font-lock-syntactic-keywords
     (,(concat "\\_<type\\_>[[:space:]]+\\([^[:space:](]+\\)") 1 font-lock-type-face) ;; types
     (,(concat "\\_<type\\_>[[:space:]]+" go-identifier-regexp "[[:space:]]*" go-type-name-regexp) 1 font-lock-type-face) ;; types
     (,(concat "[^[:word:][:multibyte:]]\\[\\([[:digit:]]+\\|\\.\\.\\.\\)?\\]" go-type-name-regexp) 2 font-lock-type-face) ;; Arrays/slices
     (,(concat "\\(" go-identifier-regexp "\\)" "{") 1 font-lock-type-face)
     (,(concat "\\_<map\\_>\\[[^]]+\\]" go-type-name-regexp) 1 font-lock-type-face) ;; map value type
     (,(concat "\\_<map\\_>\\[" go-type-name-regexp) 1 font-lock-type-face) ;; map key type
     (,(concat "\\_<chan\\_>[[:space:]]*\\(?:<-[[:space:]]*\\)?" go-type-name-regexp) 1 font-lock-type-face) ;; channel type
     (,(concat "\\_<\\(?:new\\|make\\)\\_>\\(?:[[:space:]]\\|)\\)*(" go-type-name-regexp) 1 font-lock-type-face) ;; new/make type
     ;; TODO do we actually need this one or isn't it just a function call?
     (,(concat "\\.\\s *(" go-type-name-regexp) 1 font-lock-type-face) ;; Type conversion
     ;; Like the original go-mode this also marks compound literal
     ;; fields. There, it was marked as to fix, but I grew quite
     ;; accustomed to it, so it'll stay for now.
     (,(concat "^[[:space:]]*\\(" go-label-regexp "\\)[[:space:]]*:\\(\\S.\\|$\\)") 1 font-lock-constant-face) ;; Labels and compound literal fields
     (,(concat "\\_<\\(goto\\|break\\|continue\\)\\_>[[:space:]]*\\(" go-label-regexp "\\)") 2 font-lock-constant-face)))) ;; labels in goto/break/continue

(let ((m (define-prefix-command 'go-goto-map)))
  (define-key m "a" #'go-goto-arguments)
  (define-key m "d" #'go-goto-docstring)
  (define-key m "f" #'go-goto-function)
  (define-key m "i" #'go-goto-imports)
  (define-key m "m" #'go-goto-method-receiver)
  (define-key m "n" #'go-goto-function-name)
  (define-key m "r" #'go-goto-return-values))

(defvar go-mode-map
  (let ((m (make-sparse-keymap)))
    (unless (boundp 'electric-indent-chars)
        (define-key m "}" #'go-mode-insert-and-indent)
        (define-key m ")" #'go-mode-insert-and-indent))
    (define-key m (kbd "C-c C-a") #'go-import-add)
    (define-key m (kbd "C-c C-j") #'godef-jump)
    (define-key m (kbd "C-x 4 C-c C-j") #'godef-jump-other-window)
    (define-key m (kbd "C-c C-d") #'godef-describe)
    (define-key m (kbd "C-c C-f") 'go-goto-map)
    m)
  "Keymap used by ‘go-mode’.")

(easy-menu-define go-mode-menu go-mode-map
  "Menu for Go mode."
  '("Go"
    ["Describe Expression"   godef-describe t]
    ["Jump to Definition"    godef-jump t]
    "---"
    ["Add Import"            go-import-add t]
    ["Remove Unused Imports" go-remove-unused-imports t]
    ["Go to Imports"         go-goto-imports t]
    "---"
    ("Playground"
     ["Send Buffer"          go-play-buffer t]
     ["Send Region"          go-play-region t]
     ["Download"             go-download-play t])
    "---"
    ["Coverage"              go-coverage t]
    ["Gofmt"                 gofmt t]
    ["Godoc"                 godoc t]
    "---"
    ["Customize Mode"        (customize-group 'go) t]))

(defun go-mode-insert-and-indent (key)
  "Invoke the global binding of KEY, then reindent the line."

  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (indent-according-to-mode))

(defmacro go-paren-level ()
  `(car (syntax-ppss)))

(defmacro go-in-string-or-comment-p ()
  `(nth 8 (syntax-ppss)))

(defmacro go-in-string-p ()
  `(nth 3 (syntax-ppss)))

(defmacro go-in-comment-p ()
  `(nth 4 (syntax-ppss)))

(defmacro go-goto-beginning-of-string-or-comment ()
  `(goto-char (nth 8 (syntax-ppss))))

(defun go--backward-irrelevant (&optional stop-at-string)
  "Skip backwards over any characters that are irrelevant for
indentation and related tasks.

It skips over whitespace, comments, cases and labels and, if
STOP-AT-STRING is not true, over strings."

  (let (pos (start-pos (point)))
    (skip-chars-backward "\n\s\t")
    (if (and (save-excursion (beginning-of-line) (go-in-string-p))
             (= (char-before) ?`)
             (not stop-at-string))
        (backward-char))
    (if (and (go-in-string-p)
             (not stop-at-string))
        (go-goto-beginning-of-string-or-comment))
    (if (looking-back "\\*/" (line-beginning-position))
        (backward-char))
    (if (go-in-comment-p)
        (go-goto-beginning-of-string-or-comment))
    (setq pos (point))
    (beginning-of-line)
    (if (or (looking-at (concat "^" go-label-regexp ":"))
            (looking-at "^[[:space:]]*\\(case .+\\|default\\):"))
        (end-of-line 0)
      (goto-char pos))
    (if (/= start-pos (point))
        (go--backward-irrelevant stop-at-string))
    (/= start-pos (point))))

(defun go--buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (/= (buffer-size)
      (- (point-max)
         (point-min))))

(defun go-previous-line-has-dangling-op-p ()
  "Return non-nil if the current line is a continuation line."
  (let* ((cur-line (line-number-at-pos))
         (val (gethash cur-line go-dangling-cache 'nope)))
    (if (or (go--buffer-narrowed-p) (equal val 'nope))
        (save-excursion
          (beginning-of-line)
          (go--backward-irrelevant t)
          (setq val (looking-back go-dangling-operators-regexp
                                  (- (point) go--max-dangling-operator-length)))
          (if (not (go--buffer-narrowed-p))
              (puthash cur-line val go-dangling-cache))))
    val))

(defun go--at-function-definition ()
  "Return non-nil if point is on the opening curly brace of a
function definition.

We do this by first calling (beginning-of-defun), which will take
us to the start of *some* function.  We then look for the opening
curly brace of that function and compare its position against the
curly brace we are checking.  If they match, we return non-nil."
  (if (= (char-after) ?\{)
      (save-excursion
        (let ((old-point (point))
              start-nesting)
          (beginning-of-defun)
          (when (looking-at "func ")
            (setq start-nesting (go-paren-level))
            (skip-chars-forward "^{")
            (while (> (go-paren-level) start-nesting)
              (forward-char)
              (skip-chars-forward "^{") 0)
            (if (and (= (go-paren-level) start-nesting) (= old-point (point)))
                t))))))

(defun go--indentation-for-opening-parenthesis ()
  "Return the semantic indentation for the current opening parenthesis.

If point is on an opening curly brace and said curly brace
belongs to a function declaration, the indentation of the func
keyword will be returned.  Otherwise the indentation of the
current line will be returned."
  (save-excursion
    (if (go--at-function-definition)
        (progn
          (beginning-of-defun)
          (current-indentation))
      (current-indentation))))

(defun go-indentation-at-point ()
  (save-excursion
    (let (start-nesting)
      (back-to-indentation)
      (setq start-nesting (go-paren-level))

      (cond
       ((go-in-string-p)
        (current-indentation))
       ((looking-at "[])}]")
        (go-goto-opening-parenthesis)
        (if (go-previous-line-has-dangling-op-p)
            (- (current-indentation) tab-width)
          (go--indentation-for-opening-parenthesis)))
       ((progn (go--backward-irrelevant t)
               (looking-back go-dangling-operators-regexp
                             (- (point) go--max-dangling-operator-length)))
        ;; only one nesting for all dangling operators in one operation
        (if (go-previous-line-has-dangling-op-p)
            (current-indentation)
          (+ (current-indentation) tab-width)))
       ((zerop (go-paren-level))
        0)
       ((progn (go-goto-opening-parenthesis) (< (go-paren-level) start-nesting))
        (if (go-previous-line-has-dangling-op-p)
            (current-indentation)
          (+ (go--indentation-for-opening-parenthesis) tab-width)))
       (t
        (current-indentation))))))

(defun go-mode-indent-line ()
  (interactive)
  (let (indent
        shift-amt
        (pos (- (point-max) (point)))
        (point (point))
        (beg (line-beginning-position)))
    (back-to-indentation)
    (if (go-in-string-or-comment-p)
        (goto-char point)
      (setq indent (go-indentation-at-point))
      (if (looking-at (concat go-label-regexp ":\\([[:space:]]*/.+\\)?$\\|case .+:\\|default:"))
          (cl-decf indent tab-width))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))))

(defun go-beginning-of-defun (&optional count)
  (unless (bolp)
    (end-of-line))
  (setq count (or count 1))
  (let (first failure)
    (dotimes (i (abs count))
      (setq first t)
      (while (and (not failure)
                  (or first (go-in-string-or-comment-p)))
        (if (>= count 0)
            (progn
              (go--backward-irrelevant)
              (if (not (re-search-backward go-func-meth-regexp nil t))
                  (setq failure t)))
          (if (looking-at go-func-meth-regexp)
              (forward-char))
          (if (not (re-search-forward go-func-meth-regexp nil t))
              (setq failure t)))
        (setq first nil)))
    (if (< count 0)
        (beginning-of-line))
    (not failure)))

(defun go-end-of-defun ()
  (let (orig-level)
    ;; It can happen that we're not placed before a function by emacs
    (if (not (looking-at "func"))
        (go-beginning-of-defun -1))
    ;; Find the { that starts the function, i.e., the next { that isn't
    ;; preceded by struct or interface, or a comment or struct tag.  BUG:
    ;; breaks if there's a comment between the struct/interface keyword and
    ;; bracket, like this:
    ;;
    ;;     struct /* why? */ {
    (while (progn
      (skip-chars-forward "^{")
      (forward-char)
      (or (go-in-string-or-comment-p)
          (looking-back "\\(struct\\|interface\\)\\s-*{"
                        (line-beginning-position)))))
    (setq orig-level (go-paren-level))
    (while (>= (go-paren-level) orig-level)
      (skip-chars-forward "^}")
      (forward-char))))

(defun go--find-enclosing-parentheses (position)
  "Return points of outermost '(' and ')' surrounding POSITION if
such parentheses exist.

If outermost '(' exists but ')' does not, it returns the next blank
line or end-of-buffer position instead of the position of the closing
parenthesis.

If the starting parenthesis is not found, it returns (POSITION
POSITION)."
  (save-excursion
    (let (beg end)
      (goto-char position)
      (while (> (go-paren-level) 0)
        (re-search-backward "[(\\[{]" nil t)
        (when (looking-at "(")
          (setq beg (point))))
      (if (null beg)
          (list position position)
        (goto-char position)
        (while (and (> (go-paren-level) 0)
                    (search-forward ")" nil t)))
        (when (> (go-paren-level) 0)
          (unless (re-search-forward "^[[:space:]]*$" nil t)
            (goto-char (point-max))))
        (list beg (point))))))

(defun go--search-next-comma (end)
  "Search forward from point for a comma whose nesting level is
the same as point.  If it reaches the end of line or a closing
parenthesis before a comma, it stops at it."
  (let ((orig-level (go-paren-level)))
    (while (and (< (point) end)
                (or (looking-at "[^,)\n]")
                    (> (go-paren-level) orig-level)))
      (forward-char))
    (when (and (looking-at ",")
               (< (point) (1- end)))
      (forward-char))))

(defun go--looking-at-keyword ()
  (and (looking-at (concat "\\(" go-identifier-regexp "\\)"))
       (member (match-string 1) go-mode-keywords)))

(defun go--match-func (end)
  "Search for identifiers used as type names from a function
parameter list, and set the identifier positions as the results
of last search.  Return t if search succeeded."
  (when (re-search-forward "\\_<func\\_>" end t)
    (let ((regions (go--match-func-type-names end)))
      (if (null regions)
          ;; Nothing to highlight. This can happen if the current func
          ;; is "func()". Try next one.
          (go--match-func end)
        ;; There are something to highlight. Set those positions as
        ;; last search results.
        (setq regions (go--filter-match-data regions end))
        (when regions
          (set-match-data (go--make-match-data regions))
          t)))))

(defun go--match-func-type-names (end)
  (cond
   ;; Function declaration (e.g. "func foo(")
   ((looking-at (concat "[[:space:]\n]*" go-identifier-regexp "[[:space:]\n]*("))
    (goto-char (match-end 0))
    (nconc (go--match-parameter-list end)
           (go--match-function-result end)))
   ;; Method declaration, function literal, or function type
   ((looking-at "[[:space:]]*(")
    (goto-char (match-end 0))
    (let ((regions (go--match-parameter-list end)))
      ;; Method declaration (e.g. "func (x y) foo(")
      (when (looking-at (concat "[[:space:]]*" go-identifier-regexp "[[:space:]\n]*("))
        (goto-char (match-end 0))
        (setq regions (nconc regions (go--match-parameter-list end))))
      (nconc regions (go--match-function-result end))))))

(defun go--parameter-list-type (end)
  "Return `present' if the parameter list has names, or `absent' if not.
Assumes point is at the beginning of a parameter list, just
after '('."
  (save-excursion
    (skip-chars-forward "[:space:]\n" end)
    (cond ((> (point) end)
           nil)
          ((looking-at (concat go-identifier-regexp "[[:space:]\n]*,"))
           (goto-char (match-end 0))
           (go--parameter-list-type end))
          ((or (looking-at go-qualified-identifier-regexp)
               (looking-at (concat go-type-name-no-prefix-regexp "[[:space:]\n]*\\(?:)\\|\\'\\)"))
               (go--looking-at-keyword)
               (looking-at "[*\\[]\\|\\.\\.\\.\\|\\'"))
           'absent)
          (t 'present))))

(defconst go--opt-dotdotdot-regexp "\\(?:\\.\\.\\.\\)?")
(defconst go--parameter-type-regexp
  (concat go--opt-dotdotdot-regexp "[[:space:]*\n]*\\(" go-type-name-no-prefix-regexp "\\)[[:space:]\n]*\\([,)]\\|\\'\\)"))
(defconst go--func-type-in-parameter-list-regexp
  (concat go--opt-dotdotdot-regexp "[[:space:]*\n]*\\(\\_<func\\_>" "\\)"))

(defun go--match-parameters-common (identifier-regexp end)
  (let ((acc ())
        (start -1))
    (while (progn (skip-chars-forward "[:space:]\n" end)
                  (and (not (looking-at "\\(?:)\\|\\'\\)"))
                       (< start (point))
                       (<= (point) end)))
      (setq start (point))
      (cond
       ((looking-at (concat identifier-regexp go--parameter-type-regexp))
        (setq acc (nconc acc (list (match-beginning 1) (match-end 1))))
        (goto-char (match-beginning 2)))
       ((looking-at (concat identifier-regexp go--func-type-in-parameter-list-regexp))
        (goto-char (match-beginning 1))
        (setq acc (nconc acc (go--match-func-type-names end)))
        (go--search-next-comma end))
       (t
        (go--search-next-comma end))))
    (when (and (looking-at ")")
               (< (point) end))
      (forward-char))
    acc))

(defun go--match-parameters-with-identifier-list (end)
  (go--match-parameters-common
   (concat go-identifier-regexp "[[:space:]\n]+")
   end))

(defun go--match-parameters-without-identifier-list (end)
  (go--match-parameters-common "" end))

(defun go--filter-match-data (regions end)
  "Remove points from REGIONS if they are beyond END.
REGIONS are a list whose size is multiple of 2.  Element 2n is beginning of a
region and 2n+1 is end of it.

This function is used to make sure we don't override end point
that `font-lock-mode' gave to us."
  (when regions
    (let* ((vec (vconcat regions))
           (i 0)
           (len (length vec)))
      (while (and (< i len)
                  (<= (nth i regions) end)
                  (<= (nth (1+ i) regions) end))
        (setq i (+ i 2)))
      (cond ((= i len)
             regions)
            ((zerop i)
             nil)
            (t
             (butlast regions (- (length regions) i)))))))

(defun go--make-match-data (regions)
  (let ((deficit (- (* 2 go--font-lock-func-param-num-groups)
                    (length regions))))
    (when (> deficit 0)
      (let ((last (car (last regions))))
        (setq regions (nconc regions (make-list deficit last))))))
  `(,(car regions) ,@(last regions) ,@regions))

(defun go--match-parameter-list (end)
  "Return a list of identifier positions that are used as type
names in a function parameter list, assuming point is at the
beginning of a parameter list.  Return nil if the text after
point does not look like a parameter list.

Set point to end of closing parenthesis on success.

In Go, the names must either all be present or all be absent
within a list of parameters.

Parsing a parameter list is a little bit complicated because we
have to scan through the parameter list to determine whether or
not the list has names. Until a type name is found or reaching
end of a parameter list, we are not sure which form the parameter
list is.

For example, X and Y are type names in a parameter list \"(X,
Y)\" but are parameter names in \"(X, Y int)\". We cannot say if
X is a type name until we see int after Y.

Note that even \"(int, float T)\" is a valid parameter
list. Builtin type names are not reserved words. In this example,
int and float are parameter names and only T is a type name.

In this function, we first scan the parameter list to see if the
list has names, and then handle it accordingly."
  (let ((name (go--parameter-list-type end)))
    (cond ((eq name 'present)
           (go--match-parameters-with-identifier-list end))
          ((eq name 'absent)
           (go--match-parameters-without-identifier-list end))
          (t nil))))

(defun go--match-function-result (end)
  "Return a list of identifier positions that are used as type
names in a function result, assuming point is at the beginning of
a result.

Function result is a unparenthesized type or a parameter list."
  (cond ((and (looking-at (concat "[[:space:]*]*\\(" go-type-name-no-prefix-regexp "\\)"))
              (not (member (match-string 1) go-mode-keywords)))
         (list (match-beginning 1) (match-end 1)))
        ((looking-at "[[:space:]]*(")
         (goto-char (match-end 0))
         (go--match-parameter-list end))
        (t nil)))

(defun go--reset-dangling-cache-before-change (&optional _beg _end)
  "Reset `go-dangling-cache'.

This is intended to be called from `before-change-functions'."
  (setq go-dangling-cache (make-hash-table :test 'eql)))

;;;###autoload
(define-derived-mode go-mode prog-mode "Go"
  "Major mode for editing Go source text.

This mode provides (not just) basic editing capabilities for
working with Go code. It offers almost complete syntax
highlighting, indentation that is almost identical to gofmt and
proper parsing of the buffer content to allow features such as
navigation by function, manipulation of comments or detection of
strings.

In addition to these core features, it offers various features to
help with writing Go code. You can directly run buffer content
through gofmt, read godoc documentation from within Emacs, modify
and clean up the list of package imports or interact with the
Playground (uploading and downloading pastes).

The following extra functions are defined:

- `gofmt'
- `godoc' and `godoc-at-point'
- `go-import-add'
- `go-remove-unused-imports'
- `go-goto-arguments'
- `go-goto-docstring'
- `go-goto-function'
- `go-goto-function-name'
- `go-goto-imports'
- `go-goto-return-values'
- `go-goto-method-receiver'
- `go-play-buffer' and `go-play-region'
- `go-download-play'
- `godef-describe' and `godef-jump'
- `go-coverage'
- `go-set-project'
- `go-reset-gopath'

If you want to automatically run `gofmt' before saving a file,
add the following hook to your emacs configuration:

\(add-hook 'before-save-hook #'gofmt-before-save)

If you want to use `godef-jump' instead of etags (or similar),
consider binding godef-jump to `M-.', which is the default key
for `find-tag':

\(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd \"M-.\") #'godef-jump)))

Please note that godef is an external dependency. You can install
it with

go get github.com/rogpeppe/godef


If you're looking for even more integration with Go, namely
on-the-fly syntax checking, auto-completion and snippets, it is
recommended that you look at flycheck
\(see URL `https://github.com/flycheck/flycheck') or flymake in combination
with goflymake \(see URL `https://github.com/dougm/goflymake'), gocode
\(see URL `https://github.com/nsf/gocode'), go-eldoc
\(see URL `github.com/syohex/emacs-go-eldoc') and yasnippet-go
\(see URL `https://github.com/dominikh/yasnippet-go')"

  ;; Font lock
  (set (make-local-variable 'font-lock-defaults)
       '(go--build-font-lock-keywords))

  ;; Indentation
  (set (make-local-variable 'indent-line-function) #'go-mode-indent-line)

  ;; Comments
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end)   "")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")

  (set (make-local-variable 'beginning-of-defun-function) #'go-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'go-end-of-defun)

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'syntax-propertize-function) #'go-propertize-syntax)

  (if (boundp 'electric-indent-chars)
      (set (make-local-variable 'electric-indent-chars) '(?\n ?} ?\))))

  (set (make-local-variable 'compilation-error-screen-columns) nil)

  (set (make-local-variable 'go-dangling-cache) (make-hash-table :test 'eql))
  (add-hook 'before-change-functions #'go--reset-dangling-cache-before-change t t)

  ;; ff-find-other-file
  (setq ff-other-file-alist 'go-other-file-alist)

  (setq imenu-generic-expression
        '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
          ("func" "^func *\\(.*\\) {" 1)))
  (imenu-add-to-menubar "Index")

  ;; Go style
  (setq indent-tabs-mode t)

  ;; Handle unit test failure output in compilation-mode
  ;;
  ;; Note that we add our entry to the beginning of
  ;; compilation-error-regexp-alist. In older versions of Emacs, the
  ;; list was processed from the end, and we would've wanted to add
  ;; ours last. But at some point this changed, and now the list is
  ;; processed from the beginning. It's important that our entry comes
  ;; before gnu, because gnu matches go test output, but includes the
  ;; leading whitespace in the file name.
  ;;
  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2001-12/msg00674.html
  ;; documents the old, reverseed order.
  (when (and (boundp 'compilation-error-regexp-alist)
             (boundp 'compilation-error-regexp-alist-alist))
    (add-to-list 'compilation-error-regexp-alist 'go-test)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(go-test . ("^\t+\\([^()\t\n]+\\):\\([0-9]+\\):? .*$" 1 2)) t)))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))

(defun go--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in go--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (go--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (go--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in go--apply-rcs-patch")))))))))

(defun gofmt--is-goimports-p ()
  (string-equal (file-name-base gofmt-command) "goimports"))

(defun gofmt ()
  "Format the current buffer according to the gofmt tool."
  (interactive)
  (let ((tmpfile (make-temp-file "gofmt" nil ".go"))
        (patchbuf (get-buffer-create "*Gofmt patch*"))
        (errbuf (if gofmt-show-errors (get-buffer-create "*Gofmt Errors*")))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        our-gofmt-args)

    (unwind-protect
        (save-restriction
          (widen)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))

          (write-region nil nil tmpfile)

          (when (and (gofmt--is-goimports-p) buffer-file-name)
            (setq our-gofmt-args
                  (append our-gofmt-args
                          ;; srcdir, despite its name, supports
                          ;; accepting a full path, and some features
                          ;; of goimports rely on knowing the full
                          ;; name.
                          (list "-srcdir" (file-truename buffer-file-name)))))
          (setq our-gofmt-args (append our-gofmt-args
                                       gofmt-args
                                       (list "-w" tmpfile)))
          (message "Calling gofmt: %s %s" gofmt-command our-gofmt-args)
          ;; We're using errbuf for the mixed stdout and stderr output. This
          ;; is not an issue because gofmt -w does not produce any stdout
          ;; output in case of success.
          (if (zerop (apply #'call-process gofmt-command nil errbuf nil our-gofmt-args))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer is already gofmted")
                  (go--apply-rcs-patch patchbuf)
                  (message "Applied gofmt"))
                (if errbuf (gofmt--kill-error-buffer errbuf)))
            (message "Could not apply gofmt")
            (if errbuf (gofmt--process-errors (buffer-file-name) tmpfile errbuf))))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))


(defun gofmt--process-errors (filename tmpfile errbuf)
  (with-current-buffer errbuf
    (if (eq gofmt-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (gofmt--kill-error-buffer errbuf))
      ;; Convert the gofmt stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (if (save-excursion
            (save-match-data
              (search-forward "flag provided but not defined: -srcdir" nil t)))
          (insert "Your version of goimports is too old and doesn't support vendoring. Please update goimports!\n\n"))
      (insert "gofmt errors:\n")
      (let ((truefile
             (if (gofmt--is-goimports-p)
                 (concat (file-name-directory filename) (file-name-nondirectory tmpfile))
               tmpfile)))
        (while (search-forward-regexp (concat "^\\(" (regexp-quote truefile) "\\):") nil t)
          (replace-match (file-name-nondirectory filename) t t nil 1)))
      (compilation-mode)
      (display-buffer errbuf))))

(defun gofmt--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

;;;###autoload
(defun gofmt-before-save ()
  "Add this to .emacs to run gofmt on the current buffer when saving:
\(add-hook 'before-save-hook 'gofmt-before-save).

Note that this will cause ‘go-mode’ to get loaded the first time
you save any file, kind of defeating the point of autoloading."

  (interactive)
  (when (eq major-mode 'go-mode) (gofmt)))

(defun godoc--read-query ()
  "Read a godoc query from the minibuffer."
  (if godoc-use-completing-read
      (completing-read "godoc; "
                       (go-packages) nil nil nil 'go-godoc-history)
    (read-from-minibuffer "godoc: " nil nil nil 'go-godoc-history)))

(defun godoc--get-buffer (query)
  "Get an empty buffer for a godoc QUERY."
  (let* ((buffer-name (concat "*godoc " query "*"))
         (buffer (get-buffer buffer-name)))
    ;; Kill the existing buffer if it already exists.
    (when buffer (kill-buffer buffer))
    (get-buffer-create buffer-name)))

(defun godoc--buffer-sentinel (proc event)
  "Sentinel function run when godoc command completes."
  (with-current-buffer (process-buffer proc)
    (cond ((string= event "finished\n")  ;; Successful exit.
           (goto-char (point-min))
           (godoc-mode)
           (display-buffer (current-buffer) t))
          ((/= (process-exit-status proc) 0)  ;; Error exit.
           (let ((output (buffer-string)))
             (kill-buffer (current-buffer))
             (message (concat "godoc: " output)))))))

(define-derived-mode godoc-mode special-mode "Godoc"
  "Major mode for showing Go documentation."
  (view-mode-enter))

;;;###autoload
(defun godoc (query)
  "Show Go documentation for QUERY, much like \\<go-mode-map>\\[man]."
  (interactive (list (godoc--read-query)))
  (go--godoc query godoc-command))

(defun go--godoc (query command)
  (unless (string= query "")
    (set-process-sentinel
     (start-process-shell-command "godoc" (godoc--get-buffer query)
                                  (concat command " " query))
     'godoc--buffer-sentinel)
    nil))

(defun godoc-at-point (point)
  "Show Go documentation for the identifier at POINT.

It uses `godoc-at-point-function' to look up the documentation."
  (interactive "d")
  (funcall godoc-at-point-function point))

(defun go-goto-imports ()
  "Move point to the block of imports.

If using

  import (
    \"foo\"
    \"bar\"
  )

it will move point directly behind the last import.

If using

  import \"foo\"
  import \"bar\"

it will move point to the next line after the last import.

If no imports can be found, point will be moved after the package
declaration."
  (interactive)
  ;; FIXME if there's a block-commented import before the real
  ;; imports, we'll jump to that one.

  ;; Generally, this function isn't very forgiving. it'll bark on
  ;; extra whitespace. It works well for clean code.
  (let ((old-point (point)))
    (goto-char (point-min))
    (cond
     ((re-search-forward "^import ()" nil t)
      (backward-char 1)
      'block-empty)
     ((re-search-forward "^import ([^)]+)" nil t)
      (backward-char 2)
      'block)
     ((re-search-forward "\\(^import \\([^\"]+ \\)?\"[^\"]+\"\n?\\)+" nil t)
      'single)
     ((re-search-forward "^[[:space:]\n]*package .+?\n" nil t)
      (message "No imports found, moving point after package declaration")
      'none)
     (t
      (goto-char old-point)
      (message "No imports or package declaration found. Is this really a Go file?")
      'fail))))

(defun go-play-buffer ()
  "Like `go-play-region', but acts on the entire buffer."
  (interactive)
  (go-play-region (point-min) (point-max)))

(defun go-play-region (start end)
  "Send the region between START and END to the Playground.
If non-nil `go-play-browse-function' is called with the
Playground URL."
  (interactive "r")
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          (encode-coding-string
           (buffer-substring-no-properties start end)
           'utf-8))
         (content-buf (url-retrieve
                       "https://play.golang.org/share"
                       (lambda (arg)
                         (cond
                          ((equal :error (car arg))
                           (signal 'go-play-error (cdr arg)))
                          (t
                           (re-search-forward "\n\n")
                           (let ((url (format "https://play.golang.org/p/%s"
                                              (buffer-substring (point) (point-max)))))
                             (when go-play-browse-function
                               (funcall go-play-browse-function url)))))))))))

;;;###autoload
(defun go-download-play (url)
  "Download a paste from the playground and insert it in a Go buffer.
Tries to look for a URL at point."
  (interactive (list (read-from-minibuffer "Playground URL: " (ffap-url-p (ffap-string-at-point 'url)))))
  (with-current-buffer
      (let ((url-request-method "GET") url-request-data url-request-extra-headers)
        (url-retrieve-synchronously (concat url ".go")))
    (let ((buffer (generate-new-buffer (concat (car (last (split-string url "/"))) ".go"))))
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (copy-to-buffer buffer (point) (point-max))
      (kill-buffer)
      (with-current-buffer buffer
        (go-mode)
        (switch-to-buffer buffer)))))

(defun go-propertize-syntax (start end)
  (save-excursion
    (goto-char start)
    (while (search-forward "\\" end t)
      (put-text-property (1- (point)) (point) 'syntax-table (if (= (char-after) ?`) '(1) '(9))))))

(defun go-import-add (arg import)
  "Add a new IMPORT to the list of imports.

When called with a prefix ARG asks for an alternative name to
import the package as.

If no list exists yet, one will be created if possible.

If an identical import has been commented, it will be
uncommented, otherwise a new import will be added."

  ;; - If there's a matching `// import "foo"`, uncomment it
  ;; - If we're in an import() block and there's a matching `"foo"`, uncomment it
  ;; - Otherwise add a new import, with the appropriate syntax
  (interactive
   (list
    current-prefix-arg
    (replace-regexp-in-string "^[\"']\\|[\"']$" "" (completing-read "Package: " (go-packages)))))
  (save-excursion
    (let (as line import-start)
      (if arg
          (setq as (read-from-minibuffer "Import as: ")))
      (if as
          (setq line (format "%s \"%s\"" as import))
        (setq line (format "\"%s\"" import)))

      (goto-char (point-min))
      (if (re-search-forward (concat "^[[:space:]]*//[[:space:]]*import " line "$") nil t)
          (uncomment-region (line-beginning-position) (line-end-position))
        (cl-case (go-goto-imports)
          ('fail (message "Could not find a place to add import."))
          ('block-empty
           (insert "\n\t" line "\n"))
          ('block
              (save-excursion
                (re-search-backward "^import (")
                (setq import-start (point)))
            (if (re-search-backward (concat "^[[:space:]]*//[[:space:]]*" line "$")  import-start t)
                (uncomment-region (line-beginning-position) (line-end-position))
              (insert "\n\t" line)))
          ('single (insert "import " line "\n"))
          ('none (insert "\nimport (\n\t" line "\n)\n")))))))

(defun go-root-and-paths ()
  (let* ((output (process-lines go-command "env" "GOROOT" "GOPATH"))
         (root (car output))
         (paths (split-string (cadr output) path-separator)))
    (cons root paths)))

(defun go--string-prefix-p (s1 s2 &optional ignore-case)
  "Return non-nil if S1 is a prefix of S2.
If IGNORE-CASE is non-nil, the comparison is case-insensitive."
  (eq t (compare-strings s1 nil nil
                         s2 0 (length s1) ignore-case)))

(defun go--directory-dirs (dir)
  "Recursively return all subdirectories in DIR."
  (if (file-directory-p dir)
      (let ((dir (directory-file-name dir))
            (dirs '())
            (files (directory-files dir nil nil t)))
        (dolist (file files)
          (unless (member file '("." ".."))
            (let ((file (concat dir "/" file)))
              (if (file-directory-p file)
                  (setq dirs (append (cons file
                                           (go--directory-dirs file))
                                     dirs))))))
        dirs)
    '()))


(defun go-packages ()
  (funcall go-packages-function))

(defun go-packages-native ()
  "Return a list of all installed Go packages.
It looks for archive files in /pkg/."
  (sort
   (delete-dups
    (cl-mapcan
     (lambda (topdir)
       (let ((pkgdir (concat topdir "/pkg/")))
         (cl-mapcan (lambda (dir)
                   (mapcar (lambda (file)
                             (let ((sub (substring file (length pkgdir) -2)))
                               (unless (or (go--string-prefix-p "obj/" sub) (go--string-prefix-p "tool/" sub))
                                 (mapconcat #'identity (cdr (split-string sub "/")) "/"))))
                           (if (file-directory-p dir)
                               (directory-files dir t "\\.a$"))))
                 (if (file-directory-p pkgdir)
                     (go--directory-dirs pkgdir)))))
     (go-root-and-paths)))
   #'string<))

(defun go-packages-go-list ()
  "Return a list of all Go packages, using `go list'."
  (process-lines go-command "list" "-e" "all"))

(defun go-unused-imports-lines ()
  (reverse (remove nil
                   (mapcar
                    (lambda (line)
                      (when (string-match "^\\(.+\\):\\([[:digit:]]+\\): imported and not used: \".+\".*$" line)
                        (let ((error-file-name (match-string 1 line))
                              (error-line-num (match-string 2 line)))
                          (if (string= (file-truename error-file-name) (file-truename buffer-file-name))
                              (string-to-number error-line-num)))))
                    (split-string (shell-command-to-string
                                   (concat go-command
                                           (if (string-match "_test\\.go$" buffer-file-truename)
                                               " test -c"
                                             (concat " build -o " null-device))
                                           " -gcflags=-e"
                                           " "
                                           (shell-quote-argument (file-truename buffer-file-name)))) "\n")))))

(defun go-remove-unused-imports (arg)
  "Remove all unused imports.
If ARG is non-nil, unused imports will be commented, otherwise
they will be removed completely."
  (interactive "P")
  (save-excursion
    (let ((cur-buffer (current-buffer)) flymake-state lines)
      (when (boundp 'flymake-mode)
        (setq flymake-state flymake-mode)
        (flymake-mode-off))
      (save-some-buffers nil (lambda () (equal cur-buffer (current-buffer))))
      (if (buffer-modified-p)
          (message "Cannot operate on unsaved buffer")
        (setq lines (go-unused-imports-lines))
        (dolist (import lines)
          (go--goto-line import)
          (beginning-of-line)
          (if arg
              (comment-region (line-beginning-position) (line-end-position))
            (go--delete-whole-line)))
        (message "Removed %d imports" (length lines)))
      (if flymake-state (flymake-mode-on)))))

(defun godef--find-file-line-column (specifier other-window)
  "Given a file name in the format of `filename:line:column',
visit FILENAME and go to line LINE and column COLUMN."
  (if (not (string-match "\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)" specifier))
      ;; We've only been given a directory name
      (funcall (if other-window #'find-file-other-window #'find-file) specifier)
    (let ((filename (match-string 1 specifier))
          (line (string-to-number (match-string 2 specifier)))
          (column (string-to-number (match-string 3 specifier))))
      (funcall (if other-window #'find-file-other-window #'find-file) filename)
      (go--goto-line line)
      (beginning-of-line)
      (forward-char (1- column))
      (if (buffer-modified-p)
          (message "Buffer is modified, file position might not have been correct")))))

(defun godef--call (point)
  "Call godef, acquiring definition position and expression
description at POINT."
  (if (not (buffer-file-name (go--coverage-origin-buffer)))
      (error "Cannot use godef on a buffer without a file name")
    (let ((outbuf (generate-new-buffer "*godef*"))
          (coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))
      (prog2
          (call-process-region (point-min)
                               (point-max)
                               godef-command
                               nil
                               outbuf
                               nil
                               "-i"
                               "-t"
                               "-f"
                               (file-truename (buffer-file-name (go--coverage-origin-buffer)))
                               "-o"
                               (number-to-string (position-bytes point)))
          (with-current-buffer outbuf
            (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
        (kill-buffer outbuf)))))

(defun godef--successful-p (output)
  (not (or (string= "-" output)
           (string= "godef: no identifier found" output)
           (go--string-prefix-p "godef: no declaration found for " output)
           (go--string-prefix-p "error finding import path for " output))))

(defun godef--error (output)
  (cond
   ((godef--successful-p output)
    nil)
   ((string= "-" output)
    "godef: expression is not defined anywhere")
   (t
    output)))

(defun godef-describe (point)
  "Describe the expression at POINT."
  (interactive "d")
  (condition-case nil
      (let ((description (cdr (butlast (godef--call point) 1))))
        (if (not description)
            (message "No description found for expression at point")
          (message "%s" (mapconcat #'identity description "\n"))))
    (file-error (message "Could not run godef binary"))))

(defun godef-jump (point &optional other-window)
  "Jump to the definition of the expression at POINT."
  (interactive "d")
  (condition-case nil
      (let ((file (car (godef--call point))))
        (if (not (godef--successful-p file))
            (message "%s" (godef--error file))
          (push-mark)
          (if (eval-when-compile (fboundp 'xref-push-marker-stack))
              ;; TODO: Integrate this facility with XRef.
              (xref-push-marker-stack)
            (ring-insert find-tag-marker-ring (point-marker)))
          (godef--find-file-line-column file other-window)))
    (file-error (message "Could not run godef binary"))))

(defun godef-jump-other-window (point)
  (interactive "d")
  (godef-jump point t))

(defun go--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun go--line-column-to-point (line column)
  (save-excursion
    (go--goto-line line)
    (forward-char (1- column))
    (point)))

(cl-defstruct go--covered
  start-line start-column end-line end-column covered count)

(defun go--coverage-file ()
  "Return the coverage file to use, either by reading it from the
current coverage buffer or by prompting for it."
  (if (boundp 'go--coverage-current-file-name)
      go--coverage-current-file-name
    (read-file-name "Coverage file: " nil nil t)))

(defun go--coverage-origin-buffer ()
  "Return the buffer to base the coverage on."
  (or (buffer-base-buffer) (current-buffer)))

(defun go--coverage-face (count divisor)
  "Return the intensity face for COUNT when using DIVISOR
to scale it to a range [0,10].

DIVISOR scales the absolute cover count to values from 0 to 10.
For DIVISOR = 0 the count will always translate to 8."
  (let* ((norm (cond
                ((= count 0)
                 -0.1) ;; Uncovered code, set to -0.1 so n becomes 0.
                ((= divisor 0)
                 0.8) ;; covermode=set, set to 0.8 so n becomes 8.
                (t
                 (/ (log count) divisor))))
         (n (1+ (floor (* norm 9))))) ;; Convert normalized count [0,1] to intensity [0,10]
    (concat "go-coverage-" (number-to-string n))))

(defun go--coverage-make-overlay (range divisor)
  "Create a coverage overlay for a RANGE of covered/uncovered code.
Use DIVISOR to scale absolute counts to a [0,10] scale."
  (let* ((count (go--covered-count range))
         (face (go--coverage-face count divisor))
         (ov (make-overlay (go--line-column-to-point (go--covered-start-line range)
                                                     (go--covered-start-column range))
                           (go--line-column-to-point (go--covered-end-line range)
                                                     (go--covered-end-column range)))))

    (overlay-put ov 'face face)
    (overlay-put ov 'help-echo (format "Count: %d" count))))

(defun go--coverage-clear-overlays ()
  "Remove existing overlays and put a single untracked overlay
over the entire buffer."
  (remove-overlays)
  (overlay-put (make-overlay (point-min) (point-max))
               'face
               'go-coverage-untracked))

(defun go--coverage-parse-file (coverage-file file-name)
  "Parse COVERAGE-FILE and extract coverage information and
divisor for FILE-NAME."
  (let (ranges
        (max-count 0))
    (with-temp-buffer
      (insert-file-contents coverage-file)
      (go--goto-line 2) ;; Skip over mode
      (while (not (eobp))
        (let* ((parts (split-string (buffer-substring (point-at-bol) (point-at-eol)) ":"))
               (file (car parts))
               (rest (split-string (nth 1 parts) "[., ]")))

          (cl-destructuring-bind
              (start-line start-column end-line end-column num count)
              (mapcar #'string-to-number rest)

            (when (string= (file-name-nondirectory file) file-name)
              (if (> count max-count)
                  (setq max-count count))
              (push (make-go--covered :start-line start-line
                                      :start-column start-column
                                      :end-line end-line
                                      :end-column end-column
                                      :covered (/= count 0)
                                      :count count)
                    ranges)))

          (forward-line)))

      (list ranges (if (> max-count 0) (log max-count) 0)))))

(defun go-coverage (&optional coverage-file)
  "Open a clone of the current buffer and overlay it with
coverage information gathered via go test -coverprofile=COVERAGE-FILE.

If COVERAGE-FILE is nil, it will either be inferred from the
current buffer if it's already a coverage buffer, or be prompted
for."
  (interactive)
  (let* ((cur-buffer (current-buffer))
         (origin-buffer (go--coverage-origin-buffer))
         (gocov-buffer-name (concat (buffer-name origin-buffer) "<gocov>"))
         (coverage-file (or coverage-file (go--coverage-file)))
         (ranges-and-divisor (go--coverage-parse-file
                              coverage-file
                              (file-name-nondirectory (buffer-file-name origin-buffer))))
         (cov-mtime (nth 5 (file-attributes coverage-file)))
         (cur-mtime (nth 5 (file-attributes (buffer-file-name origin-buffer)))))

    (if (< (float-time cov-mtime) (float-time cur-mtime))
        (message "Coverage file is older than the source file."))

    (with-current-buffer (or (get-buffer gocov-buffer-name)
                             (make-indirect-buffer origin-buffer gocov-buffer-name t))
      (set (make-local-variable 'go--coverage-current-file-name) coverage-file)

      (save-excursion
        (go--coverage-clear-overlays)
        (dolist (range (car ranges-and-divisor))
          (go--coverage-make-overlay range (cadr ranges-and-divisor))))

      (if (not (eq cur-buffer (current-buffer)))
          (display-buffer (current-buffer) `(,go-coverage-display-buffer-func))))))

(defun go-goto-function (&optional arg)
  "Go to the function defintion (named or anonymous) surrounding point.

If we are on a docstring, follow the docstring down.
If no function is found, assume that we are at the top of a file
and search forward instead.

If point is looking at the func keyword of an anonymous function,
go to the surrounding function.

If ARG is non-nil, anonymous functions are ignored."
  (interactive "P")
  (let ((p (point)))
    (cond
     ((save-excursion
        (beginning-of-line)
        (looking-at "^//"))
      ;; In case we are looking at the docstring, move on forward until we are
      ;; not anymore
      (beginning-of-line)
      (while (looking-at "^//")
        (forward-line 1))
      ;; If we are still not looking at a function, retry by calling self again.
      (when (not (looking-at "\\<func\\>"))
        (go-goto-function arg)))

     ;; If we're already looking at an anonymous func, look for the
     ;; surrounding function.
     ((and (looking-at "\\<func\\>")
           (not (looking-at "^func\\>")))
      (re-search-backward "\\<func\\>" nil t))

     ((not (looking-at "\\<func\\>"))
      ;; If point is on the "func" keyword, step back a word and retry
      (if (string= (symbol-name (symbol-at-point)) "func")
          (backward-word)
        ;; If we are not looking at the beginning of a function line, do a regexp
        ;; search backwards
        (re-search-backward "\\<func\\>" nil t))

      ;; If nothing is found, assume that we are at the top of the file and
      ;; should search forward instead.
      (when (not (looking-at "\\<func\\>"))
        (re-search-forward "\\<func\\>" nil t)
        (go--forward-word -1))

      ;; If we have landed at an anonymous function, it is possible that we
      ;; were not inside it but below it. If we were not inside it, we should
      ;; go to the containing function.
      (while (and (not (go--in-function-p p))
                  (not (looking-at "^func\\>")))
        (go-goto-function arg)))))

  (cond
   ((go-in-comment-p)
    ;; If we are still in a comment, redo the call so that we get out of it.
    (go-goto-function arg))

   ((and (looking-at "\\<func(") arg)
    ;; If we are looking at an anonymous function and a prefix argument has
    ;; been supplied, redo the call so that we skip the anonymous function.
    (go-goto-function arg))))

(defun go--goto-opening-curly-brace ()
  ;; Find the { that starts the function, i.e., the next { that isn't
  ;; preceded by struct or interface, or a comment or struct tag.  BUG:
  ;; breaks if there's a comment between the struct/interface keyword and
  ;; bracket, like this:
  ;;
  ;;     struct /* why? */ {
  (go--goto-return-values)
  (while (progn
           (skip-chars-forward "^{")
           (forward-char)
           (or (go-in-string-or-comment-p)
               (looking-back "\\(struct\\|interface\\)\\s-*{"
                             (line-beginning-position)))))
  (backward-char))

(defun go--in-function-p (compare-point)
  "Return t if COMPARE-POINT lies inside the function immediately surrounding point."
  (save-excursion
    (when (not (looking-at "\\<func\\>"))
      (go-goto-function))
    (let ((start (point)))
      (go--goto-opening-curly-brace)

      (unless (looking-at "{")
        (error "Expected to be looking at opening curly brace"))
      (forward-list 1)
      (and (>= compare-point start)
           (<= compare-point (point))))))

(defun go-goto-function-name (&optional arg)
  "Go to the name of the current function.

If the function is a test, place point after 'Test'.
If the function is anonymous, place point on the 'func' keyword.

If ARG is non-nil, anonymous functions are skipped."
  (interactive "P")
  (when (not (looking-at "\\<func\\>"))
    (go-goto-function arg))
  ;; If we are looking at func( we are on an anonymous function and
  ;; nothing else should be done.
  (when (not (looking-at "\\<func("))
    (let ((words 1)
          (chars 1))
      (when (looking-at "\\<func (")
        (setq words 3
              chars 2))
      (go--forward-word words)
      (forward-char chars)
      (when (looking-at "Test")
        (forward-char 4)))))

(defun go-goto-arguments (&optional arg)
  "Go to the arguments of the current function.

If ARG is non-nil, anonymous functions are skipped."
  (interactive "P")
  (go-goto-function-name arg)
  (go--forward-word 1)
  (forward-char 1))

(defun go--goto-return-values (&optional arg)
  "Go to the declaration of return values for the current function."
  (go-goto-arguments arg)
  (backward-char)
  (forward-list)
  (forward-char))

(defun go-goto-return-values (&optional arg)
  "Go to the return value declaration of the current function.

If there are multiple ones contained in a parenthesis, enter the parenthesis.
If there is none, make space for one to be added.

If ARG is non-nil, anonymous functions are skipped."
  (interactive "P")
  (go--goto-return-values arg)

  ;; Opening parenthesis, enter it
  (when (looking-at "(")
    (forward-char 1))

  ;; No return arguments, add space for adding
  (when (looking-at "{")
    (insert " ")
    (backward-char 1)))

(defun go-goto-method-receiver (&optional arg)
  "Go to the receiver of the current method.

If there is none, add parenthesis to add one.

Anonymous functions cannot have method receivers, so when this is called
interactively anonymous functions will be skipped.  If called programmatically,
an error is raised unless ARG is non-nil."
  (interactive "P")

  (when (and (not (called-interactively-p 'interactive))
             (not arg)
             (go--in-anonymous-funcion-p))
    (error "Anonymous functions cannot have method receivers"))

  (go-goto-function t)  ; Always skip anonymous functions
  (forward-char 5)
  (when (not (looking-at "("))
    (save-excursion
      (insert "() ")))
  (forward-char 1))

(defun go-goto-docstring (&optional arg)
  "Go to the top of the docstring of the current function.

If there is none, add one beginning with the name of the current function.

Anonymous functions do not have docstrings, so when this is called
interactively anonymous functions will be skipped.  If called programmatically,
an error is raised unless ARG is non-nil."
  (interactive "P")

  (when (and (not (called-interactively-p 'interactive))
             (not arg)
             (go--in-anonymous-funcion-p))
    (error "Anonymous functions do not have docstrings"))

  (go-goto-function t)
  (forward-line -1)
  (beginning-of-line)

  (while (looking-at "^//")
    (forward-line -1))
  (forward-line 1)
  (beginning-of-line)

  (cond
   ;; If we are looking at an empty comment, add a single space in front of it.
   ((looking-at "^//$")
    (forward-char 2)
    (insert (format " %s " (go--function-name t))))
   ;; If we are not looking at the function signature, we are looking at a docstring.
   ;; Move to the beginning of the first word of it.
   ((not (looking-at "^func"))
    (forward-char 3))
   ;; If we are still at the function signature, we should add a new docstring.
   (t
    (forward-line -1)
    (newline)
    (insert "// ")
    (insert (go--function-name t)))))

(defun go--function-name (&optional arg)
  "Return the name of the surrounding function.

If ARG is non-nil, anonymous functions will be ignored and the
name returned will be that of the top-level function.  If ARG is
nil and the surrounding function is anonymous, nil will be
returned."
  (when (or (not (go--in-anonymous-funcion-p))
            arg)
    (save-excursion
      (go-goto-function-name t)
      (symbol-name (symbol-at-point)))))

(defun go--in-anonymous-funcion-p ()
  "Return t if point is inside an anonymous function, nil otherwise."
  (save-excursion
    (go-goto-function)
    (looking-at "\\<func(")))

(defun go-guess-gopath (&optional buffer)
  "Determine a suitable GOPATH for BUFFER, or the current buffer if BUFFER is nil.

This function supports gb-based projects as well as Godep, in
addition to ordinary uses of GOPATH."
  (with-current-buffer (or buffer (current-buffer))
    (let ((gopath (cl-some (lambda (el) (funcall el))
                           go-guess-gopath-functions)))
      (if gopath
          (mapconcat
           (lambda (el) (file-truename el))
           gopath
           path-separator)))))

(defun go-plain-gopath ()
  "Detect a normal GOPATH, by looking for the first `src'
directory up the directory tree."
  (let ((d (locate-dominating-file buffer-file-name "src")))
    (if d
        (list d))))

(defun go-godep-gopath ()
  "Detect a Godeps workspace by looking for Godeps/_workspace up
the directory tree. The result is combined with that of
`go-plain-gopath'."
  (let* ((d (locate-dominating-file buffer-file-name "Godeps"))
         (workspace (concat d
                            (file-name-as-directory "Godeps")
                            (file-name-as-directory "_workspace"))))
    (if (and d
             (file-exists-p workspace))
        (list workspace
              (locate-dominating-file buffer-file-name "src")))))

(defun go-gb-gopath ()
  "Detect a gb project."
  (or (go--gb-vendor-gopath)
      (go--gb-vendor-gopath-reverse)))

(defun go--gb-vendor-gopath ()
  (let* ((d (locate-dominating-file buffer-file-name "src"))
         (vendor (concat d (file-name-as-directory "vendor"))))
    (if (and d
             (file-exists-p vendor))
        (list d vendor))))

(defun go--gb-vendor-gopath-reverse ()
  (let* ((d (locate-dominating-file buffer-file-name "vendor"))
         (src (concat d (file-name-as-directory "src"))))
    (if (and d
             (file-exists-p src))
        (list d (concat d
                        (file-name-as-directory "vendor"))))))

(defun go-wgo-gopath ()
  "Detect a wgo project."
  (or (go--wgo-gocfg "src")
      (go--wgo-gocfg "vendor")))

(defun go--wgo-gocfg (needle)
  (let* ((d (locate-dominating-file buffer-file-name needle))
         (gocfg (concat d (file-name-as-directory ".gocfg"))))
    (if (and d
             (file-exists-p gocfg))
        (with-temp-buffer
          (insert-file-contents (concat gocfg "gopaths"))
          (append
           (mapcar (lambda (el) (concat d (file-name-as-directory el))) (split-string (buffer-string) "\n" t))
           (list (go-original-gopath)))))))

(defun go-set-project (&optional buffer)
  "Set GOPATH based on `go-guess-gopath' for BUFFER, or the current buffer if BUFFER is nil.

If go-guess-gopath returns nil, that is if it couldn't determine
a valid value for GOPATH, GOPATH will be set to the initial value
of when Emacs was started.

This function can for example be used as a
projectile-switch-project-hook, or simply be called manually when
switching projects."
  (interactive)
  (let ((gopath (or (go-guess-gopath buffer)
                    (go-original-gopath))))
    (setenv "GOPATH" gopath)
    (message "Set GOPATH to %s" gopath)))

(defun go-reset-gopath ()
  "Reset GOPATH to the value it had when Emacs started."
  (interactive)
  (let ((gopath (go-original-gopath)))
    (setenv "GOPATH" gopath)
    (message "Set GOPATH to %s" gopath)))

(defun go-original-gopath ()
  "Return the original value of GOPATH from when Emacs was started."
  (let ((process-environment initial-environment)) (getenv "GOPATH")))

(defun go--insert-modified-files ()
  "Insert the contents of each modified Go buffer into the
current buffer in the format specified by guru's -modified flag."
  (mapc #'(lambda (b)
            (and (buffer-modified-p b)
                 (buffer-file-name b)
                 (string= (file-name-extension (buffer-file-name b)) "go")
                 (go--insert-modified-file (buffer-file-name b) b)))
        (buffer-list)))

(defun go--insert-modified-file (name buffer)
  (insert (format "%s\n%d\n" name (go--buffer-size-bytes buffer)))
  (insert-buffer-substring buffer))

(defun go--buffer-size-bytes (&optional buffer)
  (message "buffer; %s" buffer)
  "Return the number of bytes in the current buffer.
If BUFFER, return the number of characters in that buffer instead."
  (with-current-buffer (or buffer (current-buffer))
    (1- (position-bytes (point-max)))))


(provide 'go-mode)

;;; go-mode.el ends here

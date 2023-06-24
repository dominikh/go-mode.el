;;; go-mode.el --- Major mode for the Go programming language

;;; Commentary:

;; Copyright 2013 The go-mode Authors.  All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

;; Author: The go-mode Authors
;; Version: 1.6.0
;; Keywords: languages go
;; Package-Requires: ((emacs "26.1"))
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
(require 'xref)


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
  "Move up one level of parentheses.

Return non-nil if there was a paren to move up to."
  ;; The old implementation of go-goto-opening-parenthesis had an
  ;; optional argument to speed up the function.  It didn't change the
  ;; function's outcome.

  ;; Silently fail if there's no matching opening parenthesis.
  (let ((open-char (nth 1 (syntax-ppss))))
    (when open-char
      (goto-char open-char))))


(defconst go-dangling-operators-regexp "[^-]-\\|[^+]\\+\\|[/*&><.=|^]")
(defconst go--max-dangling-operator-length 2
  "The maximum length of dangling operators.
This must be at least the length of the longest string matched by
‘go-dangling-operators-regexp’ and must be updated whenever that
constant is changed.")

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

(defconst go--comment-start-regexp "[[:space:]]*\\(?:/[/*]\\)")
(defconst go--case-regexp "\\([[:space:]]*case\\([[:space:]]\\|$\\)\\)")
(defconst go--case-or-default-regexp (concat "\\(" go--case-regexp "\\|"  "[[:space:]]*default:\\)"))

(defconst go-builtins
  '("append"  "cap"    "clear"   "close" "complex"
    "copy"    "delete" "imag"    "len"   "make"
    "max"     "min"    "new"     "panic" "print"
    "println" "real"   "recover")
  "All built-in functions in the Go language.  Used for font locking.")

(defconst go-mode-keywords
  '("break"    "default"     "func"   "interface" "select"
    "case"     "defer"       "go"     "map"       "struct"
    "chan"     "else"        "goto"   "package"   "switch"
    "const"    "fallthrough" "if"     "range"     "type"
    "continue" "for"         "import" "return"    "var")
  "All keywords in the Go language.  Used for font locking.")

(defconst go-constants '("nil" "true" "false" "iota"))
(defconst go-type-name-regexp (concat "\\**\\(\\(?:" go-identifier-regexp "\\.\\)?" go-identifier-regexp "\\)"))

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

(defcustom go-fontify-variables t
  "Fontify variable declarations if this is non-nil."
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

(defcustom go-packages-function 'go-packages-go-list
  "Function called by `go-packages' to determine the list of available packages.
This is used in e.g. tab completion in `go-import-add'.

This package provides two functions: `go-packages-go-list' uses
'go list all' to determine all Go packages. `go-packages-native' uses
elisp to find all .a files in all /pkg/ directories.
`go-packages-native' is obsolete as it doesn't behave correctly with
the Go build cache or Go modules."
  :type 'function
  :package-version '(go-mode . 1.4.0)
  :group 'go)

(defcustom go-guess-gopath-functions (list #'go-plain-gopath)
  "Functions to call in sequence to detect a project's GOPATH.

The functions in this list will be called one after another,
until a function returns non-nil.  The order of the functions in
this list is important, as some project layouts may superficially
look like others."
  :type '(repeat function)
  :group 'go)

(make-obsolete-variable 'go-guess-gopath-functions "GOPATH has been deprecated in favour of Go modules." "1.7.0")

(defcustom go-confirm-playground-uploads t
  "Ask before uploading code to the public Go Playground.

Set this to nil to upload without prompting."
  :type 'boolean
  :group 'go)

(defcustom godoc-command "go doc"
  "Which executable to use for `godoc'.
This can be either an absolute path or an executable in PATH."
  :type 'string
  :group 'go)

(defcustom godoc-and-godef-command "go doc"
  "Which executable to use for `godoc-and-godef'.
This can be either an absolute path or an executable in PATH."
  :type 'string
  :group 'go)

(defcustom godoc-use-completing-read nil
  "Provide auto-completion for godoc.
Only really desirable when using `godoc' instead of `go doc'."
  :type 'boolean
  :group 'godoc)

(defcustom godoc-reuse-buffer nil
  "Reuse a single *godoc* buffer to display godoc-at-point calls.
The default behavior is to open a separate buffer for each call."
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
      ;; new artificial files, so this limitation will stay for now.
      (error "Cannot use gogetdoc on a buffer without a file name"))
  (let ((posn (format "%s:#%d" (file-truename buffer-file-name) (1- (position-bytes point))))
        (out (godoc--get-buffer "<at point>")))
  (with-temp-buffer
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

(defun go--fontify-type-switch-case-pre ()
  "Move point to line following the end of case statement.

This is used as an anchored font lock keyword PRE-MATCH-FORM. We
expand the font lock region to include multiline type switch case
statements."
  (save-excursion
    (beginning-of-line)
    (while (or (looking-at "[[:space:]]*\\($\\|//\\)") (go--line-suffix-p ","))
      (forward-line))
    (when (go--line-suffix-p ":")
      (forward-line))
    (point)))

(defun go--build-font-lock-keywords ()
  ;; we cannot use 'symbols in regexp-opt because GNU Emacs <24
  ;; doesn't understand that
  (append
   `(
     ;; Match param lists in func signatures. This uses the
     ;; MATCH-ANCHORED format (see `font-lock-keywords' docs).
     ;;
     ;; Parent/anchor match. It matches the param list opening "(".
     (go--match-param-start
      ;; Sub-matcher that matches individual params in the param list.
      (go--fontify-param
       ;; Pre-match form that runs before the first sub-match.
       (go--fontify-param-pre)
       ;; Post-match form that runs after last sub-match.
       (go--fontify-param-post)
       ;; Subexp 1 is the param variable name, if any.
       (1 font-lock-variable-name-face nil t)
       ;; Subexp 2 is the param type name, if any. We set the LAXMATCH
       ;; flag to allow optional regex groups.
       (2 font-lock-type-face nil t)))

     ;; Special case to match non-parenthesized function results. For
     ;; example, "func(i int) string".
     (go--match-single-func-result 1 font-lock-type-face)

     ;; Match name+type pairs, such as "foo bar" in "var foo bar".
     (go--match-ident-type-pair 2 font-lock-type-face)

     ;; An anchored matcher for type switch case clauses.
     (go--match-type-switch-case
      (go--fontify-type-switch-case
       (go--fontify-type-switch-case-pre)
       nil
       (1 font-lock-type-face)))

     ;; Match variable names in var decls, constant names in const
     ;; decls, and type names in type decls.
     (go--match-decl
      (1 font-lock-variable-name-face nil t)
      (2 font-lock-constant-face nil t)
      (3 font-lock-type-face nil t))

     (,(concat "\\_<" (regexp-opt go-mode-keywords t) "\\_>") . font-lock-keyword-face)
     (,(concat "\\(\\_<" (regexp-opt go-builtins t) "\\_>\\)[[:space:]]*(") 1 font-lock-builtin-face)
     (,(concat "\\_<" (regexp-opt go-constants t) "\\_>") . font-lock-constant-face)

     ;; Function (not method) name
     (,go-func-regexp 1 font-lock-function-name-face))

   (if go-fontify-function-calls
       ;; Function call/method name
       `((,(concat "\\(" go-identifier-regexp "\\)[[:space:]]*(") 1 font-lock-function-name-face)
         ;; Bracketed function call
         (,(concat "[^[:word:][:multibyte:]](\\(" go-identifier-regexp "\\))[[:space:]]*(") 1 font-lock-function-name-face))
     ;; Method name
     `((,go-func-meth-regexp 2 font-lock-function-name-face)))

   `(
     ;; Raw string literal, needed for font-lock-syntactic-keywords
     ("\\(`[^`]*`\\)" 1 font-lock-multiline)

     ;; RHS of type alias.
     (go--match-type-alias 2 font-lock-type-face)

     ;; Arrays/slices: []<type> | [123]<type> | [some.Const]<type> | [someConst]<type> | [...]<type>
     (,(concat "\\(?:^\\|[^[:word:][:multibyte:]]\\)\\[\\(?:[[:digit:]]+\\|" go-qualified-identifier-regexp "\\|" go-identifier-regexp "\\|\\.\\.\\.\\)?\\]" go-type-name-regexp) 1 font-lock-type-face)

     ;; Unary "!"
     ("\\(!\\)[^=]" 1 font-lock-negation-char-face)

     ;; Composite literal type
     (,(concat go-type-name-regexp "{") 1 font-lock-type-face)

     ;; Map value type
     (go--match-map-value 1 font-lock-type-face)

     ;; Map key type
     (,(concat "\\_<map\\_>\\[" go-type-name-regexp) 1 font-lock-type-face)

     ;; Channel type
     (,(concat "\\_<chan\\_>[[:space:]]*\\(?:<-[[:space:]]*\\)?" go-type-name-regexp) 1 font-lock-type-face)

     ;; "new()"/"make()" type
     (,(concat "\\_<\\(?:new\\|make\\)\\_>\\(?:[[:space:]]\\|)\\)*(" go-type-name-regexp) 1 font-lock-type-face)

     ;; Type assertion
     (,(concat "\\.\\s *(" go-type-name-regexp) 1 font-lock-type-face)

     ;; Composite literal field names and label definitions.
     (go--match-ident-colon 1 font-lock-constant-face)

     ;; Labels in goto/break/continue
     (,(concat "\\_<\\(?:goto\\|break\\|continue\\)\\_>[[:space:]]*\\(" go-label-regexp "\\)") 1 font-lock-constant-face))))

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
  "Return non-nil if the current line is a continuation line.
The return value is cached based on the current `line-beginning-position'."
  (let* ((line-begin (line-beginning-position))
         (val (gethash line-begin go-dangling-cache 'nope)))
    (when (or (go--buffer-narrowed-p) (equal val 'nope))
      (save-excursion
        (go--forward-line -1)
        (if (go--current-line-has-dangling-op-p)
            (setq val (line-end-position))
          (setq val nil))

        (if (not (go--buffer-narrowed-p))
            (puthash line-begin val go-dangling-cache))))
    val))

(defun go--current-line-has-dangling-op-p ()
  "Return non-nil if current line ends in a dangling operator.
The return value is not cached."
  (or
   (and
    (go--line-suffix-p go-dangling-operators-regexp)

    ;; "=" does not behave like a dangling operator in decl statements.
    (not (go--line-suffix-p "\\(?:var\\|type\\|const\\)[[:space:]].*="))

    ;; Don't mistake "1234." for a dangling operator.
    (not (go--line-suffix-p "[[:space:]]-?[[:digit:]][_0-9]*\\.")))

   ;; treat comma as dangling operator in certain cases
   (and
    (go--line-suffix-p ",")
    (save-excursion (end-of-line) (go--commas-indent-p)))))


(defun go--commas-indent-p ()
  "Return non-nil if in a context where dangling commas indent next line."
  (not (or
        (go--open-paren-position)
        (go--in-composite-literal-p)
        (go--in-case-clause-list-p)
        (go--in-struct-definition-p))))

(defun go--in-case-clause-list-p ()
  "Return non-nil if inside a multi-line case cause list.

This function is only concerned with list items on lines after the
case keyword. It returns nil for the case line itself."
  (save-excursion
    (beginning-of-line)
    (when (not (looking-at go--case-or-default-regexp))
      (let (saw-colon)
        ;; optionally skip line with the colon
        (when (go--line-suffix-p ":")
          (setq saw-colon t)
          (forward-line -1))

        ;; go backwards while at a comment or a line ending in comma
        (while (and
                (or
                 (go--boring-line-p)
                 (go--line-suffix-p ","))
                (not (looking-at go--case-regexp))
                (go--forward-line -1)))

        (and
         (looking-at-p go--case-regexp)
         ;; we weren't in case list if first line ended in colon
         ;; and the "case" line ended in colon
         (not (and saw-colon (looking-at ".*:[[:space:]]*$"))))))))

(defun go--in-composite-literal-p ()
  "Return non-nil if point is in a composite literal."
  (save-excursion
    (save-match-data
      (and
       (go-goto-opening-parenthesis)

       ;; Opening paren-like character is a curly.
       (eq (char-after) ?{)

       (or
        ;; Curly is preceded by non space (e.g. "Foo{"), definitely
        ;; composite literal.
        (zerop (skip-syntax-backward " "))

        ;; Curly preceded by comma or semicolon. This is a composite
        ;; literal with implicit type name.
        (looking-back "[,:]" (1- (point)))

        ;; If we made it to the beginning of line we are either a naked
        ;; block or a composite literal with implicit type name. If we
        ;; are the latter, we must be contained in another composite
        ;; literal.
        (and (bolp) (go--in-composite-literal-p)))))))

(defun go--in-paren-with-prefix-p (paren prefix)
  (save-excursion
    (and
     (go-goto-opening-parenthesis)
     (eq (char-after) paren)
     (skip-syntax-backward " ")
     (> (point) (length prefix))
     (string= prefix (buffer-substring (- (point) (length prefix)) (point))))))

(defun go--in-struct-definition-p ()
  "Return non-nil if point is inside a struct definition."
  (go--in-paren-with-prefix-p ?{ "struct"))

(defun go--in-interface-p ()
  "Return non-nil if point is inside an interface definition."
  (go--in-paren-with-prefix-p ?{ "interface"))


(defun go--in-type-switch-p ()
  "Return non-nil if point is inside a type switch statement."
  (go--in-paren-with-prefix-p ?{ ".(type)"))

(defun go--open-paren-position ()
  "Return non-nil if point is between '(' and ')'.

The return value is the position of the opening paren."
  (save-excursion
    (let ((start-paren-level (go-paren-level)))
      (and
       (go-goto-opening-parenthesis)

       ;; opening paren-like character is actually a paren
       (eq (char-after) ?\()

       ;; point is before the closing paren
       (< (go-paren-level) start-paren-level)

       (point)))))

(defun go-indentation-at-point ()
  "Return the appropriate indentation for the current line."
  (save-excursion
    (beginning-of-line)

    (if (go-in-comment-p)
        (go--multiline-comment-indent)
      (go--indentation-at-point))))

;; It's unfortunate that the user cannot reindent the current line to
;; align with the previous line; however, if they could, then people
;; who use reindent-then-newline-and-indent wouldn't be able to
;; explicitly indent lines inside comments.
(defun go--multiline-comment-indent ()
  "Return the appropriate indent inside multiline comment.

Assumes point is at beginning of line within comment. This
function has basic logic to indent as you add new lines to a
multiline comment, and to line up all the `*' if each line starts
with `*'. The gofmt behavior for multiline comments is
surprisingly complex and strange/buggy, so we just aim to do
something simple rather than encode all the subtle behavior."
  (let* (;; Indent of current line.
         (indent (current-indentation))
         ;; Indent of opening "/*".
         start-indent
         ;; Default indent to use based on preceding context.
         natural-indent
         ;; Non-nil means keep existing indent and give up calculating indent.
         give-up
         ;; Whether all comment lines (except first) begin with "*".
         (all-star t))

    (save-excursion
      (go-goto-beginning-of-string-or-comment)

      (setq start-indent (current-indentation))

      ;; If other stuff precedes start of multiline comment, give up.
      (setq give-up (/= (current-column) start-indent))

      ;; Skip "/*".
      (forward-char 2)

      (skip-syntax-forward " ")

      (if (not (eolp))
          ;; If we aren't at EOL, we have content on the first line.
          ;; Base our natural indent on that.
          (setq natural-indent (current-column))
        ;; Otherwise default to 1 space beyond "/*".
        (setq natural-indent (+ start-indent 3)))

      (let (done)
        (while (not done)
          (setq done (or (looking-at ".*\\*/") (not (zerop (forward-line)))))
          (setq all-star (and all-star (looking-at "[[:space:]]*\\*"))))))

    ;; If previous line has comment content, use its indent as our
    ;; natural indent.
    (save-excursion
      (when (zerop (forward-line -1))
        (beginning-of-line)
        (when (and (go-in-comment-p) (> (current-indentation) 0))
          (setq natural-indent (current-indentation)))))

    (cond
     (give-up indent)

     (all-star (1+ start-indent))

     ;; Closing "*/" with no preceding content always lines up with "/*".
     ((looking-at "[[:space:]]*\\*/") start-indent)

     ;; If the line is already indented, leave it.
     (t (if (zerop indent) natural-indent indent)))))

(defun go--indentation-at-point ()
  "Return the appropriate indentation for the current non-comment line.

This function works by walking a line's characters backwards. When it
encounters a closing paren or brace it bounces to the corresponding
opener. If it arrives at the beginning of the line you are indenting,
it moves to the end of the previous line if the current line is a
continuation line, else it moves to the containing opening paren or
brace. If it arrives at the beginning of a line other than the line
you are indenting, it will continue to the previous dangling line if
the line you are indenting was not a continuation line, otherwise it
is done."
  (save-excursion
    (beginning-of-line)

    (let (
          ;; Beginning of our starting line.
          (start-line (point))

          ;; Whether this is our first iteration of the outer while loop.
          (first t)

          ;; Whether we start in a block (i.e. our first line is not a
          ;; continuation line and is in an "if", "for", etc. block).
          (in-block)

          ;; Our desired indent relative to our ending line's indent.
          (indent 0))

      ;; Skip leading whitespace.
      (skip-syntax-forward " ")

      ;; Decrement indent if the first character on the line is a closer.
      (when (or (eq (char-after) ?\)) (eq (char-after) ?}))
        (cl-decf indent tab-width))

      (while (or
              ;; Always run the first iteration so we process empty lines.
              first

              ;; Otherwise stop if we are at the start of a line.
              (not (bolp)))
        (setq first nil)

        (cl-case (char-before)

          ;; We have found a closer (paren or brace).
          ((?\) ?})
           (backward-char)
           (let ((bol (line-beginning-position)))

             ;; Jump back to corresponding opener.
             (go-goto-opening-parenthesis)

             ;; Here we decrement the indent if we are closing an indented
             ;; expression. In other words, the closer's line was indented
             ;; relative to the opener's line, and that indent should not
             ;; be inherited by our starting line.
             (when (and
                    ;; We care about dangling expressions, not child blocks.
                    (not in-block)

                    ;; Opener and closer aren't on same line.
                    (< (point) bol)

                    (go-previous-line-has-dangling-op-p)

                    ;; Opener is at same paren level as start of line (ignore sub-expressions).
                    (eq (go-paren-level) (save-excursion (beginning-of-line) (go-paren-level)))

                    ;; This dangling line opened indent relative to previous dangling line.
                    (go--continuation-line-indents-p))
               (cl-decf indent tab-width))))

          ;; Brackets don't affect indentation, so just skip them.
          ((?\])
           (backward-char)))

        ;; Skip non-closers since we are only interested in closing parens/braces.
        (skip-syntax-backward "^)" (line-beginning-position))

        (when (go-in-string-or-comment-p)
          (go-goto-beginning-of-string-or-comment))

        ;; At the beginning of the starting line.
        (when (= start-line (point))

          ;; We are a continuation line.
          (if (go-previous-line-has-dangling-op-p)
              (progn
                ;; Presume a continuation line always gets an extra indent.
                ;; We reduce the indent after the loop, if necessary.
                (cl-incf indent tab-width)

                ;; Go to the end of the dangling line.
                (goto-char (go-previous-line-has-dangling-op-p)))

            ;; If we aren't a continuation line and we have an enclosing paren
            ;; or brace, jump to opener and increment our indent.
            (when (go-goto-opening-parenthesis)
              (setq in-block (go--flow-block-p))
              (cl-incf indent tab-width))))

        ;; If we started in a child block we must follow dangling lines
        ;; until they don't dangle anymore. This is to handle cases like:
        ;;
        ;; if foo ||
        ;;      foo &&
        ;;        foo {
        ;;   X
        ;;
        ;; There can be an arbitrary number of indents, so we must go back to
        ;; the "if" to determine the indent of "X".
        (when (and in-block (bolp) (go-previous-line-has-dangling-op-p))
          (goto-char (go-previous-line-has-dangling-op-p))))

      ;; If our ending line is a continuation line but doesn't open
      ;; an extra indent, reduce indent. We tentatively gave indents to all
      ;; dangling lines and all lines inside open parens, so here we take that
      ;; indent back.
      ;;
      ;;                1 +                      1 +
      ;; ending line      1 + foo(                 1 + foo(
      ;; starting line      1,        becomes      1,
      ;;                  )                      )
      ;;
      ;;
      ;;                1 +                     1 +
      ;; ending line      1 +         becomes     1 +
      ;; starting line      1                     1
      (when (and
             (go-previous-line-has-dangling-op-p)
             (not (go--continuation-line-indents-p)))
        (cl-decf indent tab-width))

      ;; Apply our computed indent relative to the indent of the
      ;; ending line, or 0 if we are at the top level.
      (if (and
           (= 0 (go-paren-level))
           (not (go-previous-line-has-dangling-op-p)))
          indent
        (+ indent (current-indentation))))))

(defconst go--operator-chars "*/%<>&\\^+\\-|=!,."
  "Individual characters that appear in operators.
Comma and period are included because they can be dangling operators, so
they need to be considered by `go--continuation-line-indents-p'")

(defun go--operator-precedence (op)
  "Go operator precedence (higher binds tighter)."
  (cl-case (intern op)
    (\. 7) ; "." in "foo.bar", binds tightest
    (! 6)
    ((* / % << >> & &^) 5)
    ((+ - | ^) 4)
    ((== != < <= > >=) 3)
    (&& 2)
    (|| 1)
    (t 0)))

(defun go--flow-block-p ()
  "Return whether looking at a { that opens a control flow block.

We check for a { that is preceded by a space and is not a func
literal opening brace."
  (save-excursion
    (when (and
           (eq (char-after) ?{)
           (not (zerop (skip-syntax-backward " "))))

      (let ((eol (line-end-position))
            (level (go-paren-level))
            (found-func-literal))

        (beginning-of-line)

        ;; See if we find any "func" keywords on this line at the same paren
        ;; level as the curly.
        (while (and
                (not found-func-literal)
                (re-search-forward "\\_<func\\_>" eol t))
          (setq found-func-literal (and
                                    (= level (go-paren-level))
                                    (not (go-in-string-or-comment-p)))))
        (not found-func-literal)))))

(defun go--continuation-line-indents-p ()
  "Return non-nil if the current continuation line opens an additional indent.

This function works by looking at the Go operators used on the current
line. If all the operators bind tighter than the previous line's
dangling operator and the current line ends in a dangling operator or
open paren, the next line will have an additional indent.

For example:
foo ||
  foo && // this continuation line opens another indent
    foo"
  (save-excursion
    (let (prev-op (all-tighter t))

      ;; Record the dangling operator from previous line.
      (save-excursion
        (goto-char (go-previous-line-has-dangling-op-p))
        (go--end-of-line)
        (skip-syntax-backward " ")
        (let ((end (point)))
          (skip-chars-backward go--operator-chars)
          (setq prev-op (buffer-substring-no-properties (point) end))))

      (beginning-of-line)

      (when (or
             ;; We can only open indent if we have a dangling operator, or
             (go--current-line-has-dangling-op-p)

             (save-excursion
               (go--end-of-line)
               (backward-char)
               (or
                ;; Line ends in a "(" or ",", or
                (eq (char-after) ?\()
                (eq (char-after) ?,)

                ;; Line ends in a "{" that isn't a control block.
                (and
                 (eq (char-after) ?{)
                 (not (go--flow-block-p))))))

        (let ((prev-precedence (go--operator-precedence prev-op))
              (start-depth (go-paren-level))
              (line-start (line-beginning-position)))

          (end-of-line)

          ;; While we haven't found a looser operator and are on the starting line...
          (while (and all-tighter (> (point) line-start))

            ;; Skip over non-operator characters.
            (skip-chars-backward (concat "^" go--operator-chars) line-start)

            (let ((end (point)))
              (cond
               ;; Ignore sub-expressions at different paren levels.
               ((/= (go-paren-level) start-depth)
                (skip-syntax-backward "^()"))

               ((go-in-string-or-comment-p)
                (go-goto-beginning-of-string-or-comment))

               ;; We found an operator. Check if it has lower precedence.
               ((/= (skip-chars-backward go--operator-chars) 0)
                (when (>=
                       prev-precedence
                       (go--operator-precedence (buffer-substring (point) end)))
                  (setq all-tighter nil)))))))
        all-tighter))))

(defun go--end-of-line ()
  "Move to the end of the code on the current line.
Point will be left before any trailing comments. Point will be left
after the opening backtick of multiline strings."
  (end-of-line)
  (let ((keep-going t))
    (while keep-going
      (skip-syntax-backward " ")
      (when (looking-back "\\*/" (- (point) 2))
        ;; back up so we are in the /* comment */
        (backward-char))
      (if (go-in-comment-p)
          (go-goto-beginning-of-string-or-comment)
        (setq keep-going nil))))
  (when (go-in-string-p)
    (go-goto-beginning-of-string-or-comment)
    ;; forward one so point is after the opening "`"
    (forward-char)))

(defun go--line-suffix-p (re)
  "Return non-nil if RE matches the end of the line starting from `point'.

Trailing whitespace, trailing comments and trailing multiline strings are
ignored."
  (let ((start (point))
        (end (save-excursion (go--end-of-line) (point))))
    (when (< start end)
      (string-match-p
       (concat "\\(?:" re "\\)$")
       (buffer-substring-no-properties start end)))))

(defun go--boring-line-p ()
  "Return non-nil if the current line probably doesn't impact indentation.

A boring line is one that starts with a comment, is empty, is part of a
multiline comment, or starts and ends in a multiline string."
  (or
   (looking-at (concat go--comment-start-regexp "\\|[[:space:]]*$"))
   (go-in-comment-p)
   (and (go-in-string-p) (save-excursion (end-of-line) (go-in-string-p)))))

(defun go--forward-line (&optional count)
  "Like `forward-line' but skip comments and empty lines.

Return non-nil if point changed lines."
  (let (moved)
    (while (and
            (zerop (forward-line count))
            (setq moved t)
            (go--boring-line-p))
      (setq count (if (and count (< count 0 )) -1 1)))
    moved))

(defun go--case-comment-p (indent)
  "Return non-nil if looking at a comment attached to a case statement.

INDENT is the normal indent of this line, i.e. that of the case body."
  (when (and
         (> (current-indentation) 0)
         (looking-at go--comment-start-regexp))

    (let (switch-before
          case-after
          has-case-aligned-preceding-comment)

      (save-excursion
        ;; Search for previous case-aligned comment.
        (while (and
                (zerop (forward-line -1))
                (cond
                 ((looking-at "^[[:space:]]*$"))

                 ((looking-at go--comment-start-regexp)
                  (when (= (current-indentation) (- indent tab-width))
                    (setq has-case-aligned-preceding-comment t))
                  t)

                 ((go-in-comment-p)))))

        ;; Record if a switch (or select) precedes us.
        (setq switch-before (looking-at "^[[:space:]]*\\(switch\\|select\\)[[:space:]]")))

      ;; Record if first proceeding non-comment line is a case statement.
      (save-excursion
        (while (and
                (zerop (forward-line 1))
                (or
                 (looking-at go--comment-start-regexp)
                 (looking-at "^[[:space:]]*$")
                 (go-in-comment-p))))

        (setq case-after (looking-at go--case-or-default-regexp)))

      (and
       ;; a "case" statement comes after our comment
       case-after

       (or
        ;; "switch" statement precedes us, always align with "case"
        switch-before

        ;; a preceding comment is aligned with "case", we should too
        has-case-aligned-preceding-comment

        ;; other cases are ambiguous, so if comment is currently
        ;; aligned with "case", leave it that way
        (= (current-indentation) (- indent tab-width)))))))

(defun go-mode-indent-line ()
  (interactive)
  (let (indent
        ;; case sensitively match "case", "default", etc.
        (case-fold-search nil)
        (pos (- (point-max) (point)))
        (point (point))
        (beg (line-beginning-position))
        (non-tab-indents 0))
    (back-to-indentation)
    (if (go-in-string-p)
        (goto-char point)
      (setq indent (go-indentation-at-point))
      (when (or
             (and
              (looking-at (concat go-label-regexp ":\\([[:space:]]*/.+\\)?$\\|" go--case-or-default-regexp))
              ;; don't think last part of multiline case statement is a label
              (not (go-previous-line-has-dangling-op-p))
              (not (go--in-case-clause-list-p))
              (not (go--in-composite-literal-p)))

             ;; comment attached above a "case" statement
             (go--case-comment-p indent))
        (cl-decf indent tab-width))

      ;; Don't do anything if current indent is correct.
      (when (/= indent (current-column))
        ;; Don't use tabs for indenting beyond "/*" in multiline
        ;; comments. They don't play well with gofmt.
        (when (go-in-comment-p)
          (save-excursion
            (go-goto-beginning-of-string-or-comment)
            (when (> indent (current-indentation))
              (setq non-tab-indents (- indent (current-indentation)))
              (setq indent (current-indentation)))))

        (delete-region beg (point))
        (indent-to indent)
        (insert-char ?  non-tab-indents))

      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))))

(defun go-beginning-of-defun (&optional count)
  (when (and (not (go-in-string-or-comment-p))
			 (not (bolp))
			 (save-excursion
			   (beginning-of-line)
			   (looking-at go-func-meth-regexp)))
	;; Point is already somewhere on the function definition. Move to the end of line so that searching backwards finds
	;; it. We don't go to the end of line unconditionally because that confuses evil-mode
	;; (https://github.com/dominikh/go-mode.el/issues/186)
	;;
	;; If point is already at the beginning of line and looking at a function, then we want go-beginning-of-defun to
	;; jump to the previous function instead.
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


(defvar go--fontify-param-has-name nil
  "Whether the current params list has names.

This is used during fontification of function signatures.")

(defvar go--fontify-param-beg nil
  "Position of \"(\" starting param list.

This is used during fontification of function signatures.")

(defun go--fontify-param-pre ()
  "Set `go--fontify-param-has-name' and `go--fontify-param-beg' appropriately.

This is used as an anchored font lock keyword PRE-MATCH-FORM. We
must set `go--fontify-param-has-name' ahead of time because you
can't know if the param list is types only or names and types
until you see the end. For example:

// types only
func foo(int, string) {}

// names and types (don't know so until you see the \"int\").
func foo(i, j int) {}"
  (setq go--fontify-param-has-name (eq
                                    (go--parameter-list-type (point-max))
                                    'present))

  ;; Remember where our match started so we can continue our search
  ;; from here.
  (setq go--fontify-param-beg (point))

  ;; Return position of closing paren so we process the entire
  ;; multiline param list.
  (save-excursion
    (let ((depth (go-paren-level)))
      ;; First check that our paren is closed by the end of the file. This
      ;; avoids expanding the fontification region to the entire file when you
      ;; have an unclosed paren at file scope.
      (when (save-excursion
              (goto-char (1+ (buffer-size)))
              (< (go-paren-level) depth))
        (while (and
                (re-search-forward ")" nil t)
                (>= (go-paren-level) depth)))))
    (point)))

(defun go--fontify-param-post ()
  "Move point back to opening paren.

This is used as an anchored font lock keyword POST-MATCH-FORM. We
move point back to the opening \"(\" so we find nested param
lists."
  (goto-char go--fontify-param-beg))

(defun go--match-param-start (end)
  "Search for the starting of param lists.

Search for the opening `(' of function signature param lists.
This covers the func receiver, params, and results. Interface
declarations are also included."
  (let (found-match)
    (while (and
            (not found-match)
            (re-search-forward (concat "\\(\\_<" go-identifier-regexp "\\)?(") end t))
      (when (not (go-in-string-or-comment-p))
        (save-excursion
          (goto-char (match-beginning 0))

          (let ((name (match-string 1)))
            (when name
              ;; We are in a param list if "func" preceded the "(" (i.e.
              ;; func literal), or if we are in an interface
              ;; declaration, e.g. "interface { foo(i int) }".
              (setq found-match (or (string= name "func") (go--in-interface-p))))

            ;; Otherwise we are in a param list if our "(" is preceded
            ;; by ") " or "func ".
            (when (and (not found-match) (not (zerop (skip-syntax-backward " "))))
              (setq found-match (or
                                 (eq (char-before) ?\))
                                 (looking-back "\\_<func" (- (point) 4)))))))))
    found-match))


(defconst go--named-param-re
  (concat "[[:space:]\n]*\\(" go-identifier-regexp "\\)\\(?:[[:space:]]+\\(?:\\.\\.\\.\\)?" go-type-name-regexp "[[:space:]]*[,)]\\)?")
  "Regexp to match named param such as \"s *string\" in:

func(i int, s *string) { }")

(defconst go--unnamed-param-re
  (concat "\\(\\)[[:space:]\n]*\\(?:\\.\\.\\.\\)?" go-type-name-regexp "[[:space:]]*[,)]")
  "Regexp to match unnamed param such as \"*string\" in:

func(int, *string) { }

We start with an empty subexp since our font lock keyword expects
subexp 1 to a variable name, but we have no variable.")

(defun go--fontify-param (end)
  "Match a param within a param list.

Our parent font lock matcher is anchored to the beginning of the
param list. `go--fontify-param-has-name' has been set
appropriately. We match the next param and advance point to after
the next comma or to the closing paren."
  (let (found-match done)
    ;; We loop until match because there are some params that we can't
    ;; handle (but we may need to handle subsequent params). For
    ;; example:
    ;;
    ;; // We don't handle the interface, so we must skip it and handle
    ;; // "string".
    ;; func(int, interface { foo() }, string)
    (while (and (not found-match) (not done))
      (if go--fontify-param-has-name
          (when (looking-at go--named-param-re)
            (when (not go-fontify-variables)
              (let ((md (match-data)))
                (setf (nth 2 md) nil (nth 3 md) nil)
                (set-match-data md)))
            (setq found-match t))
        (when (looking-at go--unnamed-param-re)
          (setq found-match t)))

      ;; Advance to next comma. We are done if there are no more commas.
      (setq done (not (go--search-next-comma end))))
    found-match))

(defun go--search-next-comma (end)
  "Search forward from point for a comma whose nesting level is
the same as point. If it reaches a closing parenthesis before a
comma, it stops at it. Return non-nil if comma was found."
  (let ((orig-level (go-paren-level)))
    (while (and (< (point) end)
                (or (looking-at-p "[^,)]")
                    (> (go-paren-level) orig-level)))
      (forward-char))
    (when (and (looking-at-p ",")
               (< (point) (1- end)))
      (forward-char)
      t)))

(defun go--looking-at-keyword ()
  (and (looking-at (concat "\\(" go-identifier-regexp "\\)"))
       (member (match-string 1) go-mode-keywords)))

(defun go--match-type-switch-case (end)
  "Match a \"case\" clause within a type switch."
  (let (found-match)
    (while (and
            (not found-match)

            ;; Search for "case" statements.
            (re-search-forward "^[[:space:]]*case " end t))

      ;; Make sure we are in a type switch statement.
      (setq found-match (go--in-type-switch-p)))
    found-match))

(defun go--fontify-type-switch-case (end)
  "Match a single type within a type switch case."
  (let (found-match done)
    ;; Loop until we find a match because we must skip types we don't
    ;; handle, such as "interface { foo() }".
    (while (and (not found-match) (not done))
      (when (looking-at (concat "\\(?:[[:space:]]*\\|//.*\\|\n\\)*" go-type-name-regexp "[[:space:]]*[,:]"))
        (goto-char (match-end 1))
        (unless (member (match-string 1) go-constants)
          (setq found-match t)))
      (setq done (not (go--search-next-comma end))))
    found-match))

(defun go--containing-decl ()
  "Return containing decl kind var|const|type, if any."
  (save-match-data
    (or
     (save-excursion
       (and
        (go-goto-opening-parenthesis)
        (eq (char-after) ?\()
        (skip-syntax-backward " ")
        (skip-syntax-backward "w")
        (looking-at "\\(var\\|const\\|type\\)[[:space:]]")
        (match-string-no-properties 1)))

     (save-excursion
       (let ((depth (go-paren-level)))
         (beginning-of-line)
         (and
          (= (go-paren-level) depth)
          (looking-at "[[:space:]]*\\(var\\|const\\|type\\)[[:space:]]")
          (match-string-no-properties 1)))))))

(defconst go--decl-ident-re (concat "\\(?:^\\|[[:space:]]\\)\\(\\(\\(" go-identifier-regexp "\\)\\)\\)\\_>"))

(defun go--match-decl (end)
  "Match identifiers in \"var\", \"type\" and \"const\" decls, as
well as \":=\" assignments.

In order to only scan once, the regex has three subexpressions
that match the same identifier. Depending on the kind of
containing decl we zero out the subexpressions so the right one
gets highlighted by the font lock keyword."
  (let (found-match decl)
    (while (and
            (not found-match)
            (re-search-forward go--decl-ident-re end t))

      (save-excursion
        ;; Skip keywords.
        (cond
         ((member (match-string 1) go-mode-keywords))

         ((and
           ;; We are in a decl of some kind.
           (setq decl (go--containing-decl))

           ;; We aren't on right side of equals sign.
           (not (go--looking-back-p "=")))

          (setq found-match t)

          ;; Unset match data subexpressions that don't apply based on
          ;; the decl kind.
          (let ((md (match-data)))
            (cond
             ((string= decl "var")
              (setf (nth 4 md) nil (nth 5 md) nil (nth 6 md) nil (nth 7 md) nil)
              (when (not go-fontify-variables)
                (setf (nth 2 md) nil (nth 3 md) nil)))
             ((string= decl "const")
              (setf (nth 2 md) nil (nth 3 md) nil (nth 6 md) nil (nth 7 md) nil))
             ((string= decl "type")
              (setf (nth 2 md) nil (nth 3 md) nil (nth 4 md) nil (nth 5 md) nil)))
            (set-match-data md)))

         (go-fontify-variables
          (save-match-data
            ;; Left side of ":=" assignment.
            (when (looking-at ".*:=")
              (let ((depth (go-paren-level)))
                (goto-char (match-end 0))
                ;; Make sure the ":=" isn't in a comment or a sub-block.
                (setq found-match (and
                                   (not (go-in-string-or-comment-p))
                                   (= depth (go-paren-level)))))))))))
    found-match))

(defun go--looking-back-p (re)
  "Return non-nil if RE matches beginning of line to point.

RE is not anchored automatically."
  (string-match-p
   re
   (buffer-substring-no-properties (point) (line-beginning-position))))


(defconst go--ident-type-pair-re (concat "\\_<\\(" go-identifier-regexp "\\)[[:space:]]+" go-type-name-regexp))

(defun go--match-ident-type-pair (end)
  "Search for identifier + type-name pairs.

For example, this looks for the \"foo bar\" in \"var foo bar\",
yielding match-data for \"bar\" since that is a type name to be
fontified. This approach matches type names in var and const
decls, and in struct definitions. Return non-nil if search
succeeds."
  (let (found-match)
    (while (and
            (not found-match)
            (re-search-forward go--ident-type-pair-re end t))

      ;; Make sure the neither match is a keyword.
      (if (member (match-string 2) go-mode-keywords)
          (goto-char (match-end 2))
        (if (member (match-string 1) go-mode-keywords)
            (goto-char (match-end 1))
          (setq found-match t))))

    found-match))

(defconst go--single-func-result-re (concat ")[[:space:]]+" go-type-name-regexp "\\(?:$\\|[[:space:]),]\\)"))

(defun go--match-single-func-result (end)
  "Match single result types.

Parenthetical result lists are handled by the param list keyword,
so we need a separate keyword to handle singular result types
such as \"string\" in:

func foo(i int) string"
  (let (found-match)
    (while (and
            (not found-match)
            (re-search-forward go--single-func-result-re end t))
      (when (not (member (match-string 1) go-mode-keywords))
        (setq found-match t)
        (goto-char (match-end 1))))
    found-match))

(defconst go--type-alias-re
  (concat "^[[:space:]]*\\(type[[:space:]]+\\)?" go-identifier-regexp "[[:space:]]*=[[:space:]]*" go-type-name-regexp))

(defun go--match-type-alias (end)
  "Search for type aliases.

We are looking for the right-hand-side of the type alias"
  (let (found-match)
    (while (and
            (not found-match)
            (re-search-forward go--type-alias-re end t))
      ;; Either line started with "type", or we are in a "type" block.
      (setq found-match (or
                         (match-string 1)
                         (go--in-paren-with-prefix-p ?\( "type"))))
    found-match))


(defconst go--map-value-re
  (concat "\\_<map\\_>\\[\\(?:\\[[^]]*\\]\\)*[^]]*\\]" go-type-name-regexp))

(defun go--match-map-value (end)
  "Search for map value types."
  (when (re-search-forward go--map-value-re end t)
    ;; Move point to beginning of map value in case value itself is
    ;; also a map (we will match it next iteration).
    (goto-char (match-beginning 1))
    t))

(defconst go--label-re (concat "\\(" go-label-regexp "\\):"))

(defun go--match-ident-colon (end)
  "Search for composite literal field names and label definitions."
  (let (found-match)
    (while (and
            (not found-match)
            (re-search-forward go--label-re end t))

      (save-excursion
        (goto-char (match-beginning 1))
        (skip-syntax-backward " ")

        (setq found-match (or
                           ;; We are a label/field name if we are at the
                           ;; beginning of the line.
                           (bolp)

                           ;; Composite literal field names, e.g. "Foo{Bar:". Note
                           ;; that this gives false positives for literal maps,
                           ;; arrays, and slices.
                           (and
                            (or (eq (char-before) ?,) (eq (char-before) ?{))
                            (go--in-composite-literal-p))))))

    found-match))

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

(defun go--reset-dangling-cache-before-change (&optional _beg _end)
  "Reset `go-dangling-cache'.

This is intended to be called from `before-change-functions'."
  (setq go-dangling-cache (make-hash-table :test 'eql)))

(defun go--electric-indent-function (inserted-char)
  (let ((prev (char-before (1- (point)))))
    (cond
     ;; Indent after starting/ending a comment. This is handy for
     ;; comments above "case" statements and closing multiline
     ;; comments.
     ((or
       (and (eq inserted-char ?/) (eq prev ?/))
       (and (eq inserted-char ?/) (eq prev ?*))
       (and (eq inserted-char ?*) (eq prev ?/)))
      'do-indent)

     ((eq inserted-char ? )
      (and
       (eq prev ?e)
       (eq (char-before (- (point) 2)) ?s)
       (eq (char-before (- (point) 3)) ?a)
       (eq (char-before (- (point) 4)) ?c)))

     ;; Trick electric-indent-mode into indenting inside multiline
     ;; comments.
     ((and (eq inserted-char ?\n) (go-in-comment-p))
      'do-indent))))

(defun go--comment-region (beg end &optional arg)
  "Switch to block comment when commenting a partial line."
  (save-excursion
    (goto-char beg)
    (let ((beg-bol (line-beginning-position)))
      (goto-char end)
      (if (and
           ;; beg and end are on the same line
           (eq (line-beginning-position) beg-bol)
           ;; end is not at end of line
           (not (eq end (line-end-position))))
          (let ((comment-start "/* ")
                (comment-end " */")
                (comment-padding ""))
            (comment-region-default beg end arg))
        (comment-region-default beg end arg)))))

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

If you want to automatically run `gofmt' before saving a file,
add the following hook to your Emacs configuration:

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
with goflymake (see URL `https://github.com/dougm/goflymake'), gocode
\(see URL `https://github.com/nsf/gocode'), go-eldoc
\(see URL `github.com/syohex/emacs-go-eldoc') and yasnippet-go
\(see URL `https://github.com/dominikh/yasnippet-go')"

  ;; Font lock
  (setq font-lock-defaults '(go--build-font-lock-keywords))
  (setq font-lock-multiline t)

  ;; Indentation
  (set (make-local-variable 'indent-line-function) #'go-mode-indent-line)

  ;; Comments
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end)   "")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")
  (set (make-local-variable 'comment-region-function) #'go--comment-region)
  ;; Set comment-multi-line to t so that comment-indent-new-line
  ;; doesn't use one /* */ per line. Thanks to comment-use-syntax,
  ;; Emacs is smart enough to still insert new // for single-line
  ;; comments.
  (set (make-local-variable 'comment-multi-line) t)

  (set (make-local-variable 'beginning-of-defun-function) #'go-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'go-end-of-defun)
  (setq-local paragraph-start
              (concat "[[:space:]]*\\(?:"
                      comment-start-skip
                      "\\|\\*/?[[:space:]]*\\|\\)$"))
  (setq-local paragraph-separate paragraph-start)
  (setq-local fill-paragraph-function #'go-fill-paragraph)
  (setq-local fill-forward-paragraph-function #'go--fill-forward-paragraph)
  (setq-local adaptive-fill-function #'go--find-fill-prefix)
  (setq-local adaptive-fill-first-line-regexp "")
  (setq-local comment-line-break-function #'go--comment-indent-new-line)

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'syntax-propertize-function) #'go-propertize-syntax)

  (when (boundp 'electric-indent-chars)
    (set (make-local-variable 'electric-indent-chars) '(?\n ?} ?\) ?:))
    (add-hook 'electric-indent-functions #'go--electric-indent-function nil t))

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
  ;; documents the old, reversed order.
  (when (and (boundp 'compilation-error-regexp-alist)
             (boundp 'compilation-error-regexp-alist-alist))
    (add-to-list 'compilation-error-regexp-alist 'go-test)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(go-test . ("^\\s-+\\([^()\t\n]+\\):\\([0-9]+\\):? .*$" 1 2)) t)))

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
        (line-offset 0)
        (column (current-column)))
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
              (error "Invalid rcs patch or internal error in go--apply-rcs-patch")))))))
    (move-to-column column)))

(defun gofmt--is-goimports-p ()
  (string-equal (file-name-base gofmt-command) "goimports"))

(defun gofmt ()
  "Format the current buffer according to the formatting tool.

The tool used can be set via ‘gofmt-command’ (default: gofmt) and additional
arguments can be set as a list via ‘gofmt-args’."
  (interactive)
  (let ((tmpfile (make-nearby-temp-file "gofmt" nil ".go"))
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
                          (list "-srcdir" (file-local-name
                                           (file-truename buffer-file-name))))))
          (setq our-gofmt-args
                (append our-gofmt-args gofmt-args
                        (list "-w" (file-local-name tmpfile))))
          (message "Calling gofmt: %s %s" gofmt-command our-gofmt-args)
          ;; We're using errbuf for the mixed stdout and stderr output. This
          ;; is not an issue because gofmt -w does not produce any stdout
          ;; output in case of success.
          (if (zerop (apply #'process-file gofmt-command nil errbuf nil our-gofmt-args))
              (progn
                ;; There is no remote variant of ‘call-process-region’, but we
                ;; can invoke diff locally, and the results should be the same.
                (if (zerop (let ((local-copy (file-local-copy tmpfile)))
                             (unwind-protect
                                 (call-process-region
                                  (point-min) (point-max) "diff" nil patchbuf
                                  nil "-n" "-" (or local-copy tmpfile))
                               (when local-copy (delete-file local-copy)))))
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
        (while (search-forward-regexp
                (concat "^\\(" (regexp-quote (file-local-name truefile))
                        "\\):")
                nil t)
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

(defun godoc--buffer-name (query)
  "Determine the name to use for the output buffer of a given godoc QUERY."
  (if godoc-reuse-buffer
      "*godoc*"
    (concat "*godoc " query "*")))

(defun godoc--get-buffer (query)
  "Get an empty buffer for a godoc QUERY."
  (let* ((buffer-name (godoc--buffer-name query))
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
Playground URL.

By default this function will prompt to confirm you want to upload
code to the Playground. You can disable the confirmation by setting
`go-confirm-playground-uploads' to nil."
  (interactive "r")
  (if (and go-confirm-playground-uploads
           (not (yes-or-no-p "Upload to public Go Playground? ")))
      (message "Upload aborted")
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "text/plain; charset=UTF-8")))
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
                                 (funcall go-play-browse-function url))))))))))))

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
          (fail (message "Could not find a place to add import."))
          (block-empty
           (insert "\n\t" line "\n"))
          (block
              (save-excursion
                (re-search-backward "^import (")
                (setq import-start (point)))
            (if (re-search-backward (concat "^[[:space:]]*//[[:space:]]*" line "$")  import-start t)
                (uncomment-region (line-beginning-position) (line-end-position))
              (insert "\n\t" line)))
          (single (insert "import " line "\n"))
          (none (insert "\nimport (\n\t" line "\n)\n")))))))

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
              (if (and (file-directory-p file)
                       (not (file-symlink-p file)))
                  (setq dirs (append (cons file
                                           (go--directory-dirs file))
                                     dirs))))))
        dirs)
    '()))


(defun go-packages ()
  (funcall go-packages-function))

(defun go-packages-native ()
  "Return a list of all installed Go packages."
  (declare (obsolete "this function does not work well with modern versions of Go. You should use `go-packages-go-list' instead." "1.7.0"))
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
                      (when (string-match "^\\(.+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): imported and not used: \".+\".*$" line)
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
  (declare (obsolete "set `gofmt-command' to goimports instead, or use LSP and gopls's \"Organize Imports\" code action." "1.7.0"))
  (interactive "P")
  (save-excursion
    (let ((cur-buffer (current-buffer)) flymake-state lines)
      (when (boundp 'flymake-mode)
        (setq flymake-state flymake-mode)
        (flymake-mode -1))
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
      (if flymake-state (flymake-mode 1)))))

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
                               ;; Emacs point and byte positions are 1-indexed.
                               (number-to-string (1- (position-bytes point))))
          (with-current-buffer outbuf
            (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
        (kill-buffer outbuf)))))

(defun godef--successful-p (output)
  (not (or (string= "-" output)
           (string= "godef: no identifier found" output)
           (string= "godef: no object" output)
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
		  ;; TODO: Integrate this facility with XRef.
		  (xref-push-marker-stack)
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
        (let* ((parts (split-string (buffer-substring (line-beginning-position) (line-end-position)) ":"))
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
  "Go to the function definition (named or anonymous) surrounding point.

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
  "Return t if COMPARE-POINT is inside the function immediately surrounding point."
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
  "Determine a suitable GOPATH for BUFFER, or the current buffer if BUFFER is nil."
  (declare (obsolete "GOPATH has been deprecated in favour of Go modules." "1.7.0"))
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
  (declare (obsolete "GOPATH has been deprecated in favour of Go modules." "1.7.0"))
  (let ((d (locate-dominating-file buffer-file-name "src")))
    (if d
        (list d))))

(defun go-set-project (&optional buffer)
  "Set GOPATH based on `go-guess-gopath' for BUFFER.
Set it to the current buffer if BUFFER is nil.

If go-guess-gopath returns nil, that is if it couldn't determine
a valid value for GOPATH, GOPATH will be set to the initial value
of when Emacs was started.

This function can for example be used as a
projectile-switch-project-hook, or simply be called manually when
switching projects."
  (declare (obsolete "GOPATH has been deprecated in favour of Go modules." "1.7.0"))
  (interactive)
  (let ((gopath (or (go-guess-gopath buffer)
                    (go-original-gopath))))
    (setenv "GOPATH" gopath)
    (message "Set GOPATH to %s" gopath)))

(defun go-reset-gopath ()
  "Reset GOPATH to the value it had when Emacs started."
  (declare (obsolete "GOPATH has been deprecated in favour of Go modules." "1.7.0"))
  (interactive)
  (let ((gopath (go-original-gopath)))
    (setenv "GOPATH" gopath)
    (message "Set GOPATH to %s" gopath)))

(defun go-original-gopath ()
  "Return the original value of GOPATH from when Emacs was started."
  (declare (obsolete "GOPATH has been deprecated in favour of Go modules." "1.7.0"))
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

(defvar go-dot-mod-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `go-dot-mod-mode'.")

(defvar go-dot-mod-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; handle '//' comment syntax
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `go-dot-mod-mode'.")

(defconst go-dot-mod-mode-keywords
  '("module" "go" "require" "replace" "exclude")
  "All keywords for go.mod files.  Used for font locking.")

(defgroup go-dot-mod nil
  "Options specific to `go-dot-mod-mode`."
  :group 'go)

(defface go-dot-mod-module-name '((t :inherit default))
  "Face for module name in \"require\" list."
  :group 'go-dot-mod)

(defface go-dot-mod-module-version '((t :inherit default))
  "Face for module version in \"require\" list."
  :group 'go-dot-mod)

(defface go-dot-mod-module-semver '((t :inherit go-dot-mod-module-version))
  "Face for module semver in \"require\" list."
  :group 'go-dot-mod)


(defvar go-dot-mod-font-lock-keywords
  `(
    (,(concat "^\\s-*\\(" (regexp-opt go-dot-mod-mode-keywords t) "\\)\\s-") 1 font-lock-keyword-face)
    ("\\(?:^\\|=>\\)\\s-*\\([^[:space:]\n()]+\\)\\(?:\\s-+\\(v[0-9]+\\.[0-9]+\\.[0-9]+\\)\\([^[:space:]\n]*\\)\\)?" (1 'go-dot-mod-module-name) (2 'go-dot-mod-module-semver nil t) (3 'go-dot-mod-module-version nil t)))
  "Keyword highlighting specification for `go-dot-mod-mode'.")

;;;###autoload
(define-derived-mode go-dot-mod-mode fundamental-mode "Go Mod"
  "A major mode for editing go.mod files."
  :syntax-table go-dot-mod-mode-syntax-table
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end)   "")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\)\\s *")

  (set (make-local-variable 'font-lock-defaults)
       '(go-dot-mod-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'go-mode-indent-line)

  ;; Go style
  (setq indent-tabs-mode t)

  ;; we borrow the go-mode-indent function so we need this buffer cache
  (set (make-local-variable 'go-dangling-cache) (make-hash-table :test 'eql))
  (add-hook 'before-change-functions #'go--reset-dangling-cache-before-change t t))

;;;###autoload
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-dot-mod-mode))

(defconst go-dot-work-mode-keywords
  '("go" "replace" "use")
  "All keywords for go.work files.  Used for font locking.")

;;;###autoload
(define-derived-mode go-dot-work-mode fundamental-mode "Go Work"
  "A major mode for editor go.work files."
  :syntax-table go-dot-mod-mode-syntax-table
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end)   "")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\)\\s *")

  (set (make-local-variable 'font-lock-defaults)
       '(go-dot-work-mode-keywords))
  (set (make-local-variable 'indent-line-function) 'go-mode-indent-line)

  ;; Go style
  (setq indent-tabs-mode t)

  ;; we borrow the go-mode-indent function so we need this buffer cache
  (set (make-local-variable 'go-dangling-cache) (make-hash-table :test 'eql))
  (add-hook 'before-change-functions #'go--reset-dangling-cache-before-change t t))

;;;###autoload
(add-to-list 'auto-mode-alist '("go\\.work\\'" . go-dot-work-mode))

;; The following functions were copied (and modified) from rust-mode.el.
;;
;; Copyright (c) 2015 The Rust Project Developers
;;
;; Permission is hereby granted, free of charge, to any
;; person obtaining a copy of this software and associated
;; documentation files (the "Software"), to deal in the
;; Software without restriction, including without
;; limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software
;; is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice
;; shall be included in all copies or substantial portions
;; of the Software.

(defun go--fill-prefix-for-comment-start (line-start)
  "Determine what to use for `fill-prefix' based on the text at LINE-START."
  (let ((result
         ;; Replace /* with same number of spaces
         (replace-regexp-in-string
          "\\(?:/\\*+?\\)[!*]?"
          (lambda (s)
            (let ((offset (if (eq t
                                  (compare-strings "/*" nil nil
                                                   s
                                                   (- (length s) 2)
                                                   (length s)))
                              1 2)))
              (make-string (1+ (- (length s) offset)) ?\x20)))
          line-start)))
    ;; Make sure we've got at least one space at the end
    (if (not (= (aref result (- (length result) 1)) ?\x20))
        (setq result (concat result " ")))
    result))

(defun go--in-comment-paragraph (body)
  ;; We might move the point to fill the next comment, but we don't want it
  ;; seeming to jump around on the user
  (save-excursion
    ;; If we're outside of a comment, with only whitespace and then a comment
    ;; in front, jump to the comment and prepare to fill it.
    (when (not (go-in-comment-p))
      (beginning-of-line)
      (when (looking-at (concat "[[:space:]\n]*" comment-start-skip))
        (goto-char (match-end 0))))

    ;; If we're at the beginning of a comment paragraph with nothing but
    ;; whitespace til the next line, jump to the next line so that we use the
    ;; existing prefix to figure out what the new prefix should be, rather than
    ;; inferring it from the comment start.
    (while (save-excursion
             (end-of-line)
             (and (go-in-comment-p)
                  (save-excursion
                    (beginning-of-line)
                    (looking-at paragraph-start))
                  (looking-at "[[:space:]]*$")
                  (nth 4 (syntax-ppss (line-beginning-position 2)))))
      (goto-char (line-beginning-position 2)))

    ;; If we're on the last line of a multiline-style comment that started
    ;; above, back up one line so we don't mistake the * of the */ that ends
    ;; the comment for a prefix.
    (when (save-excursion
            (and (nth 4 (syntax-ppss (line-beginning-position 1)))
                 (looking-at "[[:space:]]*\\*/")))
      (goto-char (line-end-position 0)))
    (funcall body)))

(defun go--with-comment-fill-prefix (body)
  (let*
      ((line-string (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
       (line-comment-start
        (when (go-in-comment-p)
          (cond
           ;; If we're inside the comment and see a * prefix, use it
           ((string-match "^\\([[:space:]]*\\*+[[:space:]]*\\)"
                          line-string)
            (match-string 1 line-string))
           ;; If we're at the start of a comment, figure out what prefix
           ;; to use for the subsequent lines after it
           ((string-match (concat "[[:space:]]*" comment-start-skip) line-string)
            (go--fill-prefix-for-comment-start
             (match-string 0 line-string))))))
       (fill-prefix
        (or line-comment-start
            fill-prefix)))
    (funcall body)))

(defun go--find-fill-prefix ()
  (go--in-comment-paragraph
   (lambda ()
     (go--with-comment-fill-prefix
      (lambda ()
        fill-prefix)))))

(defun go-fill-paragraph (&rest args)
  "Special wrapping for `fill-paragraph'.
This handles multi-line comments with a * prefix on each line."
  (go--in-comment-paragraph
   (lambda ()
     (go--with-comment-fill-prefix
      (lambda ()
        (let
            ((fill-paragraph-function
              (if (not (eq fill-paragraph-function 'go-fill-paragraph))
                  fill-paragraph-function))
             (fill-paragraph-handle-comment t))
          (apply 'fill-paragraph args)
          t))))))

(defun go--do-auto-fill (&rest args)
  "Special wrapping for `do-auto-fill'.
This handles multi-line comments with a * prefix on each line."
  (go--with-comment-fill-prefix
   (lambda ()
     (apply 'do-auto-fill args)
     t)))

(defun go--fill-forward-paragraph (arg)
  ;; This is to work around some funny behavior when a paragraph separator is
  ;; at the very top of the file and there is a fill prefix.
  (let ((fill-prefix nil)) (forward-paragraph arg)))

(defun go--comment-indent-new-line (&optional arg)
  (go--with-comment-fill-prefix
   (lambda () (comment-indent-new-line arg))))



(provide 'go-mode)

;;; go-mode.el ends here

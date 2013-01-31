;;; go-mode.el --- Major mode for the Go programming language

;; Improvements over original go-mode:
;; - Use a proper syntax table, so that emacs and other packages know
;;   about strings and comments (e.g. expand-region needs this)
;; - Fix various indentation/font locking problems caused by the lack
;;   of proper syntax table
;; - Fix gofmt issue with buffers that do not end with a newline
;; - Correctly fontify (foo)(bar) function calls
;; - Correctly fontify identifiers with unicode characters in them
;; - Fontifies type names in struct literals
;; - Fontifies type names in maps, slices and arrays
;; - Does not fontify anonymous functions as if they were methods
;; - Support for C-M-a (beginning-of-defun)
;; - Support for C-M-e (end-of-defun)
;; - Support for C-M-h (mark-defun
;; - go-goto-imports
;; - go-play-buffer and go-play-region
;;
;; Minor changes:
;; - use view-mode for the godoc buffer
;;
;; TODO:
;; Features:
;; - imports manipulations (add, remove, with sorting and grouping)
;; Bugs:
;; - Disable escapes in `` strings
;; - Correct indentation for http://sprunge.us/iEaN
;; - fontify types in struct definitions

(defconst go-dangling-operators-regexp "[^-]-\\|[^+]\\+\\|[/*&><.=|^]")
(defconst gofmt-stdin-tag "<standard input>")
(defconst go-identifier-regexp "[[:word:][:multibyte:]_]+")
(defconst go-func-regexp (concat "\\<func\\>\\s *\\(" go-identifier-regexp "\\)"))

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
    (modify-syntax-entry ?_  "_" st)

    st)
  "Syntax table for Go mode.")

(defvar go-mode-keywords
  '("break"    "default"     "func"   "interface" "select"
    "case"     "defer"       "go"     "map"       "struct"
    "chan"     "else"        "goto"   "package"   "switch"
    "const"    "fallthrough" "if"     "range"     "type"
    "continue" "for"         "import" "return"    "var")
  "All keywords in the Go language.  Used for font locking and
some syntax analysis.")


(defvar go-mode-font-lock-keywords
  (let ((builtins '("append" "cap" "close" "complex" "copy" "delete" "imag" "len"
                    "make" "new" "panic" "print" "println" "real" "recover"))
        (constants '("nil" "true" "false" "iota"))
        (type-name "\\(?:[*(]\\)*\\(?:\\w+\\.\\)?\\(\\w+\\)") ;; XXX wtf? and what about unicode? and is this an identifier or wtf is it?
        )
    `((,(regexp-opt go-mode-keywords 'words) . font-lock-keyword-face)
      (,(regexp-opt builtins 'words) . font-lock-builtin-face)
      (,(regexp-opt constants 'words) . font-lock-constant-face)
      (,go-func-regexp 1 font-lock-function-name-face) ;; function (not method) name
      (,(concat "\\(" go-identifier-regexp "\\)\\s *(") 1 font-lock-function-name-face) ;; function call/method name
      (,(concat "(\\(" go-identifier-regexp "\\))\\s *(") 1 font-lock-function-name-face) ;; bracketed function call
      ("\\<type\\>\\s *\\(\\S +\\)" 1 font-lock-type-face) ;; types
      (,(concat "\\<type\\>\\s *" go-identifier-regexp "\\s *" type-name) 1 font-lock-type-face) ;; types
      (,(concat "\\(?:[[:space:]]+\\|\\]\\)\\[\\([[:digit:]]+\\|\\.\\.\\.\\)?\\]" type-name) 2 font-lock-type-face) ;; Arrays/slices
      (,(concat "map\\[[^]]+\\]" type-name) 1 font-lock-type-face) ;; map value type

      (,(concat "\\(" go-identifier-regexp "\\)" "{") 1 font-lock-type-face)

      (,(concat "\\<map\\[" type-name) 1 font-lock-type-face) ;; map key type
      (,(concat "\\<chan\\>\\s *\\(?:<-\\)?" type-name) 1 font-lock-type-face) ;; channel type
      (,(concat "\\<\\(?:new\\|make\\)\\>\\(?:\\s \\|)\\)*(" type-name) 1 font-lock-type-face) ;; new/make type
      ;; TODO do we actually need this one or isn't it just a function call?
      (,(concat "\\.\\s *(" type-name) 1 font-lock-type-face) ;; Type conversion
      (,(concat "\\<func\\>\\s +(" go-identifier-regexp "\\s +" type-name ")") 1 font-lock-type-face) ;; Method receiver
      ;; Like the original go-mode this also marks compound literal
      ;; fields. There, it was marked as to fix, but I grew quite
      ;; accustomed to it, so it'll stay for now.
      ("^\\s *\\(\\w+\\)\\s *:\\(\\S.\\|$\\)" 1 font-lock-constant-face) ;; Labels and compound literal fields
      ("\\<\\(goto\\|break\\|continue\\)\\>\\s *\\(\\w+\\)" 2 font-lock-constant-face))) ;; labels in goto/break/continue
  "Basic font lock keywords for Go mode.  Highlights keywords,
built-ins, functions, and some types.")


(defvar go-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "}" 'go-mode-insert-and-indent)
    (define-key m ")" 'go-mode-insert-and-indent)
    (define-key m "," 'go-mode-insert-and-indent)
    (define-key m ":" 'go-mode-insert-and-indent)
    (define-key m "=" 'go-mode-insert-and-indent)
    m)
  "Keymap used by Go mode to implement electric keys.")

(defun go-mode-insert-and-indent (key)
  "Invoke the global binding of KEY, then reindent the line."

  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (indent-according-to-mode))

(defun go-paren-level ()
  (car (syntax-ppss)))

(defun go-in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))

(defun go-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun go-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun go-goto-beginning-of-string-or-comment ()
  (goto-char (nth 8 (syntax-ppss))))

(defun go-backward-irrelevant ()
  (let (pos (start-pos (point)))
    (skip-chars-backward "\n[:blank:]")
    (if (looking-back "`")
        (backward-char))
    (if (go-in-string-p)
        (go-goto-beginning-of-string-or-comment))
    (if (looking-back "\\*/")
        (backward-char))
    (if (go-in-comment-p)
        (go-goto-beginning-of-string-or-comment))
    (setq pos (point))
    (beginning-of-line)
    (if (or (looking-at "^[[:word:]]+:$") (looking-at "^[[:space:]]*\\(case .+\\|default\\):"))
        (progn (previous-line)
               (end-of-line))
      (goto-char pos))
    (if (not (= start-pos (point)))
        (go-backward-irrelevant))
    (not (= start-pos (point)))))

(defun go-previous-line-has-dangling-op-p ()
  (save-excursion
    (beginning-of-line)
    (go-backward-irrelevant)
    (looking-back go-dangling-operators-regexp)))

(defun go-indentation-at-point (point)
  (save-excursion
    (let (start-nesting line-begin (outindent 0))
      (goto-char point)
      (back-to-indentation)
      (if (go-in-string-p)
          (current-indentation)
        (progn
          (setq start-nesting (go-paren-level))
          (if (looking-at "[])}]")
              (progn
                (while (and
                        (not (bobp))
                        (>= (go-paren-level) start-nesting))
                  (skip-chars-backward "^[]{}()")
                  (backward-char))
                (if (go-previous-line-has-dangling-op-p)
                    (- (current-indentation) tab-width)
                  (current-indentation)))
            (progn
              (go-backward-irrelevant)
              (if (looking-back go-dangling-operators-regexp)
                  ;; only one nesting for all dangling operators in one operation
                  (if (go-previous-line-has-dangling-op-p)
                      (current-indentation)
                    (+ (current-indentation) tab-width))
                (progn
                  (if (go-previous-line-has-dangling-op-p)
                      (setq outindent tab-width))
                  (setq line-begin (line-beginning-position))
                  (while (and
                          (not (bobp))
                          (> (point) line-begin)
                          (>= (go-paren-level) start-nesting))
                    (if (= 0 (skip-chars-backward "^[]{}()" line-begin))
                        (backward-char)))
                  (if (and (< (go-paren-level) start-nesting))
                      (+ (current-indentation) tab-width (- outindent))
                    (- (current-indentation) outindent)))))))))))

(defun go-mode-indent-line ()
  (interactive)
  (let ((indent (go-indentation-at-point (point)))
        shift-amt
        end
        (pos (- (point-max) (point)))
        (beg (progn (beginning-of-line) (point))))
    (back-to-indentation)
    (if (looking-at "case .+:[[:space:]]*$\\|default:[[:space:]]*$")
        (setq indent (- indent tab-width)))
    (beginning-of-line)
    (if (and (looking-at "^[[:space:]]*[[:word:]]+:$")
             (not (looking-at "^[[:space:]]*default:$")))
        (setq indent 0))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
        nil
      (delete-region beg (point))
      (indent-to indent))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

(defun go-beginning-of-defun (&optional count)
  (unless count (setq count 1))
  (let ((first t) failure)
    (dotimes (i (abs count))
      (while (or first (go-in-string-or-comment-p))
        (if (>= count 0)
            (if (not (re-search-backward go-func-regexp nil t))
                (setq failure t))
          (if (looking-at go-func-regexp)
              (forward-char))
          (if (not (re-search-forward go-func-regexp nil t))
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
    (skip-chars-forward "^{")
    (forward-char)
    (setq orig-level (go-paren-level))
    (while (>= (go-paren-level) orig-level)
      (skip-chars-forward "^}")
      (forward-char))))

(define-derived-mode go-mode fundamental-mode "Go"
  "Major mode for editing Go source text.

This provides basic syntax highlighting for keywords, built-ins,
functions, and some types.  It also provides indentation that is
\(almost) identical to gofmt."

  ;; Font lock
  (set (make-local-variable 'font-lock-defaults)
       '(go-mode-font-lock-keywords))

  ;; Indentation
  (set (make-local-variable 'indent-line-function) 'go-mode-indent-line)

  ;; Comments
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end)   "")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")

  (set (make-local-variable 'beginning-of-defun-function) 'go-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'go-end-of-defun)


  (setq imenu-generic-expression
        '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
          ("func" "^func *\\(.*\\) {" 1)))
  (imenu-add-to-menubar "Index")

  ;; Go style
  (setq indent-tabs-mode t)

  ;; Handle unit test failure output in compilation-mode
  ;;
  ;; Note the final t argument to add-to-list for append, ie put these at the
  ;; *ends* of compilation-error-regexp-alist[-alist]. We want go-test to be
  ;; handled first, otherwise other elements will match that don't work, and
  ;; those alists are traversed in *reverse* order:
  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2001-12/msg00674.html
  (when (and (boundp 'compilation-error-regexp-alist)
             (boundp 'compilation-error-regexp-alist-alist))
    (add-to-list 'compilation-error-regexp-alist 'go-test t)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(go-test . ("^\t+\\([^()\t\n]+\\):\\([0-9]+\\):? .*$" 1 2)) t)))

(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))

(defun gofmt ()
  "Pipe the current buffer through the external tool `gofmt`.
Replace the current buffer on success; display errors on failure."

  (interactive)
  (let ((currconf (current-window-configuration)))
    (let ((srcbuf (current-buffer))
          (filename buffer-file-name)
          (patchbuf (get-buffer-create "*Gofmt patch*")))
      (with-current-buffer patchbuf
        (let ((errbuf (get-buffer-create "*Gofmt Errors*"))
              (coding-system-for-read 'utf-8)    ;; use utf-8 with subprocesses
              (coding-system-for-write 'utf-8))
          (with-current-buffer errbuf
            (toggle-read-only 0)
            (erase-buffer))
          (with-current-buffer srcbuf
            (save-restriction
              (let (deactivate-mark)
                (widen)
                ;; If this is a new file, diff-mode can't apply a
                ;; patch to a non-exisiting file, so replace the buffer
                ;; completely with the output of 'gofmt'.
                ;; If the file exists, patch it to keep the 'undo' list happy.
                (let* ((newfile (not (file-exists-p filename)))
                       (flag (if newfile "" " -d")))

                  ;; diff-mode doesn't work too well with missing
                  ;; end-of-file newline, so add one
                  (if (/= (char-after (1- (point-max))) ?\n)
                      (save-excursion
                        (goto-char (point-max))
                        (insert ?\n)))

                  (if (= 0 (shell-command-on-region (point-min) (point-max)
                                                    (concat "gofmt" flag)
                                                    patchbuf nil errbuf))
                      ;; gofmt succeeded: replace buffer or apply patch hunks.
                      (let ((old-point (point))
                            (old-mark (mark t)))
                        (kill-buffer errbuf)
                        (if newfile
                            ;; New file, replace it (diff-mode won't work)
                            (gofmt-replace-buffer srcbuf patchbuf)
                          ;; Existing file, patch it
                          (gofmt-apply-patch filename srcbuf patchbuf))
                        (goto-char (min old-point (point-max)))
                        ;; Restore the mark and point
                        (if old-mark (push-mark (min old-mark (point-max)) t))
                        (set-window-configuration currconf))

                    ;; gofmt failed: display the errors
                    (gofmt-process-errors filename errbuf))))))

          ;; Collapse any window opened on outbuf if shell-command-on-region
          ;; displayed it.
          (delete-windows-on patchbuf)))
      (kill-buffer patchbuf))))

(defun gofmt-replace-buffer (srcbuf patchbuf)
  (with-current-buffer srcbuf
    (erase-buffer)
    (insert-buffer-substring patchbuf)))

(defun gofmt-apply-patch (filename srcbuf patchbuf)
  (require 'diff-mode)
  ;; apply all the patch hunks
  (with-current-buffer patchbuf
    (goto-char (point-min))
    ;; The .* is for TMPDIR, but to avoid dealing with TMPDIR
    ;; having a trailing / or not, it's easier to just search for .*
    ;; especially as we're only replacing the first instance.
    (if (re-search-forward "^--- \\(.*/gofmt[0-9]*\\)" nil t)
        (replace-match filename nil nil nil 1))
    (condition-case nil
        (while t
          (diff-hunk-next)
          (diff-apply-hunk))
      ;; When there's no more hunks, diff-hunk-next signals an error, ignore it
      (error nil))))

(defun gofmt-process-errors (filename errbuf)
  ;; Convert the gofmt stderr to something understood by the compilation mode.
  (with-current-buffer errbuf
    (goto-char (point-min))
    (insert "gofmt errors:\n")
    (if (search-forward gofmt-stdin-tag nil t)
        (replace-match (file-name-nondirectory filename) nil t))
    (display-buffer errbuf)
    (compilation-mode)))

(defun gofmt-before-save ()
  "Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook 'gofmt-before-save)"

  (interactive)
  (when (eq major-mode 'go-mode) (gofmt)))

(defun godoc-read-query ()
  "Read a godoc query from the minibuffer."
  ;; Compute the default query as the symbol under the cursor.
  ;; TODO: This does the wrong thing for e.g. multipart.NewReader (it only grabs
  ;; half) but I see no way to disambiguate that from e.g. foobar.SomeMethod.
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (symbol (if bounds
                     (buffer-substring-no-properties (car bounds)
                                                     (cdr bounds)))))
    (read-string (if symbol
                     (format "godoc (default %s): " symbol)
                   "godoc: ")
                 nil nil symbol)))

(defun godoc-get-buffer (query)
  "Get an empty buffer for a godoc query."
  (let* ((buffer-name (concat "*godoc " query "*"))
         (buffer (get-buffer buffer-name)))
    ;; Kill the existing buffer if it already exists.
    (when buffer (kill-buffer buffer))
    (get-buffer-create buffer-name)))

(defun godoc-buffer-sentinel (proc event)
  "Sentinel function run when godoc command completes."
  (with-current-buffer (process-buffer proc)
    (cond ((string= event "finished\n")  ;; Successful exit.
           (goto-char (point-min))
           (view-buffer (current-buffer) 'kill-buffer))
          ((not (= (process-exit-status proc) 0))  ;; Error exit.
           (let ((output (buffer-string)))
             (kill-buffer (current-buffer))
             (message (concat "godoc: " output)))))))

(defun godoc (query)
  "Show go documentation for a query, much like M-x man."
  (interactive (list (godoc-read-query)))
  (unless (string= query "")
    (set-process-sentinel
     (start-process-shell-command "godoc" (godoc-get-buffer query)
                                  (concat "godoc " query))
     'godoc-buffer-sentinel)
    nil))

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
     ((re-search-forward "^import ([^)]+)" nil t)
      (backward-char 2))
     ((re-search-forward "\\(^import \\([^\"]+ \\)?\"[^\"]+\"\n?\\)+" nil t))
     ((re-search-forward "^[[:space:]\n]*package .+?\n" nil t))
     (t
      (goto-char old-point)
      (message "No imports found. Is this really a Go file?")))))


(defun go-play-buffer ()
  "Like `go-play-region', but acts on the entire buffer."
  (interactive)
  (go-play-region (point-min) (point-max)))

(defun go-play-region (start end)
  "Send the region to the Playground and stores the resulting
link in the kill ring."
  (interactive "r")
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (buffer-substring-no-properties start end))
         (content-buf (url-retrieve
                       "http://play.golang.org/share"
                       (lambda (arg)
                         (cond
                          ((equal :error (car arg))
                           (signal 'go-play-error (cdr arg)))
                          (t
                           (re-search-forward "\n\n")
                           (kill-new (format "http://play.golang.org/p/%s" (buffer-substring (point) (point-max))))
                           (message "http://play.golang.org/p/%s" (buffer-substring (point) (point-max)))))))))))


;; "However, it should not call syntax-ppss-flush-cache; so, it is not allowed to call syntax-ppss on some position and later modify the buffer at an earlier position."
;; ↑ let's hope this doesn't screw me over

;; TODO do we have to deal with removing the text property again, too?
;; and what happens when we edit a string? etc...
;; FIXME make this work :D
;; (set (make-local-variable 'syntax-propertize-function) 'go-propertize-syntax)
(defun go-propertize-syntax (start end)
  (save-excursion
    (let (start-of-string end-of-string)
      (goto-char start)
      ;; TODO loop this
      (skip-chars-forward "^`" end)
      (if (go-in-string-p)
          (progn
            (setq end-of-string (point))
            (go-goto-beginning-of-string-or-comment)
            (if (looking-at "`")
                (progn
                  (setq start-of-string (point))
                  ;; (syntax-code . matching-char)
                  (skip-chars-forward "^\\" end-of-string) ;; TODO loop this
                  (if (looking-at "\\")
                      (message "%s" (point))
                    (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax "."))))))))))

(defun go-common-prefix (sequences)
  (assert sequences)
  (flet ((common-prefix (s1 s2)
                        (let ((diff-pos (mismatch s1 s2)))
                          (if diff-pos (subseq s1 0 diff-pos) s1))))
    (reduce #'common-prefix sequences)))



(provide 'go-mode)


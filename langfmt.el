;;; langfmt.el --- Formatting library for programming languages

;; Copyright 2013 The go-mode Authors. All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

;; Author: The go-mode Authors
;; Version: 0.2.0alpha
;; Keywords: languages, convenience
;; URL: https://github.com/dominikh/go-mode.el
;;
;; This file is not part of GNU Emacs.

;;; Code:

(eval-when-compile (require 'cl))

(defun langfmt--delete-whole-line (&optional arg)
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

(defun langfmt--apply-rcs-patch (patch-buffer)
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
            (error "invalid rcs patch or internal error in langfmt--apply-rcs-patch"))
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
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (langfmt--goto-line (- from line-offset))
                (incf line-offset len)
                (langfmt--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in langfmt--apply-rcs-patch")))))))))

(defmacro define-langfmt (symbol docstring &rest properties)
  "Define formatting function `SYMBOL' and some helpers:
Function `SYMBOL-before-save' is to be added to .emacs to run `SYMBOL'
on the current buffer when saving.
Variable `SYMBOL-show-errors' is where to display the error output.

DOCSTRING is the documentation of the formatting function `SYMBOL'.
The following PROPERTIES are used by `SYMBOL' and above helpers.
All properties are mandatory.

The formatting function `SYMBOL' copies the file of the current buffer
into the temporary file, then run :format-command with :format-args and
the temporary file.  Thus, the temporary file should be formatted.

:after-format function is called after finishing :format-command.
If the return value of the function is nil, the contents of the original
file and the result of the formatting is compared.  If they differ, the RCS
patch is generated and applied automatically, then :after-diff function is
called.  :after-diff function is usually used to inform the result of the
patching to the users.

:after-format and :after-diff should be functions that take one argument,
a context described below.

The formatting errors are recognized by the exit status of :format-command.
If it is non-nil and `SYMBOL-show-errors' is 'buffer, the error buffer
filtered by :error-filter is displayed.

:group  VALUE should be a customization group for `SYMBOL-show-errors'.
:modes  VALUE should be a list of major modes.
        `SYMBOL-before-save' should run in those mode hooks.
:format-command
        VALUE should be a command name of a formatting tool.
:format-args
        VALUE should be a list of command line options for :format-command.
        The temporary file name is followed.
:after-format
        VALUE should be a function to be run after the formatting.

        If the function returns nil, the result of the formatting is
        compared with the original file content.
:after-diff
        VALUE should be a function to be run after the comparison between
        the original file content and the result of the formatting.
:error-filter
        VALUE should be a function to filter the error buffer content.
        The function takes two arguments, a original file name and a
        temporary file name.

        Because the error buffer is displayed in the compilation mode,
        the function should filter the buffer to be compatible with the
        compilation mode.

A context is a property list which has the following keys:

:exit-status
        The exit status of :format-command.
:no-diff-p
        Whether the original file content and the result of the formatting
        differ or not.  This property exists within :after-diff function
        only."
  (declare (indent 1) (doc-string 2))
  (let* ((name (symbol-name symbol))
         (show-errors-var (intern (format "%s-show-errors" name)))
         (process-errors-func (intern (format "%s--process-errors" name)))
         (before-save-func (intern (format "%s-before-save" name)))
         (patchbuf-name (format "*%s patch*" (capitalize name)))
         (errbuf-name (format "*%s Errors*" (capitalize name)))
         (group (plist-get properties :group))
         (modes (plist-get properties :modes))
         (format-command (plist-get properties :format-command))
         (format-args (plist-get properties :format-args))
         (after-format-func (plist-get properties :after-format))
         (after-diff-func (plist-get properties :after-diff))
         (error-filter (plist-get properties :error-filter)))

    `(progn

       (defcustom ,show-errors-var 'buffer
         ,(format "Where to display %s error output.
It can either be displayed in its own buffer, in the echo area, or not at all.

Please note that Emacs outputs to the echo area when writing
files and will overwrite %s's echo output if used from inside
a `before-save-hook'." name name)
         :type '(choice
                 (const :tag "Own buffer" buffer)
                 (const :tag "Echo area" echo)
                 (const :tag "None" nil))
         :group ,group)

       (defun ,process-errors-func (filename tmpfile errbuf)
         (with-current-buffer errbuf
           (if (eq ,show-errors-var 'echo)
               (progn
                 (message "%s" (buffer-string))
                 (langfmt--kill-error-buffer errbuf))
             ;; Convert stderr to something understood by the compilation mode.
             (goto-char (point-min))
             (insert ,name " errors:\n")
             (funcall ,error-filter filename tmpfile)
             (compilation-mode)
             (display-buffer errbuf))))

       (defun ,symbol ()
         ,docstring
         (interactive)
         (let* ((tmpfile-suffix (file-name-extension (buffer-file-name) t))
                (tmpfile (make-temp-file ,name nil tmpfile-suffix))
                (patchbuf (get-buffer-create ,patchbuf-name))
                (errbuf (if ,show-errors-var (get-buffer-create ,errbuf-name)))
                (coding-system-for-read 'utf-8)
                (coding-system-for-write 'utf-8))

           (save-restriction
             (widen)
             (if errbuf
                 (with-current-buffer errbuf
                   (setq buffer-read-only nil)
                   (erase-buffer)))
             (with-current-buffer patchbuf
               (erase-buffer))

             (write-region nil nil tmpfile)

             (let* ((exit-status (apply #'call-process
                                        ,format-command nil errbuf nil
                                        (append ,format-args (list tmpfile))))
                    (context (list :exit-status exit-status))
                    no-diff-p)
               (unless (funcall ,after-format-func context)
                 (setq no-diff-p (zerop (call-process-region
                                         (point-min) (point-max) "diff"
                                         nil patchbuf nil "-n" "-" tmpfile)))
                 (unless no-diff-p
                   (langfmt--apply-rcs-patch patchbuf))
                 (funcall ,after-diff-func (nconc (list :no-diff-p no-diff-p)
                                                  context)))
               (when errbuf
                 (if (zerop exit-status)
                     (langfmt--kill-error-buffer errbuf)
                   (,process-errors-func (buffer-file-name) tmpfile errbuf))))

             (kill-buffer patchbuf)
             (delete-file tmpfile))))

       (defun ,before-save-func ()
         ,(format
           "Add this to .emacs to run %s on the current buffer when saving:
%s."
           name (mapconcat (lambda (mode)
                             (format " (add-hook '%s-hook '%s)"
                                     mode before-save-func))
                           (cadr modes)
                           "\n"))
         (interactive)
         (add-hook 'before-save-hook ',symbol nil t)))))

(defun langfmt--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

(defun langfmt--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(provide 'langfmt)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; langfmt.el ends here

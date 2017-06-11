;;; go-present-mode.el --- major mode for editing Go present files -*- coding: utf-8; lexical-binding:t -*-

;; Copyright 2017 The Go Authors. All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

;; Keywords: go present

;;; Commentary:

;; Go-present-mode is a mode for viewing and editing Go present files
;; (mainly slides). It strives to provide view and features consistent
;; with Go present tool web view.
;;
;;
;; Installation
;; ------------
;; (add-to-list 'load-path "path/to/go-present-mode/")
;; (require 'go-present-mode)
;;
;;
;; Installation with use-package
;; -----------------------------
;; (use-package go-present-mode
;;   :load-path "path/to/go-present-mode/")
;;
;;
;; Features
;; --------
;;
;; Standard Emacs mode features such as: syntax highlighting via
;; font-lock, commenting/uncommeting, imenu support, and defun
;; movement.
;;
;; Outline-mode integration:
;;    C-c C-o C-h         collapse all headlines
;;    C-c C-o C-s         show all headlines
;;    C-c C-o C-n         go to next headline
;;    C-c C-o C-p         go to previous headline
;;
;; Present and allow editing (C-c C-p C-l) links.
;;
;; Open slide at point in a web browser (C-c C-p C-p).
;;
;; Show inline images (toggle with C-c C-p C-i).
;;
;; Open thing at point (C-c C-p C-o) such as: image, code, link.
;;
;; Toggle fonts
;;   C-c C-p C-`         program
;;   C-c C-p C-*         bold
;;   C-c C-p C-_         italic
;;
;; Show preformatted text as such.
;;
;; Show inline code (C-c C-p C-v) as specified by .code
;; commands. Support:
;;   - Address handling, OMIT lines, line numbers, and highlights.
;;   - Modifying code inline.
;;   - Executing code at point (C-c C-p C-c).
;;   - Tracking changes to the underlying file.

;;; Code:

(require 'cl-lib)
(require 'outline)
(require 'overlay)

(defgroup go-present nil
  "Configuration for go-present-mode."
  :group 'go)

(defvar go-present-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (dolist (ch (string-to-list "'\"~!@#$%^&-+=<>|"))
      (modify-syntax-entry ch "." tbl))
    (dolist (ch (string-to-list "_*`"))
      (modify-syntax-entry ch "_" tbl))
    (modify-syntax-entry ?# "<" tbl)
    (modify-syntax-entry ?\n ">" tbl)
    tbl)
  "Syntax table for `go-present-mode'.")

(defun go-present--propertize-syntax (start end)
  "Fix the syntax table in code sections.

Mark # between START and END as punctuation because there are no
comments in preformatted sections."
  (save-excursion
    (goto-char start)
    (while (search-forward "#" end t)
      (when (get-text-property (point) 'go-present--code)
        (put-text-property (1- (point)) (point)
                           'syntax-table '(1))))))

(defcustom go-present-max-image-size 500
  "Maximum size of image dimensions.
If neither dimension is specified, then limit width.  Otherwise
limit the larger one."
  :type 'integer
  :group 'go-present)

(defcustom go-present-base-port 4000
  "Base port to serve Go present on.

Go present serves on the first free port that is not smaller than
`go-present-base-port'."
  :type 'integer
  :group 'go-present)

(defface go-present-bold-face '((t (:inherit bold)))
  "Face for bold text outside code sections.")
(defvar go-present-bold-face 'go-present-bold-face)

(defface go-present-italic-face '((t (:inherit italic)))
  "Face for italic text outside code sections.")
(defvar go-present-italic-face 'go-present-italic-face)

(defface go-present-program-face '((t (:inherit variable-pitch)))
  "Face for program text outside code sections.")
(defvar go-present-program-face 'go-present-program-face)

(defface go-present-slide-face '((t (:inherit outline-1 :weight bold)))
  "Face for slide and section titles.")
(defvar go-present-slide-face 'go-present-slide-face)

(defface go-present-keyword-face '((t (:inherit font-lock-keyword-face)))
  "Face for present mode keywords.")
(defvar go-present-keyword-face 'go-present-keyword-face)

(defface go-present-link-face '((t (:inherit link)))
  "Face for links.")
(defvar go-present-link-face 'go-present-link-face)

;; Don't inherit from the default face because then you can't set
;; specific attributes of the font (e.g. weight) as the default face
;; overrides them.

(defface go-present-code-face '((t (:background "#dcdcdc")))
  "Face for text in code sections.")
(defvar go-present-code-face 'go-present-code-face)

(defface go-present-bold-code-face '((t (:inherit go-present-code-face :weight bold)))
  "Face for bold text in code sections.")
(defvar go-present-bold-code-face 'go-present-bold-code-face)

(defface go-present-preformatted-text-face '((t (:background "#dcdcdc")))
  "Face for preformatted text.")
(defvar go-present-preformatted-text-face 'go-present-preformatted-text-face)

;; Tell the compiler not to warn about our buffer-local variables.
(defvar-local go-present--hostport nil)
(defvar-local go-present--port nil)
(defvar-local go-present--preview-process-buffer nil)
(defvar-local go-present--slide-path nil)
(defvar-local go-present--update-buffers-list nil)

(defvar go-present-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p C-p") #'go-present-preview)
    (define-key map (kbd "C-c C-p C-i") #'go-present-toggle-inline-images)
    (define-key map (kbd "C-c C-p TAB") #'go-present-toggle-inline-images)
    (define-key map (kbd "C-c C-p C-o") #'go-present-open-at-point)
    (define-key map (kbd "C-c C-p C-l") #'go-present-insert-edit-link)
    (define-key map (kbd "C-c C-p *") #'go-present-toggle-font-bold)
    (define-key map (kbd "C-c C-p _") #'go-present-toggle-font-italic)
    (define-key map (kbd "C-c C-p `") #'go-present-toggle-font-program)
    (define-key map (kbd "C-c C-p C-v") #'go-present-show-code)
    (define-key map (kbd "C-c C-p C-c") #'go-present-run-code-at-point)
    (define-key map [remap save-buffer] #'go-present-save-buffer)
    (define-key map (kbd "<M-return>") #'go-present-insert-slide)
    map)
  "Keymap for `go-present-mode'.")

(defun go-present-insert-slide ()
  "Insert a new slide."
  (interactive)
  (cond
   ((or (bobp) (bolp))
    (insert "* "))
   ((eobp) (insert "\n* "))
   ((go-present--at-code (point) (1+ (point)))
    (error "Can't insert slide inside code section"))
   ((looking-at-p "^\\*+\\s-+")
    (insert "\n")
    (forward-line -1)
    (insert "* "))
   ((looking-back "^\\*+\\s-+")
    (end-of-line)
    (insert "\n* "))
   (t (insert "\n* ")
      (end-of-line))))

(defun go-present--url-near-point ()
  "Return the URL at point.

If none, return the first url after point, on the same line."
  (car (or (get-text-property (point) 'go-present-link)
           (get-text-property
            (or (next-single-property-change
                 (point) 'go-present-link nil (point-at-eol)) (point))
            'go-present-link))))

(defun go-present--find-code-other-window (location &optional offset)
  "Open code location in a other window.

LOCATION is a dotted pair of file name and Go present address
string to seek to in the file.
After moving the point to LOCATION, move it forward by OFFSET
characters."
  (let ((slide-buf (current-buffer)))
    (find-file-other-window (car location))
    (go-present--add-code-modification-hook (current-buffer) slide-buf))
  (when (cdr location)
    (goto-char (car (go-present--addr-str-to-region (cdr location))))
    (forward-char (or offset 0))))

(defconst go-present--image-regexp
  "^\\.\\(?:image\\|background\\)\\s-+\\(.*?\\)\\(?:\\s-+\\([0-9_]+\\)\\s-+\\([0-9_]+\\)\\)?$"
  "Regular expression matching .image and .background commands.

Subexpressions are:
  1. file name
  2. optional height (number or '_')
  3. optional widith (number or '_')")

(defconst go-present--link-regexp
  "^\\(\\.link\\|\\.iframe\\)\\s-+\\([^\t\v\n\f ]+\\)"
  "Regular expression matching .link and .iframe commands.

Subexpression 1 the command (.link or .iframe).
Subexpression 2 is the file name.")

(defconst go-present--html-regexp "\\.html\\s-+\\(.*\\)$"
  "Regular expression matching .html command.

Subexpression 1 is the file name.")

(defconst go-present--code-start-regexp "^\\.\\(code\\|play\\)\\s-+"
  "A ragular expression matching start of a code section.

Subexpression 1 is the command (.code or .play).")

(cl-defun go-present-open-at-point ()
  "Open thing at point.

Open in other frame/browser the thing that is at or near the
point, such as URLs, images, and files with code."
  (interactive)
  (let ((loc (get-text-property (point) 'go-present--code-location)))
    (when loc
      (go-present--find-code-other-window
       loc
       (- (point) (car (go-present--code-at (point)))))
      (cl-return-from go-present-open-at-point)))
  (cond
   ((save-excursion
      (forward-line 0)
      (looking-at go-present--link-regexp))
    (browse-url (match-string-no-properties 2)))
   ((save-excursion
      (forward-line 0)
      (looking-at go-present--image-regexp))
    (find-file-other-window (match-string-no-properties 1)))
   ((save-excursion
      (forward-line 0)
      (looking-at go-present--html-regexp))
    (find-file-other-window (match-string-no-properties 1)))
   ((save-excursion
      (forward-line 0)
      (looking-at go-present--code-start-regexp))
    (go-present--find-code-other-window
     (let ((code (save-excursion
                   (forward-line 0)
                   (go-present--parse-code-at-point))))
       (cons (nth 2 code) (nth 3 code)))))
   ((go-present--url-near-point)
    (browse-url (go-present--url-near-point)))
   (t (error "Unrecognized thing at point"))))

(defvar go-present--link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-2>") 'browse-url-at-mouse)
    (define-key map (kbd "<mouse-3>") 'browse-url-at-mouse)
    (define-key map [follow-link] 'mouse-face)
    map)
    "Make links clickable.")

(defun go-present--font-lock-links (&optional visibility)
  "Return properties that font-lock should set for links.

VISIBILITY controls whether to set the 'invisible property to
true."
  (list
   'face go-present-link-face
   'keymap go-present--link-map
   'help-echo (match-string-no-properties 2)
   'mouse-face 'highlight
   'invisible (not (eq visibility 'visible))
   'go-present-link (cons
                     (match-string-no-properties 2)
                     (match-string-no-properties 4))))

(defun go-present--not-at-code (start end)
  "Return true if text in the region from START to END contains
no code."
  (not (go-present--at-code start end)))

(defun go-present--at-code (start end)
  "Return true if text in the region from START to END contains
code."
  (text-property-any start end 'go-present--code t))

(defun go-present--make-font-lock-matcher (pred re)
  "Return a font-lock matcher which searches for regexp RE
matches satisfying the predicate PRED.

PRED is a predicate with two arguments (start and end of a
region)."
  (lambda (bound)
    (cl-block lambda
      (while t
        (let* ((pos (re-search-forward re bound t)))
          (when (and pos (funcall pred (match-beginning 0) (match-end 0)))
            (cl-return-from lambda pos))
          (when (null pos)
            (set-match-data (match-data) t)
            (cl-return-from lambda nil)))))))

(cl-defun go-present--code-font-lock-matcher (limit)
  "Match next code section."
  (let* ((code (or (go-present--next-code (point))
                   (cl-return-from go-present--code-font-lock-matcher)))
         (start (max (point) (car code)))
         (end (min limit (cadr code))))
    (when (<= start end )
      ;; Font lock requires that we set `match-data'. The
      ;; `set-match-data' function requires that the argument is a
      ;; result of a previous call to `match-data'. Therefore we do a
      ;; dummy search that matches the entire [start,end] region.
      (goto-char start)
      (re-search-forward  "\\(.*\n\\)*.*" end t))))

(defconst go-present--code-hlmark-re "^\\(.*?\\)\\(\\s-*// HL[0-9a-zA-Z_]*\\)$"
  "A regexp matching code highlight mark.")

(defconst go-present--keywords
  `((go-present--code-font-lock-matcher
     . (0 (list 'face 'go-present-code-face)))
    ;; Make lines with highlight mark matching code defintion
    ;; invisible.
    (,(go-present--make-font-lock-matcher
       (lambda (start end)
         (and (go-present--at-code start end)
              (string-suffix-p
               (get-text-property start 'go-present--code-hlmark)
               (buffer-substring start end))))
       go-present--code-hlmark-re)
     . ((1 (list 'face go-present-bold-code-face) t)
        (2 (list 'face go-present-code-face 'invisible t))))
    ;; Make highlight mark itself invisible.
    (,(go-present--make-font-lock-matcher
       #'go-present--at-code
       go-present--code-hlmark-re)
     . (2 (list 'face go-present-code-face 'invisible t)))
    ;; Make OMIT lines invisible. Note that top and bottom OMIT lines
    ;; are stripped when converitng address to region.
    (,(go-present--make-font-lock-matcher #'go-present--at-code "^.*OMIT\n")
     . (0 (list 'face go-present-code-face 'invisible t)))

    ;; Handle links.
    (,(go-present--make-font-lock-matcher
       #'go-present--not-at-code
       "\\(\\[\\[\\)\\([^][]*?\\)\\(\\]\\[\\)\\([^][]*?\\)\\(\\]\\]\\)") ; [[url][label]]
     . ((1 (go-present--font-lock-links))          ; [[
        (2 (go-present--font-lock-links))          ; url
        (3 (go-present--font-lock-links))          ; ][
        (4 (go-present--font-lock-links 'visible)) ; label
        (5 (go-present--font-lock-links))))        ; ]]
    (,(go-present--make-font-lock-matcher
       #'go-present--not-at-code
       "\\(\\[\\[\\)\\([^][]*?\\)\\(\\]\\]\\)")    ; [[url]]
     . ((1 (go-present--font-lock-links))          ; [[
        (2 (go-present--font-lock-links 'visible)) ; url
        (3 (go-present--font-lock-links))))        ; ]]

    ;; Handle special formatting.
    (,(go-present--make-font-lock-matcher #'go-present--not-at-code "^\\*+ .*")
     . go-present-slide-face)
    (,(go-present--make-font-lock-matcher #'go-present--not-at-code "\\*\\S-+\\*")
     . go-present-bold-face)
    (,(go-present--make-font-lock-matcher #'go-present--not-at-code "_\\S-+_")
     . go-present-italic-face)
    (,(go-present--make-font-lock-matcher #'go-present--not-at-code "`\\S-+`")
     . go-present-program-face)

    (,(go-present--make-font-lock-matcher
       #'go-present--not-at-code go-present--link-regexp)
     . ((1 go-present-keyword-face)
        (2 (go-present--font-lock-links 'visible))))

    (,(go-present--make-font-lock-matcher
       #'go-present--not-at-code
       (concat "^"
               (regexp-opt '(".code" ".play" ".image" ".background"
                             ".html" ".caption"))
               "\\b"))
     . go-present-keyword-face)

    ;; The first matching entry on the keywords list sets the 'face
    ;; property unless the override flag is set. For other properties,
    ;; the last matching entry always wins.
    ;;
    ;; The preformatted text should be the first face set. Due to the
    ;; above rule about other properties, we keep the preformatted
    ;; text rule at the end and let it override other faces.
    (,(go-present--make-font-lock-matcher #'go-present--not-at-code "^[ \t].*\n")
     . (0
        (list
         'face go-present-preformatted-text-face
         'keymap nil
         'help-echo nil
         'mouse-face nil
         'invisible nil
         'go-present-link nil)
        t))))

;;;###autoload
(define-derived-mode go-present-mode text-mode "go-present"
  "Major mode for editing Go present files"
  :syntax-table go-present-mode-syntax-table

  ;; Treat # in code sections as punctuation, not comments.
  (setq syntax-propertize-function 'go-present--propertize-syntax)

  ;; imenu
  (require 'imenu)
  (setq imenu-case-fold-search nil
        imenu-generic-expression '((nil "^\\*+\\s +\\(.*\\)$" 1))
        imenu-create-index-function #'imenu-default-create-index-function)

  ;; comments
  (setq comment-style 'plain
        comment-start "#"
        comment-end ""
        comment-multi-line t
        comment-padding " "
        comment-start-skip "#+[\t ]*"
        comment-use-syntax t
        comment-column 0
        comment-indent-function (lambda () 0))

  ;; defun movement
  (set (make-local-variable 'beginning-of-defun-function)
       (lambda ()
         (re-search-backward "^\\*\\s " nil t)))
  (set (make-local-variable 'end-of-defun-function)
       (lambda ()
         (forward-char 1)
         (re-search-forward "^\\*\\s " nil t)
         (backward-char 2)))

  ;; outline mode
  (set (make-local-variable 'outline-regexp) "[*]+")
  (add-to-invisibility-spec '(outline . t)) ; Show elipsis

  ;; font lock
  (setq font-lock-defaults '(go-present--keywords))
  (set (make-local-variable 'font-lock-extra-managed-props)
       '(keymap help-echo mouse-face invisible go-present-link))

  ;; Show all images
  (go-present-toggle-inline-images 1)

  ;; When the buffer is killed, kill preview processes and buffers.
  (add-hook 'kill-buffer-hook #'go-present--preview-kill-buffer)

  ;; Register a hooks that update line numbers for .code sections.
  (add-hook 'before-change-functions #'go-present--before-change-update-code)
  (add-hook 'after-change-functions #'go-present--after-change-update-code)

  (go-present-show-code 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slide\\'\\|\\.article\\'" . go-present-mode))

(defun go-present--toggle-font (font)
  "Toggle FONT for region or text at point.

Either surround the text with font strings or, if already
surrounded, drop font characters so that the regular font is
used."
  (unless (member font '("`" "*" "_"))
    (error "Unrecognized font: %s" font))
  (let* ((chars "-a-zA-Z0-9`~!@#$%^&*()_[]{}\\|'\"<>/?+=")
         (start (if (region-active-p) (region-beginning)
                  (save-excursion (skip-chars-backward chars) (point))))
         (end (if (region-active-p) (region-end)
                (save-excursion (skip-chars-forward chars) (point)))))
    (when (or (null start) (null end))
      (error "Can't toggle the font: invalid region"))
    (save-excursion
      (cond
       ((save-excursion
          (and (goto-char start)
               (search-forward "\n" end t)))
        (error "Region spans multiple lines"))
       ;; remove font
       ((save-excursion
          (and (and (goto-char start)
                    (looking-at (regexp-quote font)))
               (and (goto-char (1- end))
                    (looking-at (regexp-quote font)))
               (and (goto-char start)
                    (not (re-search-forward "[ \t]" end t)))))
        (goto-char (1+ start))
        (while (search-forward font (1- end) t)
          (replace-match " " t t))
        (goto-char end)
        (delete-char -1)
        (goto-char start)
        (delete-char 1))
       ;; add font
       (t
        (goto-char start)
        (while (re-search-forward "[ \t]" end t)
          (replace-match font t t))
        (goto-char end)
        (insert font)
        (goto-char start)
        (insert font))))))

(defun go-present-toggle-font-bold ()
  "Make region or text at point bold."
  (interactive)
  (go-present--toggle-font "*"))

(defun go-present-toggle-font-italic ()
  "Make region or text at point italic."
  (interactive)
  (go-present--toggle-font "_"))

(defun go-present-toggle-font-program ()
  "Use program font for region or text at point."
  (interactive)
  (go-present--toggle-font "`"))

(cl-defun go-present-preview ()
  "Preview presentation in a web browser.

Save the current buffer, start a Go present process, and open URL
with the slide at point in a web browser."
  (interactive)
  (let* ((pname (format "present-%s" (buffer-name)))
         (pbuf (format "*%s*" pname))
         (fname (or (buffer-file-name)
                    (error "preview: buffer must be visiting a file.")))
         (slide-path
          (concat
           (file-name-nondirectory (directory-file-name fname))
           "#" (int-to-string (go-present--slide-number-at-point)))))
    (setq go-present--preview-process-buffer pbuf)
    (with-current-buffer (get-buffer-create pbuf)
      (erase-buffer)
      (unless (get-process pname)
        (go-present--preview-start-go-present pname
                                              pbuf
                                              slide-path
                                              go-present-base-port)
        (cl-return-from go-present-preview))
      (browse-url (concat go-present--hostport "/" slide-path)))))

(defun go-present--slide-number-at-point ()
  "Return the number of the slide where the point is currently."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((re "^\\* ")
             (n (if (and (looking-at re)
                         (not (go-present--code-at (point))))
                    2
                  1)))
        (while (re-search-backward re nil t)
          (unless (go-present--code-at (point))
            (cl-incf n)))
        n))))

(defun go-present--preview-start-go-present (pname pbuf slide port)
  "Asynchronously start Go present server on PORT and navigate to
SLIDE in the browser.

The new process is set so that Emacs won't ask about it when
quitting.

If PORT is not available, then find the first available one that
is larger than PORT.

SLIDE is a path added to server's. For example, 't.slide#2'"
  (setq go-present--port port)
  (setq go-present--slide-path slide)
  (message "present: starting Go present at 127.0.01:%d" port)
  (let ((proc (start-process pname
                             pbuf
                             "present"
                             (format "-http=127.0.0.1:%d" port))))
    (set-process-filter proc #'go-present--preview-filter)
    (set-process-query-on-exit-flag proc nil)))

(cl-defun go-present--preview-filter (proc input)
  "Process output from the Go present process.

This function is a callback called whenever a Go present process
prints a string. PROC identify the process. INPUT is the new
string.

This function collects all strings in the process buffer and
reacts to that input."
  (let ((pname (process-name proc))
        (pbuf (process-buffer proc)))
    (unless (buffer-live-p pbuf)
      (cl-return-from go-present--preview-filter))
    (with-current-buffer pbuf
      (goto-char (point-max))
      (insert input)
      (goto-char (point-min))
      (when (search-forward "bind: address already in use" nil t)
        (delete-process proc)
        (erase-buffer)
        (go-present--preview-start-go-present pname
                                              pbuf
                                              go-present--slide-path
                                              (1+ go-present--port))
        (cl-return-from go-present--preview-filter))
      (goto-char (point-min))
      (when (re-search-forward "Open your web browser and visit \\(.+\\)$" nil t)
        (setq go-present--hostport (match-string 1))
        (set-process-filter proc nil)
        (browse-url (concat go-present--hostport
                            "/"
                            go-present--slide-path))))))

(defun go-present--preview-kill-buffer ()
  "Kill preview process when the associated slides buffer is killed.

This function is registered as `kill-buffer-hook'."
  (when (and go-present--preview-process-buffer
             (buffer-live-p (get-buffer go-present--preview-process-buffer)))
    (kill-buffer go-present--preview-process-buffer)))

(cl-defun go-present-toggle-inline-images (&optional arg)
  "Toggle whether images are displayed in the slide buffer.

Dimensions of images are limited to `go-present-max-image-size'."
  (interactive)
  (unless (display-graphic-p)
    (cl-return-from go-present-toggle-inline-images))
  (save-excursion
    (save-restriction
      (widen)
      ;; Remove image overlays.
      (let ((visible
             (cl-find-if
              (lambda (ov) (overlay-get ov 'go-present--image-overlay))
              (overlays-in (point-min) (point-max)))))
        (remove-overlays (point-min) (point-max) 'go-present--image-overlay t)
        (when (and visible (not arg))
          (cl-return-from go-present-toggle-inline-images)))
      ;; Create overlays for images.
      (goto-char (point-min))
      (while (re-search-forward go-present--image-regexp nil t)
        (let*
            ((start (match-beginning 0))
             (end (match-end 0))
             (fname (directory-file-name (expand-file-name (match-string 1))))
             (height (and (match-string 2)
                          (not (string-equal (match-string 2) "_"))
                          (string-to-number (match-string 2))))
             (width (and (match-string 3)
                         (not (string-equal (match-string 3) "_"))
                         (string-to-number (match-string 3))))
             (img (and
                   (file-exists-p fname)
                   (apply #'create-image
                          (append
                           (list fname 'imagemagick nil)
                           (cond
                            ((and (null width) (null height))
                             (list :width go-present-max-image-size))
                            ((and width height)
                             (let ((s (/ go-present-max-image-size
                                         (float (max width height)))))
                               (list :width (truncate (* width s))
                                     :height (truncate (* height s)))))
                            (height
                             (list :height (min height go-present-max-image-size)))
                            (width
                             (list :width (min width go-present-max-image-size)))))))))
          (when img
            (let ((ov (make-overlay start end)))
              (overlay-put ov 'display img)
              (overlay-put ov 'face 'default)
              (overlay-put ov 'go-present--image-overlay t))))))))

(cl-defun go-present-insert-edit-link ()
  "Insert or edit a bracket-style link.

If the point is at a link, then update the url and label with
values read from the user. Otherwise, create a new link."
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at-p "\\.link\\s-+"))
    (error "Can't add a bracket-style link on a .link line"))
  ;; Insert a new link.
  (unless (get-text-property (point) 'go-present-link)
    (let* ((url (read-string "URL: " "http://"))
           (label (read-string
                   "Label: "
                   (let ((case-fold-search t))
                     (if (string-match "^http://" url)
                         (substring url (match-end 0))
                       url)))))
      (insert "[[" url "][" label "]]"))
    (cl-return-from go-present-insert-edit-link))
  ;; Edit an existing link.
  (let* ((link (get-text-property (point) 'go-present-link))
         (link-url (and link (car link)))
         (link-label (and link (cdr link)))
         (start (save-excursion
                  (re-search-forward "[^[]")
                  (re-search-backward "\\[\\[")
                  (point)))
         (end (save-excursion (re-search-forward "\\]\\]") (point)))
         (url (read-string "URL: " link-url))
         (label (read-string "Label: " link-label)))
    (delete-region start end)
    (insert "[[" url "][" label "]]")))

(cl-defun go-present--add-code-modification-hook (buf slide-buf)
  "Setup a hook that updates code in SLIDE-BUF when BUF is
modified.

BUF is typically a buffer visiting a file with code. SLIDE-BUF is
typically a Go present buffer."
  (unless buf
    (cl-return-from go-present--add-code-modification-hook))
  (with-current-buffer buf
    (add-to-list 'go-present--update-buffers-list slide-buf)
    (add-to-list 'after-change-functions #'go-present--code-modification-hook)))

(defun go-present--code-modification-hook (_start _end _length)
  "Update code sections in all Go present buffers referencing current buffer.

This function is a hook run after all changes. Buffer local
`go-present--update-buffers-list' tracks buffers which need to be
updated whenever current buffer is modified."
  ;; Emacs sets after-change-functions to nil in case of unhandled
  ;; errors. We handle all errors to avoid affecting other modes.
  (condition-case err
      (let ((fname buffer-file-name))
        (setq go-present--update-buffers-list
              (cl-loop for buf in go-present--update-buffers-list
                       if (buffer-live-p buf)
                       collect buf))
        (cl-loop for buf in go-present--update-buffers-list
                 do (with-current-buffer buf
                      (go-present--hide-code fname)
                      (go-present--show-code fname))))
    (error
     (message "go-present error: %s" (error-message-string err))
     (setq after-change-functions
           (delete #'go-present--code-modification-hook
                   after-change-functions)))))

(defun go-present-show-code (&optional arg)
  "Show code sections in the current buffer.

If the code was already shown then local modification are dropped
and the code section is reverted to the current state of the
underlying code buffer.

With negative ARG, all code sections are removed."
  (interactive "p")
  (go-present--hide-code)
  (when (and arg (>= arg 0))
    (go-present--show-code nil)))

(defun go-present--hide-code (&optional code-fname)
  "Remove all code sections presenting code from the CODE-FNAME
file.

Remove all code sections if CODE-FNAME is nil."
  (let ((mod (buffer-modified-p))
        (inhibit-read-only t)
        (prop 'go-present--code-location))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((start (next-single-property-change (point) prop))
                 (end (or (and start
                               (next-single-property-change start prop))
                          (point-max))))
            (goto-char end)
            (when (and start
                       (or (null code-fname)
                           (string-equal (car (get-text-property start prop))
                                         code-fname)))
              ;; We have to clear overlays here because
              ;; before-change-functions hooks (which usually clear
              ;; those overlays) are inhibited when
              ;; go-present--code-modification-hook runs.
              (remove-overlays start end 'go-present--show-lines-overlay t)
              (delete-region start end))))))
    (set-buffer-modified-p mod)))

(defun go-present--show-code (&optional code-fname)
  "Show all code sections referencing the CODE-FNAME file.

Show all code sections if CODE-FNAME is nil."
  (let ((mod (buffer-modified-p))
        (inhibit-read-only t))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward go-present--code-start-regexp nil t)
          (forward-line 0)
          (let* ((code (go-present--parse-code-at-point))
                 (start (progn (forward-line) (point)))
                 (show-numbers (string-equal (nth 1 code) "-numbers"))
                 (fname (expand-file-name (nth 2 code)))
                 (addr (nth 3 code))
                 (hlmark (nth 4 code)))
            (when (or (null code-fname) (string-equal fname code-fname))
              (go-present--add-code-modification-hook
               (find-buffer-visiting fname)
               (current-buffer))
              (let* ((code (go-present--extract-code fname addr))
                     (show-numbers (and show-numbers (car code))))
                (insert (or (cdr code) ""))
                (set-text-properties start (point)
                                     (list
                                      'go-present--code t
                                      'go-present--code-location (cons fname addr)
                                      'go-present--code-show-numbers show-numbers
                                      'go-present--code-hlmark hlmark
                                      'front-sticky t
                                      'rear-nonsticky t))
                ;; We have to add overlays here because
                ;; after-change-functions hooks (which usually add
                ;; line number overlays) are inhibited when
                ;; go-present--code-modification-hook runs.
                (go-present--show-line-numbers-at start)))))))
    (set-buffer-modified-p mod)))

(defun go-present--parse-code-at-point ()
  "Parse .code or .play invocation starting at point.

Given invocation:
.code -numbers code.go /start/,/end/ HLcode
returns a list with five elements:
  - code type (\".code\")
  - flags (\"-numbers\")
  - file name (\"code.go\")
  - address in the file (\"/start/,/end/\")
  - highlight mark (\"HLcode\")"
  (let* ((hl-re "\\s-+HL\\([a-zA-Z0-9_]+\\)?$")
         (hl-pos (save-excursion
                   (when (re-search-forward hl-re (point-at-eol) t)
                     (match-beginning 0))))
         (hl (concat "HL"
                     (or (and hl-pos (match-string-no-properties 1)) "")))
         (code-re (concat "^"
                          "\\.\\(code\\|play\\)\\s-+"
                          "\\(?:\\(-edit\\|-numbers\\)\\s-+\\)*"
                          "\\([^\n\t\v ]+\\)"
                          "\\(?:\\s-+\\(.*\\)\\)?"))
         (code-pos (save-excursion (re-search-forward code-re hl-pos t))))
    (when code-pos
      (list (and code-pos (match-string-no-properties 1)) ; code type
            (and code-pos (or (match-string-no-properties 2) "")) ; flags
            (and code-pos (match-string-no-properties 3)) ; filename
            (and code-pos (or (match-string-no-properties 4) "")) ; address
            hl))))

(defun go-present--to-emacs-regexp (addr)
  "Return regexp address in Emacs regexp syntax.

ADDR should be of the form /RE/ where RE is a regexp."
  (replace-regexp-in-string
   "\\(\\\\\\\\\\)*\\(\\\\?\\)\\([()]\\)"
   (lambda (s)
     (concat
      (match-string 1 s)
      (if (string-equal (match-string 2 s) "") "\\" "")
      (match-string 3 s)))
   addr
   t ; FIXEDCASE
   t)) ; LITERAL

(defun go-present--extract-code (fname addr)
  "Extract region identified by ADDR from file FNAME.

ADDR is a Go present address (e.g., \"/start/,/end/\").

The function returns a list with two elements:
  - line number at which the returned region starts in the file
  - string extracted from the file"
  (condition-case err
      (with-temp-buffer
        (go-present--insert-buffer-or-file fname)
        (let* ((region (go-present--addr-str-to-region addr))
               (start-line (progn
                             (goto-char (car region))
                             (line-number-at-pos))))
          (cons start-line
                (buffer-substring-no-properties (car region) (cdr region)))))
    (search-failed (message "Can't find '%s' in %s: %s"
                            addr
                            fname
                            (error-message-string err))
                   nil)
    (file-error (message "Can't find '%s' in %s: %s"
                         addr
                         fname
                         (error-message-string err))
                nil)
    (invalid-regexp (message "The address %s is invalid: %s"
                             addr
                             (error-message-string err))
                    nil)))

(cl-defun go-present--insert-buffer-or-file (fname)
  "Insert, at point, the content of file FNAME.

As an optimization, returns content of a buffer visiting FNAME if
one exists."
  (let ((buf (get-file-buffer fname)))
    (unless buf
      (insert-file-contents-literally fname)
      (cl-return-from go-present--insert-buffer-or-file))
    (insert
     (with-current-buffer buf
       (save-restriction
         (widen)
         (buffer-substring-no-properties (point-min) (point-max)))))))

(cl-defun go-present--code-at (pos)
  "Return region containing code section at POS.

If POS is not at a code section, then returns nil. Otherwise
returns a list with two elements: min and max point that are in
the same code section."
  (unless (get-text-property pos 'go-present--code)
    (cl-return-from go-present--code-at))
  (save-restriction
    (widen)
    (let* ((end (or (next-single-property-change pos 'go-present--code)
                    (point-max)))
           (start (or (previous-single-property-change end 'go-present--code)
                      (point-min))))
      (list start end))))

(cl-defun go-present--next-code (pos)
  "Search forward for a code section starting at POS.

Return a pair of positions (START END) that represent the first
position in the code section and the first position right after
the code section.

If POS is inside a code section then return that code section.

Otherwise search forward and return the closest code section.

If end of buffer is reached, then return nil.

This function can be used to iterate over all code
sections. Start with POS=(point-min) and keep calling the
function with cdr of the result until it returns nil."
  (let ((at-pos (go-present--code-at pos)))
    (when at-pos
      (cl-return-from go-present--next-code at-pos)))
  (let ((next (next-single-property-change pos 'go-present--code)))
    (when next
      (cl-return-from go-present--next-code (go-present--code-at next)))))

(cl-defun go-present--before-change-update-code (start end)
  "Remove line-number overlays from code sections that intersect
the region defined by START and END.

This is a hook run before all changes.

`go-present--after-change-update-code' adds those overlays back.

Those two functions together implement updating line numbers in
code sections as the code is modified."
  (unless (equal major-mode 'go-present-mode)
    (cl-return-from go-present--before-change-update-code))
  (save-excursion
    (save-restriction
      (widen)
      (let ((code-start (car (go-present--code-at start)))
            (code-end (cadr (go-present--code-at end))))
        (remove-overlays (or code-start start)
                         (or code-end end)
                         'go-present--show-lines-overlay t)))))

(cl-defun go-present--show-line-numbers-at (pos)
  "Create line number overlays for the code section at POS.

The code section must have \"-numbers\" flag set for this
function to have an effect."
  (unless (equal major-mode 'go-present-mode)
    (cl-return-from go-present--show-line-numbers-at))
  (unless (get-text-property pos 'go-present--code-show-numbers)
    (cl-return-from go-present--show-line-numbers-at))
  (save-excursion
    (save-restriction
      (widen)
      (let* ((code (or (go-present--code-at pos)
                       (cl-return-from go-present--after-change-update-code)))
             (start (car code))
             (end (cadr code))
             (n (count-lines start end))
             (lineno (get-text-property pos 'go-present--code-show-numbers))
             (fmt (format "%%%dd " (length (number-to-string (+ lineno n))))))
        (remove-overlays start end 'go-present--show-lines-overlay t)
        ;; Creating long code sections is most likely a mistake. Don't
        ;; show line numbers to limit performance impact.
        (unless (< n 1000)
          (cl-return-from go-present--show-line-numbers-at))
        (goto-char start)
        (while (< (point) end)
          ;; Don't create overlays for invisible lines because it's very
          ;; hard to setup font-lock to hide correct line number
          ;; overlays.
          (unless (looking-at ".*OMIT$")
            (let ((ov (make-overlay (point) (point))))
              (overlay-put ov 'go-present--show-lines-overlay t)
              (overlay-put ov
                           'before-string
                           (propertize (format fmt lineno)
                                       'font-lock-face
                                       'go-present-code-face))))
          (cl-incf lineno)
          (forward-line 1))))))

(cl-defun go-present--after-change-update-code (start end _len)
  "Update code sections after buffer modifications.

This is a hook run after all modifications.

Mark text inserted in a code section as code. That makes
copy-pasting behavior intuitive.

Add line number overlays for all code sections intersecting the
region from START to END that have the \"-numbers\" flag set.

`go-present--before-change-update-code' is a hook run before
modifications, which clears line number overlays. Those two
functions together keep line-number overlays up-to-date."
  (unless (equal major-mode 'go-present-mode)
    (cl-return-from go-present--after-change-update-code))
  (save-restriction
    (widen)
    ;; Inserting into code area makes the inserted text part of the
    ;; code.
    (when (and (or (equal start (point-min))
                   (get-text-property (1- start) 'go-present--code))
               (get-text-property end 'go-present--code))
      (set-text-properties start end (text-properties-at end)))
    ;; Move start backwards in case we've just deleted part of code
    ;; region and point is right after remaining code.
    (setq start (max (1- start) (point-min)))
    (while t
      (let* ((code (go-present--next-code start))
             (cstart (or (car code) (point-min)))
             (cend (or (cadr code) (point-max))))
        (unless code
          (cl-return-from go-present--after-change-update-code))
        (go-present--show-line-numbers-at cstart)
        (setq start cend)))))

;; Yes, I probably should write a real parser.
(defconst go-present--addr-part-re
  (concat
   "^"
   "\\(?:"
     "\\([0-9]+\\)" "\\|" ; Absolute line number.
     "\\(\\$\\)" "\\|" ; '$', which means end of file.
     "\\(?:"  ; A regular expression delimited by a pair of '/'.
     "/\\(\\(?:\\\\/\\|[^/]\\)*\\)/"
     "\\)"
   "\\)?"
   "\\([-+#0-9]+\\)?" ; Character or line offset.
   )
  "Regular expression for parsing Go present addresses.
An address has up to two parts separated by a comma.  This regular
expressions matches one part.

An example address: \"/start/-#1,/end/-#1\".")

(defun go-present--parse-addr-str-part (addr-part)
  "Parse a part of Go present address.

Return a list with three elements:
 - BASE
 - OFFSET
 - END

END is the index in ADDR-PART string where the parser stopped.

To find a position in a file search for BASE and then move the
point as defined by OFFSET.

BASE is a pair (TYPE VALUE), where:
 - TYPE 'line means \"go to line VALUE\".
 - TYPE 'regexp means \"search forward for regexp VALUE\".
 - TYPE 'end-of-file means \"go to the end of the file\".

OFFSET is a pair (TYPE VALUE) where:
 - TYPE 'line-offset means \"move forward by VALUE lines\". It
   can be negative.
 - TYPE 'char-offset-positive and 'char-offset-negative mean
   \"move forward/backward by VALUE characters\". If BASE is a
   regular expression then char-offset-positive moves forward
   from the end of regexp match, and char-offset-negative moves
   backwards from the beginning of regexp match.
     
Also, see `go-present--addr-part-re'."
  (unless (string-match go-present--addr-part-re addr-part)
    (error "Can't parse address: '%s'" addr-part))
  (let ((end (match-end 0))
        (base
         (let ((line (match-string 1 addr-part))
               (dollar (match-string 2 addr-part))
               (regexp (match-string 3 addr-part)))
           (cond
            (line `(line ,(string-to-number line)))
            (regexp `(regexp ,(go-present--to-emacs-regexp regexp)))
            (dollar `(end-of-file)))))
        (offset
         (let ((offset-str (match-string 4 addr-part)))
           (cond
            ((null offset-str) nil)
            ((string-match "[+-][0-9]+" offset-str)
             `(line-offset ,(string-to-number (match-string 0 offset-str))))
            ((string-match "\\+#\\([0-9]+\\)" offset-str)
             `(char-offset-positive ,(string-to-number (match-string 1 offset-str))))
            ((string-match "-#\\([0-9]+\\)" offset-str)
             `(char-offset-negative ,(- (string-to-number (match-string 1 offset-str)))))))))
    (list base offset end)))

(defun go-present--addr-part-to-point (addr &optional start)
  "Return position in the current buffer, defined by ADDR.

START defines position to start parsing ADDR from.

See `go-present--parse-addr-str-part' for instruction how address
looks like and how it defines a position in a file."
  (goto-char (or start (point-min)))
  (let* ((base (car addr))
         (offset (cadr addr))
         (base-point
          (cond
           ((null base)
            (cons (point) (point)))
           ((eq (car base) 'regexp)
            (let ((case-fold-search nil))
              (re-search-forward (cadr base)))
            (cons (match-beginning 0) (match-end 0)))
           ((eq (car base) 'line)
            (goto-char (point-min))
            (forward-line (1- (cadr base)))
            (cons (point-at-bol) (point-at-eol)))
           ((eq (car base) 'start-of-file)
            (goto-char (point-min))
            (cons (point) (point)))
           ((eq (car base) 'end-of-file)
            (goto-char (point-max))
            (cons (point) (point))))))
    (goto-char (cdr base-point))
    (cond
     ((null offset))
     ((eq (car offset) 'line-offset)
      (forward-line (cadr offset)))
     ((eq (car offset) 'char-offset-positive)
      (forward-char (cadr offset)))
     ((eq (car offset) 'char-offset-negative)
      (goto-char (car base-point))
      (forward-char (cadr offset))))
    (point)))

(defun go-present--addr-str-to-region (addr-str)
  "Return substring of the current buffer as defined by
ADDR-STR."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((addr (if (string-equal addr-str "")
                       '(((start-of-file) nil)
                         ((end-of-file) nil))
                     (let* ((low (go-present--parse-addr-str-part addr-str))
                            (low-base (car low))
                            (low-offset (cadr low))
                            (high-start (1+ (cl-caddr low))) ; skip the comma
                            (high
                             (if (<= high-start (length addr-str))
                                 (go-present--parse-addr-str-part (substring addr-str high-start))
                               nil))
                            (high-base (car high))
                            (high-offset (cadr high)))
                       (list
                        (list low-base low-offset)
                        (list high-base high-offset)))))
             (start (go-present--addr-part-to-point (car addr)))
             (end (go-present--addr-part-to-point (cadr addr) start)))
        (cons
         (progn (goto-char start)
                (forward-line 0)
                (while (and (not (eobp))
                            (looking-at "^$\\|.*OMIT$"))
                  (forward-line 1))
                (point))
         (progn (goto-char end)
                (forward-line 0)
                (while (and (not (bobp))
                            (looking-at "^$\\|.*OMIT$"))
                  (forward-line -1))
                (forward-line 1)
                (point)))))))

(defun go-present-save-buffer (&optional arg)
  "Save the buffer without storing any of the code sections.

This function replaces `save-buffer' in `go-present-mode'."
  (interactive "p")
  (go-present-show-code -1)
  (save-buffer arg)
  (go-present-show-code 1))

;; Use dynamic binding for compilation-save-buffers-predicate.
(defvar compilation-save-buffers-predicate)

(defun go-present-run-code-at-point ()
  "Execute code section at point."
  (interactive)
  (let* ((loc (or (get-text-property (point) 'go-present--code-location)
                  (error "Go present: no code at point")))
         (fname (car loc))
         (addr (cdr loc))
         (tmpfile (concat (make-temp-name "/tmp/go-present-") ".go"))
         (code-pos (go-present--code-at (point)))
         (code (buffer-substring-no-properties
                (car code-pos)
                (cadr code-pos))))
    (condition-case err
        (progn
          (with-temp-file tmpfile
            (progn
              (go-present--insert-buffer-or-file fname)
              (let ((region (go-present--addr-str-to-region addr)))
                (goto-char (car region))
                (delete-region (car region) (cdr region))
                (insert code))))
          (let ((compilation-save-buffers-predicate (lambda () nil)))
            (compile (concat "go run " tmpfile))))
      (search-failed (message "Can't find '%s' in %s: %s"
                              addr
                              fname
                              (error-message-string err))
                     ""))))

(provide 'go-present-mode)

;;; go-present-mode.el ends here

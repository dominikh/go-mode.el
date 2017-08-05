;; Copyright 2017 The Go Authors. All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

(require 'ert)
(require 'go-present-mode)

(ert-deftest go-present-test--slides ()
  ;; New slide at the beginning of line.
  (cl-letf ((buf (with-temp-buffer
                    (go-present-mode)
                    (insert "Title\n\n")
                    (go-present-insert-slide)
                    (insert "slide\n")
                    (buffer-substring (point-min) (point-max)))))
    (should (string-equal buf "Title\n\n* slide\n")))

  ;; New slide in the middle of a line.
  (cl-letf ((buf (with-temp-buffer
                    (go-present-mode)
                    (insert "Title\n\n")
                    (insert "new slide\n")
                    (search-backward "slide")
                    (go-present-insert-slide)
                    (buffer-substring (point-min) (point-max)))))
    (should (string-equal buf "Title\n\nnew \n* slide\n")))

  ;; New slide before existing one.
  (cl-letf ((buf (with-temp-buffer
                   (go-present-mode)
                   (insert "Title\n\n")
                   (insert "* slide 1\n")
                   (forward-line -1)
                   (go-present-insert-slide)
                   (insert "slide 2\n")
                   (buffer-substring (point-min) (point-max)))))
    (should (string-equal buf "Title\n\n* slide 2\n* slide 1\n")))

  ;; New slide after the current one.
  (cl-letf ((buf (with-temp-buffer
                   (go-present-mode)
                   (insert "Title\n\n")
                   (insert "* slide 1\n")
                   (search-backward "slide 1")
                   (go-present-insert-slide)
                   (insert "slide 2")
                   (buffer-substring (point-min) (point-max)))))
    (should (string-equal buf "Title\n\n* slide 1\n* slide 2\n")))

  ;; New slide at end of buffer.
  (cl-letf ((buf (with-temp-buffer
                   (go-present-mode)
                   (insert "Title\n\n")
                   (insert "* slide 1")
                   (go-present-insert-slide)
                   (insert "slide 2\n")
                   (buffer-substring (point-min) (point-max)))))
    (should (string-equal buf "Title\n\n* slide 1\n* slide 2\n"))))

(ert-deftest go-present-test--defun-movement ()
  (with-temp-buffer
    (go-present-mode)
    (insert "Title\n\n")
    (insert "* slide 1\n")
    (insert "text 1\n")
    (insert "* slide 2\n")
    (insert "* slide 3\n")

    (beginning-of-buffer)

    (end-of-defun)
    (should (string-equal
             "3:* slide 1"
             (format "%d:%s"
                     (line-number-at-pos)
                     (buffer-substring (point) (point-at-eol)))))
    (end-of-defun)
    (should (string-equal
             "5:* slide 2"
             (format "%d:%s"
                     (line-number-at-pos)
                     (buffer-substring (point) (point-at-eol)))))

    (end-of-defun)
    (should (string-equal
             "6:* slide 3"
             (format "%d:%s"
                     (line-number-at-pos)
                     (buffer-substring (point) (point-at-eol)))))

    (end-of-defun)
    (should (string-equal
             "6:* slide 3"
             (format "%d:%s"
                     (line-number-at-pos)
                     (buffer-substring (point) (point-at-eol)))))


    (beginning-of-defun)
    (should (string-equal
             "5:* slide 2"
             (format "%d:%s"
                     (line-number-at-pos)
                     (buffer-substring (point) (point-at-eol)))))

    (beginning-of-defun)
    (should (string-equal
             "3:* slide 1"
             (format "%d:%s"
                     (line-number-at-pos)
                     (buffer-substring (point) (point-at-eol)))))

    (beginning-of-defun)
    (should (string-equal
             "3:* slide 1"
             (format "%d:%s"
                     (line-number-at-pos)
                     (buffer-substring (point) (point-at-eol)))))))

(ert-deftest go-present-test--imenu ()
  (with-temp-buffer
    (go-present-mode)
    (insert "Title\n\n")
    (insert "* Slide 1\n")
    (insert "text\n")
    (insert "* Slide 2\n")

    (imenu "Slide 1")
    (should (string-equal
             "3:* Slide 1"
             (format "%d:%s"
                     (line-number-at-pos)
                     (buffer-substring (point) (point-at-eol)))))

    (imenu "Slide 2")
    (should (string-equal
             "5:* Slide 2"
             (format "%d:%s"
                     (line-number-at-pos)
                     (buffer-substring (point) (point-at-eol)))))))

(ert-deftest go-present-test--comments ()
  (with-temp-buffer
    (go-present-mode)
    (insert "Title\n\n")
    (insert "* Slide 1\n")
    (insert "text\n")
    (insert "* Slide 2\n")

    (beginning-of-buffer)
    (comment-region (save-excursion
                      (re-search-forward "^\\* Slide 1")
                      (match-beginning 0))
                    (save-excursion
                      (re-search-forward "^\\* Slide 2")
                      (match-beginning 0)))
    (should (string-equal "Title\n\n# * Slide 1\n# text\n* Slide 2\n"
                          (buffer-substring (point-min) (point-max))))

    (uncomment-region (save-excursion
                        (beginning-of-buffer)
                        (re-search-forward "#")
                        (match-beginning 0))
                      (save-excursion
                        (end-of-buffer)
                        (re-search-backward "#")
                        (match-end 0)))
    (should (string-equal "Title\n\n* Slide 1\ntext\n* Slide 2\n"
                          (buffer-substring (point-min) (point-max))))))

(ert-deftest go-present-test--open-at-point ()
  (with-temp-buffer
    (cl-letf ((opened-url "")
              (opened-file ""))
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url) (setq opened-url url))))
        (insert (concat "Title\n\n"
                        ".link www.example1.com\n"
                        ".iframe www.example2.com\n"
                        "[[www.example3.com][example 3]]  [[www.example4.com]]\n"
                        ".image testdata/go-present-mode/img.png\n"
                        ".background testdata/go-present-mode/bg.png\n"
                        ".html testdata/go-present-mode/index.html\n"
                        ".code testdata/go-present-mode/code.go\n"
                        ".play testdata/go-present-mode/play.go\n"))
        (go-present-mode)
        (font-lock-fontify-buffer)
        (beginning-of-buffer)

        (search-forward ".link")
        (go-present-open-at-point)
        (should (string-equal "www.example1.com" opened-url))

        (search-forward ".iframe")
        (go-present-open-at-point)
        (should (string-equal "www.example2.com" opened-url))

        (search-forward " 3")
        (go-present-open-at-point)
        (should (string-equal "www.example3.com" opened-url))

        (search-forward "4")
        (go-present-open-at-point)
        (should (string-equal "www.example4.com" opened-url))

        (save-window-excursion
          (search-forward ".image")
          (go-present-open-at-point)
          (should (string-equal "img.png" (buffer-name))))

        (save-window-excursion
          (search-forward ".background")
          (go-present-open-at-point)
          (should (string-equal "bg.png" (buffer-name))))

        (save-window-excursion
          (search-forward ".html")
          (go-present-open-at-point)
          (should (string-equal "index.html" (buffer-name)))
          (should (looking-at-p "^<html></html>")))

        (save-window-excursion
          (search-forward ".code")
          (go-present-open-at-point)
          (should (string-equal "code.go" (buffer-name)))
          (should (looking-at-p "^// code")))

        (save-window-excursion
          (search-forward ".play")
          (go-present-open-at-point)
          (should (string-equal "play.go" (buffer-name)))
          (should (looking-at-p "^// play")))))))

(ert-deftest go-present-test--preview ()
  (cl-letf*
      ((go-present-base-port 7000)
       (f (make-temp-file "go-present-preview-test"))
       (prev-start-process (symbol-function 'start-process))
       (present (concat (expand-file-name default-directory) "testdata/go-present-mode/fake-present.sh"))
       (visited-url "")
       ((symbol-function 'browse-url)
        (lambda (url &rest args)
          (setq visited-url url)))
       ((symbol-function 'start-process)
        (lambda (name buffer program &rest args)
          (unless (string-equal program "present")
            (error "Unexpected program value '%s'. Should be 'present'" program))
          (apply prev-start-process
                 (append (list name buffer "/bin/sh" present) args)))))
    (find-file f)
    (go-present-mode)
    (insert "Title\n"
            "*subtitle*\n"
            "\n"
            "Author\n"
            "* Slide 1\n"
            "Text\n\n"
            "* Slide 2\n")
    (beginning-of-buffer)

    (go-present-preview)
    (sleep-for 0.1)
    (should (string-equal visited-url
                          (format "http://127.0.0.1:%d/%s#%d"
                                  go-present-base-port
                                  (file-name-base f)
                                  1)))

    (search-forward "Author")
    (go-present-preview)
    (sleep-for 0.1)
    (should (string-equal visited-url
                          (format "http://127.0.0.1:%d/%s#%d"
                                  go-present-base-port
                                  (file-name-base f)
                                  1)))

    (search-forward "Slide 1")
    (go-present-preview)
    (sleep-for 0.1)
    (should (string-equal visited-url
                          (format "http://127.0.0.1:%d/%s#%d"
                                  go-present-base-port
                                  (file-name-base f)
                                  2)))

    (search-forward "Text")
    (go-present-preview)
    (sleep-for 0.1)
    (should (string-equal visited-url
                          (format "http://127.0.0.1:%d/%s#%d"
                                  go-present-base-port
                                  (file-name-base f)
                                  2)))

    (search-forward "Slide 2")
    (go-present-preview)
    (sleep-for 0.1)
    (should (string-equal visited-url
                          (format "http://127.0.0.1:%d/%s#%d"
                                  go-present-base-port
                                  (file-name-base f)
                                  3)))))

(ert-deftest go-present-test--fonts ()
  (let ((text "text to test fonts on"))
    (with-temp-buffer
      (go-present-mode)
      (insert "Title\n\n"
              "* slide\n"
              text "\n")
      (beginning-of-buffer)

      (search-forward "text")
      (go-present-toggle-font-bold)
      (should (string-equal (buffer-substring (point-at-bol) (point-at-eol))
                            "*text* to test fonts on"))

      (search-forward "test")
      (go-present-toggle-font-program)
      (should (string-equal (buffer-substring (point-at-bol) (point-at-eol))
                            "*text* to `test` fonts on"))

      (search-forward "fonts")
      (go-present-toggle-font-italic)
      (should (string-equal (buffer-substring (point-at-bol) (point-at-eol))
                            "*text* to `test` _fonts_ on"))

      (transient-mark-mode 1)
      (end-of-buffer)
      (insert text "\n")
      (search-backward "text")
      (push-mark)
      (end-of-line)
      (go-present-toggle-font-bold)
      (should (string-equal (buffer-substring (point-at-bol) (point-at-eol))
                            "*text*to*test*fonts*on*")))))

(ert-deftest go-present-test--font-lock ()
  (with-temp-buffer
    (insert  "Title\n"
             "\n"
             "* Slide\n"
             "\n"
             "  preformatted\n"
             "\n"
             "*bold* `program` _italic_\n"
             "\n"
             ".link http://www.example.com\n"
             "\n"
             "[[http://www.example.com][example]]\n"
             "\n"
             "# Comment\n")
    (beginning-of-buffer)
    (go-present-mode)
    (font-lock-fontify-buffer)

    (search-forward "* Sli")
    (should (equal (face-at-point) 'go-present-slide-face))

    (search-forward "pref")
    (should (equal (face-at-point) 'go-present-preformatted-text-face))

    (search-forward "bold")
    (should (equal (face-at-point) 'go-present-bold-face))

    (search-forward "program")
    (should (equal (face-at-point) 'go-present-program-face))

    (search-forward "italic")
    (should (equal (face-at-point) 'go-present-italic-face))

    (search-forward "][exa")
    (should (equal (face-at-point) 'go-present-link-face))

    (search-forward "# Comm")
    (should (equal (face-at-point) 'font-lock-comment-face))))

(ert-deftest go-present--links ()
  (with-temp-buffer
    (insert "Title\n"
            "\n"
            "* Slide\n"
            "\n")
    (go-present-mode)

    ;; Create a new link.
    (font-lock-fontify-buffer)
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &optional initial-input history default-value
                               inherit-input-method)
                 (cond
                  ((string-equal prompt "Label: ") "Example")
                  ((string-equal prompt "URL: ") "http://www.example.com")
                  (t (error (format "unrecognized prompt '%s'" prompt)))))))
      (go-present-insert-edit-link)
      (should (string-equal (buffer-substring (point-at-bol) (point-at-eol))
                            "[[http://www.example.com][Example]]")))

    ;; Update a link.
    (font-lock-fontify-buffer)
    (search-backward "Exam")
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &optional initial-input history default-value
                               inherit-input-method)
                 (cond
                  ((string-equal prompt "Label: ") "EXAMPLE")
                  ((string-equal prompt "URL: ") "HTTP://WWW.EXAMPLE.COM")
                  (t (error (format "unrecognized prompt '%s'" prompt)))))))
      (go-present-insert-edit-link)
      (should (string-equal (buffer-substring (point-at-bol) (point-at-eol))
                            "[[HTTP://WWW.EXAMPLE.COM][EXAMPLE]]")))))

(ert-deftest go-present--code ()
  (with-temp-buffer
    (insert "Title\n"
            "\n"
            "* Slide\n"
            "\n"
            ".code testdata/go-present-mode/code.go\n")
    (go-present-mode)
    (font-lock-fontify-buffer)

    (beginning-of-buffer)
    (search-forward ".code")
    (forward-line)

    (should (string-equal (buffer-substring (point) (point-max))
                          (with-temp-buffer
                            (insert-file-contents-literally "testdata/go-present-mode/code.go")
                            (buffer-string))))

    (should (equal (face-at-point) 'go-present-code-face))

    (search-forward "skipped")
    (should (equal (get-text-property (point) 'invisible) t))

    (search-forward "fmt.Printf")
    (should (equal (face-at-point) 'go-present-bold-code-face))

    (search-forward "// H")
    (should (equal (get-text-property (point) 'invisible) t))))

(ert-deftest go-present--code-line-numbers ()
  (with-temp-buffer
    (insert "Title\n"
            "\n"
            "* Slide\n"
            "\n"
            ".code -numbers testdata/go-present-mode/code.go /func main/,/}/\n")
    (go-present-mode)
    (font-lock-fontify-buffer)
    (beginning-of-buffer)
    (search-forward ".code")
    (forward-line)

    ;; Verify that line number are correct.
    (let ((lines
           (reverse
            (cl-loop for ov in (overlays-in (point) (point-max))
                     if (overlay-get ov 'go-present--show-lines-overlay)
                     collect (overlay-get ov 'before-string)))))
      (should (equal lines '(" 8 " "10 " "11 "))))

    ;; Delete invisible text.
    (while (not (eobp))
      (let* ((start (next-single-property-change (point) 'invisible))
             (end (or (and start (next-single-property-change start 'invisible))
                      (point-max))))
        (goto-char end)
        (when (and start end)
          (delete-region start end))))

    ;; Verify that the code is correct.
    (search-backward ".code")
    (forward-line)
    (should (equal (buffer-substring (point) (point-max))
                   (concat "func main() {\n"
                           "	fmt.Printf(\"Hello World!\")\n"
                           "}\n")))))


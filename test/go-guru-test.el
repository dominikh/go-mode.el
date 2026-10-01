;;; go-guru-test.el --- Tests for go-guru.el  -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "..")
(require 'go-guru)

(ert-deftest go-guru--parse-position-supports-colons-in-paths ()
  (should (equal (go-guru--parse-position "/tmp/example.go:12:3")
                 '("/tmp/example.go" 12 3)))
  (should (equal (go-guru--parse-position "C:\\work\\example.go:12:3")
                 '("C:\\work\\example.go" 12 3)))
  (should-error (go-guru--parse-position "example.go:line:column")))

(ert-deftest go--godoc-command-keeps-query-as-one-argument ()
  (should (equal (go--godoc-command-args "go doc" "fmt; echo unsafe")
                 '("go" "doc" "fmt; echo unsafe")))
  (should-error (go--godoc-command-args "" "fmt")))

(ert-deftest go--godoc-starts-process-without-a-shell ()
  (let (start-args)
    (cl-letf (((symbol-function 'godoc--get-buffer)
               (lambda (_query) (get-buffer-create " *godoc test*")))
              ((symbol-function 'start-process)
               (lambda (&rest args) (setq start-args args) 'fake-process))
              ((symbol-function 'set-process-sentinel) (lambda (&rest _args) nil)))
      (unwind-protect
          (go--godoc "fmt; echo unsafe" "go doc")
        (when (get-buffer " *godoc test*")
          (kill-buffer " *godoc test*"))))
    (should (equal (nthcdr 2 start-args)
                   '("go" "doc" "fmt; echo unsafe")))))

(ert-deftest go-guru--error-output-preserves-content-without-newline ()
  (with-temp-buffer
    (insert "failed")
    (should (equal (go-guru--error-output (current-buffer)) "failed")))
  (with-temp-buffer
    (insert "failed\n")
    (should (equal (go-guru--error-output (current-buffer)) "failed")))
  (with-temp-buffer
    (should (equal (go-guru--error-output (current-buffer)) ""))))

(ert-deftest go-guru--compilation-filter-ignores-non-location-diagnostics ()
  (with-temp-buffer
    (insert "go: warning: missing module\n")
    (let ((compilation-filter-start (point-min)))
      (go-guru--compilation-filter-hook))
    (should (equal (buffer-string) "go: warning: missing module\n"))))

(ert-deftest go-guru--compilation-filter-keeps-drive-colon-in-path ()
  (with-temp-buffer
    (insert "C:\\src\\example.go:10:5: error\n")
    (let ((compilation-filter-start (point-min)))
      (go-guru--compilation-filter-hook))
    (should (equal (get-text-property (point-min) 'display)
                   "C:\\src\\example.go"))))

(ert-deftest go--coverage-parse-line-supports-drive-colons ()
  (should (equal (go--coverage-parse-line "C:/work/example.go:1.2,3.4 1 7")
                 '("C:/work/example.go" 1 2 3 4 1 7)))
  (should-not (go--coverage-parse-line "not a coverage record")))

(ert-deftest go--buffer-size-bytes-has-no-message-side-effect ()
  (let (message-called)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) (setq message-called t))))
      (with-temp-buffer
        (insert "abc")
        (should (= (go--buffer-size-bytes) 3))))
    (should-not message-called)))

;;; go-guru-test.el ends here

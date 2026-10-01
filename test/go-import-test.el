;;; go-import-test.el --- Tests for Go import editing  -*- lexical-binding: t; -*-

(require 'ert)
(require 'go-mode)

(ert-deftest go-import-add-does-not-duplicate-existing-imports ()
  (with-temp-buffer
    (go-mode)
    (insert "package main\n\nimport (\n\t\"fmt\"\n)\n")
    (let ((before (buffer-string)))
      (go-import-add nil "fmt")
      (should (equal (buffer-string) before))))
  (with-temp-buffer
    (go-mode)
    (insert "package main\n\nimport io \"io\"\n")
    (let ((before (buffer-string)))
      (go-import-add nil "io")
      (should (equal (buffer-string) before)))))

;;; go-import-test.el ends here

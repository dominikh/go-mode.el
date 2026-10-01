;;; go-rename-test.el --- Tests for go-rename.el  -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "..")
(require 'go-rename)

(ert-deftest go-rename--go-buffer-p-selects-go-files ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example.go")
    (should (go-rename--go-buffer-p))
    (setq buffer-file-name "/tmp/example.go.txt")
    (should-not (go-rename--go-buffer-p))
    (setq buffer-file-name nil)
    (should-not (go-rename--go-buffer-p))))

;;; go-rename-test.el ends here

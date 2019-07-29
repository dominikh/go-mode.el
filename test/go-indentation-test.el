;;; go-indentation-test.el

;; Copyright 2019 The go-mode Authors.  All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

(require 'ert)
(require 'go-mode)

(ert-deftest go--indent-line ()
  (dolist (file (directory-files (expand-file-name "testdata/indentation_tests/") t ".*\\.go$"))
    (with-temp-buffer
      (go-mode)
      (insert-file-contents file)
      (let ((contents-before-indent (buffer-string)))
        (indent-region (point-min) (point-max) nil)
        (should (string= contents-before-indent (buffer-string)))))))

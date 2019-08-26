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

(defun go--should-indent (input expected)
  "Run `indent-region' against INPUT and make sure it matches EXPECTED."
  (with-temp-buffer
    (go-mode)
    (insert input)
    (indent-region (point-min) (point-max))
    (should (string= (buffer-string) expected))))

(ert-deftest go--indent-top-level ()
  (go--should-indent
   "
package foo
  var foo = 123 +
    456 +
    789
"

   "
package foo
var foo = 123 +
	456 +
	789
"
   ))

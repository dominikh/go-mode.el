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
      (let ((contents-before-indent (buffer-string)) (inhibit-message t))
        (indent-region (point-min) (point-max) nil)
        (should (string= contents-before-indent (buffer-string)))))))

(ert-deftest go-dot-mod--indent-line ()
  (with-temp-buffer
    (go-dot-mod-mode)
    (insert-file-contents "testdata/indentation_tests/go.mod")
    (let ((contents-before-indent (buffer-string)) (inhibit-message t))
      (indent-region (point-min) (point-max) nil)
      (should (string= contents-before-indent (buffer-string))))))

(defun go--should-indent (input expected)
  "Run `indent-region' against INPUT and make sure it matches EXPECTED."
  (with-temp-buffer
    (go-mode)
    (insert input)
    (let ((inhibit-message t))
      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) expected)))))

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

(ert-deftest go--indent-multiline-comment ()
  (go--should-indent
   "
{
	/*
a
	*/
}
"

   "
{
	/*
	   a
	*/
}
")

  (go--should-indent
   "
{
	/*   LISTEN
a
	*/
}
"

   "
{
	/*   LISTEN
	     a
	*/
}
")

  (go--should-indent
   "
{
	/* c
	c
c
	*/
}
"

   "
{
	/* c
	c
	c
	*/
}
")

  (go--should-indent
   "
{
	/* cool
	*  cat
	 *
	*/
}
"

   "
{
	/* cool
	 *  cat
	 *
	 */
}
"))

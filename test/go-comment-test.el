;;; go-comment-test.el

;; Copyright 2020 The go-mode Authors. All rights reserved. Use of
;; this source code is governed by a BSD-style license that can be
;; found in the LICENSE file.

(require 'ert)
(require 'go-mode)
(require 'cl-lib)

(ert-deftest go--comment-region ()
  (go--should-comment
   "
<var foo int
>"
   "
// var foo int
")

  (go--should-comment
   "
<// var foo int
>"
   "
var foo int
")

  (go--should-comment
   "var <foo> int"
   "var /* foo */ int")

  (go--should-comment
   "var </* foo */> int"
   "var foo int"))

(defun go--should-comment (got expected)
  "Run `comment-dwim' against GOT and make sure it matches EXPECTED.

<> in GOT represents point. If they aren't next to each other, then it
represents point and mark to test the region based comment-region."
  (with-temp-buffer
    (go-mode)
    (transient-mark-mode)
    (insert got)
    (goto-char (point-min))
    (let ((beg (progn (search-forward "<") (delete-char -1) (point)))
          (end (progn (search-forward ">") (delete-char -1) (point))))
      (when (/= beg end)
        (set-mark beg))
      (goto-char end)
      (call-interactively 'comment-dwim)
      (should (string= (buffer-string) expected)))))

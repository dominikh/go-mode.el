;;; go-compile-test.el --- unit tests for ‘go-mode’ compilation -*- lexical-binding: t; -*-

;; Copyright 2020 The go-mode Authors. All rights reserved. Use of
;; this source code is governed by a BSD-style license that can be
;; found in the LICENSE file.

;;; Commentary:

;; Unit tests for the ‘compilation-mode’ integration of ‘go-mode’.

;;; Code:

(require 'go-mode)

(require 'cl-lib)
(require 'ert)

(ert-deftest go-greedy-test-pattern ()
  "Verify that https://github.com/dominikh/go-mode.el/issues/361 is fixed."
  ;; ‘compilation-mode’ doesn’t have its own syntax table, so we use the
  ;; standard one.
  (with-temp-buffer (go-mode))  ; initialize once
  (with-syntax-table (standard-syntax-table)
    (should-not (string-match-p
                 (cadr (assq 'go-test compilation-error-regexp-alist-alist))
                 "\nfile.go:1:2: word word"))))

(ert-deftest go-1.14-test-v ()
  "Verify that https://github.com/dominikh/go-mode.el/issues/362 is fixed."
  (with-temp-buffer (go-mode))  ; initialize once
  (with-syntax-table (standard-syntax-table)
    (cl-destructuring-bind (regexp file line)
        (cdr (assq 'go-test compilation-error-regexp-alist-alist))
      (let ((text "    Test: foo_test.go:6: message"))
        (should (string-match regexp text))
        (should (equal (match-string file text) "foo_test.go"))
        (should (equal (match-string line text) "6"))))))

;;; go-compile-test.el ends here

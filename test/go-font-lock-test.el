;;; go-indentation-test.el

;; Copyright 2019 The go-mode Authors. All rights reserved. Use of
;; this source code is governed by a BSD-style license that can be
;; found in the LICENSE file.

(require 'ert)
(require 'go-mode)
(require 'cl-lib)

(ert-deftest go--fontify-signature ()
  (should-fontify "KfuncK FfooF() { }")
  (should-fontify "KfuncK FfooF(TaT) { }")
  (should-fontify "KfuncK FfooF(TaT, TbT) { }")
  (should-fontify "KfuncK FfooF(TaT) TaT { }")
  (should-fontify "KfuncK FfooF(a TbT) (a TbT) { }")
  (should-fontify "KfuncK FfooF(a, b TcT) (a TbT, c TdT) { }")

  (should-fontify "KfuncK (TbT) FfooF(a, b TcT) TdT { }")
  (should-fontify "KfuncK (a TbT) FfooF(a TbT) (TdT) { }")

  (should-fontify "foo := KfuncK(a TbT) TcT { }"))

(ert-deftest go--fontify-decls ()
  (should-fontify "KvarK foo TintT")
  (should-fontify "KvarK foo *[3]TintT")
  (should-fontify "KvarK foo Tfmt.StringerT")
  (should-fontify "KvarK foo, bar Tfmt.StringerT")

  (should-fontify "
KvarK (
  a TbT
  a, b TbT
  a KfuncK(b TcT)
)")

  (should-fontify "
KconstK (
  a = 1
  a TintT = 1
)"))

(ert-deftest go--fontify-struct ()
  (should-fontify "KstructK { i TintT }")
  (should-fontify "KstructK { a, b TintT }")

  (should-fontify "
KstructK {
  a TboolT
  c KstructK { f *Tfmt.StringerT }
}"))

(defun should-fontify (contents)
  "Verify fontification.

CONTENTS is a template that uses single capital letters to
represent expected font lock face names. For example:

BmakeB([]TintT, 0)

expects \"make\" to be a (B)uiltin and \"int\" to be a (T)type."
  (with-temp-buffer
    (go-mode)
    (insert contents)
    (goto-char (point-min))

    ;; First pass through buffer looks for the face tags. We delete
    ;; the tags and record the expected face ranges in `faces'.
    (let ((case-fold-search nil) faces start start-pos)
      (while (re-search-forward "[TBKCF]" nil t)
        (let ((found-char (char-before)))
          (backward-delete-char 1)
          (if start
              (progn
                (should (= found-char start))
                (let ((face (cl-case found-char
                              (?T 'font-lock-type-face)
                              (?B 'font-lock-builtin-face)
                              (?K 'font-lock-keyword-face)
                              (?C 'font-lock-constant-face)
                              (?F 'font-lock-function-name-face))))
                  (setq faces (append faces `((,face ,start-pos ,(point))))))
                (setq start nil))
            (setq start found-char)
            (setq start-pos (point)))))

      ;; Fontify buffer now that we have removed the tags.
      (font-lock-fontify-buffer)
      (goto-char (point-min))

      ;; Go through buffer one character at a time making sure the
      ;; character's face is correct.
      (let ((face (pop faces)))
        (while (not (eobp))
          (while (and face (>= (point) (nth 2 face)))
            (setq face (pop faces)))
          (if (and face (>= (point) (nth 1 face)))
              (should (eq (nth 0 face) (get-text-property (point) 'face)))
            (should (eq nil (get-text-property (point) 'face))))
          (forward-char))))))

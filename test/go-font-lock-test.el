;;; go-indentation-test.el

;; Copyright 2019 The go-mode Authors. All rights reserved. Use of
;; this source code is governed by a BSD-style license that can be
;; found in the LICENSE file.

(require 'ert)
(require 'go-mode)
(require 'cl-lib)

(ert-deftest go--fontify-signature ()
  (go--should-fontify "KfuncK FfooF() { }")
  (go--should-fontify "KfuncK FfooF(TaT) { }")
  (go--should-fontify "KfuncK FfooF(TaT, TbT) { }")
  (go--should-fontify "KfuncK FfooF(TaT) TaT { }")
  (go--should-fontify "KfuncK FfooF(a TbT) (a TbT) { }")
  (go--should-fontify "KfuncK FfooF(a, b TcT) (a TbT, c TdT) { }")

  (go--should-fontify "KfuncK (TbT) FfooF(a, b TcT) TdT { }")
  (go--should-fontify "KfuncK (a TbT) FfooF(a TbT) (TdT) { }")

  (go--should-fontify "foo := KfuncK(a TbT) TcT { }")

  (go--should-fontify "
KfuncK FfooF(
  i TintT,
) TstringT {
  bar
}")

  (go--should-fontify "
KfuncK FfooF(
  i TintT,
  j Tfloat64T) (TstringT, TboolT) {
  bar
}")

  (go--should-fontify "
KfuncK FfooF(
  i TintT,
) (a TstringT, b *Tfoo.ZarT) {
  bar
}"))

(ert-deftest go--fontify-decls ()
  (go--should-fontify "KvarK foo TintT")
  (go--should-fontify "KvarK foo *[3]TintT")
  (go--should-fontify "KvarK foo Tfoo.ZebraT")
  (go--should-fontify "KvarK foo, bar Tfoo.ZebraT")

  (go--should-fontify "
KvarK (
  a TbT
  a, b TbT
  a KfuncK(b TcT)
)")

  (go--should-fontify "
KconstK (
  a = 1
  a TintT = 1
)"))

(ert-deftest go--fontify-struct ()
  (go--should-fontify "KstructK { i TintT }")
  (go--should-fontify "KstructK { a, b TintT }")

  (go--should-fontify "
KstructK {
  a TboolT
  c KstructK { f *Tfoo.ZebraT }
}"))

(ert-deftest go--fontify-interface ()
  (go--should-fontify "
KinterfaceK {
  FfooF(a, b TcT) *TstringT
}")

  (go--should-fontify "
KinterfaceK {
  FfooF(KinterfaceK { FaF() TintT }) (c TdT)
}")

  (go--should-fontify "
KmapK[TstringT]KinterfaceK{}{
  S`foo`S: foo.FbarF(baz),
}"))


(ert-deftest go--fontify-type-switch ()
  (go--should-fontify "
KswitchK foo.(KtypeK) {
KcaseK TstringT, *Tfoo.ZebraT, [2]TbyteT:
}")

  (go--should-fontify "
KswitchK 123 {
KcaseK string:
}"))

(ert-deftest go--fontify-composite-literal ()
  (go--should-fontify "TfooT{")
  (go--should-fontify "[]TfooT{")
  (go--should-fontify "Tfoo.ZarT{")
  (go--should-fontify "[]Tfoo.ZarT{"))

(ert-deftest go--fontify-slices-arrays-maps ()
  (go--should-fontify "[]TfooT")
  (go--should-fontify "[]Tfoo.ZarT")
  (go--should-fontify "[]*Tfoo.ZarT")

  (go--should-fontify "[123]TfooT")
  (go--should-fontify "[...]TfooT")
  (go--should-fontify "[foo.Zar]TfooT")

  (go--should-fontify "KmapK[*Tfoo.ZarT]*Tbar.ZarT")
  (go--should-fontify "[]KmapK[TfooT]TbarT")
  (go--should-fontify "KmapK[[1][2][three]*Tfoo.ZarT][four][]*Tbar.ZarT"))

(defun go--should-fontify (contents)
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
      (while (re-search-forward "[TBKCFS]" nil t)
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
                              (?F 'font-lock-function-name-face)
                              (?S 'font-lock-string-face))))
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

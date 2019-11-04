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
  (should-fontify "KfuncK FfooF(VaV TbT) (VaV TbT) { }")
  (should-fontify "KfuncK FfooF(VaV, VbV TcT) (VaV TbT, VcV TdT) { }")

  (should-fontify "KfuncK (TbT) FfooF(VaV, VbV TcT) TdT { }")
  (should-fontify "KfuncK (VaV TbT) FfooF(VaV TbT) (TdT) { }")

  (should-fontify "VfooV := KfuncK(VaV TbT) TcT { }")

  (should-fontify "KfuncK(...TintT) { }")
  (should-fontify "KfuncK(VaV ...TintT) { }")
  (should-fontify "KfuncK(VaV ...KinterfaceK{}) { }")

  (should-fontify "KfuncK(KinterfaceK { FfooF() }, TstringT) KinterfaceK{}")

  (should-fontify "KfuncK(VaV TbT, VcV KfuncK(VdV *TeT) TdT) TfT")
  (should-fontify "KfuncK(VaV KfuncK() TbT, VcV TdT)")

  (should-fontify "
KfuncK FfooF(
  VaV TcatT, VbV KinterfaceK { FbarkF() },
  VcV TbananaT,
) (
  VwhyV TdothisT,
  VjustV TstopT,
) { }"))

(ert-deftest go--fontify-struct ()
  (should-fontify "KstructK { i TintT }")
  (should-fontify "KstructK { a, b TintT }")

  (should-fontify "
KstructK {
  a TboolT
  c KstructK { f *Tfoo.ZebraT }
}"))

(ert-deftest go--fontify-interface ()
  (should-fontify "
KinterfaceK {
  FfooF(VaV, VbV TcT) *TstringT
}")

  (should-fontify "
KinterfaceK {
  FfooF(KinterfaceK { FaF() TintT }) (VcV TdT)
}")

  (should-fontify "
KmapK[TstringT]KinterfaceK{}{
  S`foo`S: foo.FbarF(baz),
}"))


(ert-deftest go--fontify-type-switch ()
  (should-fontify "
KswitchK foo.(KtypeK) {
KcaseK TstringT, *Tfoo.ZebraT, [2]TbyteT:
}")

  (should-fontify "
KswitchK foo.(KtypeK) {
KcaseK KinterfaceK { FfooF(TintT, TstringT) }, KstructK { i, j TintT }, TstringT:
}")

  (should-fontify "
KswitchK 123 {
KcaseK string:
}"))

(ert-deftest go--fontify-composite-literal ()
  (should-fontify "TfooT{")
  (should-fontify "[]TfooT{")
  (should-fontify "Tfoo.ZarT{")
  (should-fontify "[]Tfoo.ZarT{"))

(ert-deftest go--fontify-slices-arrays-maps ()
  (should-fontify "[]TfooT")
  (should-fontify "[]Tfoo.ZarT")
  (should-fontify "[]*Tfoo.ZarT")

  (should-fontify "[123]TfooT")
  (should-fontify "[...]TfooT")
  (should-fontify "[foo.Zar]TfooT")

  (should-fontify "KmapK[*Tfoo.ZarT]*Tbar.ZarT")
  (should-fontify "[]KmapK[TfooT]TbarT")
  (should-fontify "KmapK[[1][2][three]*Tfoo.ZarT][four][]*Tbar.ZarT"))

(ert-deftest go--fontify-negation ()
  ;; Fontify unary "!".
  (should-fontify "N!Nfoo")

  ;; Alternate fontification with multiple "!".
  (should-fontify "N!N!foo")
  (should-fontify "N!N!N!Nfoo")

  ;; Don't fontify "!=" operator.
  (should-fontify "foo != bar"))


(ert-deftest go--fontify-type-decl ()
  (should-fontify "KtypeK TfooT TbarT")
  (should-fontify "KtypeK TfooT Tbar.ZarT")
  (should-fontify "KtypeK TfooT KstructK { }")
  (should-fontify "KtypeK TfooT = Tbar.ZarT")
  (should-fontify "KtypeK TfooT = KmapK[TstringT]TstringT")

  (should-fontify "
KtypeK (
  TfooT TbarT
  TfooT KstructK {}
  TfooT = *Tbar.ZarT
)"))

(ert-deftest go--fontify-var-decl ()
  (should-fontify "KvarK VfooV = bar")
  (should-fontify "KvarK VfooV, VbarV = bar, baz")
  (should-fontify "KvarK VfooV TbarT D// DQcoolQ")
  (should-fontify "KvarK VfooV TbarT = baz")
  (should-fontify "KvarK VfooV KstructK { i TintT } = baz")
  (should-fontify "KvarK VfooV []*Tfoo.ZarT D// DQcoolQ")

  (should-fontify "
KvarK (
  VfooV TbarT
  VfooV KfuncK(ViV TintT)
  VfooV = bar
  VfooV TbarT = baz
  VfooV, VbarV = baz, qux
  VfooV, VbarV TbazT = qux, zorb
)"))

(ert-deftest go--fontify-const-decl ()
  (should-fontify "KconstK CfooC, CbarC = 123, 456 D// D")
  (should-fontify "KconstK CfooC, CbarC TbazT = 123, 456")
  (should-fontify "
KconstK (
  CaC = 1
  CaC TintT = 1
  CaC, CbC TintT = 1, 2
)"))

(ert-deftest go--fontify-assign ()
  (should-fontify "VfooV := bar")
  (should-fontify "foo = bar D// DQ:=Q")
  (should-fontify "VfooV, VbarV := baz, qux")
  (should-fontify "foo, bar = baz, qux")
  (should-fontify "KfuncK FfooF(ViV TintT) { VbarV := baz }"))

(defun go--should-match-face (want-face)
  (let ((got-face (get-text-property (point) 'face)))
    (if (not (eq got-face want-face))
        (progn
          (message "char '%s' (%s): wanted %s, got %s" (char-to-string (char-after)) (point) want-face got-face)
          nil)
      t)))

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
      (while (re-search-forward "[TBKCFSNVDQ]" nil t)
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
                              (?S 'font-lock-string-face)
                              (?N 'font-lock-negation-char-face)
                              (?V 'font-lock-variable-name-face)
                              (?D 'font-lock-comment-delimiter-face)
                              (?Q 'font-lock-comment-face))))
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
              (should (go--should-match-face (nth 0 face)))
            (should (go--should-match-face nil)))
          (forward-char))))))

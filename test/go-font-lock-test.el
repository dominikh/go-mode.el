;;; go-font-lock-test.el

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
  (go--should-fontify "KfuncK FfooF(VaV TbT) (VaV TbT) { }")
  (go--should-fontify "KfuncK FfooF(VaV, VbV TcT) (VaV TbT, VcV TdT) { }")

  (go--should-fontify "KfuncK (TbT) FfooF(VaV, VbV TcT) TdT { }")
  (go--should-fontify "KfuncK (VaV TbT) FfooF(VaV TbT) (TdT) { }")

  (go--should-fontify "VfooV := KfuncK(VaV TbT) TcT { }")

  (go--should-fontify "KfuncK(...TintT) { }")
  (go--should-fontify "KfuncK(VaV ...TintT) { }")
  (go--should-fontify "KfuncK(VaV ...KinterfaceK{}) { }")

  (go--should-fontify "KfuncK(KinterfaceK { FfooF() }, TstringT) KinterfaceK{}")

  (go--should-fontify "KfuncK(VaV TbT, VcV KfuncK(VdV *TeT) TdT) TfT")
  (go--should-fontify "KfuncK(VaV KfuncK() TbT, VcV TdT)")

  (go--should-fontify "
KfuncK FfooF(
  VaV TcatT, VbV KinterfaceK { FbarkF() },
  VcV TbananaT,
) (
  VwhyV TdothisT,
  VjustV TstopT,
) { }")

  (go--should-fontify "
D// DQ
QD// DQ(
QKfuncK (VfV TintT) {}
"))

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
  FfooF(VaV, VbV TcT) *TstringT
}")

  (go--should-fontify "
KinterfaceK {
  FfooF(KinterfaceK { FaF() TintT }) (VcV TdT)
}")

  (go--should-fontify "
KmapK[TstringT]KinterfaceK{}{
  S`foo`S: foo.FbarF(baz),
}"))


(ert-deftest go--fontify-type-switch ()
  (go--should-fontify "
KswitchK foo.(KtypeK) {
KcaseK TstringT, *Tfoo.ZebraT, [2]TbyteT:
KcaseK CnilC:
KcaseK TfooT, TbarT, D// DQhi
Q
  D// DQthere
Q  TbazT, TquxT:
KdefaultK:
}")

  (go--should-fontify "
KswitchK foo.(KtypeK) {
KcaseK KinterfaceK { FfooF(TintT, TstringT) }, KstructK { i, j TintT }, TstringT:
}")

  (go--should-fontify "
KswitchK 123 {
KcaseK string:
}"))

(ert-deftest go--fontify-composite-literal ()
  (go--should-fontify "TfooT{")
  (go--should-fontify "[]TfooT{")
  (go--should-fontify "Tfoo.ZarT{")
  (go--should-fontify "[]Tfoo.ZarT{")

  (go--should-fontify "TfooT{CbarC:baz, CquxC: 123}")

  (go--should-fontify "TfooT{
CbarC: baz,
}")

  (go--should-fontify "[]TfooT{{
CbarC: baz,
}, {
CbarC: baz,
}}")

  (go--should-fontify "TsomeMapT{
foo.Zar: baz,
a + b: 3,
a-b: 4,
}"))

(ert-deftest go--fontify-slices-arrays-maps ()
  (go--should-fontify "[]TfooT")
  (go--should-fontify "[]Tfoo.ZarT")
  (go--should-fontify "[]*Tfoo.ZarT")

  (go--should-fontify "[123]TfooT")
  (go--should-fontify "[...]TfooT")
  (go--should-fontify "[foo.Zar]TfooT")
  (go--should-fontify "D/*DQhi*/Q[1]*TfooT")

  (go--should-fontify "KmapK[*Tfoo.ZarT]*Tbar.ZarT")
  (go--should-fontify "[]KmapK[TfooT]TbarT")
  (go--should-fontify "KmapK[[1][2][three]*Tfoo.ZarT][four][]*Tbar.ZarT")
  (go--should-fontify "KmapK[TstringT]KmapK[TstringT]Tfloat64T")
  (go--should-fontify "KmapK[[2][c]*TintT]TboolT"))

(ert-deftest go--fontify-negation ()
  ;; Fontify unary "!".
  (go--should-fontify "N!Nfoo")

  ;; Alternate fontification with multiple "!".
  (go--should-fontify "N!N!foo")
  (go--should-fontify "N!N!N!Nfoo")

  ;; Don't fontify "!=" operator.
  (go--should-fontify "foo != bar"))


(ert-deftest go--fontify-type-decl ()
  (go--should-fontify "KtypeK TfooT TbarT")
  (go--should-fontify "KtypeK TfooT Tbar.ZarT")
  (go--should-fontify "KtypeK TfooT KstructK { }")
  (go--should-fontify "KtypeK TfooT = Tbar.ZarT")
  (go--should-fontify "KtypeK TfooT = KmapK[TstringT]TstringT")

  (go--should-fontify "
KtypeK (
  TfooT TbarT
  TfooT KstructK {}
  TfooT = *Tbar.ZarT
)"))

(ert-deftest go--fontify-var-decl ()
  (go--should-fontify "KvarK VfooV = bar")
  (go--should-fontify "KvarK VfooV, VbarV = bar, baz")
  (go--should-fontify "KvarK VfooV TbarT D// DQcoolQ")
  (go--should-fontify "KvarK VfooV TbarT = baz")
  (go--should-fontify "KvarK VfooV KstructK { i TintT } = baz")
  (go--should-fontify "KvarK VfooV []*Tfoo.ZarT D// DQcoolQ")

  (go--should-fontify "
KvarK (
  VfooV TbarT
  VfooV KfuncK(ViV TintT)
  VfooV = bar
  VfooV TbarT = baz
  VfooV, VbarV = baz, qux
  VfooV, VbarV TbazT = qux, zorb
)"))

(ert-deftest go--fontify-const-decl ()
  (go--should-fontify "KconstK CfooC, CbarC = 123, 456 D// D")
  (go--should-fontify "KconstK CfooC, CbarC TbazT = 123, 456")
  (go--should-fontify "
KconstK (
  CaC = 1
  CaC TintT = 1
  CaC, CbC TintT = 1, 2
)"))

(ert-deftest go--fontify-labels ()
  (go--should-fontify "
CfooC:
KforK {
  KcontinueK CfooC
  KbreakK CfooC
  KgotoK CfooC
}
"))

(ert-deftest go--fontify-assign ()
  (go--should-fontify "VfooV := bar")
  (go--should-fontify "foo = bar D// DQ:=Q")
  (go--should-fontify "VfooV, VbarV := baz, qux")
  (go--should-fontify "foo, bar = baz, qux")
  (go--should-fontify "KfuncK FfooF(ViV TintT) { VbarV := baz }"))

(ert-deftest go--fontify-index-multiply ()
  (go--should-fontify "foo[1]*10 + 1")
  (go--should-fontify "foo[1]*foo[2] + 1"))

(ert-deftest go--fontify-go-dot-mod ()
  (go--should-fontify "
KmoduleK foo

KgoK 1.13

KrequireK (
  Nexample.com/require/go/bananaN Sv12.34.56SV-1234-456abcV D// DQindirect
Q	Nnoslash.devN Sv1.2.3S
)

KreplaceK (
	Nfoo.example.com/barN Sv1.2.3S => Nfoo.example.com/barN Sv1.2.3S
	Nexample.com/foo/barN => Nexample.com/baz/barN Sv0.0.0SV-20201112005413-933910cbaea0V
)
" 'go-dot-mod-mode))

(defun go--should-match-face (want-face)
  (let ((got-face (get-text-property (point) 'face)))
    (if (not (eq got-face want-face))
        (progn
          (message "char '%s' (%s): wanted %s, got %s" (char-to-string (char-after)) (point) want-face got-face)
          nil)
      t)))

(defun go--should-fontify (contents &optional mode)
  "Verify fontification.

CONTENTS is a template that uses single capital letters to
represent expected font lock face names. For example:

BmakeB([]TintT, 0)

expects \"make\" to be a (B)uiltin and \"int\" to be a (T)type."
  (with-temp-buffer
    (setq mode (or mode 'go-mode))
    (funcall mode)
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
                              (?S (if (eq mode 'go-mode) 'font-lock-string-face 'go-dot-mod-module-semver))
                              (?N (if (eq mode 'go-mode) 'font-lock-negation-char-face 'go-dot-mod-module-name))
                              (?V (if (eq mode 'go-mode) 'font-lock-variable-name-face 'go-dot-mod-module-version))
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

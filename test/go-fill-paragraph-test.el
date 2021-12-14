;;; go-fill-paragraph-test.el

;; Copyright 2019 The go-mode Authors.  All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

(require 'ert)
(require 'go-mode)

(defun go--should-fill (got expected)
  "Run `fill-paragraph' against GOT and make sure it matches EXPECTED.

<> in GOT represents point. If they aren't next to each other, then it
represents point and mark to test the region based fill-paragraph."
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
      (call-interactively 'fill-paragraph)
      (should (string= (buffer-string) expected)))))

(ert-deftest go--fill-paragraph-single ()
  (go--should-fill
   "
func main() {
<>  // Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
}"

   "
func main() {
  // Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
  // eiusmod tempor incididunt ut labore et dolore magna aliqua.
}"
   ))

(ert-deftest go--fill-paragraph-single-region ()
  (go--should-fill
   "
func main() {
<  // Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
>}"

   "
func main() {
  // Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
  // eiusmod tempor incididunt ut labore et dolore magna aliqua.
}"
   ))

(ert-deftest go--fill-paragraph-block ()
  (go--should-fill
   "
func main() {
<>  /* Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. */
}"

   "
func main() {
  /* Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
     eiusmod tempor incididunt ut labore et dolore magna aliqua. */
}"
   ))

(ert-deftest go--fill-paragraph-block-region ()
  (go--should-fill
   "
func main() {
<  /* Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. */
>}"

   "
func main() {
  /* Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
     eiusmod tempor incididunt ut labore et dolore magna aliqua. */
}"
   ))

(ert-deftest go--fill-paragraph-block-empty-first ()
  (go--should-fill
   "
func main() {
<>  /*
       Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
  */
}"

   "
func main() {
  /*
       Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
       do eiusmod tempor incididunt ut labore et dolore magna aliqua.
  */
}"
   ))

(ert-deftest go--fill-paragraph-block-empty-first-region ()
  (go--should-fill
   "
func main() {
<  /*
       Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
  */
>}"

   "
func main() {
  /*
       Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
       do eiusmod tempor incididunt ut labore et dolore magna aliqua.
  */
}"
   ))


(ert-deftest go--fill-paragraph-block-offset ()
  (go--should-fill
   "
func main() {
<>  /*
       Lorem ipsum dolor sit amet, consectetur adipisicing elit,
         sed do eiusmod tempor incididunt ut labore
         et dolore magna aliqua.
  */
}"

   "
func main() {
  /*
       Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
         do eiusmod tempor incididunt ut labore et dolore magna
         aliqua.
  */
}"
   ))

(ert-deftest go--fill-paragraph-block-region ()
  (go--should-fill
   "
func main() {
<  /* Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. */
>}"

   "
func main() {
  /* Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
     eiusmod tempor incididunt ut labore et dolore magna aliqua. */
}"
   ))

(ert-deftest go--fill-paragraph-single-artful ()
  (go--should-fill
   "
func main() {
  /////////////////////
<>  // Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
  /////////////////////
}"

   "
func main() {
  /////////////////////
  // Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
  // eiusmod tempor incididunt ut labore et dolore magna aliqua.
  /////////////////////
}"
   ))

(ert-deftest go--fill-paragraph-single-artful-region ()
  (go--should-fill
   "
func main() {
<  /////////////////////
  // Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
  /////////////////////
>}"

   "
func main() {
  /////////////////////
  // Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
  // eiusmod tempor incididunt ut labore et dolore magna aliqua.
  /////////////////////
}"
   ))

(ert-deftest go--fill-paragraph-code-region ()
  (go--should-fill
   "
func main() {
<	if something() {
		somethingElse()
	}
>}"

   ;; important thing is we don't get stuck in an infinite loop
   "
func main() {
	if something() { somethingElse() }
}"
   ))


(ert-deftest go--fill-paragraph-bob ()
  (go--should-fill
   "<>// Lorem
// ipsum."
   "// Lorem ipsum."
   )

  (go--should-fill
   "<>/*
   Lorem
   ipsum.
*/"
   "/*
   Lorem ipsum.
*/"
   ))

// This file can be used to manually test that go-beginning-of-def and
// go-end-of-defun are correct by entering into each function and mark-defun
// (C-M-h)
package main

type typea int

func easy(a, b, c int) int {
	c += a
	c += b
	return c
}

func harder(a chan struct{}) {
	close(a)
}

func harder(a struct {
	b struct {
		c interface {
			Foo()
			Bar()
			Baz()
		}
	}
}) interface {
	Channer() chan struct{}
} {
	return nil
}

func oneline(a struct{}) (r struct{ a int }) { return r }

type typeb struct {
	a, b, c int
}

// Function with a pathological comment that breaks go-end-of-defun.
func comment(a chan struct /* why? */ {

}) {
	close(a)
}

// Another function with a comment that breaks go-end-of-defun.
func anotherComment(a struct {
	b int // this b is sad :{
	c int
}) {
	a.b += a.c
	a.c += a.b
	return
}

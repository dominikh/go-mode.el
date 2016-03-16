// This file can be used to manually test that go-beginning-of-def and
// go-end-of-defun are correct by entering into each function and mark-defun
// (C-M-h).
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

// comment1 breaks end-of-defun by splitting "struct" from "{". (This also
// apparently breaks gofmt, is why this is formatted so weird.)
func comment1(a chan struct /* why? */ {

}) {
	close(a)
}

func comment2(a struct {
	b int // b is sad :{
	c int
}) {
	a.b += a.c
	a.c += a.b
	return
}

func structWithTag(a chan struct {
	v int `{`
}) {
	close(a)
}

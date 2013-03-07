**Note: This go-mode.el has been merged upstream and is now part of
  the Go distribution. New features and bugfixes will be made
  available here first, but will be sent upstream as well.**

This is a replacement for the old Go mode that came with the Go
distribution <= v1.0.3. It fixes several issues and adds new features,
such as movement by functions, like one is used to from other major
modes in emacs.


Fixes
=====
- Uses a proper syntax table, so that emacs and other packages know
  about strings and comments (e.g. expand-region needs this)
- Fixes various indentation/font locking problems caused by the lack
  of proper syntax table
- Fixes gofmt issue with buffers that do not end with a newline
- Does not fontify anonymous functions as if they were methods

Improvements
============
- Fontifies `(foo)(bar)` function calls
- Fontifies identifiers with unicode characters in them
- Fontifies type names in struct literals
- Fontifies type names in maps, slices and arrays
- Uses view-mode for the godoc buffer
- Complete rewrite of gofmt, fixing several issues

Features
========
- Support for `C-M-a` (`beginning-of-defun`), `C-M-e` (`end-of-defun`)
  and all functions that make use of defun navigation, such as `C-M-h`
  (`mark-defun`), `C-x n d` (`narrow-to-defun`) and more.
- Two functions for sending code to the Playground (`go-play-buffer` and `go-play-region`)
- A function for downloading code from the Playground into a Go buffer (`go-download-play`)
- A function for jumping to the file's imports (`go-goto-imports`)
- A function for adding imports, including tab completion (`go-import-add`, bound to `C-c C-a`)
- A function for removing or commenting unused imports (`go-remove-unused-imports`)
- `godef-describe` and `godef-jump` (`C-c C-d` and `C-c C-j`) to
  describe expressions and jump to their declarations.
- Adds basic support for imenu (functions and variables)

Other extensions
================
For a richer experience, consider installing
[goflymake](https://github.com/dougm/goflymake) for on-the-fly syntax
checking and [gocode](https://github.com/nsf/gocode) for auto
completion. Some features require you to install godef via `go get
code.google.com/p/rog-go/exp/cmd/godef`.

Alternatively there is also
[flymake-go](http://marmalade-repo.org/packages/flymake-go), which
uses _gofmt_ to check for syntax errors and doesn't require installing
any additional binaries. It **only** catches syntactic errors though.

Also, if you're using YASnippet, consider using the snippets from
[yasnippet-go](https://github.com/dominikh/yasnippet-go).

Contributing
============

Because go-mode.el is also part of the Go distribution, there are
essentially two ways to contribute changes:

- By sending a pull request on GitHub
- By submitting a CL to the Go project

Because of copyright reasons, I will not accept any non-trivial
contributions via pull requests. Please submit a CL instead. This will
ensure that the changes go through a proper review process and that
your name will be noted in the list of contributors and possibly
authors. Instructions can be found at
http://golang.org/doc/contribute.html.

Trivial changes, and changes to files that are not part of the Go
distribution (mainly this README), can be submitted via pull request.

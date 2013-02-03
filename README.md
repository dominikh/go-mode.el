This is a replacement for the Go mode that comes with the Go
distribution. It fixes several issues and adds new features, such as
movement by functions, like one is used to from other major modes in
emacs.

To use this mode instead of the default one, add it to your load path
and load it instead of the old one. It acts as a direct drop-in
replacement. Please make sure, however, that you **load only this
go-mode and not the old one first**. The old one installs some hooks
that will interfere with this mode.

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

Features
========
- Support for `C-M-a` (`beginning-of-defun`)
- Support for `C-M-e` (`end-of-defun`)
- Support for `C-M-h` (`mark-defun`)
- Two functions for sending code to the Playground (`go-play-buffer` and `go-play-region`)
- A function for adding imports, including tab completion (`go-import-add`, bound to `C-c C-a`)
- A function for removing or commenting unused imports (`go-remove-unused-imports`)
- A function for jumping to the file's imports (`go-goto-imports`)
- Adds basic support for imenu (functions and variables)

Other extensions
================
For a richer experience, consider installing
[goflymake](https://github.com/dougm/goflymake) for on-the-fly syntax
checking and [gocode](https://github.com/nsf/gocode) for auto
completion.

Also, if you're using YASnippet, consider using the snippets from
[yasnippet-go](https://github.com/dominikh/yasnippet-go).

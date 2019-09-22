This is go-mode, the Emacs mode for editing Go code.

It is a complete rewrite of the go-mode that shipped with Go 1.0.3 and
before, and was part of Go 1.1 until Go 1.3. Beginning with Go 1.4,
editor integration will not be part of the Go distribution anymore,
making this repository the canonical place for go-mode.


# Features

In addition to normal features, such as fontification and indentation,
and close integration with familiar Emacs functionality (for example
syntax-based navigation like `beginning-of-defun`), go-mode comes with
the following extra features to provide an improved experience:

- Integration with `gofmt` by providing a command of the same name,
  and `gofmt-before-save`, which can be used in a hook to format Go
  buffers before saving them.
  - Setting the `gofmt-command` variable also allows using
    `goimports`.
  - Setting the `gofmt-args` variable with a list of arguments allows
    using e.g. `gofmt -s`.
- Integration with `godoc` via the functions `godoc` and
  `godoc-at-point`.
- Integration with the Playground
  - `go-play-buffer` and `go-play-region` to send code to the
    Playground
  - `go-download-play` to download a Playground entry into a new
    buffer
- Managing imports
  - A function for jumping to the file's imports (`go-goto-imports` -
    `C-c C-f i`)
  - A function for adding imports, including tab completion
    (`go-import-add`, bound to `C-c C-a`)
  - A function for removing or commenting unused imports
    (`go-remove-unused-imports`)
  - It is recommended that you use `goimports` or the
    `organize-imports` feature of `gopls` to manage
    adding/removing/organizing imports automatically.
- Integration with godef
  - `godef-describe` (`C-c C-d`) to describe expressions
  - `godef-jump` (`C-c C-j`) and `godef-jump-other-window` (`C-x 4 C-c
    C-j`) to jump to declarations
  - This requires you to install godef via `go get
  github.com/rogpeppe/godef`.
- Basic support for imenu (functions and variables)
- Built-in support for displaying code coverage as calculated by `go
  test` (`go-coverage`)
- Several functions for jumping to and manipulating the individual
  parts of function signatures. These functions support anonymous
  functions, but are smart enough to skip them when required (e.g.
  when jumping to a method receiver or docstring.)
  - Jump to the argument list (`go-goto-arguments` - `C-c C-f a`)
  - Jump to the docstring, create it if it does not exist yet
    (`go-goto-docstring` - `C-c C-f d`).
  - Jump to the function keyword (`go-goto-function` - `C-c C-f f`)
  - Jump to the function name (`go-goto-function-name` - `C-c C-f n`)
  - Jump to the return values (`go-goto-return-values` - `C-c C-f r`)
  - Jump to the method receiver, adding a pair of parentheses if no
    method receiver exists (`go-goto-method-receiver` - `C-c C-f m`).

  All of these functions accept a prefix argument (`C-u`), causing
  them to skip anonymous functions.
- GOPATH detection – the function `go-guess-gopath` will guess a
  suitable value for GOPATH, based on gb or wgo projects, Godeps and
  src folders for plain GOPATH workspaces. The command
  `go-set-project` uses the return value of `go-guess-gopath` to set
  the GOPATH environment variable.

  You can either call `go-set-project` manually, or integrate it with
  Projectile's project switching hooks, or any other means of
  switching projects you may employ.

# Installation

## MELPA

The recommended way of installing go-mode is via
[ELPA](http://www.emacswiki.org/emacs/ELPA), the Emacs package
manager, and the
[MELPA Stable repository](http://emacsredux.com/blog/2014/05/16/melpa-stable/), which provides
an up-to-date version of go-mode.

If you're not familiar with ELPA yet, consider reading
[this guide](http://ergoemacs.org/emacs/emacs_package_system.html).

## Manual



To install go-mode manually, check out the `go-mode.el` repository in
a directory of your choice, add it to your load path and configure
Emacs to automatically load it when opening a `.go` file:

    (add-to-list 'load-path "/place/where/you/put/it/")
    (autoload 'go-mode "go-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

Either evaluate the statements with `C-x C-e`, or restart Emacs.

# Other extensions

There are several third party extensions that can enhance the Go
experience in Emacs.

## Gopls integration

[Gopls](https://github.com/golang/tools/blob/master/gopls/README.md)
is the official language server protocol (lsp) implementation provided
by the Go team. It is intended to replace the existing third party
tools for code formatting (gofmt), automatic imports (goimports), code
navigation (godef/guru), type and function descriptions (godoc/godef),
error checking, auto completion (gocode), variable and type renaming
(rename), and more. Once gopls is stable the older tools will no
longer be supported.

Gopls is a supported backend for
[lsp-mode](https://github.com/emacs-lsp/lsp-mode). It will be used
automatically by lsp-mode if `gopls` is found in your PATH. You can
install gopls via: `go get golang.org/x/tools/gopls@latest`. To enable
lsp-mode for go buffers:

    (add-hook 'go-mode-hook 'lsp-deferred)


## Syntax/error checking

There are two ways of using flymake with Go:

1. [goflymake](https://github.com/dougm/goflymake), which internally
uses `go build` to capture all errors that a regular compilation would
also produce
2. [flymake-go](http://marmalade-repo.org/packages/flymake-go) for a
more lightweight solution that only uses `gofmt` and as such is only
able to catch syntax errors. Unlike goflymake, however, it does not
require an additional executable.

Additionally, there is
[flycheck](https://github.com/flycheck/flycheck), a modern replacement
for flymake, which comes with built-in support for Go. In addition to
using `go build` or `gofmt`, it also has support for `go vet`,
`golint` and `errcheck`.

## Auto completion

For auto completion, take a look at
[gocode](https://github.com/nsf/gocode).

## eldoc

https://github.com/syohex/emacs-go-eldoc provides eldoc functionality
for go-mode.

## Snippets

I maintain a set of YASnippet snippets for go-mode at
https://github.com/dominikh/yasnippet-go

## Integration with errcheck

https://github.com/dominikh/go-errcheck.el provides integration with
[errcheck](https://github.com/kisielk/errcheck).

# Stability

go-mode.el has regular, tagged releases and is part of the MELPA
Stable repository. These tagged releases are intended to provide a
stable experience. APIs added in tagged releases will usually not be
removed or changed in future releases.

Changes made on the master branch, which is tracked by the normal
MELPA repository, however, are under active development. New APIs are
experimental and may be changed or removed before the next release.
Furthermore, there is a higher chance for bugs.

If you want a stable experience, use MELPA Stable. If you want cutting
edge features, or "beta-test" future releases, use MELPA or the master
branch.

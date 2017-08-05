#!/bin/sh

emacs -batch -l ert -l go-present-mode.el -l go-present-mode-tests.el -f ert-run-tests-batch-and-exit

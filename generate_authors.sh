#!/bin/sh
export LC_ALL=C
(
    grep -v "^#" AUTHORS.old | grep -v '^$'
    git log --format='%aN <%aE>'
) | sort -u

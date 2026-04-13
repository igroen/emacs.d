#!/usr/bin/env bash

# Bootstrap Emacs configuration
emacs --batch --load org readme.org -f org-babel-tangle
emacs --batch --eval "(package-initialize)" --load init.el

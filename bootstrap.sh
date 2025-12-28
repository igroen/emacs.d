#!/usr/bin/env bash

# Bootstrap Emacs configuration
emacs --batch --load org readme.org -f org-babel-tangle
emacs --batch --load init.el

#!/usr/bin/env bash

emacs --batch --load org readme.org -f org-babel-tangle
emacs --batch --load init.el

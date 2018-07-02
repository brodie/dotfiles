#!/usr/bin/env zsh

setopt NO_global_rcs

ZDOTDIR=~/.zsh
[[ -d "$ZDOTDIR/functions" ]] && fpath=("$ZDOTDIR/functions" $fpath)

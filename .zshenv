#!/usr/bin/env zsh

setopt NO_global_rcs

ZDOTDIR=~/.zsh
[[ -d "$ZDOTDIR/functions" ]] && fpath=("$ZDOTDIR/functions" $fpath)

path=(/usr/local/sbin /usr/local/bin $path)
[[ -d ~/.bin ]] && path=(~/.bin $path)
[[ -d /usr/local/share/python3 ]] && path+=/usr/local/share/python3
[[ -d /usr/local/share/pypy ]] && path+=/usr/local/share/pypy
[[ -d /usr/local/share/npm/bin ]] && path+=/usr/local/share/npm/bin

if [[ -d /usr/local/lib/node_modules ]]
then
    export NODE_PATH=/usr/local/lib/node_modules
fi

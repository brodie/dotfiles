#!/usr/bin/env zsh

setopt NO_global_rcs

ZDOTDIR="$HOME/.zsh"

if [ ! -n "$LANG" ]
then
    export LANG='en_US.UTF-8'
elif [ "$LANG" != "*.UTF-8" ]
then
    export LANG="${LANG%.*}.UTF-8"
fi

[ -d /usr/X11/bin ] && path=($path /usr/X11/bin)
[ -d /usr/X11R6/bin ] && path=($path /usr/X11R6/bin)
[ -d /usr/local/bin ] && path=(/usr/local/bin $path)
[ -d /usr/local/sbin ] && path=(/usr/local/sbin $path)
[ -d /usr/local/mysql/bin ] && path=(/usr/local/mysql/bin $path)
[ -d /opt/local/bin ] && path=(/opt/local/bin $path)
[ -d /opt/local/sbin ] && path=(/opt/local/sbin $path)
[ -d "$HOME/bin" ] && path=("$HOME/bin" $path)

[ -d /usr/X11/man ] && manpath=($manpath /usr/X11/man)
[ -d /usr/X11R6/man ] && manpath=($manpath /usr/X11R6/man)
[ -d /usr/share/man ] && manpath=(/usr/share/man $manpath)
[ -d /usr/local/share/man ] && manpath=(/usr/local/share/man $manpath)
[ -d /usr/local/man ] && manpath=(/usr/local/man $manpath)
[ -d /opt/local/share/man ] && manpath=(/opt/local/share/man $manpath)

[ -d "$ZDOTDIR/functions" ] && fpath=("$ZDOTDIR/functions" $fpath)

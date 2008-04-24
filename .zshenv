#!/bin/zsh

setopt NO_global_rcs

LANG='en_US.UTF-8'; export LANG
EDITOR='vim'; export EDITOR
PAGER='less'; export PAGER
BROWSER='open'; export BROWSER
PYTHONSTARTUP="$HOME/.pythonrc.py"; export PYTHONSTARTUP

[ -d /usr/X11/bin ] && path=($path /usr/X11/bin)
[ -d /usr/local/X11/bin ] && path=($path /usr/local/X11/bin)
[ -d /usr/local/bin ] && path=(/usr/local/bin $path)
[ -d /usr/local/sbin ] && path=(/usr/local/sbin $path)
[ -d /usr/local/mysql/bin ] && path=(/usr/local/mysql/bin $path)
[ -d /opt/local/bin ] && path=(/opt/local/bin $path)
[ -d /opt/local/sbin ] && path=(/opt/local/sbin $path)
[ -d "$HOME/bin" ] && path=("$HOME/bin" $path)

[ -d /usr/X11/man ] && manpath=(/usr/X11/man $manpath)
[ -d /usr/share/man ] && manpath=(/usr/share/man $manpath)
[ -d /usr/local/share/man ] && manpath=(/usr/local/share/man $manpath)
[ -d /usr/local/man ] && manpath=(/usr/local/man $manpath)

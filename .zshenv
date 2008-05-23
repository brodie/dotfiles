#!/bin/zsh

setopt NO_global_rcs

export LANG='en_US.UTF-8'
export EDITOR='vim'
export PAGER='less'
export BROWSER='open'
export PYTHONSTARTUP="$HOME/.pythonrc.py"

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

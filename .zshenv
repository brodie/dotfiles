#!/bin/zsh

setopt NO_global_rcs

LANG="en_US.UTF-8"; export LANG
EDITOR="vim"; export EDITOR
PAGER="less"; export PAGER
BROWSER="open"; export BROWSER

[ -d /usr/X11/bin ] && PATH="$PATH:/usr/X11/bin"; export PATH
[ -d /usr/local/X11/bin ] && PATH="$PATH:/usr/local/X11/bin"; export PATH
[ -d /usr/local/bin ] && PATH="/usr/local/bin:$PATH"; export PATH
[ -d /usr/local/sbin ] && PATH="/usr/local/sbin:$PATH"; export PATH
[ -d /usr/local/mysql/bin ] && PATH="/usr/local/mysql/bin:$PATH"; export PATH
[ -d /opt/local/bin ] && PATH="/opt/local/bin:$PATH"; export PATH
[ -d /opt/local/sbin ] && PATH="/opt/local/sbin:$PATH"; export PATH
[ -d "$HOME/bin" ] && PATH="$HOME/bin:$PATH"; export PATH

[ -d /usr/X11/man ] && MANPATH="/usr/X11/man:$MANPATH"; export MANPATH
[ -d /usr/share/man ] && MANPATH="/usr/share/man:$MANPATH"; export MANPATH
[ -d /usr/local/share/man ] && MANPATH="/usr/local/share/man:$MANPATH"; export MANPATH
[ -d /usr/local/man ] && MANPATH="/usr/local/man:$MANPATH"; export MANPATH

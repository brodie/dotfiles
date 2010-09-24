#!/usr/bin/env zsh

setopt NO_global_rcs

ZDOTDIR="$HOME/.zsh"

path=(/usr/local/sbin /usr/local/bin $path)
[[ -d /usr/local/mysql/bin ]] && path=(/usr/local/mysql/bin $path)
[[ -d /sw/bin ]] && path=(/sw/sbin /sw/bin $path)
[[ -d /opt/local/bin ]] && path=(/opt/local/sbin /opt/local/bin $path)
[[ -d /opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin ]] && \
 path=(/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin $path)
#[[ -d /Applications/MacPorts/Emacs.app/Contents/MacOS/bin ]] && \
# path=(/Applications/MacPorts/Emacs.app/Contents/MacOS/bin $path)
[[ -d /usr/X11R6/bin ]] && path+=/usr/X11R6/bin
[[ -d /usr/local/X11R6/bin ]] && path+=/usr/local/X11R6/bin
path=("$HOME/.bin" $path)

[[ -d /usr/X11R6/man ]] && manpath+=/usr/X11R6/man
[[ -d /usr/share/man ]] && manpath=(/usr/share/man $manpath)
[[ -d /usr/local/share/man ]] && manpath=(/usr/local/share/man $manpath)
[[ -d /usr/local/man ]] && manpath=(/usr/local/man $manpath)
[[ -d /opt/local/share/man ]] && manpath=(/opt/local/share/man $manpath)

[[ -d "$ZDOTDIR/functions" ]] && fpath=("$ZDOTDIR/functions" $fpath)

[[ -d "$HOME/tmp" ]] && export TMPDIR="$HOME/tmp"

#!/usr/bin/env bash

export PATH="/usr/local/sbin:/usr/local/bin:$PATH"
[[ -d /usr/local/mysql/bin ]] && export PATH="/usr/local/mysql/bin:$PATH"
[[ -d /sw/bin ]] && export PATH="/sw/sbin:/sw/bin:$PATH"
[[ -d /opt/local/bin ]] && export PATH="/opt/local/sbin:/opt/local/bin:$PATH"
[[ -d /opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin ]] && \
 export PATH="/opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin:$PATH"
[[ -d /usr/X11R6/bin ]] && export PATH="$PATH:/usr/X11R6/bin"
[[ -d /usr/local/X11R6/bin ]] && export PATH="$PATH:/usr/local/X11R6/bin"
export PATH="$HOME/bin:$PATH"

[[ -d /usr/X11R6/man ]] && export MANPATH="$MANPATH:/usr/X11R6/man"
[[ -d /usr/share/man ]] && export MANPATH="/usr/share/man:$MANPATH"
[[ -d /usr/local/share/man ]] && export MANPATH="/usr/local/share/man:$MANPATH"
[[ -d /usr/local/man ]] && export MANPATH="/usr/local/man:$MANPATH"
[[ -d /opt/local/share/man ]] && export MANPATH="/opt/local/share/man:$MANPATH"

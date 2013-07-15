#!/usr/bin/env bash

export PATH="/usr/local/sbin:/usr/local/bin:$PATH"
[[ -d "$HOME/.bin" ]] && export PATH="$HOME/.bin:$PATH"
[[ -d /usr/local/share/python3 ]] && \
 export PATH="$PATH:/usr/local/share/python3"
[[ -d /usr/local/share/pypy ]] && export PATH="$PATH:/usr/local/share/pypy"
[[ -d /usr/local/share/npm/bin ]] && export PATH="$PATH:/usr/local/share/npm"

if [[ -d /usr/local/lib/node_modules ]]
then
    export NODE_PATH=/usr/local/lib/node_modules
fi

. ~/.bashrc

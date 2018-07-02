#!/usr/bin/env bash

export PATH="/usr/local/sbin:/usr/local/bin:$PATH:/usr/sbin:/sbin"
[[ -d "$HOME/.bin" ]] && export PATH="$HOME/.bin:$PATH"
[[ -d "$HOME/.cargo/bin" ]] && export PATH="$HOME/.cargo/bin:$PATH"
#[[ -d /usr/local/share/pypy ]] && export PATH="$PATH:/usr/local/share/pypy"
#[[ -d /usr/local/share/npm/bin ]] && export PATH="$PATH:/usr/local/share/npm"

#if [[ -d /usr/local/lib/node_modules ]]
#then
#    export NODE_PATH=/usr/local/lib/node_modules
#fi

#if [[ -x /usr/libexec/java_home ]]
#then
#    export MAVEN_OPTS=-Xmx1024m \
#           JAVA_HOME="$(/usr/libexec/java_home 2> /dev/null)"
#fi

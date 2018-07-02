#!/usr/bin/env zsh

path=(/usr/local/sbin /usr/local/bin $path /usr/sbin /sbin)
[[ -d ~/.bin ]] && path=(~/.bin $path)
[[ -d ~/.cargo/bin ]] && path=(~/.cargo/bin $path)
#[[ -d /usr/local/share/pypy ]] && path+=/usr/local/share/pypy
#[[ -d /usr/local/share/npm/bin ]] && path+=/usr/local/share/npm/bin

#if [[ -d /usr/local/lib/node_modules ]]
#then
#    export NODE_PATH=/usr/local/lib/node_modules
#fi

#if [[ -x /usr/libexec/java_home ]]
#then
#    export MAVEN_OPTS=-Xmx1024m \
#           JAVA_HOME="$(/usr/libexec/java_home 2> /dev/null)" \
#fi

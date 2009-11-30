#!/usr/bin/env bash

[[ -z "$LANG" ]] && export LANG='en_US.UTF-8'

export PATH="/usr/local/sbin:/usr/local/bin:$PATH"
[[ -d /usr/local/mysql/bin ]] && export PATH="/usr/local/mysql/bin:$PATH"
[[ -d /sw/bin ]] && export PATH="/sw/sbin:/sw/bin:$PATH"
[[ -d /opt/local/bin ]] && export PATH="/opt/local/sbin:/opt/local/bin:$PATH"
[[ -d /opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin ]] && \
 export PATH="$PATH:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin"
export PATH="$HOME/bin:$PATH"
[[ -d /usr/X11R6/bin ]] && export PATH="$PATH:/usr/X11R6/bin"
[[ -d /usr/local/X11R6/bin ]] && export PATH="$PATH:/usr/local/X11R6/bin"

[[ -d /usr/X11R6/man ]] && export MANPATH="$MANPATH:/usr/X11R6/man"
[[ -d /usr/share/man ]] && export MANPATH="/usr/share/man:$MANPATH"
[[ -d /usr/local/share/man ]] && export MANPATH="/usr/local/share/man:$MANPATH"
[[ -d /usr/local/man ]] && export MANPATH="/usr/local/man:$MANPATH"
[[ -d /opt/local/share/man ]] && export MANPATH="/opt/local/share/man:$MANPATH"

# Interactive-only settings follow
[[ -z "$PS1" ]] && return

# Options

export EDITOR='vim' \
       PAGER='less' \
       BROWSER='open' \
       PYTHONSTARTUP="$HOME/.pythonrc.py"

shopt -s checkhash \
         checkwinsize \
         cmdhist \
         extglob \
         histappend \

HISTCONTROL=ignoreboth
HISTFILESIZE=1000

# Disable flow control, which makes ^S and ^Q work
stty -ixoff -ixon

# Aliases

if [[ "$TERM" != dumb && -n "$(command -v dircolors)" ]]
then
    eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='ls --color=auto --format=vertical'
    alias vdir='ls --color=auto --format=long'
else
    export CLICOLOR=1
    export LSCOLORS=ExGxFxdaCxDaDaHbadabec
    alias dir='ls --format=vertical'
    alias vdir='ls --format=long'
fi

alias emacs='emacs -nw' \
      ll='ls -l' \
      la='ls -A' \
      l='ls -CF' \
      grep='grep --color=always' \
      zgrep='zgrep --color=always'

beep()
{
    echo -n '\a'
}

if [[ "$TERM" != dumb && -n "$(command -v colordiff)" ]]
then
    alias diff=colordiff
fi

# less niceties

reset_color=$(tput sgr0)
bold_color=$(tput bold)
fg_blue=$(tput setaf 4)
bg_blue=$(tput setab 4)
fg_yellow=$(tput setaf 3)
fg_green=$(tput setaf 2)

export LESS='-R' \
       LESSOPEN="| $HOME/bin/lesspipe %s" \
       LESS_TERMCAP_mb="$bold_color$fg_blue" \
       LESS_TERMCAP_md="$bold_color$fg_blue" \
       LESS_TERMCAP_me=$reset_color \
       LESS_TERMCAP_se=$reset_color \
       LESS_TERMCAP_so="$bold_color$bg_blue$fg_yellow" \
       LESS_TERMCAP_ue=$reset_color \
       LESS_TERMCAP_us=$fg_green

# Prompt

_prompt_pwd()
{
    case $PWD in
        $HOME)
            echo -n '~'
            ;;
        *)
            local first=$(echo -n "${PWD/$HOME/~}" | sed -e \
                          's-/\([^/]\)\([^/]*\)-/\1-g')
            local last="${PWD/#*\//}"
            echo -n "$first${last:1}"
    esac
}

PS1="\u \[$fg_blue\]\$(_prompt_pwd)\[$reset_color\]: "
#PS1="\h \[$fg_blue\]\$(_prompt_pwd)\[$reset_color\]: "

# Window title

case $TERM in
    xterm*|rxvt*)
        PROMPT_COMMAND='echo -ne "\e]0;${USER}@${HOSTNAME}: $(_prompt_pwd)\a"'
        ;;
    *)
        ;;
esac

# Completion

[[ -f "$HOME/.bash_completion" ]] && . "$HOME/.bash_completion"

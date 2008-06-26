#!/bin/bash

if [ ! -n "$LANG" ]
then
    export LANG='en_US.UTF-8'
elif [ "$LANG" != "*.UTF-8" ]
then
    export LANG="${LANG%.*}.UTF-8"
fi

[ -d /usr/X11R6/bin ] && export PATH="$PATH:/usr/X11R6/bin"
[ -d /usr/local/X11R6/bin ] && export PATH="$PATH:/usr/local/X11R6/bin"
[ -d /usr/local/bin ] && export PATH="/usr/local/bin:$PATH"
[ -d /usr/local/sbin ] && export PATH="/usr/local/sbin:$PATH"
[ -d /usr/local/mysql/bin ] && export PATH="/usr/local/mysql/bin:$PATH"
[ -d /opt/local/bin ] && export PATH="/opt/local/bin:$PATH"
[ -d /opt/local/sbin ] && export PATH="/opt/local/sbin:$PATH"
[ -d "$HOME/bin" ] && export PATH="$HOME/bin:$PATH"

[ -d /usr/X11/man ] && export MANPATH="$MANPATH:/usr/X11/man"
[ -d /usr/X11R6/man ] && export MANPATH="$MANPATH:/usr/X11R6/man"
[ -d /usr/share/man ] && export MANPATH="/usr/share/man:$MANPATH"
[ -d /usr/local/share/man ] && export MANPATH="/usr/local/share/man:$MANPATH"
[ -d /usr/local/man ] && export MANPATH="/usr/local/man:$MANPATH"
[ -d /opt/local/share/man ] && export MANPATH="/opt/local/share/man:$MANPATH"

# Interactive-only settings follow
[ -z "$PS1" ] && return

# Options

export EDITOR='vim'
export PAGER='less'
export BROWSER='open'
export PYTHONSTARTUP="$HOME/.pythonrc.py"

shopt -s checkhash \
         checkwinsize \
         cmdhist \
         extglob \
         histappend \

HISTCONTROL=ignoreboth
HISTFILESIZE=1000

# Aliases

if [ "$TERM" != "dumb" -a ! -z "$(command -v dircolors)" -a -x \
     "$(command -v dircolors)" ]; then
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

function beep()
{
    echo -n '\a'
}

# less niceties

export LESS='-R'
export LESSOPEN="| $HOME/bin/lesspipe %s"
export LESS_TERMCAP_mb=${fg_bold[red]}
export LESS_TERMCAP_md=${fg_bold[blue]}
export LESS_TERMCAP_me=$reset_color
export LESS_TERMCAP_se=$reset_color
LESS_TERMCAP_so=$'\e['"${color[bold]};${color[bg-blue]};${color[yellow]}m"
export LESS_TERMCAP_so
export LESS_TERMCAP_ue=$reset_color
export LESS_TERMCAP_us=${fg_bold[green]}

# Prompt

_prompt_pwd()
{
    case $PWD in
        $HOME)
            echo -n '~'
            ;;
        *)
            local last=${PWD/#*\//}
            echo -n $PWD | sed -e "s|^$HOME|~|" \
                               -e 's-/\([^/]\)\([^/]*\)-/\1-g' \
                               -e "s|\$|${last:1}|"
    esac
}

PS1="\u \[$(tput setaf 4)\]\$(_prompt_pwd)\[$(tput sgr0)\]: "
#PS1="\h \[$(tput setaf 4)\]\$(_prompt_pwd)\[$(tput sgr0)\]: "

# Window title

case $TERM in
    xterm*|rxvt*)
        PROMPT_COMMAND='echo -ne "\e]0;${USER}@${HOSTNAME}: $(_prompt_pwd)\a"'
        ;;
    *)
        ;;
esac

# Completion

[ -r $HOME/.bash_completion ] && . $HOME/.bash_completion

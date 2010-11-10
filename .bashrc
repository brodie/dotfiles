#!/usr/bin/env bash

export LANG='en_US.UTF-8'

# Options

export ACK_COLOR_MATCH='red' \
       ACK_COLOR_FILENAME='magenta' \
       ACK_PAGER='less' \
       ALTERNATE_EDITOR='emacs' \
       BROWSER='open' \
       EDITOR='emacsclient -nw' \
       GREP_OPTIONS='--color=always' \
       PAGER='less' \
       PIP_RESPECT_VIRTUALENV=true \
       PIP_VIRTUALENV_BASE="$WORKON_HOME" \
       PYTHONSTARTUP="$HOME/.pythonrc.py" \
       TZ='America/Los_Angeles' \
       VIRTUALENV_USE_DISTRIBUTE=1 \
       VISUAL='emacsclient -nw' \
       WORKON_HOME="$HOME/Documents/Envs"

shopt -s checkhash \
         checkwinsize \
         cmdhist \
         extglob \
         histappend \

HISTCONTROL=ignoreboth
HISTFILESIZE=1000

# Disable flow control, which makes ^S and ^Q work
[[ "$TERM" != dumb ]] && stty -ixoff -ixon

# Aliases

if [[ "$TERM" != dumb && -n "$(command -v dircolors)" ]]
then
    eval "$(dircolors -b)"
    alias ls='ls --color=auto' \
          dir='ls --color=auto --format=vertical' \
          vdir='ls --color=auto --format=long'
elif [[ "$TERM" != dumb && -n "$(command -v gdircolors)" ]]
then
    eval "$(gdircolors -b)"
    export CLICOLOR=1 \
           LSCOLORS=ExGxFxdaCxDaDaHbadabec
    alias ls='gls --color=auto' \
          dir='gls --color=auto --format=vertical' \
          vdir='gls --color=auto --format=long'
else
    export CLICOLOR=1 \
           LSCOLORS=ExGxFxdaCxDaDaHbadabec
    LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:'\
'bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:'\
'st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:'\
'*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:'\
'*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:'\
'*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:'\
'*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:'\
'*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:'\
'*.xwd=01;35:*.flac=01;35:*.mp3=01;35:*.mpc=01;35:*.ogg=01;35:*.wav=01;35:'
    export LS_COLORS
    alias dir='ls --format=vertical' \
          vdir='ls --format=long'
fi

alias ll='ls -l' \
      la='ls -A' \
      l='less' \
      mq='hg --cwd $(hg root)/.hg/patches' \
      tm='tmux a -d' \
      ecn='emacsclient -n' \
      ec='emacsclient -nw'

beep()
{
    echo -n '\a'
}

if [[ "$TERM" != dumb && -n "$(command -v colordiff)" ]]
then
    alias diff='colordiff -u'
fi

[[ -n "$(command -v bpython)" ]] \
    && alias py=bpython || alias py='python -i /dev/null'

# less niceties

if [[ "$TERM" != dumb ]]
then
    reset_color=$(tput sgr0)
    bold_color=$(tput bold)
    fg_blue=$(tput setaf 4)
    bg_blue=$(tput setab 4)
    fg_yellow=$(tput setaf 3)
    fg_green=$(tput setaf 2)
fi

export LESS='-R' \
       LESSOPEN="| $HOME/.bin/lesspipe %s" \
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

#PS1="\u \[$fg_blue\]\$(_prompt_pwd)\[$reset_color\]: "
PS1="\[$fg_blue\]\$(_prompt_pwd) \[$fg_green\]$ \[$reset_color\]"

# Window title
PROMPT_COMMAND='printf "\e]0;${HOSTNAME}: $(_prompt_pwd)\a"'

# Completion

[[ -f "$HOME/.bash_completion" ]] && . "$HOME/.bash_completion"

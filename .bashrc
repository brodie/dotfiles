# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ ! -z "$(command -v lesspipe)" -a -x "$(command -v lesspipe)" ] && \
    eval "$(lesspipe)"

export PS1="\u:\w$ "

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -r $HOME/.bash_aliases ]; then
#    . $HOME/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" -a ! -z "$(command -v dircolors)" -a -x \
     "$(command -v dircolors)" ]; then
    eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='ls --color=auto --format=vertical'
    alias vdir='ls --color=auto --format=long'
else
    export CLICOLOR=1
    export LSCOLORS=ExFxCxDxBxegedabagacad
    alias dir='ls --format=vertical'
    alias vdir='ls --format=long'
fi

# Some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# Extra paths
[ -d /usr/X11R6/bin ] && export PATH="$PATH:/usr/X11R6/bin"
[ -d /usr/local/X11R6/bin ] && export PATH="$PATH:/usr/local/X11R6/bin"
[ -d /usr/local/bin ] && export PATH="/usr/local/bin:$PATH"
[ -d /usr/local/sbin ] && export PATH="/usr/local/sbin:$PATH"
[ -d /usr/local/mysql/bin ] && export PATH="/usr/local/mysql/bin:$PATH"
[ -d /opt/local/bin ] && export PATH="/opt/local/bin:$PATH"
[ -d /opt/local/sbin ] && export PATH="/opt/local/sbin:$PATH"
[ -d /Library/Frameworks/Python.framework/Versions/Current/bin ] &&
export PATH="/Library/Frameworks/Python.framework/Versions/Current/bin:$PATH"
[ -d "$HOME/bin" ] && export PATH="$HOME/bin:$PATH"

# Extra man pages
[ -d /Library/Frameworks/Python.framework/Versions/Current/man ] && \
export MANPATH="/Library/Frameworks/Python.framework/Versions/Current/man:\
$MANPATH"
[ -d /opt/local/share/man ] && export MANPATH="/opt/local/share/man:$MANPATH"
[ -d /usr/X11R6/man ] && export MANPATH="/usr/X11R6/man:$MANPATH"

#unset PROMPT_COMMAND
export EDITOR="vim"
export PAGER="less"
export HISTIGNORE="&:cd:ls:[bf]g:clear:exit:logout"
#export LANG="fr_FR.UTF-8"
shopt -s cdspell

# Fink
[ -r /sw/bin/init.sh ] && . /sw/bin/init.sh

# Enable programmable completion
[ -r $HOME/.bash_completion ] && . $HOME/.bash_completion

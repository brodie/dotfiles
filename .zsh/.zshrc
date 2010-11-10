#!/usr/bin/env zsh

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
       TZ='Australia/Sydney' \
       VIRTUALENV_USE_DISTRIBUTE=1 \
       VISUAL='emacsclient -nw' \
       WORKON_HOME="$HOME/Documents/Envs"

[[ -n "$(command -v virtualenvwrapper.sh)" ]] && source virtualenvwrapper.sh

setopt NO_clobber \
       extended_history \
       glob_complete \
       hist_allow_clobber \
       hist_expire_dups_first \
       hist_ignore_all_dups \
       hist_ignore_dups \
       hist_ignore_space \
       hist_reduce_blanks \
       hist_verify \
       share_history \
       interactive_comments \
       list_packed \
       long_list_jobs \
       multios \
       numeric_glob_sort \
       posix_builtins \
       prompt_subst \
       pushd_ignore_dups

HISTSIZE=1000
HISTFILE="$ZDOTDIR/.zsh_history"
SAVEHIST=1000

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

ZLS_COLORS="$LS_COLORS"

alias ll='ls -l' \
      la='ls -A' \
      l='less' \
      mq='hg --cwd $(hg root)/.hg/patches' \
      tm='tmux a -d' \
      ecn='emacsclient -n' \
      ec='emacsclient -nw'

beep() { echo -n '\a' }

if [[ "$TERM" != dumb && -n "$(command -v colordiff)" ]]
then
    alias diff='colordiff -u'
fi

[[ -n "$(command -v bpython)" ]] \
    && alias py=bpython || alias py='python -i /dev/null'

autoload -Uz colors && colors

# less niceties

export LESS='-iR' \
       LESSOPEN="| $HOME/.bin/lesspipe %s" \
       LESS_TERMCAP_mb=${fg_bold[red]} \
       LESS_TERMCAP_md=${fg_bold[blue]} \
       LESS_TERMCAP_me=$reset_color \
       LESS_TERMCAP_se=$reset_color \
       LESS_TERMCAP_ue=$reset_color \
       LESS_TERMCAP_us=${fg_bold[green]}
LESS_TERMCAP_so=$'\e['"${color[bold]};${color[bg-blue]};${color[yellow]}m"
export LESS_TERMCAP_so

# Completion

zmodload -i zsh/complist
autoload -Uz compinit && compinit

# Formatting
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format "%{${fg[blue]}%}%d%{$reset_color%}"
zstyle ':completion:*:messages' format "%{${fg[green]}%}%d%{$reset_color%}"
zstyle ':completion:*:warnings' format \
       "%{${fg[red]}%}No matches for:%{$reset_color%} %d"
zstyle ':completion:*:corrections' format \
       "%{${fg[red]}%}%d (errors: %e)%{$reset_color%}"
zstyle ':completion:*' group-name ''
[[ -n "$LS_COLORS" ]] && zstyle ':completion:*' list-colors \
                                "${(s.:.)LS_COLORS}"

# Rehash PATH for new commands
_force_rehash()
{
    if [[ $CURRENT -eq 1 ]]
    then
        rehash
    elif [[ $CURRENT -eq 2 && "$words[1]" = sudo ]]
    then
        rehash
    fi
    return 1
}

# Matching
zstyle ':completion:*' use-cache on
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' \
                                    'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle -e ':completion:*' completer '
    if [[ $_last_try != "$HISTNO$BUFFER$CURSOR" ]]
    then
        _last_try="$HISTNO$BUFFER$CURSOR"
        reply=(_force_rehash _complete _match _prefix)
    else
        reply=(_ignored _correct _approximate _complete)
    fi'

zstyle ':completion:*match:*' original only
zstyle -e ':completion:*:approximate:*' \
           max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:(cp|emacs|diff|kill|ls|rm|rmdir|scp|vim|vimdiff):*' \
       ignore-line yes
zstyle -e ':completion:*:processes' command '
    if (( $funcstack[(eI)$_comps[sudo]] ))
    then
       reply="ps axho user,pid,command"
    else
       reply="ps xho pid,command"
    fi'
zstyle -e ':completion:*:processes-names' command '
    if (( $funcstack[(eI)$_comps[sudo]] ))
    then
       reply="ps axho command"
    else
       reply="ps xho command"
    fi'

# Prompt

_prompt_pwd()
{
    case $PWD in
        $HOME)
            echo -n '~'
            ;;
        *)
            local p=$(echo -n "${PWD/$HOME/~}" | sed -e \
                      's-/\([^/]\)\([^/]*\)-/\1-g')
            echo -n "$p${${PWD/#*\/}[2,-1]}"
            ;;
    esac
}

autoload -Uz vcs_dir
_prompt_vcs()
{
    vcs_dir
    if [[ "$vcs_dir_type" == hg ]]
    then
        local b=$(cat "$vcs_dir_path/.hg/branch" 2>/dev/null || echo default)
        echo -n " %{$fg[green]%}$b"
        if [[ -f "$vcs_dir_path/.hg/patches/status" ]]
        then
            local p=${${(s-:-)$(tail -1 "$vcs_dir_path/.hg/patches/status")}[2]}
            [[ -n "$p" ]] && echo -n "/%{$fg[yellow]%}$p"
        fi
    elif [[ "$vcs_dir_type" == git ]]
    then
        if [[ -n "$vcs_dir_path" ]]
        then
            local b=${${(s:/:)$(cat "$vcs_dir_path/.git/HEAD")}[-1]}
            echo -n " %{$fg[green]%}$b"
        fi
    fi
}


PROMPT="%{${fg[blue]}%}$%{$reset_color%} "
RPROMPT="%{${fg[cyan]}%}\$(_prompt_pwd)\$(_prompt_vcs)%{$reset_color%}"

# Window title
precmd () { print -Pn "\e]0;%m: $(_prompt_pwd)\a" }

# Load/configure key bindings

bindkey -e # Revert back to emacs mode
WORDCHARS='' # Use emacs-style word matching

autoload -Uz select-word-style
select-word-style normal
zstyle ':zle:*' word-style subword

autoload -Uz down-line-or-beginning-search up-line-or-beginning-search
zle -N down-line-or-history down-line-or-beginning-search
zle -N up-line-or-history up-line-or-beginning-search

tmux-copy-mode-pageup() { [[ "$TMUX" != "" ]] && tmux copy-mode -u }
tmux-copy-mode() { [[ "$TMUX" != "" ]] && tmux copy-mode }
zle -N tmux-copy-mode-pageup tmux-copy-mode-pageup
zle -N tmux-copy-mode tmux-copy-mode

# Make ^W work like it does in bash (while leaving other bindings alone)
zle -N backward-kill-word-bash backward-kill-word-match
zstyle ':zle:backward-kill-word-bash' word-style whitespace

bindkey '^Q' quoted-insert \
        '^U' vi-kill-line \
        '^W' backward-kill-word-bash \
        '\ev'  tmux-copy-mode-pageup \
        '^V' tmux-copy-mode

if [[ "$TERM" = xterm* ]]
then
    bindkey '\e[1;5A' up-line-or-history \
            '\e[1;3A' up-line-or-history \
            '\e[1;5B' down-line-or-history \
            '\e[1;3B' down-line-or-history \
            '\e[1;5D' backward-word \
            '\e[1;3D' backward-word \
            '\e[1;5C' forward-word \
            '\e[1;3C' forward-word
fi

autoload -Uz zkbd
[[ ! -d "$ZDOTDIR/.zkbd" ]] && mkdir "$ZDOTDIR/.zkbd"
[[ ! -f "$ZDOTDIR/.zkbd/$TERM-$VENDOR-$OSTYPE" ]] && zkbd
source "$ZDOTDIR/.zkbd/$TERM-$VENDOR-$OSTYPE"

[[ -n "${key[Backspace]}" ]] && bindkey "${key[Backspace]}" \
                                        backward-delete-char
[[ -n "${key[Insert]}" ]] && bindkey "${key[Insert]}" beep
[[ -n "${key[Home]}" ]] && bindkey "${key[Home]}" beginning-of-line
[[ -n "${key[PageUp]}" ]] && bindkey "${key[PageUp]}" tmux-copy-mode-pageup
[[ -n "${key[Delete]}" ]] && bindkey "${key[Delete]}" delete-char
[[ -n "${key[End]}" ]] && bindkey "${key[End]}" end-of-line
[[ -n "${key[PageDown]}" ]] && bindkey "${key[PageDown]}" tmux-copy-mode
[[ -n "${key[Up]}" ]] && bindkey "${key[Up]}" up-line-or-history && \
                         bindkey "\e${key[Up]}" up-line-or-history
[[ -n "${key[Left]}" ]] && bindkey "${key[Left]}" backward-char && \
                           bindkey "\e${key[Left]}" backward-word
[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-history && \
                           bindkey "\e${key[Down]}" down-line-or-history
[[ -n "${key[Right]}" ]] && bindkey "${key[Right]}" forward-char && \
                            bindkey "\e${key[Right]}" forward-word
[[ -n "${key[Control-Left]}" ]] && bindkey "${key[Control-Left]}" backward-word
[[ -n "${key[Control-Right]}" ]] && bindkey "${key[Control-Right]}" \
                                            forward-word

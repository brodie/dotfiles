#!/bin/zsh

# Options

setopt append_history \
       auto_list \
       auto_menu \
       auto_remove_slash \
       correct \
    NO_clobber \
       extended_glob \
       extended_history \
       glob_complete \
    NO_hash_list_all \
       hist_allow_clobber \
       hist_beep \
       hist_expire_dups_first \
       hist_ignore_all_dups \
       hist_ignore_dups \
       hist_ignore_space \
       hist_reduce_blanks \
       hist_verify \
       inc_append_history \
       interactive_comments \
       list_types \
       list_packed \
       long_list_jobs \
       multios \
       numeric_glob_sort \
       posix_builtins \
       prompt_subst \
       pushd_ignore_dups

HISTSIZE=1000
HISTFILE="$HOME/.zsh_history"
SAVEHIST=1000

# Aliases

if [ "$TERM" != "dumb" -a ! -z "$(command -v dircolors)" -a -x \
     "$(command -v dircolors)" ]; then
    eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='ls --color=auto --format=vertical'
    alias vdir='ls --color=auto --format=long'
else
    CLICOLOR=1; export CLICOLOR
    LSCOLORS=ExGxFxdaCxDaDaHbadabec; export LSCOLORS
    LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.flac=01;35:*.mp3=01;35:*.mpc=01;35:*.ogg=01;35:*.wav=01;35:'
    export LS_COLORS
    alias dir='ls --format=vertical'
    alias vdir='ls --format=long'
fi

ZLS_COLORS="$LS_COLORS"
export ZLS_COLORS

alias ll='ls -l' \
      la='ls -A' \
      l='ls -CF' \
      grep='grep --color=auto'

function beep()
{
    echo -n '\a'
}

autoload -U colors && colors

# less niceties

LESS='-R'; export LESS
LESSOPEN="| $HOME/bin/lesspipe %s"; export LESSOPEN
LESS_TERMCAP_mb=${fg_bold[red]}; export LESS_TERMCAP_mb
LESS_TERMCAP_md=${fg_bold[blue]}; export LESS_TERMCAP_md
LESS_TERMCAP_me=$reset_color; export LESS_TERMCAP_me
LESS_TERMCAP_se=$reset_color; export LESS_TERMCAP_se
LESS_TERMCAP_so="$lc${color[bold]};${color[bg-blue]};${color[yellow]}$rc"
export LESS_TERMCAP_so
LESS_TERMCAP_ue=$reset_color; export LESS_TERMCAP_ue
LESS_TERMCAP_us=${fg_bold[green]}; export LESS_TERMCAP_us

# fish-style history search

__UP_DOWN_LINE_OR_SEARCH_BUFFER=
function up-line-or-search-all()
{
    if [[ ($LASTWIDGET != 'down-line-or-search-all' &&
       $LASTWIDGET != 'up-line-or-search-all')
      || "$BUFFER" == '' ]]
    then
        __UP_DOWN_LINE_OR_SEARCH_BUFFER="$BUFFER"
    fi
    zle up-line-or-search -- "$__UP_DOWN_LINE_OR_SEARCH_BUFFER"
}
function down-line-or-search-all()
{
    if [[ ($LASTWIDGET != 'down-line-or-search-all' &&
       $LASTWIDGET != 'up-line-or-search-all')
      || "$BUFFER" == '' ]]
    then
        __UP_DOWN_LINE_OR_SEARCH_BUFFER="$BUFFER"
    fi
    zle down-line-or-search -- "$__UP_DOWN_LINE_OR_SEARCH_BUFFER"
}

zle -N up-line-or-search-all
zle -N down-line-or-search-all

# Load/configure key bindings

autoload zkbd
[[ ! -d "$HOME/.zkbd" ]] && mkdir "$HOME/.zkbd"
[[ ! -f "$HOME/.zkbd/$TERM-$VENDOR-$OSTYPE" ]] && zkbd
source "$HOME/.zkbd/$TERM-$VENDOR-$OSTYPE"

bindkey -e # Revert back to emacs mode
WORDCHARS='' # Use emacs-style word matching

[[ -n "${key[Backspace]}" ]] && bindkey "${key[Backspace]}" \
                                        backward-delete-char
[[ -n "${key[Insert]}" ]] && bindkey "${key[Insert]}" beep
[[ -n "${key[Home]}" ]] && bindkey "${key[Home]}" beginning-of-line
[[ -n "${key[PageUp]}" ]] && bindkey "${key[PageUp]}" beep
[[ -n "${key[Delete]}" ]] && bindkey "${key[Delete]}" delete-char
[[ -n "${key[End]}" ]] && bindkey "${key[End]}" end-of-line
[[ -n "${key[PageDown]}" ]] && bindkey "${key[PageDown]}" beep
[[ -n "${key[Up]}" ]] && bindkey "${key[Up]}" up-line-or-search-all
[[ -n "${key[Left]}" ]] && bindkey "${key[Left]}" backward-char
[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-search-all
[[ -n "${key[Right]}" ]] && bindkey "${key[Right]}" forward-char

bindkey '^[[5C' forward-word \
        '^[[5D' backward-word \
        '^[[1;5C' forward-word \
        '^[[1;5D' backward-word \
        '^P' up-line-or-search-all \
        '^N' down-line-or-search-all \
        '^W' vi-backward-kill-word

# Completion

zmodload -i zsh/complist
autoload -U compinit && compinit

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
function _force_rehash()
{
    (( CURRENT == 1 )) && rehash
    return 1
}

# Matching
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' \
                                    'l:|=* r:|=*'
zstyle ':completion:*' completer _force_rehash _complete _match _correct \
                                 _approximate _prefix
zstyle ':completion:*match:*' original only
zstyle -e ':completion:*:approximate:*' \
           max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes

# Prompt

function prompt_pwd()
{
    case $PWD in
        $HOME)
            echo '~'
            ;;
        *)
            printf '%s' `echo $PWD | sed -e "s|^$HOME|~|" \
                                         -e 's-/\([^/]\)\([^/]*\)-/\1-g'`
            echo $PWD | sed -n -e 's-.*/.\([^/]*\)-\1-p'
            ;;
    esac
}

PROMPT="%n %{${fg[blue]}%}\$(prompt_pwd)%{$reset_color%}: "

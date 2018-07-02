#!/usr/bin/env zsh

setopt NO_clobber \
       combiningchars \
       extended_history \
       hist_allow_clobber \
       hist_expire_dups_first \
       hist_ignore_all_dups \
       hist_ignore_dups \
       hist_ignore_space \
       hist_reduce_blanks \
       interactive_comments \
       numeric_glob_sort \
       prompt_subst \
       pushd_ignore_dups \
       share_history

HISTSIZE=5000
HISTFILE="$ZDOTDIR/.zsh_history"
SAVEHIST=5000

export ACK_PAGER='less' \
       ALTERNATE_EDITOR='nano' \
       BROWSER='open' \
       EDITOR='nvim' \
       GREP_COLOR='auto' \
       LESS='-iR' \
       PAGER='less' \
       PYTHONSTARTUP="$HOME/.pythonrc.py" \
       VISUAL='nvim' \
       WORKON_HOME="$HOME/Documents/Envs"

if [[ "$TERM" != dumb ]]
then
    autoload -Uz colors && colors

    stty -ixoff -ixon # Disable flow control, which makes ^S and ^Q work
    if (( $+commands[dircolors] ))
    then
        eval "$(dircolors -b)"
        alias ls='ls --color=auto'
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
    fi

    ZLS_COLORS="$LS_COLORS"

    export LESS_TERMCAP_mb="${fg_bold[red]}" \
           LESS_TERMCAP_md="${fg_bold[blue]}" \
           LESS_TERMCAP_me="$reset_color" \
           LESS_TERMCAP_se="$reset_color" \
           LESS_TERMCAP_so="${bg[blue]}${fg_bold[yellow]}" \
           LESS_TERMCAP_ue="$reset_color" \
           LESS_TERMCAP_us="${fg_bold[green]}" \

    alias ag='ag --pager "$PAGER"'
    #rg() { command rg --pretty $@ | "$PAGER" }
fi

alias tm='tmux a -d'
#ec() { [[ -n "$@" ]] && mvim --remote-silent $@ || open -a MacVim }
#alias ec='subl -n'
#alias ec='emacsclient -nw'
alias nv=nvim
alias vim=nvim
alias vimdiff='nvim -d'
beep() { echo -n '\a' }

# Completion
autoload -Uz compinit && compinit

# Formatting
zstyle ':completion:*:descriptions' format "%{${fg[blue]}%}%d%{$reset_color%}"
zstyle ':completion:*:messages' format "%{${fg[green]}%}%d%{$reset_color%}"
zstyle ':completion:*:warnings' format \
       "%{${fg[red]}%}No matches for:%{$reset_color%} %d"
zstyle ':completion:*:corrections' format \
       "%{${fg[red]}%}%d (errors: %e)%{$reset_color%}"
zstyle ':completion:*' group-name ''
[[ -n "$LS_COLORS" ]] && zstyle ':completion:*' list-colors \
                                "${(s.:.)LS_COLORS}"

# Matching
zstyle ':completion:*' use-cache on
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' \
                                    'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
#zstyle -e ':completion:*' completer '
#    if [[ $_last_try != "$HISTNO$BUFFER$CURSOR" ]]
#    then
#        _last_try="$HISTNO$BUFFER$CURSOR"
#        reply=(_complete _match _prefix)
#    else
#        reply=(_ignored _correct _approximate _complete)
#    fi'
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
#zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# Prompt

_prompt_pwd()
{
    local pwd="${1-$PWD}"
    case $pwd in
        $HOME)
            echo -n '~'
            ;;
        *)
            pwd="${PWD/$HOME/~}"
            local parts; parts=(${(s-/-)pwd})
            local s=""
            [[ "$pwd" == /* ]] && s+='/'
            for p in $parts[1,-2]
            do
                s+="$p[1]/"
            done
            s+="$parts[-1]"
            echo -n "$s"
            ;;
    esac
}

autoload -Uz vcs_dir
_prompt_vcs()
{
    vcs_dir
    if [[ "$vcs_dir_type" == hg ]]
    then
        local b="`cat "$vcs_dir_path/.hg/bookmarks.current" 2>/dev/null`"
        [[ -z "$b" ]] && b="`cat "$vcs_dir_path/.hg/branch" 2>/dev/null ||
                             echo default`"
        echo -n " %{$fg[green]%}$b"
        if [[ -f "$vcs_dir_path/.hg/patches/status" ]]
        then
            local p="`tail -1 "$vcs_dir_path/.hg/patches/status"`"
            p="${${(s-:-)p}[2]}"
            [[ -n "$p" ]] && echo -n "/%{$fg[yellow]%}$p"
        fi
    elif [[ "$vcs_dir_type" == git ]]
    then
        if [[ -n "$vcs_dir_path" ]]
        then
            local b="`cat "$vcs_dir_path/.git/HEAD"`"
            case "$b" in
                "ref: refs/heads/"*) b="${b[17,-1]}";;
                "ref: refs/tags/"*) b="${b[16,-1]}";;
                "ref: refs/remotes/"*) b="${b[19,-1]}";;
                "ref: "*) b="${b[6,-1]}";;
                *) b="$b[0,7]";;
            esac
            echo -n " %{$fg[green]%}$b"
        fi
    fi
}

PROMPT="%{${fg[blue]}%}$%{$reset_color%} "
RPROMPT="%{${fg[cyan]}%}\$(_prompt_pwd)\$(_prompt_vcs)%{$reset_color%}"

# Window title
precmd ()
{
    if [[ "$TERM_PROGRAM" == "Apple_Terminal" ]] && [[ -z "$INSIDE_EMACS" ]]
    then
        echo -n "\e]7;file://$HOSTNAME${PWD// /%20}\a"
    fi
    print -Pn "\e]0;%m: $(_prompt_pwd)\a"
}

# Load/configure key bindings

bindkey -e # Revert back to emacs mode
WORDCHARS='' # Use emacs-style word matching

autoload -Uz down-line-or-beginning-search up-line-or-beginning-search
zle -N down-line-or-history down-line-or-beginning-search
zle -N up-line-or-history up-line-or-beginning-search

tmux-copy-mode-pageup() { [[ "$TMUX" != "" ]] && tmux copy-mode -u }
tmux-copy-mode() { [[ "$TMUX" != "" ]] && tmux copy-mode }
zle -N tmux-copy-mode-pageup tmux-copy-mode-pageup
zle -N tmux-copy-mode tmux-copy-mode

# Make ^W work like it does in bash (while leaving other bindings alone)
autoload -Uz backward-kill-word-match
zle -N backward-kill-word-bash backward-kill-word-match
zstyle ':zle:backward-kill-word-bash' word-style whitespace

bindkey '^Q' quoted-insert \
        '^U' vi-kill-line \
        '^W' backward-kill-word-bash \
        '\ev' tmux-copy-mode-pageup \
        '^V' tmux-copy-mode

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

if (( $+commands[virtualenvwrapper_lazy.sh] ))
then
    source virtualenvwrapper_lazy.sh
fi

#if (( $+commands[rustc] ))
#then
#     export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
#fi

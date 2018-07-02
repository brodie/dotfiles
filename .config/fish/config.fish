if status --is-interactive
    set -g fish_greeting ''

    set -x ACK_PAGER less
    set -x ALTERNATE_EDITOR nano
    set -x BROWSER open
    set -x EDITOR nvim
    set -x GREP_COLOR auto
    set -x LESS '-iR'
    set -x LESS_TERMCAP_mb (set_color red)
    set -x LESS_TERMCAP_md (set_color blue)
    set -x LESS_TERMCAP_me (set_color normal)
    set -x LESS_TERMCAP_se (set_color normal)
    set -x LESS_TERMCAP_so (set_color -o -b blue yellow)
    set -x LESS_TERMCAP_ue (set_color normal)
    set -x LESS_TERMCAP_us (set_color green)
    set -x PAGER less
    set -x PYTHONSTARTUP ~/.pythonrc.py
    set -x VISUAL nvim
    set -x WORKON_HOME ~/Documents/Envs

    alias vim nvim
    alias vimdiff 'nvim -d'

    function beep
        echo -e -n '\a'
    end

    set -g fish_user_abbreviations
    abbr nv nvim
    abbr tm 'tmux a -d'
end

if status --is-login
    set -x PATH /usr/local/sbin $PATH
    test -d ~/.bin; and set -x PATH ~/.bin $PATH
    test -d ~/.cargo/bin; and set -x PATH ~/.cargo/bin $PATH
end

set -g fish_color_prompt_root red
set -g fish_color_prompt blue

function fish_prompt --description "Write out the prompt"
    set -l suffix
    switch "$USER"
        case root toor
            set color_prompt $fish_color_prompt_root
            set suffix '#'
        case '*'
            set color_prompt $fish_color_prompt
            set suffix '$'
    end

    if test -n "$SSH_CONNECTION"
        echo -n -s (set_color $color_prompt) (prompt_hostname) " $suffix " \
            (set_color normal)
    else
        echo -n -s (set_color $color_prompt) "$suffix " (set_color normal)
    end
end

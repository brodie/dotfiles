set -g fish_color_cwd cyan

function fish_right_prompt --description "Write out the right prompt"
    set -l color_cwd
    switch "$USER"
        case root toor
            if set -q fish_color_cwd_root
                set color_cwd $fish_color_cwd_root
            else
                set color_cwd $fish_color_cwd
            end
        case '*'
            set color_cwd $fish_color_cwd
    end

    echo -n -s (set_color $color_cwd) (prompt_pwd) (set_color normal) (prompt_vcs)
end

set -g fish_color_vcs_branch green
set -g fish_color_vcs_extra blue

function prompt_vcs --description \
        'Print the prompt for Git and Mercurial repositories'
    set -l dir $PWD
    set -l vcs
    while test $dir != '/'
        if test -d $dir/.hg
            set vcs hg
            break
        else if test -d $dir/.git
            set vcs git
            break
        end
        set dir (string replace -r '[^/]*/?$' '' $dir)
    end

    if test "$vcs" = git
        set -l branch (cat $dir/.git/HEAD)
        set branch (string replace -r 'ref: (?:refs/heads/|refs/)' '' $branch)
        if test $status -ne 0
            set branch (string match -r '.{0,7}' $branch)
        end

        echo -n -s (set_color $fish_color_vcs_branch) " $branch" \
            (set_color normal)
    else if test "$vcs" = hg
        set -l branch (cat $dir/.hg/bookmarks.current ^/dev/null)
        if test -z "$branch"
            set branch (cat $dir/.hg/branch ^/dev/null; or echo default)
        end
        echo -n -s (set_color $fish_color_vcs_branch) " $branch" \
            (set_color normal)

        if test -f $dir/.hg/patches/status
            set -l patch (tail -1 $dir/.hg/patches/status)
            if test -n "$patch"
                echo -n -s (set_color $fish_color_vcs_branch) '/' \
                    (set_color $fish_color_vcs_extra) \
                    (string split --max 1 : $patch)[2] (set_color normal)
            end
        end
    end
end

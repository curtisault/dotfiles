function tmux_mksystem
    # A fixed, non-project "system" session for personal/mixed use:
    #   scratch -> plain shell for ad-hoc commands
    #   rss     -> eilmeldung RSS reader
    #   tasks   -> taskwarrior
    #   sys     -> btm (bottom) system monitor
    #   disk    -> ncdu disk-usage browser ($HOME)
    set session system
    set dir $HOME

    if tmux has-session -t $session 2>/dev/null
        echo "Session '$session' already exists"
        return 0
    end

    tmux new-session -d -s $session -n scratch -c $dir
    tmux new-window -t $session -n rss -c $dir
    tmux send-keys -t $session:rss rss Enter
    tmux new-window -t $session -n tasks -c $dir
    tmux send-keys -t $session:tasks task Enter
    tmux new-window -t $session -n sys -c $dir
    tmux send-keys -t $session:sys btm Enter
    tmux new-window -t $session -n disk -c $dir
    tmux send-keys -t $session:disk 'ncdu ~' Enter
    tmux select-window -t $session:scratch
end

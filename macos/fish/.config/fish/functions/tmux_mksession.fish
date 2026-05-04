function tmux_mksession
    if test (count $argv) -eq 0
        echo "Usage: tmux_mksession <session-name> [directory]"
        return 1
    end

    set session $argv[1]

    if test (count $argv) -ge 2
        set dir $argv[2]
    else
        set dir $HOME
    end

    if not test -d $dir
        echo "Directory '$dir' does not exist"
        return 1
    end

    if tmux has-session -t $session 2>/dev/null
        echo "Session '$session' already exists"
        return 0
    end

    tmux new-session -d -s $session -n nvim -c $dir
    tmux new-window -t $session -n run -c $dir
    tmux split-window -h -t $session:run -c $dir
    tmux new-window -t $session -n git -c $dir
    tmux new-window -t $session -n logs -c $dir
    tmux new-window -t $session -n db -c $dir
    tmux new-window -t $session -n claudius -c $dir
    tmux new-window -t $session -n claude-work -c $dir
    tmux new-window -t $session -n claude-logs -c $dir 'fish -c claude_logs_tail'
    tmux select-window -t $session:nvim
end

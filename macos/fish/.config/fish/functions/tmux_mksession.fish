function tmux_mksession
    argparse 'cli=' -- $argv
    or return 1

    if test (count $argv) -eq 0
        echo "Usage: tmux_mksession <session-name> [directory] [--cli claude|pi]"
        return 1
    end

    # Choose the AI CLI client to launch (default: claude)
    set cli claude
    if set -q _flag_cli
        set cli $_flag_cli
    end

    switch $cli
        case claude
            set ai_window claudius
            set ai_command claude
        case pi
            set ai_window pious
            set ai_command pi
        case '*'
            echo "Unknown --cli '$cli' (expected: claude or pi)"
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
    tmux send-keys -t $session:git lazygit Enter
    tmux new-window -t $session -n github -c $dir
    tmux send-keys -t $session:github 'gh dash' Enter
    tmux new-window -t $session -n db -c $dir
    tmux send-keys -t $session:db 'pgcli -u postgres' Enter
    tmux new-window -t $session -n $ai_window -c $dir
    tmux send-keys -t $session:$ai_window $ai_command Enter
    tmux select-window -t $session:nvim
end

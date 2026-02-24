function tmux_mksession
    if test (count $argv) -eq 0
        echo "Usage: tmux_mksession <session-name>"
        return 1
    end

    set session $argv[1]

    if tmux has-session -t $session 2>/dev/null
        echo "Session '$session' already exists"
        return 0
    end

    tmux new-session -d -s $session -n nvim
    tmux new-window -t $session -n run
    tmux new-window -t $session -n git
    tmux new-window -t $session -n claudius
    tmux new-window -t $session -n github
    tmux select-window -t $session:nvim
end

function herdr_mkspace
    argparse 'cli=' -- $argv
    or return 1

    if test (count $argv) -eq 0
        echo "Usage: herdr_mkspace <space-name> [directory] [--cli claude|pi]"
        return 1
    end

    # Choose the AI CLI client to launch (default: claude)
    set cli claude
    if set -q _flag_cli
        set cli $_flag_cli
    end

    switch $cli
        case claude
            set ai_label claudius
            set ai_command claude
        case pi
            set ai_label pious
            set ai_command pi
        case '*'
            echo "Unknown --cli '$cli' (expected: claude or pi)"
            return 1
    end

    set space $argv[1]

    if test (count $argv) -ge 2
        set dir $argv[2]
    else
        set dir $HOME
    end

    if not test -d $dir
        echo "Directory '$dir' does not exist"
        return 1
    end

    # The herdr server resolves --cwd against its own working directory,
    # not this shell's — a relative path would silently land tabs in ~.
    set dir (path resolve $dir)

    if herdr workspace list | jq -e --arg label $space '.result.workspaces[] | select(.label == $label)' >/dev/null
        echo "Space '$space' already exists"
        return 0
    end

    # The workspace's initial tab becomes nvim (stays active, like tmux -d + select-window)
    set created (herdr workspace create --cwd $dir --label $space --no-focus)
    or return 1
    set ws (echo $created | jq -r '.result.workspace.workspace_id')
    herdr tab rename (echo $created | jq -r '.result.tab.tab_id') nvim >/dev/null

    # run: dev server | tests/watcher, side by side
    set run_pane (herdr tab create --workspace $ws --label run --cwd $dir --no-focus | jq -r '.result.root_pane.pane_id')
    herdr pane split $run_pane --direction right --cwd $dir --no-focus >/dev/null

    set git_pane (herdr tab create --workspace $ws --label git --cwd $dir --no-focus | jq -r '.result.root_pane.pane_id')
    herdr pane run $git_pane lazygit >/dev/null

    set github_pane (herdr tab create --workspace $ws --label github --cwd $dir --no-focus | jq -r '.result.root_pane.pane_id')
    herdr pane run $github_pane gh dash >/dev/null

    set db_pane (herdr tab create --workspace $ws --label db --cwd $dir --no-focus | jq -r '.result.root_pane.pane_id')
    herdr pane run $db_pane pgcli -u postgres >/dev/null

    set ai_pane (herdr tab create --workspace $ws --label $ai_label --cwd $dir --no-focus | jq -r '.result.root_pane.pane_id')
    herdr pane run $ai_pane $ai_command >/dev/null
end

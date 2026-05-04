function claude_logs_archive --description "Archive Claude session logs to SQLite (default: --all for manual runs)"
    $HOME/.claude/bin/claude-logs-archive.py --all $argv
end

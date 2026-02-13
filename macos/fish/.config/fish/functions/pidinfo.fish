function pidinfo
    set pids (pgrep $argv[1] 2>/dev/null)
    test -n "$pids" && ps -p $pids || echo "No process found: $argv[1]"
end

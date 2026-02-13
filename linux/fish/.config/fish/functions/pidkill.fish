function pidkill
    set pids (pgrep $argv[1] 2>/dev/null)
    test -n "$pids" && kill -9 $pids || echo "No process found: $argv[1]"
end

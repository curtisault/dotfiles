function claude_logs_tail --description "Tail current tmux session's Claude action log"
    set session (tmux display-message -p '#S' 2>/dev/null)
    if test -z "$session"
        echo "Not inside a tmux session"
        return 1
    end

    set logfile $HOME/.claude/logs/$session.jsonl
    mkdir -p (dirname $logfile)
    touch $logfile

    tail -F $logfile 2>/dev/null \
        | jq -r --unbuffered '[(.ts // ""), (.tool // ""), (.target // ""), (.exit_code // "")] | @tsv' \
        | awk -F'\t' '{
            t = substr($1, 12, 8)
            extra = ($4 != "" ? "  exit=" $4 : "")
            printf "[%s] %-8s %s%s\n", t, $2, $3, extra
            fflush()
        }'
end

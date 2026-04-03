function ghview
    set type $argv[1]
    set number $argv[2]

    if test -z "$type" -o -z "$number"
        echo "Usage: ghview [issue|pr] <number>"
        return 1
    end

    switch $type
        case issue
            gh issue view $number --comments | nvim - -c 'set ft=markdown'
        case pr
            gh pr view $number --comments | nvim - -c 'set ft=markdown'
        case '*'
            echo "Usage: ghview [issue|pr] <number>"
            return 1
    end
end

function git_contributors --description "List current line count by author in the repository"
    # Check if we're in a git repository
    if not git rev-parse --git-dir >/dev/null 2>&1
        echo "Error: Not a git repository"
        return 1
    end

    echo "Analyzing repository..." >&2

    # Get all author lines from all files and count them
    set -l stats (git ls-files | while read -l file
        git blame --line-porcelain "$file" 2>/dev/null | grep '^author '
    end | sed 's/^author //' | sort | uniq -c | sort -rn)

    if test -z "$stats"
        echo "Error: No data found in repository"
        return 1
    end

    # Calculate grand total
    set -l grand_total (printf '%s\n' $stats | awk '{sum += $1} END {print sum}')

    if test "$grand_total" = "0" -o -z "$grand_total"
        echo "Error: No data found in repository"
        return 1
    end

    # Print header
    printf "\n%-50s %12s %10s\n" "CONTRIBUTOR" "LINES" "PERCENT"
    printf "%s\n" (string repeat -n 75 "─")

    # Process each line
    printf '%s\n' $stats | while read -l line
        # Extract count (first field) and author (rest of line)
        set -l count (echo "$line" | awk '{print $1}')
        set -l author (echo "$line" | sed -E 's/^[[:space:]]*[0-9]+[[:space:]]*//')

        set -l percentage (math "$count / $grand_total * 100")

        printf "%-50s %12s %9.2f%%\n" \
            (string sub -l 50 "$author") \
            (printf "%'d" $count) \
            $percentage
    end

    # Print total
    printf "%s\n" (string repeat -n 75 "─")
    printf "%-50s %12s %9s\n" \
        "TOTAL" (printf "%'d" $grand_total) "100.00%"
    printf "\n"
end

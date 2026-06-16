# Show the status of the latest CI checks on a GitHub PR.
# For failed checks it surfaces the failed step names (and, with -l, a log tail)
# plus the run ID and job ID needed to re-run them (see rerun_ci_jobs).
#
# Usage: pr_ci_status <pr-number> [-r owner/repo] [-l] [--failed-ids]
#   -r/--repo     target repo (defaults to the repo in the current dir)
#   -l/--logs     fetch and show a log tail for each failed job
#   --failed-ids  print only the failed job IDs (space separated), nothing else
#                 -> pipe into rerun_ci_jobs: rerun_ci_jobs (pr_ci_status 123 --failed-ids)
function pr_ci_status --description "Show latest CI check status for a GitHub PR"
    argparse 'r/repo=' 'l/logs' 'failed-ids' -- $argv
    or return 1

    set -l pr $argv[1]
    if test -z "$pr"
        echo "Usage: pr_ci_status <pr-number> [-r owner/repo] [-l] [--failed-ids]"
        return 1
    end

    for dep in gh jq
        if not command -v $dep >/dev/null
            echo "$dep not found. Install it and try again."
            return 1
        end
    end

    set -l repo
    if set -q _flag_repo
        set repo $_flag_repo
    else
        set repo (gh repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null)
    end
    if test -z "$repo"
        echo "Could not determine repository. Pass -r owner/repo."
        return 1
    end

    set -l sha (gh pr view $pr --repo $repo --json headRefOid -q .headRefOid 2>/dev/null)
    if test -z "$sha"
        echo "Could not find PR #$pr in $repo."
        return 1
    end

    # Latest run per workflow for the PR's head commit (highest id == most recent).
    set -l runs (gh api "repos/$repo/actions/runs?head_sha=$sha&per_page=100" \
        --jq '.workflow_runs | group_by(.workflow_id) | map(max_by(.id)) | .[]
              | "\(.id)\t\(.name)\t\(.status)\t\(.conclusion // "")"' 2>/dev/null)

    if test -z "$runs"
        if not set -q _flag_failed_ids
            echo "No workflow runs found for PR #$pr ($sha) in $repo."
        end
        return 0
    end

    # In --failed-ids mode, collect failed job IDs and print only those.
    if set -q _flag_failed_ids
        set -l ids
        for line in $runs
            set -l run_id (string split \t -- $line)[1]
            set -a ids (gh api "repos/$repo/actions/runs/$run_id/jobs?per_page=100" \
                --jq '.jobs[] | select(.conclusion=="failure") | .id' 2>/dev/null)
        end
        echo $ids
        return 0
    end

    echo "CI status for PR #$pr in $repo"
    echo "Commit: $sha"
    echo ""

    set -l any_failed 0
    for line in $runs
        set -l parts (string split \t -- $line)
        set -l run_id $parts[1]
        set -l run_name $parts[2]
        set -l run_status $parts[3]
        set -l run_concl $parts[4]

        echo "▸ $run_name  (run $run_id) — "(__pr_ci_label $run_status $run_concl)

        set -l jobs (gh api "repos/$repo/actions/runs/$run_id/jobs?per_page=100" \
            --jq '.jobs[] | "\(.id)\t\(.name)\t\(.status)\t\(.conclusion // "")"' 2>/dev/null)

        for jline in $jobs
            set -l jparts (string split \t -- $jline)
            set -l job_id $jparts[1]
            set -l job_name $jparts[2]
            set -l job_status $jparts[3]
            set -l job_concl $jparts[4]

            printf "    %s  %s  (job %s)\n" (__pr_ci_icon $job_status $job_concl) $job_name $job_id

            if test "$job_concl" = failure
                set any_failed 1
                set -l failed_steps (gh api "repos/$repo/actions/jobs/$job_id" \
                    --jq '.steps[]? | select(.conclusion=="failure") | .name' 2>/dev/null)
                for step in $failed_steps
                    echo "        failed step: $step"
                end
                if set -q _flag_logs
                    echo "        --- log tail ---"
                    gh api "repos/$repo/actions/jobs/$job_id/logs" 2>/dev/null \
                        | tail -n 40 | sed 's/^/        /'
                    echo "        --- end log ---"
                end
            end
        end
        echo ""
    end

    if test $any_failed -eq 1
        echo "Re-run failed jobs with:"
        echo "    rerun_ci_jobs (pr_ci_status $pr --failed-ids)"
    end
end

function __pr_ci_label
    set -l st $argv[1]
    set -l concl $argv[2]
    if test "$st" != completed
        echo "$st"
    else
        echo "$concl"
    end
end

function __pr_ci_icon
    set -l st $argv[1]
    set -l concl $argv[2]
    if test "$st" != completed
        echo "◷"
    else
        switch $concl
            case success
                echo "✓"
            case failure
                echo "✗"
            case skipped cancelled neutral
                echo "−"
            case '*'
                echo "?"
        end
    end
end

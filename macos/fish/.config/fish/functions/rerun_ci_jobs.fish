# Re-run one or more GitHub Actions jobs by job ID.
# Pairs with pr_ci_status, which prints the failed job IDs:
#   rerun_ci_jobs (pr_ci_status 123 --failed-ids)
#
# Usage: rerun_ci_jobs <job-id>... [-r owner/repo]
#   -r/--repo  target repo (defaults to the repo in the current dir)
function rerun_ci_jobs --description "Re-run GitHub Actions jobs by job ID"
    argparse 'r/repo=' -- $argv
    or return 1

    if test (count $argv) -eq 0
        echo "Usage: rerun_ci_jobs <job-id>... [-r owner/repo]"
        return 1
    end

    if not command -v gh >/dev/null
        echo "gh not found. Install it and try again."
        return 1
    end

    set -l repo_args
    if set -q _flag_repo
        set repo_args --repo $_flag_repo
    end

    set -l ok 0
    set -l fail 0
    for job in $argv
        set -l out (gh run rerun --job $job $repo_args 2>&1)
        if test $status -eq 0
            echo "✓ requested re-run of job $job"
            set ok (math $ok + 1)
        else
            echo "✗ failed to re-run job $job: $out"
            set fail (math $fail + 1)
        end
    end

    echo ""
    echo "Re-ran $ok job(s), $fail failed."
    test $fail -eq 0
end

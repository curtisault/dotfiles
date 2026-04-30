function git_commit_files --description "List files changed in a specific commit"
    if test (count $argv) -lt 1
        echo "Usage: git_commit_files <commit-hash>"
        return 1
    end

    set -l commit $argv[1]

    if not git rev-parse --git-dir >/dev/null 2>&1
        echo "Error: Not a git repository"
        return 1
    end

    if not git rev-parse --verify --quiet "$commit^{commit}" >/dev/null
        echo "Error: '$commit' is not a valid commit"
        return 1
    end

    git diff-tree --no-commit-id -r --name-only "$commit"
end

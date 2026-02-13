function git_diff_tree
    git diff --name-only --diff-filter=AM main...HEAD | tree --fromfile . -C
end

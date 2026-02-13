function git_save_msg
    git log --all --author='Curtis Ault' --format='%h | %ad | %s' --date=short | tee curtis_commits.txt
end

---
name: branch-context
description: Read branch diff and get context of changes on the current branch compared to main. Use when you need to understand what has been modified on the current branch.
allowed-tools: Bash(git:*)
---

# Branch Context

## Context

Gather the following information:

1. **Current git status**:
   ```bash
   git status
   ```

2. **Current branch**:
   ```bash
   git branch --show-current
   ```

3. **Current git diff from main**:
   ```bash
   git diff main...HEAD
   ```

4. **Branch commits**:
   ```bash
   git log --oneline main..HEAD
   ```

5. **Show tree structure**:
   ```bash
   git diff --name-only --diff-filter=AM main...HEAD | tree --fromfile . -C
   ```

   followed by:
   ```bash
   git diff --stat main...HEAD
   ```

## Your Task

Get the context of the changes on this branch by analyzing the output of the commands above.

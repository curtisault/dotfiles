---
name: branch-context
description: Read branch diff and get context of changes on the current branch compared to a base branch. Use when you need to understand what has been modified on the current branch. Accepts an optional argument naming the base branch to compare against (defaults to main).
allowed-tools: Bash(git:*)
---

# Branch Context

## Parameters

- **base branch** *(optional)* — the git branch to compare `HEAD` against.
  Defaults to `main`. If the invocation includes an argument, use it as the
  base branch in every command below (substitute it for `main`).

## Context

Gather the following information. Replace `main` with the requested base branch
if one was provided.

1. **Current git status**:
   ```bash
   git status
   ```

2. **Current branch**:
   ```bash
   git branch --show-current
   ```

3. **Current git diff from base**:
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

Get the context of the changes on this branch by analyzing the output of the
commands above.

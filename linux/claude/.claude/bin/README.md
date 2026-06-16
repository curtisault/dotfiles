# `~/.claude/bin/` — stow package target

This directory is stowed to `~/.claude/bin/` (via `stow claude` from `dotfiles/linux/`).

## `statusline.sh`

Custom Claude Code status line. Claude Code pipes a JSON blob describing the
current session on stdin; the script prints a single line rendered at the
bottom of the TUI:

```
 <project>    <branch><dirty>    <model>    <path>
```

e.g. ` baraddur    linux-updates-20260417*    Opus 4.8    ~/dotfiles/linux`

- `project`  — basename of the git toplevel (else the cwd basename)
- `branch`   — current git branch (or short SHA when detached); `*` if the
               working tree is dirty
- `model`    — `model.display_name` from the session JSON
- `path`     — `workspace.current_dir`, with `$HOME` abbreviated to `~`

### Wiring

Enabled via the `statusLine` block in `~/.claude/settings.json`:

```json
"statusLine": {
  "type": "command",
  "command": "~/.claude/bin/statusline.sh"
}
```

Note: `~/.claude/settings.json` is a live file, **not** tracked by this repo.

### Requirements

- `jq` (parses the session JSON on stdin)
- `git` (branch / dirty detection; degrades gracefully outside a repo)

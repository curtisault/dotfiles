# `~/.claude/bin/` — stow package target

This directory is stowed to `~/.claude/bin/` (via `stow -t ~ claude` from `dotfiles/macos/`).

## `statusline.sh`

Custom Claude Code status line. Claude Code pipes a JSON blob describing the
current session on stdin; the script prints a single line rendered at the
bottom of the TUI:

```
 <project>    <branch><dirty>    <model>    <path>
```

e.g. ` dotfiles    2026-june-updates*    Opus 4.8    ~/dotfiles/macos`

- `project`  — basename of the git toplevel (else the cwd basename)
- `branch`   — current git branch (or short SHA when detached); `*` if the
               working tree is dirty
- `model`    — `model.display_name` from the session JSON
- `path`     — `workspace.current_dir`, with `$HOME` abbreviated to `~`

### Wiring

Add the `statusLine` block to `~/.claude/settings.json` (this is **required** —
stowing the script alone does nothing until Claude Code is told to run it):

```json
"statusLine": {
  "type": "command",
  "command": "$HOME/.claude/bin/statusline.sh"
}
```

`settings.json` is read at startup, so **restart Claude Code** (or start a new
session) after adding the block for it to take effect.

Note: `~/.claude/settings.json` is a live, machine-local file, **not** tracked
by this repo.

### Requirements

- `jq` (parses the session JSON on stdin)
- `git` (branch / dirty detection; degrades gracefully outside a repo)

# tmux + Claude Code setup

This documents the workflow that lets Claude Code observe and operate inside
tmux sessions safely.

## Goals

1. **Observe my workflow.** Claude reads from the panes I'm already watching
   (dev server, tests, logs, db) instead of running duplicate processes.
2. **Stay out of my way.** Claude reads my panes but never sends keys to them.

## Session layout

One tmux session per project, named after the project, created by the
`tmux_mksession` fish function. Every session has the same window layout:

| Window     | Purpose                                                    | Owner |
|------------|------------------------------------------------------------|-------|
| `nvim`     | editor                                                     | me    |
| `run`      | 2 horizontal panes: `.0` = dev server, `.1` = tests/watcher | me    |
| `git`      | git operations                                             | me    |
| `logs`     | application/server logs                                    | me    |
| `db`       | database client                                            | me    |
| `claudius` | where Claude is running                                    | Claude |

`claudius` is grouped at the end so Claude's window stays out of the way of my
own windows.

## Read/write rules (defined in `~/.claude/CLAUDE.md`)

- **Read** from any window with `tmux capture-pane`. This is how Claude
  observes my dev server, tests, logs, and db state.
- **Never send keys** to `nvim`, `run`, `git`, `logs`, or `db`. Those are
  mine — they may have running processes I'm watching.
- **Targeting is by name**, not index — reordering windows is fine, renaming
  them is not.

## Capture-pane recipe

```
tmux capture-pane -t <session>:<window> -p -S -10000
```

The `-S -10000` matters — without it you only get the visible viewport and
miss errors scrolled offscreen. For the `run` window's panes, target
`<session>:run.0` (dev server) or `<session>:run.1` (tests/watcher).

## File map

### Fish functions (live in `dotfiles/macos/fish/.config/fish/functions/`)

| File | Purpose |
|---|---|
| `tmux_mksession.fish` | Creates a project session with the standard layout |

### Settings

| Path | Purpose |
|---|---|
| `~/.claude/CLAUDE.md` | Tells Claude the layout, read/write rules, capture-pane recipe |

## Onboarding a new machine

1. Install dependencies: `tmux`, `fish`. Both standard.
2. Stow the dotfiles so the fish functions land in `~/.config/fish/functions/`.
3. Append the tmux section to `~/.claude/CLAUDE.md`.
4. Create a project session with `tmux_mksession <name> [dir]`.

## History

An earlier version of this setup included `claude-work` and `claude-logs`
windows plus a full action-logging pipeline (a `claude-logger.sh` hook
appending JSONL per action, a `claude-logs-archive.py` archiver into SQLite,
and a daily launchd job). That system was removed on 2026-06-16 — it wasn't
useful enough to keep. See git history if you ever want to resurrect it.

## Future enhancements (not built)

- **Auto-rename `claudius` window with current task** — Claude could update
  the window name to reflect the task it's on, making the tmux status bar
  more informative across multiple parallel sessions.

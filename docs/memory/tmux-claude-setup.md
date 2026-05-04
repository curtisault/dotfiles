# tmux + Claude Code setup

This documents the workflow that lets Claude Code observe and operate inside
tmux sessions safely, with a full action log streamed to a dedicated window
and archived to SQLite for long-term analysis.

## Goals

1. **Observe my workflow.** Claude reads from the panes I'm already watching
   (dev server, tests, logs, db) instead of running duplicate processes.
2. **Stay out of my way.** Claude has its own sandbox window for running
   commands, splitting panes, and inspecting output.
3. **Audit everything.** Every Bash command, Edit, Write, and prompt
   Claude processes is logged. I can read the live tail in a window or grep
   archived sessions later.

## Session layout

One tmux session per project, named after the project, created by the
`tmux_mksession` fish function. Every session has the same window layout:

| Window        | Purpose                                                    | Owner |
|---------------|------------------------------------------------------------|-------|
| `nvim`        | editor                                                     | me    |
| `run`         | 2 horizontal panes: `.0` = dev server, `.1` = tests/watcher | me    |
| `git`         | git operations                                             | me    |
| `logs`        | application/server logs                                    | me    |
| `db`          | database client                                            | me    |
| `claudius`    | where Claude is running                                    | Claude |
| `claude-work` | sandbox: Claude can split, run, inspect freely             | Claude |
| `claude-logs` | live tail of Claude's action log (auto-populated)          | shared (read-only) |

Window order keeps Claude's stuff grouped at the end (`claudius`,
`claude-work`, `claude-logs`).

## Read/write rules (defined in `~/.claude/CLAUDE.md`)

- **Read** from any window with `tmux capture-pane`. This is how Claude
  observes my dev server, tests, logs, and db state.
- **Never send keys** to `nvim`, `run`, `git`, `logs`, or `db`. Those are
  mine — they may have running processes I'm watching.
- **`claude-work` is Claude's.** Split it, run commands, leave panes around
  for its own observation. Clean up panes when done.
- **Targeting is by name**, not index — reordering windows is fine, renaming
  them is not.

## Capture-pane recipe

```
tmux capture-pane -t <session>:<window> -p -S -10000
```

The `-S -10000` matters — without it you only get the visible viewport and
miss errors scrolled offscreen. For the `run` window's panes, target
`<session>:run.0` (dev server) or `<session>:run.1` (tests/watcher).

## Action logging

### Architecture

```
hook fires → append JSONL line → ~/.claude/logs/<session>.jsonl
                                       ↓ (claude-logs window)
                                    tail -F | jq | awk
                                       ↓ (archive job, periodic)
                                    insert rows → ~/.claude/logs/archive.db
                                       ↓
                                    delete flat file
```

### What gets logged (lean verbosity)

- `PROMPT` — user prompt (truncated to 500 chars)
- `BASH` — Bash tool command + exit code
- `EDIT` — Edit tool file path
- `WRITE` — Write tool file path
- `CAPTURE` — re-tagged Bash invocations matching `tmux capture-pane`,
  surfacing the `-t` target

Read is intentionally not logged — it's noisy and the file path is already
visible in subsequent EDIT/WRITE events.

### Log line format

JSONL on disk:

```jsonl
{"ts":"2026-05-02T14:23:45Z","session":"myproj","tool":"BASH","target":"npm run test","exit_code":0}
{"ts":"2026-05-02T14:24:01Z","session":"myproj","tool":"EDIT","target":"src/parser.ts"}
```

Pretty-printed in the `claude-logs` window:

```
[14:23:45] BASH     npm run test  exit=0
[14:24:01] EDIT     src/parser.ts
```

### SQLite archive schema

```sql
CREATE TABLE events (
  id          INTEGER PRIMARY KEY,
  ts          TEXT NOT NULL,        -- ISO 8601
  session     TEXT NOT NULL,
  tool        TEXT NOT NULL,        -- BASH, EDIT, WRITE, PROMPT, CAPTURE
  target      TEXT,                 -- command / file path / prompt
  exit_code   INTEGER,
  duration_ms INTEGER,              -- reserved for future use
  raw_json    TEXT                  -- original event line for forensics
);
CREATE INDEX idx_events_session ON events(session);
CREATE INDEX idx_events_ts      ON events(ts);
CREATE INDEX idx_events_tool    ON events(tool);
```

`raw_json` lets us add structured columns later without losing data.

### Archive trigger

- **Manual:** `claude_logs_archive` fish function — archives ALL flat files
  immediately (`--all`).
- **Scheduled:** launchd job at 03:00 daily — archives flat files unmodified
  for 24+ hours (lets active sessions keep their files).

## File map

### Hook + scripts (live in `~/.claude/`)

| Path | Purpose |
|---|---|
| `~/.claude/hooks/claude-logger.sh` | Bash hook that reads payload on stdin, emits JSONL line per event |
| `~/.claude/bin/claude-logs-archive.py` | Python archiver: JSONL → SQLite, then deletes flat file |
| `~/.claude/logs/<session>.jsonl` | Live per-session log (deleted after archive) |
| `~/.claude/logs/archive.db` | SQLite archive of all historical events |
| `~/.claude/logs/archive.{out,err}.log` | launchd job stdout/stderr |

### Fish functions (live in `dotfiles/macos/fish/.config/fish/functions/`)

| File | Purpose |
|---|---|
| `tmux_mksession.fish` | Creates a project session with the standard layout |
| `claude_logs_tail.fish` | Tails the current session's JSONL log via jq+awk; runs in `claude-logs` window |
| `claude_logs_archive.fish` | Wrapper around the python archiver with `--all` baked in |

### Settings + system integration

| Path | Purpose |
|---|---|
| `~/.claude/settings.json` | Hook registration: PostToolUse (Bash\|Edit\|Write), UserPromptSubmit |
| `~/.claude/CLAUDE.md` | Tells Claude the layout, read/write rules, capture-pane recipe |
| `~/Library/LaunchAgents/com.curtisault.claude-logs-archive.plist` | Daily archival job at 03:00 |

## Hook configuration (settings.json excerpt)

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Bash|Edit|Write",
        "hooks": [
          { "type": "command", "command": "$HOME/.claude/hooks/claude-logger.sh PostToolUse" }
        ]
      }
    ],
    "UserPromptSubmit": [
      {
        "hooks": [
          { "type": "command", "command": "$HOME/.claude/hooks/claude-logger.sh UserPromptSubmit" }
        ]
      }
    ]
  }
}
```

## launchd plist

```xml
<?xml version="1.0" encoding="UTF-8"?>
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>com.curtisault.claude-logs-archive</string>
  <key>ProgramArguments</key>
  <array>
    <string>/Users/curtisault/.claude/bin/claude-logs-archive.py</string>
  </array>
  <key>StartCalendarInterval</key>
  <dict>
    <key>Hour</key><integer>3</integer>
    <key>Minute</key><integer>0</integer>
  </dict>
  <key>StandardOutPath</key>
  <string>/Users/curtisault/.claude/logs/archive.out.log</string>
  <key>StandardErrorPath</key>
  <string>/Users/curtisault/.claude/logs/archive.err.log</string>
  <key>RunAtLoad</key><false/>
</dict>
</plist>
```

Load with: `launchctl load ~/Library/LaunchAgents/com.curtisault.claude-logs-archive.plist`

## Common queries

Once events are in SQLite, useful one-liners:

```bash
# All sessions seen
sqlite3 ~/.claude/logs/archive.db "SELECT DISTINCT session FROM events ORDER BY session;"

# Bash commands from a specific session
sqlite3 ~/.claude/logs/archive.db \
  "SELECT ts, target FROM events WHERE session='myproj' AND tool='BASH' ORDER BY ts;"

# Failed commands across all sessions
sqlite3 ~/.claude/logs/archive.db \
  "SELECT ts, session, target FROM events WHERE tool='BASH' AND exit_code != 0 ORDER BY ts DESC LIMIT 50;"

# Most-edited files
sqlite3 ~/.claude/logs/archive.db \
  "SELECT target, COUNT(*) AS n FROM events WHERE tool='EDIT' GROUP BY target ORDER BY n DESC LIMIT 20;"

# Activity by day
sqlite3 ~/.claude/logs/archive.db \
  "SELECT substr(ts,1,10) AS day, tool, COUNT(*) FROM events GROUP BY day, tool ORDER BY day DESC;"
```

## Design decisions

### Why hooks instead of Claude self-logging
Claude Code hooks fire deterministically. Asking Claude to log its own
actions is fragile — easy to forget, easy to be incomplete. The hook runs in
the harness, outside Claude's control.

### Why JSONL flat file as primary store
- `tail -F` works trivially → live `claude-logs` view
- `grep`, `less`, `awk` for ad-hoc analysis without parsing
- Append-only, zero ops, survives crashes
- Trivial to parse for archival

### Why SQLite for archive (not just keep flat files forever)
- Structured queries beat grep at scale ("all failed Bash commands across all
  sessions in the last week")
- One file, zero ops, real SQL
- Disk stays clean — flat files don't accumulate per-project

### Why not qdrant
Qdrant is for **semantic search**. The use cases here are time-series and
structured (when, what tool, exit code, file path). Qdrant adds embedding
cost and complexity without solving a problem we have. If a "find similar
past sessions" use case emerges later, qdrant can be added as a secondary
index built from the SQLite archive.

### Why 24h staleness threshold for the launchd job
A session can stay open for many hours. Archiving a flat file while its
session is still active would race with the hook's appends. 24h is a safe
"this session is dormant" heuristic for the automated path; manual
`claude_logs_archive` flushes everything immediately when I want to.

## Onboarding a new machine

1. Install dependencies: `tmux`, `jq`, `python3` (3.x), `sqlite3`, `fish`,
   `awk`. All standard.
2. Stow the dotfiles so the fish functions land in `~/.config/fish/functions/`.
3. Copy `~/.claude/hooks/claude-logger.sh` and `~/.claude/bin/claude-logs-archive.py`
   to their respective paths; `chmod +x` both.
4. Merge the hook entries into `~/.claude/settings.json`.
5. Append the tmux section to `~/.claude/CLAUDE.md`.
6. Drop the launchd plist into `~/Library/LaunchAgents/` and `launchctl load` it.
7. Reload Claude Code (or open `/hooks`) so the new hooks are picked up.
8. Create a project session with `tmux_mksession <name> [dir]`.

## Future enhancements (not built)

- **Duration tracking** — capture PreToolUse start, compute delta in
  PostToolUse. Currently `duration_ms` column exists but is always NULL.
- **Per-pane CAPTURE detail** — currently logs the `-t` target only; could
  also store byte count or line count of captured output.
- **Vector index over archived sessions** — feed completed-session summaries
  into qdrant for semantic retrieval ("show me past sessions where I
  debugged a flaky test"). Only worth building if the need is real.
- **`claude-logs` filtering** — keystrokes in the tail window to filter by
  tool type or grep target. Currently raw stream.
- **Auto-rename `claudius` window with current task** — Claude could update
  the window name to reflect the task it's on, making the tmux status bar
  more informative across multiple parallel sessions.

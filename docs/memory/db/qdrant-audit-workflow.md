# Qdrant KB Freshness Audit Workflow

A weekly background job that flags KB entries that may have drifted from the current state of the repo they describe. One audit per qdrant collection, scheduled via launchd, executed by headless `claude -p`.

## Why

KB entries are point-in-time snapshots. Without a refresh loop, they rot — a renamed helper or removed flag silently corrupts every future answer. The audit doesn't fix entries (writes are gated on human review), it surfaces drift so the next interactive session can apply corrections via `qdrant-store`.

## Architecture

Four phases per run:

| Phase | Tool | Cost | What it does |
| --- | --- | --- | --- |
| 1 | bash | ~free | `git diff <last-audited-sha>..HEAD` — list changed files, extract changed exports. Early-exit if empty. |
| 2 | one `claude -p` | 1 LLM call | Query qdrant by changed paths, basenames, and symbols. Output JSON of candidate entries. |
| 3 | parallel `claude -p` | N LLM calls | One verifier per candidate, capped at `MAX_PARALLEL`. Each gets the entry content inline — no qdrant call needed. |
| 4 | bash | ~free | Aggregate verifier sections into a report, append `drift.log` if any entry drifted, advance `last-sha` to HEAD. |

**Key invariants:**

- Phase 1 is the cost optimizer. Quiet weeks cost zero — the script exits before any LLM call.
- Phase 2 is the search. The MCP server is similarity-search-only, so we hand it path/basename/symbol queries and dedupe results. We don't try to enumerate the whole collection.
- Phase 3 is the workhorse. Each verifier runs in its own process with a self-contained prompt (entry content embedded). No shared context, no inter-process plumbing — fan-out by `wait -n` against a parallelism bound.
- The whole pipeline is read-only at the qdrant level. Verifiers cannot call `qdrant-store`. Corrections happen only when a human reads the report and applies them in an interactive session.

## File layout

```
~/.claude/kb-audit/
  bin/
    run-audit.sh                          # orchestrator, takes <collection-name>
  collections/
    <collection-name>/
      config.env                          # COLLECTION_NAME, REPO_PATH, MCP_FIND_TOOL, MAX_PARALLEL
      prompts/
        gather-candidates.md              # Phase 2 prompt template
        verify-entry.md                   # Phase 3 prompt template (one per candidate)
      state/
        last-sha                          # SHA of the last successful audit's HEAD
      reports/
        audit-YYYY-MM-DD.md               # one report per run
      logs/
        run-YYYY-MM-DD.log                # phase log
        launchd-stdout.log                # launchd stdout (mostly empty)
        launchd-stderr.log                # launchd stderr
      drift.log                           # one line appended per run that found drift

~/Library/LaunchAgents/
  com.curtisault.claude.kb-audit-<collection-name>.plist
```

Templates use `__PLACEHOLDER__` markers (not `${}` or `{{}}`) substituted by an inline Python heredoc. Keeps the substitution boring — no shell-quoting hazards from entry content.

## Allowed tools (read-only)

Both phases use a narrow `--allowedTools` allowlist. No `Write`, no `Edit`, no `qdrant-store`, no broad `Bash`.

```
Read,Grep,Glob,
Bash(rg:*),Bash(ls:*),Bash(find:*),Bash(date:*),Bash(cat:*),Bash(head:*),
mcp__kb-<collection>__qdrant-find          # only for Phase 2
```

The Phase 3 allowlist drops the qdrant tool — verifiers don't need it because the entry content is in their prompt.

## Reading the output

After a run, three files matter:

1. `collections/<name>/reports/audit-<date>.md` — full per-entry verdict.
2. `collections/<name>/drift.log` — one-liner per drifted-week. The next interactive Claude session should grep this and surface findings.
3. `collections/<name>/logs/run-<date>.log` — phase-by-phase trace. Useful when a run errors out.

A clean week looks like:

```
**Status:** CLEAN
**Entries reviewed:** 0
**Drift detected:** 0
```

A drifted week's report contains a `## Drift summary` block with the offending entry titles and recommended `qdrant-store` updates.

## Operational commands

### Bootstrap a collection's launchd job (first time)

```bash
launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.curtisault.claude.kb-audit-<name>.plist
```

`bootstrap` is a one-time registration. It survives reboots until you `bootout`.

### Manual run (smoke test)

```bash
launchctl kickstart -p gui/$(id -u)/com.curtisault.claude.kb-audit-<name>
```

Or invoke the script directly (faster, no launchd indirection):

```bash
~/.claude/kb-audit/bin/run-audit.sh <collection-name>
```

### Stop scheduling

```bash
launchctl bootout gui/$(id -u)/com.curtisault.claude.kb-audit-<name>
```

The plist file is left in place. `bootstrap` again to re-arm.

### Force a full re-audit (ignore last-sha)

```bash
rm ~/.claude/kb-audit/collections/<name>/state/last-sha
```

Next run treats the last 30 days of git history as the diff window.

### Inspect the most recent drift findings

```bash
tail -20 ~/.claude/kb-audit/collections/<name>/drift.log
```

## Adding a new collection

Three files plus one plist. Example: `kb-global`.

1. **Create the collection directory** with config and prompts:

   ```bash
   mkdir -p ~/.claude/kb-audit/collections/global/{prompts,state,reports,logs}
   ```

2. **Write `config.env`** — set the four variables:

   ```bash
   COLLECTION_NAME="global"
   REPO_PATH="/path/to/repo"          # the repo whose git history drives Phase 1
   MCP_FIND_TOOL="mcp__kb-global__qdrant-find"
   MAX_PARALLEL=4
   ```

3. **Copy and adapt the prompts** from an existing collection:

   ```bash
   cp ~/.claude/kb-audit/collections/playwright-playbooks/prompts/*.md \
      ~/.claude/kb-audit/collections/global/prompts/
   ```

   The prompts are repo-agnostic except for the placeholder values (substituted at runtime), so usually no edits are needed.

4. **Create the plist** at `~/Library/LaunchAgents/com.curtisault.claude.kb-audit-global.plist`. Copy the playwright one and change:
   - `Label` → `com.curtisault.claude.kb-audit-global`
   - `ProgramArguments[2]` → `global` (the collection name argument)
   - `StandardOutPath` / `StandardErrorPath` → point at the new collection's logs/
   - `WorkingDirectory` → the new repo path
   - `StartCalendarInterval.Minute` → **stagger** by ~10 min from other collections (e.g. `13` instead of `3`) so MCP servers aren't competing

5. **Bootstrap and smoke-test:**

   ```bash
   launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.curtisault.claude.kb-audit-global.plist
   launchctl kickstart -p gui/$(id -u)/com.curtisault.claude.kb-audit-global
   tail -f ~/.claude/kb-audit/collections/global/logs/run-$(date +%Y-%m-%d).log
   ```

## Re-setup from scratch (e.g. on a new machine)

If `~/.claude/kb-audit/` is gone but this doc and the dotfiles remain:

```bash
mkdir -p ~/.claude/kb-audit/{bin,collections}

# 1. Recreate the orchestrator
#    Source of truth: this dotfiles repo or a backup. The script lives at
#    ~/.claude/kb-audit/bin/run-audit.sh and is ~200 lines of bash.
#    Re-derive from this doc's "Architecture" section if no backup exists.

# 2. Recreate each collection (see "Adding a new collection" above)

# 3. Bootstrap each plist
for plist in ~/Library/LaunchAgents/com.curtisault.claude.kb-audit-*.plist; do
  launchctl bootstrap gui/$(id -u) "$plist"
done

# 4. Smoke-test each collection
for cfg in ~/.claude/kb-audit/collections/*/config.env; do
  name=$(basename "$(dirname "$cfg")")
  ~/.claude/kb-audit/bin/run-audit.sh "$name"
done
```

## Troubleshooting

**"phase 2: model output was not valid JSON"**
The verifier copies the raw model output to `logs/gather-out-<date>.txt`. Inspect — usually means the model added prose around the `<candidates>` tags. Tighten the prompt's "emit ONLY this" wording.

**"command not found: claude" in launchd logs**
launchd starts with a minimal `PATH`. The orchestrator exports `/Users/curtisault/.local/bin:/opt/homebrew/bin:...` early; if you moved `claude`, update `CLAUDE_BIN` at the top of `run-audit.sh`.

**Phase 3 verifiers all fail simultaneously**
Likely an MCP-startup race when N processes spin up at once. Drop `MAX_PARALLEL` to 2 in `config.env`. The audit runs serial-ish with no real loss — Phase 1 already culled the candidate set.

**Audit runs but `git diff` returns nothing despite obvious changes**
`last-sha` is ahead of where you think it is (a previous run already advanced it). `cat state/last-sha` to confirm. Delete the file to force the 30-day fallback.

**Permission prompts on `--allowedTools`**
Headless `claude -p` will refuse rather than prompt. If a verifier needs a tool not on the allowlist, it'll mark the entry UNVERIFIABLE. Add the tool to `ALLOWED_TOOLS_VERIFY` in `run-audit.sh` only if it's strictly read-only.

## Schedule

| Collection | Cron | Local time | Plist |
| --- | --- | --- | --- |
| `playwright-playbooks` | `Mon 09:03` | America/Chicago | `com.curtisault.claude.kb-audit-playwright.plist` |

When adding collections, stagger the minute (`:03`, `:13`, `:23`, …) to avoid MCP server contention.

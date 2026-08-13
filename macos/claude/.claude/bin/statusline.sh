#!/usr/bin/env bash
# Claude Code status line.
# Claude Code pipes a JSON blob on stdin describing the current session; we
# print a single line that is rendered at the bottom of the TUI.
#
# Output:  <project> | <branch><dirty> | <model> | <ctx%> | <path>
#
# Wired up via the "statusLine" block in ~/.claude/settings.json.

set -euo pipefail

input="$(cat)"

# --- parse the fields we care about (jq, with safe fallbacks) ----------------
model="$(printf '%s' "$input" | jq -r '.model.display_name // "Claude"')"
ctx_pct="$(printf '%s' "$input" | jq -r '.context_window.used_percentage // empty')"
ctx_pct="${ctx_pct%%.*}"  # used_percentage may be fractional; keep the integer part
cwd="$(printf '%s' "$input" | jq -r '.workspace.current_dir // .cwd // empty')"
[ -z "$cwd" ] && cwd="$PWD"

# --- path: abbreviate $HOME to ~ ---------------------------------------------
path_disp="$cwd"
case "$path_disp" in
    "$HOME"*) path_disp="~${path_disp#"$HOME"}" ;;
esac

# --- project name: basename of the git toplevel, else basename of cwd --------
project="$(git -C "$cwd" rev-parse --show-toplevel 2>/dev/null || true)"
if [ -n "$project" ]; then
    project="$(basename "$project")"
else
    project="$(basename "$cwd")"
fi

# --- git branch + dirty marker -----------------------------------------------
branch="$(git -C "$cwd" symbolic-ref --quiet --short HEAD 2>/dev/null \
    || git -C "$cwd" rev-parse --short HEAD 2>/dev/null || true)"
dirty=""
if [ -n "$branch" ] && ! git -C "$cwd" diff --quiet --ignore-submodules HEAD 2>/dev/null; then
    dirty="*"
fi

# --- colors (ANSI) -----------------------------------------------------------
c_proj=$'\033[1;36m'   # bold cyan
c_git=$'\033[33m'      # yellow
c_model=$'\033[35m'    # magenta
c_path=$'\033[2;37m'   # dim white
c_reset=$'\033[0m'
c_dim=$'\033[2m'
sep="${c_reset} ${c_dim}|${c_reset} "

# context color: green until 70%, yellow until 90%, red after
c_ctx=$'\033[32m'
if [ -n "$ctx_pct" ]; then
    if [ "$ctx_pct" -ge 90 ]; then
        c_ctx=$'\033[31m'
    elif [ "$ctx_pct" -ge 70 ]; then
        c_ctx=$'\033[33m'
    fi
fi

# --- assemble ----------------------------------------------------------------
line="${c_proj} ${project}${c_reset}"
[ -n "$branch" ] && line="${line}${sep}${c_git} ${branch}${dirty}${c_reset}"
line="${line}${sep}${c_model}${model}${c_reset}"
[ -n "$ctx_pct" ] && line="${line}${sep}${c_ctx}${ctx_pct}%${c_reset}"
line="${line}${sep}${c_path}${path_disp}${c_reset}"

printf '%s' "$line"

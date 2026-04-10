#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOCAL_BIN="$HOME/.local/bin"

mkdir -p "$LOCAL_BIN"

ln -sf "$SCRIPT_DIR/.local/bin/tmux-battery" "$LOCAL_BIN/tmux-battery"
echo "Linked tmux-battery → $LOCAL_BIN/tmux-battery"

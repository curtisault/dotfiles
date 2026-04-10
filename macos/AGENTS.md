# AGENTS.md — macOS Dotfiles

## What This Repo Is

The **macOS** half of a two-OS dotfiles repository managed with **GNU Stow**. The Linux configs live in `../linux/`. See the root `AGENTS.md` for cross-OS differences (notably: Linux nvim uses `lazy.nvim`, not `vim.pack`).

A macOS dotfiles repository managed with **GNU Stow**. Each top-level directory (`nvim/`, `fish/`, `tmux/`, etc.) is a stow "package" whose internal structure mirrors `$HOME`. Running `stow <package>` from this directory creates symlinks into `~`.

## Deploying / Stowing

```bash
cd ~/dotfiles/macos

stow fish          # symlinks fish/.config/fish/ → ~/.config/fish/
stow nvim          # symlinks nvim/.config/nvim/ → ~/.config/nvim/
stow -t ~ --restow */   # restow everything at once

stow -D fish       # remove symlinks for a package
```

The stow target is always `~` (default when run from this directory). The directory hierarchy inside each package must exactly mirror where the files live under `$HOME`. Adding a new config file means placing it at `<package>/.config/<tool>/filename` — not at the repo root.

## Directory → Tool Mapping

| Directory   | Symlink target              | Tool               |
|-------------|-----------------------------|--------------------|
| `nvim/`     | `~/.config/nvim/`           | Neovim             |
| `fish/`     | `~/.config/fish/`           | Fish shell         |
| `tmux/`     | `~/.config/tmux/`           | tmux               |
| `ghostty/`  | `~/.config/ghostty/`        | Ghostty terminal   |
| `starship/` | `~/.config/starship.toml`   | Starship prompt    |
| `mise/`     | `~/.config/mise/`           | mise (runtimes)    |
| `atuin/`    | `~/.config/atuin/`          | Atuin shell history|
| `karabiner/`| `~/.config/karabiner/`      | Karabiner-Elements |
| `htop/`     | `~/.config/htop/`           | htop               |
| `cronjobs/` | (docs only, no symlink)     | cron reference     |
| `crush/`    | `~/.config/crush/`          | Crush AI agent     |
| `emacs/`    | (build scripts + init.el, not stowed as a package) | Emacs |

## Neovim Config

### Plugin Manager

Uses **`vim.pack`** — Neovim's built-in package manager (added in Neovim 0.11+). There is no lazy.nvim, packer, or rocks.nvim. Plugins are declared in `lua/curtis/vimpack.lua` as URLs. Update with `<leader>pu` (mapped to `vim.pack.update()`).

### Load Order

```
init.lua
  └─ require("curtis")         ← lua/curtis/init.lua
       ├─ commands, vimpack, remap, set, lualine, completions, mini
       └─ require("agentflow").setup()
  └─ require("agentflow").setup({ log = { level = "debug" } })
```

`after/plugin/` files run after all plugins load: `lsp.lua`, `treesitter.lua`, `oil.lua`, `claudecode.lua`, `fugitive.lua`, `render-markdown.lua`.

### Critical: Local Plugin Dependency

`init.lua` prepends `~/projects/agentflow.nvim` to the runtime path **before** anything else:
```lua
vim.opt.rtp:prepend("~/projects/agentflow.nvim")
```
This path **must exist** on the machine for Neovim to start without errors. It's a local development plugin, not fetched by vim.pack.

### LSP Setup

- `mason.nvim` + `mason-lspconfig.nvim` manage server installation
- `rust_analyzer` is the only `ensure_installed` server; all others auto-configure via the default handler
- `lua_ls` has a custom handler that adds nvim runtime to its workspace library
- Completions use the **built-in `vim.lsp.completion`** (no nvim-cmp/blink.cmp), enabled per-buffer on `LspAttach` with `autotrigger = true`
- Diagnostics float automatically on `CursorHold` (updatetime = 1000ms)

### Treesitter

`after/plugin/treesitter.lua` auto-installs missing parsers on `FileType` events rather than a static `ensure_installed` list.

### Key Bindings (non-obvious)

- `<leader>` = `Space`
- `<leader>ff/fg/fb` → mini.pick (files / live grep / buffers)
- `<leader>dt` → diagnostic picker
- `<leader>ac/af/ar/aC` → Claude Code toggle/focus/resume/continue
- `<leader>ab` → add current buffer to Claude; `<leader>as` (visual) → send selection
- `<leader>aa/ad` → accept/deny Claude diff
- `<leader>gs` → Fugitive Git status
- `<leader>sw/sr/sd` → mini.sessions write/restore/delete
- `tt` → move window to new tab; `th/tj/tk/tl` → tab nav; `tn/td` → new/close tab
- `<leader>pu` → update plugins (vim.pack)
- Window nav: `<C-h/j/k/l>`; resize: `<S-Arrow>`
- LSP (on attach): `<C-]>` definition, `K` hover, `gr` references, `<space>f` format, `[d/]d` diagnostics

### `heirline.lua` Is Dead Code

`lua/curtis/heirline.lua` exists but is **not loaded anywhere** and starts with a comment saying it's not in use. The actual statusline is `lualine.nvim` configured in `lua/curtis/lualine.lua`.

### Mini.nvim Modules in Use

`ai`, `diff`, `sessions`, `basics`, `comment`, `pick`, `hipatterns`

`mini.pick` is the fuzzy finder (replaces telescope). Window is centered at 61.8% of screen size. Navigate with `<C-j>/<C-k>`.

### Colorscheme

Active: `catppuccin-frappe`. Other options (gruvbox, everforest, catppuccin variants) are commented out in `set.lua`. Ghostty terminal is also configured to `Catppuccin Frappe` — keep these in sync.

## Fish Shell

### conf.d Load Order

Files in `conf.d/` load alphabetically before `config.fish` runs interactively. Prefix numbers enforce order: `00-environment.fish` (env vars) → `10-paths.fish` (PATH). Add new conf.d snippets with appropriate numeric prefix.

### Secrets File

`config.fish` sources `~/.secrets.fish` at startup. This file is **not in the repo** (intentionally). It must exist on the machine or a yellow warning prints. Put sensitive env vars (API keys, tokens) there.

### Custom Functions

| Function          | What it does                                           |
|-------------------|--------------------------------------------------------|
| `sf`              | Reload fish config in-place (`source config.fish`)    |
| `explore`         | Open yazi file manager                                 |
| `ghview issue/pr` | View GitHub issue or PR in nvim as markdown            |
| `tmux_mksession`  | Create tmux session with windows: nvim/run/git/claudius/github |
| `git_pretty`      | Pretty git log with graph                              |
| `db_port_forward` | kubectl port-forward to db-proxy namespace (default port 15433→5432) |
| `l/la/lr/lt/ldot/lS` | ls aliases defined as functions in `config.fish`   |
| `dialyze`         | Elixir dialyzer helper                                 |
| `ecto_reset_test` | Elixir Ecto test DB reset                              |
| `lsp_reset`       | Reset LSP clients                                      |

`completions/bao.fish` provides completions for `openbao` (`bao` binary at `/opt/homebrew/bin/bao`).

## tmux

- **Prefix**: `C-Space` (not the default `C-b`)
- **Pane nav**: `prefix + h/j/k/l` (vim-style)
- **Split panes in current dir**: `prefix + C-l` (vertical) / `prefix + C-j` (horizontal)
- **Window switch**: `prefix + C-h/C-l`
- **Reload config**: `prefix + r`
- **Base index**: 1 (windows and panes both start at 1, not 0)
- **Plugin manager**: TPM at `~/.config/tmux/plugins/tpm/tpm` — install with `prefix + I` on first run
- **Active plugins**: tpm-sensible, tmux-resurrect, tmux-continuum, tmux-mem-cpu-load, tmux-sidebar
- **Status bar**: requires `tmux-mem-cpu-load` binary and `~/.local/bin/tmux-battery` script

## Ghostty Terminal

- Font: JetBrainsMono Nerd Font
- Theme: Catppuccin Frappe (keep in sync with nvim colorscheme)
- `super+d` → new split right; `super+z` → close surface
- `shift+enter` sends `\x1b\r` (useful for some terminal apps)
- No window decorations / hidden titlebar

## mise (Runtime Management)

Manages: `elixir`, `erlang`, `node`, `python` (all `latest`). Activated in fish via `mise activate fish | source` in `config.fish`. Config at `mise/.config/mise/config.toml`.

## Karabiner-Elements

Single rule: **Caps Lock → Escape (tap) / Left Control (hold)**. Config at `karabiner/.config/karabiner/karabiner.json`. Automatic backups are stored in `karabiner/.config/karabiner/automatic_backups/` — don't delete these, they're referenced by the app.

## Starship Prompt

Custom symbols for: gleam (⭐), elixir (🔮), ocaml (🐪). Package module disabled. Time always shown. Blank line between prompts. Config at `starship/.config/starship.toml`.

## Adding a New Tool Config

1. Create `<toolname>/.config/<toolname>/` (or wherever the tool expects its config under `$HOME`)
2. Add config files inside that path
3. Run `cd ~/dotfiles/macos && stow <toolname>`

Never place config files directly in the repo root or in `<toolname>/` without the full path structure — stow will symlink the wrong thing.

## Secrets and Sensitive Files

- `~/.secrets.fish` — fish env vars, API keys (not in repo)
- `~/.config/sops/age/keys.txt` — SOPS age private key (`SOPS_AGE_KEY_FILE` points here)
- SSH keys in `~/.ssh/` (see `ssh-rsync-setup.md` for rsync-over-SSH setup notes)
- `pass` + `age` + `sops` + `openbao` are all installed for secrets management

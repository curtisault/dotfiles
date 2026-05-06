# Terminal-First Emacs (with GUI Polish on macOS)

Custom Emacs build + configuration with vim keybindings (Evil), native
compilation, tree-sitter, eglot, a Doom-style dashboard, and Catppuccin
Frappé theming. Targets `emacs -nw` first but gives a clean GUI experience
on macOS too (transparent dark titlebar, maximized frame).

## Building Emacs

### Quick Start

```bash
./build-emacs.sh                  # build & install Emacs 29.4
./build-emacs.sh -v emacs-30      # different release branch
./build-emacs.sh -v master        # bleeding edge
./build-emacs.sh -p ~/.local      # custom install prefix
./build-emacs.sh --no-install     # build without installing
```

### What gets built

- **Native compilation** (`--with-native-compilation=aot`)
- **Terminal + macOS NS** (no X11)
- **GnuTLS, JSON, tree-sitter** support
- **rsvg** for SVG rendering
- Source clones to `~/src/emacs`

Verify native comp:

```bash
emacs --batch --eval '(message "Native comp: %s" (native-comp-available-p))'
```

## Configuration

### Layout

The config is split into focused modules, all loaded from `init.el`:

```
~/.config/emacs/
├── early-init.el      — startup GC tuning, frame appearance (dark, maximized)
├── init.el            — core defaults, package mgr, theme, glue
├── dashboard.el       — Doom-style splash with shortcuts cheatsheet
├── completion.el      — vertico + orderless + consult + marginalia + embark + corfu + cape
├── lsp.el             — eglot config (cross-workspace xref, perf knobs)
├── treesit.el         — tree-sitter grammars, auto-install, font-lock level 4
├── modeline.el        — custom mode-line with Evil-state pill
├── org-config.el      — org mode + evil bindings (~/notes/)
├── keybindings.el     — global keymap, mirrors nvim's remap.lua sections
└── assets/
    └── banner.txt     — text banner shown by the dashboard
```

### Deploy with stow

```bash
cd ~/dotfiles/macos
stow -t ~ emacs           # symlink ~/.config/emacs → this package
stow -t ~ -R emacs        # restow after structural changes
stow -t ~ -D emacs        # unlink
```

`build-emacs.sh`, `uninstall-emacs.sh`, this README, and `.gitignore` stay
at the package root and are excluded via `.stow-local-ignore`.

### First Launch

1. **Start Emacs:** `emacs` (GUI) or `emacs -nw` (terminal).
2. **straight.el** clones every package in the background (~1–3 min).
3. **Tree-sitter grammars** auto-install on first idle after startup
   (compiles all of bash/css/elixir/heex/html/js/json/lua/python/rust/toml/tsx/typescript/yaml).
   Subsequent launches are no-ops.
4. **Nerd Font glyphs** for the dashboard navigator: run
   `M-x nerd-icons-install-fonts` once. Restart for glyphs to render.

The dashboard appears as the startup buffer.

## Performance Tuning Already Applied

- **Deferred GC at startup** in `early-init.el`, restored in `emacs-startup-hook`
- **`gcmh`** for adaptive runtime garbage collection
- **`file-name-handler-alist nil`** during init (TRAMP/archive handlers off)
- **`read-process-output-max`** raised to 4 MiB for eglot transport
- **`eglot-events-buffer-size 0`** + **`eglot-send-changes-idle-time 0.5`**
- **`treesit-font-lock-level 4`** for full syntax detail
- **Native compilation AOT** (compiled at build time, no runtime warmup)

## Quality-of-Life Defaults

- **`vc-follow-symlinks t`** — no prompts when editing stowed dotfiles
- **`use-short-answers t`** — y/n instead of yes/no everywhere
- **`recentf-mode`**, **`savehist-mode`**, **`save-place-mode`** — persistent
  recent files / minibuffer history / cursor position
- **No backup files / lockfiles / autosaves** — git is the source of truth
- **Trailing whitespace stripped on save** — only in `prog-mode` (never in markdown/diffs)
- **Full system clipboard integration** (Emacs's native NS bindings)

## Theme + UI

- **Catppuccin Frappé** color scheme
- **`ns-transparent-titlebar` + `ns-appearance dark`** — macOS titlebar blends with buffer
- **`fullscreen . maximized`** — frame opens filling the screen
- **Custom mode-line**: Evil-state pill, git branch, eglot indicator,
  flymake counts, project-relative filename, line:col + percentage
- **Nerd-icons** in the dashboard navigator + `marginalia` annotations
- **Relative line numbers** in `prog-mode`

## Dashboard

A Doom-style splash on startup:

- ASCII banner (`assets/banner.txt`)
- Title quote, navigator buttons, and rotating Tolkien-flavored footer
- Sections: Recent Files, Projects, Bookmarks, Agenda (today + week), Shortcuts
- A `notes` bookmark is auto-created on first launch (`~/notes/`)

### Single-key shortcuts (active in `*dashboard*`)

| Key | Action |
| --- | --- |
| `p` | Switch project |
| `n` | Open `~/notes/` |
| `a` | Org agenda |
| `r` | Recent files (consult) |
| `m` | Magit status |
| `q` | Close dashboard |
| `g r` | Refresh dashboard |

## Features

### Core
- **Evil Mode** — full vim keybindings + evil-collection
- **straight.el** — package manager
- **which-key** — keybinding hints after 0.5s

### Editing
- **rainbow-delimiters** — colorized matching brackets
- **evil-nerd-commenter** — easy commenting
- **hl-todo** — TODO/FIXME/HACK/IMPORTANT/NOTE highlighted in comments

### Fuzzy / Search
- **vertico**, **orderless**, **consult**, **marginalia**, **embark**

### Development
- **eglot** — built-in LSP (Elixir, Python, Lua, JS/TS, Rust)
- **corfu** + **cape** — in-buffer completion + completion-at-point extensions
- **tree-sitter** — modern syntax highlighting with grammar auto-install
- **magit** — git porcelain
- **project.el** — project management
- **pgmacs** — Postgres client (loaded if `tss-db-config.el` is present)

## Key Bindings

### Vim Basics (Evil)

`h j k l` movement, `w b e` word motion, `0 $` line bounds, `gg G` buffer
bounds, `d y p` delete/yank/paste, `u C-r` undo/redo, `/ ?` search,
`n N` next/prev match, `:` ex commands, `v V C-v` visual modes.

### Leader (`SPC`)

| Key | Action |
| --- | --- |
| **Files** | |
| `SPC ff` | Find file |
| `SPC fg` | Live grep (consult-ripgrep) |
| `SPC fb` | Switch buffer |
| **Diagnostics** | |
| `SPC dt` | `consult-flymake` |
| **Git** | |
| `SPC gs` | Magit status |
| `C-x g` | Magit status (alt) |
| **Project** | |
| `SPC pf` | Project find file |
| `SPC ps` | Project shell |
| `SPC pg` | Project grep |
| **LSP** | |
| `SPC lr` | Rename symbol |
| `SPC la` | Code actions |
| `SPC lf` | Format buffer |
| `SPC ld` | Go to definition |
| `SPC li` | Find references |
| **Windows** | |
| `SPC wl` / `SPC wj` | Vsplit / split |
| `SPC wd` | Close window |
| `C-h/j/k/l` | Navigate windows |
| `S-arrows` | Resize |
| **Tabs** | |
| `tt`/`tn` | New tab |
| `th`/`tl` | First / last |
| `tj`/`tk` | Prev / next |
| `td` | Close |
| **Buffers** | |
| `[` / `]` | Prev / next buffer |
| `C-x C-b` | ibuffer |
| **Database** | |
| `SPC db` | pgmacs |

### Org leader (in `org-mode` buffers)

`SPC ot` time-stamp · `SPC os` schedule · `SPC od` deadline · `SPC oq` tags ·
`SPC oa` archive · `SPC or` refile · `SPC ol` insert link · `SPC oe` export ·
`SPC oc` toggle checkbox · `SPC oi` toggle inline images.

Plus `M-h/j/k/l` for `org-meta{left,down,up,right}` and capital variants
(`M-H/J/K/L`) for the shifted variants.

### Other useful

- `M-;` — comment/uncomment lines
- `C-.` — embark act (context menu)
- `C-;` — embark dwim
- `M-s r` — consult-ripgrep
- `M-s l` — consult-line in current buffer
- `C-h e` — `*Messages*` buffer
- `C-x b *Warnings* RET` — warnings buffer

## Tips

- Press `SPC` and wait 0.5s — which-key shows what's available.
- `M-x` then start typing — every command is searchable; vertico + orderless
  do flexible matching.
- Magit: `s` stage / `u` unstage / `c c` commit / `P p` push / `?` for help.
- LSP autoactivates for hooked modes (see `lsp.el`). Hover for docs,
  `SPC lr` to rename, `SPC ld` to jump to definition.
- Tree-sitter modes are picked automatically via `major-mode-remap-alist`.
  Confirm via `M-x describe-mode` — should report `*-ts-mode`.

## Troubleshooting

**Packages not installing** — check internet, run `M-x straight-pull-all`,
inspect `*straight-process*` buffer.

**LSP not working** — confirm the server binary is on `PATH`
(e.g. `elixir-ls`, `pyright`, `rust-analyzer`). Try `M-x eglot-reconnect`.
Check `*Messages*` for errors.

**No syntax highlighting** — `M-x install-treesit-grammars` (the startup
hook does this on idle, but you can force it). Confirm with
`M-: treesit-font-lock-level RET` (should be 4) and
`M-: (treesit-language-available-p 'rust) RET` (or whichever language).

**Dashboard navigator icons appear as boxes** — run
`M-x nerd-icons-install-fonts RET y` and restart Emacs.

**Slow startup** — run `M-x emacs-init-time` to confirm.
The first launch is slow (straight clones + native-comp); subsequent
launches should be < 2s. If not, check `*Warnings*`.

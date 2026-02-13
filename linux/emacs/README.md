# Terminal Emacs with Native Compilation

**Terminal-only Emacs configuration** with vim keybindings (Evil mode) and native compilation support.

This configuration is designed specifically for terminal use (`emacs -nw`) with no GUI dependencies. Perfect for developers who want Emacs power with vim muscle memory in a terminal environment.

## Building Emacs

### Quick Start

```bash
# Build and install Emacs 29.4 (default)
./build-emacs.sh

# Build a different version
./build-emacs.sh -v emacs-30

# Build from master branch
./build-emacs.sh -v master

# Install to custom location
./build-emacs.sh -p ~/.local

# Build without installing
./build-emacs.sh --no-install
```

### What Gets Built

The build script configures Emacs with:

- **Native compilation** (`--with-native-compilation=aot`) - Compiles Elisp to native code for better performance
- **Terminal-only** - No GUI support (X11 or macOS native)
- **GnuTLS** - For secure package downloads
- **JSON support** - Required by many modern packages
- **Tree-sitter** - Modern syntax highlighting

### Build Location

Source code is cloned to: `~/src/emacs`

### Verifying Native Compilation

```bash
emacs --batch --eval '(message "Native comp: %s" (native-comp-available-p))'
```

## Configuration

### Installing the Configuration

Using GNU Stow from the `linux/` directory (pass `-t ~` so stow targets your home directory):

```bash
cd ~/dotfiles/linux
stow -t ~ emacs
```

This symlinks `.emacs.d/init.el` and `.emacs.d/modules/` into your home directory.

### First Launch Setup

1. **Start Emacs:**
   ```bash
   emacs -nw
   ```

2. **Install Tree-sitter Grammars** (for syntax highlighting):
   ```
   M-x install-treesit-grammars
   ```
   This installs grammars for: Elixir, Lua, Python, JavaScript, TypeScript, JSON, YAML, and more.
   Takes a few minutes to compile. You only need to do this once.

3. **Wait for Packages to Install:**
   On first launch, straight.el will clone and install all packages. This takes a few minutes.
   Native compilation will compile packages in the background.

## Module Overview

The configuration is split into focused modules, each responsible for a specific area. The `init.el` entry point bootstraps `straight.el` and loads each module via `require`.

| Module | Contents |
|---|---|
| `modules/core.el` | Performance, UI basics, defaults, backups, clipboard, project |
| `modules/evil-config.el` | Evil mode, evil-collection, commenting, all navigation/window/tab/buffer keybindings |
| `modules/completion.el` | Vertico, orderless, consult, marginalia, embark, corfu, cape + file/search leader keys |
| `modules/ui.el` | Catppuccin theme, modeline, which-key, rainbow-delimiters, hl-todo |
| `modules/git.el` | Magit + `<leader>gs` |
| `modules/lsp.el` | Eglot + LSP/project leader keys |
| `modules/lang.el` | Tree-sitter, elixir-ts-mode, lua-mode, yaml-mode, rust-mode |
| `modules/org-config.el` | Org mode with all Evil keybindings |

### `modules/core.el` - Core Settings

Foundation layer that every other module depends on. Sets up the base editing experience to feel like a modern editor while keeping Emacs' strengths.

- **Native compilation tuning** - Silences noisy warnings and enables background compilation so packages are compiled to native code without blocking startup.
- **Terminal UI cleanup** - Disables the startup screen, menu bar, and scratch buffer message for a clean terminal experience.
- **Editor defaults** - Spaces over tabs, 4-space indentation, 80-column fill, trailing whitespace removal on save, and final newlines. Matches common nvim defaults.
- **Backup/autosave management** - Redirects backup and autosave files into dedicated directories under `~/.emacs.d/` so they don't litter your project trees.
- **Scroll behavior** - 8-line scroll margin and conservative scrolling to match nvim's `scrolloff=8`.
- **Split behavior** - Prefers right/below splits to match nvim's `splitright` and `splitbelow`.
- **Clipboard integration** - Connects the kill ring to the system clipboard (`unnamedplus` equivalent). Includes macOS-specific `pbcopy`/`pbpaste` integration.
- **Project management** - Configures the built-in `project.el` with extra root markers for Go, Rust, Elixir, and generic `.project` files.

### `modules/evil-config.el` - Evil Mode and Keybindings

The vim emulation layer. This module sets up Evil mode and all global keybindings that aren't specific to a particular package.

- **Evil mode** - Full vim keybinding emulation with `undo-redo` as the undo system, `C-u`/`C-d` scrolling, and visual line respect.
- **Cursor shape** - Sends terminal escape sequences to change cursor shape: bar in insert mode, block in normal/visual, underline in replace mode.
- **Evil Collection** - Provides Evil-compatible keybindings for dozens of built-in Emacs modes (dired, help, info, etc.) so vim keybindings work everywhere.
- **Evil Nerd Commenter** - `M-;` to toggle comments on lines, paragraphs, or regions. Also provides copy-and-comment functionality.
- **Leader key (SPC)** - Set up in both normal and visual mode. Individual modules define their own leader bindings.
- **Window navigation** - `C-h/j/k/l` to move between windows, `Shift+arrows` to resize, `<leader>w` prefix for split/close operations.
- **Tab operations** - `t` prefix for tab management: `tt` new, `td` close, `tj/tk` previous/next.
- **Buffer navigation** - `[` and `]` for previous/next buffer.
- **Visual mode indent** - `<` and `>` keep the visual selection active after indenting, matching nvim behavior.

### `modules/completion.el` - Completion Framework

A modern, composable completion stack that replaces the default Emacs completion with something closer to telescope.nvim in feel.

- **Vertico** - Replaces the default minibuffer completion with a vertical candidate list. `C-j`/`C-k` navigate candidates (vim-style). Cycles through candidates at the end of the list.
- **Orderless** - Allows typing space-separated words in any order to match candidates. For example, typing `init el` matches `init.el`. Enables the fuzzy, forgiving matching style that telescope users expect.
- **Consult** - Enhanced versions of common commands. `consult-ripgrep` replaces grep with live ripgrep results. `consult-buffer` shows recent files, bookmarks, and buffers in one list. `consult-line` searches the current buffer interactively. Many commands are bound under `M-s` and `M-g` prefixes.
- **Marginalia** - Adds helpful annotations next to completion candidates (file sizes, documentation strings, keybinding descriptions).
- **Embark** - A context-menu system for completion candidates. Press `C-.` on any candidate to see available actions (open, rename, delete, grep, etc.). Think of it as right-click for the minibuffer.
- **Embark-Consult** - Bridges Embark and Consult so you can use Embark actions on Consult search results.
- **Corfu** - In-buffer completion popup. Shows completion candidates as you type in code buffers. Works with LSP (Eglot) for intelligent completions. Auto-triggers after 2 characters with a 0.2s delay.
- **Cape** - Extends Corfu with additional completion sources: `dabbrev` (words from open buffers), `file` (file path completion), and `keyword` (language keywords).
- **Leader bindings** - `<leader>ff` find file, `<leader>fg` live grep, `<leader>fb` switch buffer, `<leader>dt` diagnostics.

### `modules/ui.el` - Visual Appearance

Everything related to how Emacs looks, from the color scheme to the mode line.

- **Catppuccin Frappe** - A warm, pastel color theme. The Frappe variant provides good contrast for terminal use without being too dark or too light.
- **Which-key** - Displays available keybinding continuations in a popup after 0.5 seconds of pressing a prefix key. Press `SPC` and wait to see all leader key options. Essential for discoverability.
- **Rainbow Delimiters** - Colors matching brackets/parentheses in programming modes so nested structures are easy to read at a glance.
- **hl-todo** - Highlights keywords in comments: `TODO`, `FIXME`, `HACK`, `IMPORTANT`, `NOTE`. Each gets a distinct color matching the nvim mini.hipatterns configuration.
- **Custom mode line** - A hand-crafted mode line that displays:
  - Evil state with colored background (blue for normal, green for insert, purple for visual, red for replace)
  - Git branch name
  - Buffer modified/read-only indicator
  - File path relative to project root
  - Active LSP server name (when Eglot is connected)
  - Major mode name
  - Line:column position
  - Percentage through file

### `modules/git.el` - Git Integration

Git workflow built around Magit, widely considered the best Git interface in any editor.

- **Magit** - A complete Git porcelain inside Emacs. Stage hunks interactively, write commits, manage branches, rebase, cherry-pick, bisect, and more — all without leaving the editor. Displays in the same window to avoid layout disruption.
- **Leader binding** - `<leader>gs` opens magit-status. Also available via `C-x g`.
- **File dispatch** - `C-c M-g` opens git commands scoped to the current file (log, blame, diff).

### `modules/lsp.el` - Language Server Protocol

LSP support using the built-in Eglot client (Emacs 29+). Lighter and faster than lsp-mode with fewer dependencies.

- **Eglot** - Automatically starts language servers when opening supported file types. Configured for Elixir (Expert), Python, Lua, JavaScript, TypeScript, and Rust.
- **Performance tuning** - Events buffer disabled, non-blocking connection, 0.5s idle time before sending changes. Servers auto-shutdown when the last buffer for that language is closed.
- **Leader bindings** - `<leader>lr` rename, `<leader>la` code actions, `<leader>lf` format, `<leader>ld` go to definition, `<leader>li` find references.
- **Project bindings** - `<leader>pf` find file in project, `<leader>ps` project shell, `<leader>pg` project grep.

### `modules/lang.el` - Language Support and Tree-sitter

Language mode packages and tree-sitter grammar management for modern syntax highlighting.

- **Language packages** - Installs `elixir-ts-mode` (from GitHub), `lua-mode`, `yaml-mode`, and `rust-mode`. These provide file detection and base functionality, while tree-sitter handles highlighting.
- **Tree-sitter grammars** - Declares grammar sources for 18 languages: Bash, CMake, CSS, Elisp, Elixir, HEEx, HTML, JavaScript, JSON, Lua, Make, Markdown, Python, Rust, TOML, TSX, TypeScript, and YAML.
- **Auto-install** - When you open a file that should use a tree-sitter mode but the grammar isn't installed yet, it automatically downloads and compiles the grammar, then offers to reopen the file.
- **Mode remapping** - Automatically upgrades legacy modes to their tree-sitter equivalents (e.g., `python-mode` becomes `python-ts-mode`) for better highlighting and structural editing.
- **Manual install** - `M-x install-treesit-grammars` installs all declared grammars at once.

### `modules/org-config.el` - Org Mode

Org mode configuration with Evil keybindings for note-taking, task management, and document authoring.

- **Display settings** - Clean indented view with hidden leading stars. Opens files with headings folded to content level. Timestamps are logged when marking tasks as done.
- **Evil integration** - Fixes the TAB/S-TAB conflict between Evil and Org so heading cycling works in normal mode. Maps `RET` to follow links, `t`/`T` to toggle/create TODOs, and `,` to set priorities.
- **Structure editing** - `M-h/j/k/l` for promoting, demoting, and moving headings (vim-style directional keys instead of default Emacs arrows). Shift variants (`M-H/J/K/L`) move entire subtrees.
- **Leader shortcuts** - `<leader>o` prefix for Org operations: timestamps, scheduling, deadlines, tags, archiving, refiling, links, export, checkboxes, and inline images.

## Key Bindings

### Vim Basics (Evil Mode)
- `h j k l` - Movement
- `w b e` - Word movement
- `0 $` - Line start/end
- `gg G` - Buffer start/end
- `d y p` - Delete, yank, paste
- `u C-r` - Undo, redo
- `/ ?` - Search forward/backward
- `n N` - Next/previous match
- `:` - Command mode
- `v V C-v` - Visual mode (char, line, block)

### Leader Key (Space)

**Files:**
- `SPC ff` - Find files
- `SPC fg` - Live grep (ripgrep)
- `SPC fb` - Switch buffers

**Git:**
- `SPC gs` - Git status (magit)
- `C-x g` - Git status (alternative)

**Project:**
- `SPC pf` - Project find file
- `SPC ps` - Project shell
- `SPC pg` - Project grep

**LSP:**
- `SPC lr` - Rename symbol
- `SPC la` - Code actions
- `SPC lf` - Format buffer
- `SPC ld` - Go to definition
- `SPC li` - Find references

**Windows:**
- `SPC wl` - Split vertical
- `SPC wj` - Split horizontal
- `SPC wd` - Close window
- `C-h/j/k/l` - Navigate windows

**Tabs:**
- `tt` - New tab
- `th` - First tab
- `tj` - Previous tab
- `tk` - Next tab
- `tl` - Last tab
- `td` - Close tab

### Other Useful Bindings
- `M-;` - Comment/uncomment lines
- `C-x C-b` - Buffer list (ibuffer)
- `M-x` - Execute command
- `C-g` - Cancel/quit
- `C-.` - Embark actions (context menu)

### Consult/Search
- `M-s r` - Ripgrep in project
- `M-s l` - Search lines in buffer
- `C-x b` - Enhanced buffer switcher

## Tips

### Learning Emacs/Evil
- Press `SPC` and wait 0.5s to see available keybindings (which-key)
- Use `M-x` to search for commands by name
- `:help` or `C-h` for help system

### Magit (Git)
- `SPC gs` opens magit-status
- `s` to stage, `u` to unstage
- `c c` to commit
- `P p` to push
- `?` for help in magit buffers

### LSP
- LSP automatically activates for supported languages
- Hover over symbols shows documentation
- Use leader key LSP commands for refactoring

### Tree-sitter
- Automatically used for supported languages
- If a file isn't highlighted, you may need to install its grammar:
  ```
  M-x treesit-install-language-grammar RET <language> RET
  ```

## Troubleshooting

### Packages not installing
- Check internet connection
- Try `M-x straight-pull-all` to update packages
- Check `*straight-process*` buffer for errors

### LSP not working
- Ensure language server is installed (e.g., `elixir-ls` for Elixir)
- Check `:messages` buffer for errors
- Try `M-x eglot-reconnect`

### No syntax highlighting
- Run `M-x install-treesit-grammars` to install all grammars
- Check that you're in a `-ts-mode` (e.g., `python-ts-mode` not `python-mode`)

### Performance issues
- Native compilation runs in background on first launch - this is normal
- Check `*Warnings*` buffer if things seem slow
- Disable modules you don't need by commenting out the `require` line in `init.el`

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

```bash
mkdir -p ~/.emacs.d
cp init.el ~/.emacs.d/init.el
```

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

## Features

This configuration provides a complete development environment matching your nvim setup:

### Core Features
- **Evil Mode** - Full vim keybindings
- **Catppuccin Frappé Theme** - Beautiful color scheme
- **Straight.el** - Modern package manager
- **Relative Line Numbers** - Like vim's relativenumber
- **System Clipboard** - Yank/paste works with system clipboard

### Fuzzy Finding
- **Vertico** - Vertical completion UI
- **Consult** - Enhanced commands (ripgrep, buffer search, file finding)
- **Orderless** - Fuzzy matching
- **Marginalia** - Helpful annotations
- **Embark** - Actions on completion candidates

### Development Tools
- **Eglot** - Built-in LSP client (Elixir, Python, Lua, JS, TS)
- **Corfu** - In-buffer completion
- **Tree-sitter** - Modern syntax highlighting
- **Magit** - Powerful git interface
- **Project.el** - Project management

### UI Enhancements
- **Which-key** - Keybinding hints after 0.5s
- **hl-todo** - Highlight TODO/FIXME/NOTE/HACK in comments
- **Evil-nerd-commenter** - Easy commenting

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
- Disable features you don't need in init.el

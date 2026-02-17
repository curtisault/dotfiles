# macOS Configuration

## Table of Contents

### Dotfile Management
- [stow](#stow)

### Shell & Terminal
- [Fish](#fish)
- [Ghostty](#ghostty)
- [Starship](#starship)
- [Atuin](#atuin)
- [tmux](#tmux)

### Dev Tools
- [Neovim](#neovim)
- [mise](#mise)

### CLI Tools
- [fzf](#fzf)
- [ripgrep](#ripgrep)
- [fd](#fd)
- [yazi](#yazi)
- [ncdu](#ncdu)
- [tealdeer](#tealdeer)

### Security Tools
- [pass](#pass)
- [age](#age)
- [sops](#sops)

### Monitoring
- [bandwhich](#bandwhich)
- [lnav](#lnav)
- [tmux-mem-cpu-load](#tmux-mem-cpu-load)

---

## Dotfile Management

### stow

[GNU Stow](https://www.gnu.org/software/stow/) manages dotfiles via symlinks. Each directory in this repo maps to a package that gets symlinked into `$HOME`.

```bash
brew install stow
```

Stow a package:

```bash
cd ~/dotfiles/macos
stow fish    # symlinks .config/fish/ into ~/.config/fish/
stow tmux    # symlinks .config/tmux/ into ~/.config/tmux/
```

Unstow a package:

```bash
stow -D fish
```

---

## Shell & Terminal

### Fish

[Fish](https://fishshell.com/) is a user-friendly shell with autosuggestions, syntax highlighting, and tab completions out of the box.

```bash
brew install fish
```

Set as default shell:

```bash
echo $(which fish) | sudo tee -a /etc/shells
chsh -s $(which fish)
```

Config: `~/.config/fish/config.fish` (managed via `stow fish`)

### Ghostty

[Ghostty](https://ghostty.org/) is a GPU-accelerated terminal emulator.

```bash
brew install ghostty
```

Config: `~/.config/ghostty/config` (managed via `stow ghostty`)

### Starship

[Starship](https://starship.rs/) is a cross-shell prompt with sensible defaults.

```bash
brew install starship
```

Config: `~/.config/starship.toml` (managed via `stow starship`)

Add to Fish config:

```fish
starship init fish | source
```

### Atuin

[Atuin](https://atuin.sh/) replaces shell history with a searchable, syncable database.

```bash
brew install atuin
```

Config: `~/.config/atuin/config.toml` (managed via `stow atuin`)

Add to Fish config:

```fish
atuin init fish | source
```

Import existing history:

```bash
atuin import auto
```

### tmux

[tmux](https://github.com/tmux/tmux) is a terminal multiplexer for managing persistent sessions and split panes.

```bash
brew install tmux tmux-mem-cpu-load
```

Config: `~/.config/tmux/tmux.conf` (managed via `stow tmux`)

Install plugins via [tpm](https://github.com/tmux-plugins/tpm):

```bash
# Inside tmux, press prefix + I to install plugins
```

#### Saving & Restoring Sessions

tmux-resurrect and tmux-continuum provide session persistence:

**Manual save/restore:**
- Save: `prefix + Ctrl-s`
- Restore: `prefix + Ctrl-r`

**Auto-save/restore:**
```bash
# In tmux.conf
set -g @continuum-save-interval '15'
set -g @continuum-restore 'on'
```

---

## Dev Tools

### Neovim

[Neovim](https://neovim.io/) is a hyperextensible Vim-based text editor.

```bash
brew install neovim
```

Config: `~/.config/nvim/` (managed via `stow nvim`)

### mise

[mise](https://mise.jdx.dev/) manages dev tool versions (node, python, go, etc.).

```bash
brew install mise
```

Config: `~/.config/mise/config.toml` (managed via `stow mise`)

Add to Fish config:

```fish
mise activate fish | source
```

Usage:

```bash
mise use node@20      # Install and use Node.js 20
mise list             # List installed tools
mise upgrade          # Update all tools
```

---

## CLI Tools

### fzf

[fzf](https://github.com/junegunn/fzf) is a command-line fuzzy finder for files, command history, and more.

```bash
brew install fzf
$(brew --prefix)/opt/fzf/install  # Set up shell integration
```

Usage:

```bash
fzf           # Search files
Ctrl+R        # Search command history
Alt+C         # Change directory
```

### ripgrep

[ripgrep](https://github.com/BurntSushi/ripgrep) is an ultra-fast recursive text search tool.

```bash
brew install ripgrep
```

Usage:

```bash
rg "pattern"              # Search current directory
rg "pattern" -t py        # Search only Python files
rg "pattern" -g "*.toml"  # Search by glob
```

### fd

[fd](https://github.com/sharkdp/fd) is a fast and user-friendly alternative to `find`.

```bash
brew install fd
```

Usage:

```bash
fd filename     # Find files by name
fd -e md        # Find files by extension
```

### yazi

[yazi](https://github.com/sxyazi/yazi) is a blazing fast terminal file manager with image preview support.

```bash
brew install yazi
```

Usage:

```bash
yazi                 # Launch file manager
# Navigate with vim keys (h/j/k/l)
```

### ncdu

[ncdu](https://dev.yorhel.nl/ncdu) is an interactive disk usage analyzer with a ncurses interface.

```bash
brew install ncdu
```

Usage:

```bash
ncdu                 # Analyze current directory
ncdu /path/to/dir    # Analyze specific directory
```

### tealdeer

[tealdeer](https://github.com/tealdeer-rs/tealdeer) provides simplified man pages with practical examples.

```bash
brew install tealdeer
```

Usage:

```bash
tldr <command>       # Get simplified help
tldr --update        # Update cache
```

---

## Security Tools

### pass

[pass](https://www.passwordstore.org/) is a Unix password manager using GPG encryption.

```bash
brew install pass gnupg
```

Initialize with your GPG key:

```bash
gpg --gen-key                    # Generate GPG key if needed
pass init "your-gpg-key-id"      # Initialize password store
```

Usage:

```bash
pass insert email/gmail          # Add a password
pass email/gmail                 # Show a password
pass -c email/gmail              # Copy to clipboard
pass generate sites/example 20   # Generate a 20-char password
```

### age

[age](https://github.com/FiloSottile/age) is a modern, simple file encryption tool.

```bash
brew install age
```

Used by SOPS for encrypting secrets.

### sops

[sops](https://github.com/getsops/sops) is an editor for encrypted files (YAML, JSON, ENV, INI).

```bash
brew install sops
```

Usage:

```bash
sops -e -age <age-public-key> file.yaml > file.enc.yaml  # Encrypt
sops file.enc.yaml                                        # Edit
sops -d file.enc.yaml                                     # Decrypt
```

---

## Monitoring

### bandwhich

[bandwhich](https://github.com/imsnif/bandwhich) is a terminal bandwidth monitoring tool.

```bash
brew install bandwhich
```

Usage:

```bash
sudo bandwhich  # Monitor network utilization by process
```

### lnav

[lnav](https://lnav.org/) is an advanced log file viewer with automatic format detection.

```bash
brew install lnav
```

Usage:

```bash
lnav /var/log/*.log      # View log files
lnav -t /var/log/syslog  # View with live updates
```

### tmux-mem-cpu-load

[tmux-mem-cpu-load](https://github.com/thewtex/tmux-mem-cpu-load) displays system monitoring in the tmux status bar.

```bash
brew install tmux-mem-cpu-load
```

Integrates directly into tmux configuration.

---

## Additional Tools

Other useful tools available via Homebrew:

```bash
brew install \
    curl \        # Data transfer tool
    git \         # Version control
    vim \         # Text editor
    vhs           # Terminal GIF recorder
```

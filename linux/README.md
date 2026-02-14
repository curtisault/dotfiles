# Linux Configuration

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
- [lazygit](#lazygit)
- [GitHub CLI](#github-cli)

### CLI Tools
- [ripgrep](#ripgrep)
- [duf](#duf)
- [ncdu](#ncdu)
- [yazi](#yazi)
- [bottom](#bottom)

### System
- [keyd](#keyd)
- [pass](#pass)

---

## Dotfile Management

### stow

[GNU Stow](https://www.gnu.org/software/stow/) manages dotfiles via symlinks. Each directory in this repo maps to a package that gets symlinked into `$HOME`.

```bash
sudo pacman -S stow
```

Stow a package:

```bash
cd ~/dotfiles/linux
stow ghostty    # symlinks .config/ghostty/ into ~/.config/ghostty/
stow tmux       # symlinks .config/tmux/ into ~/.config/tmux/
```

Unstow a package:

```bash
stow -D ghostty
```

---

## Shell & Terminal

### Fish

[Fish](https://fishshell.com/) is a user-friendly shell with autosuggestions, syntax highlighting, and tab completions out of the box.

```bash
sudo pacman -S fish
```

Set as default shell:

```bash
chsh -s $(which fish)
```

Config: `~/.config/fish/config.fish` (managed via `stow fish`)

### Ghostty

[Ghostty](https://ghostty.org/) is a GPU-accelerated terminal emulator.

```bash
sudo pacman -S ghostty
```

Config: `~/.config/ghostty/config` (managed via `stow ghostty`)

### Starship

[Starship](https://starship.rs/) is a cross-shell prompt with sensible defaults.

```bash
sudo pacman -S starship
```

Config: `~/.config/starship.toml` (managed via `stow starship`)

Add to Fish config:

```fish
starship init fish | source
```

### Atuin

[Atuin](https://atuin.sh/) replaces shell history with a searchable, syncable database.

```bash
sudo pacman -S atuin
```

Config: `~/.config/atuin/config.toml` (managed via `stow atuin`)

Add to Fish config:

```fish
atuin init fish | source
```

### tmux

[tmux](https://github.com/tmux/tmux) is a terminal multiplexer.

```bash
sudo pacman -S tmux
```

Config: `~/.config/tmux/tmux.conf` (managed via `stow tmux`)

Install plugins via [tpm](https://github.com/tmux-plugins/tpm):

```bash
# Inside tmux, press prefix + I to install plugins
```

---

## Dev Tools

### Neovim

[Neovim](https://neovim.io/) is a hyperextensible Vim-based text editor.

```bash
sudo pacman -S neovim
```

### mise

[mise](https://mise.jdx.dev/) manages dev tool versions (node, python, go, etc.).

```bash
sudo pacman -S mise
```

Config: `~/.config/mise/config.toml` (managed via `stow mise`)

Add to Fish config:

```fish
mise activate fish | source
```

### lazygit

[lazygit](https://github.com/jesseduffield/lazygit) is a TUI for git.

```bash
sudo pacman -S lazygit
```

### GitHub CLI

[gh](https://cli.github.com/) is GitHub's official CLI for managing repos, PRs, issues, and more.

```bash
sudo pacman -S github-cli
```

Authenticate:

```bash
gh auth login
```

Usage:

```bash
gh repo clone owner/repo    # clone a repo
gh pr create                 # create a pull request
gh pr checkout 123           # check out a PR locally
gh issue list                # list issues
```

---

## CLI Tools

### ripgrep

[ripgrep](https://github.com/BurntSushi/ripgrep) is a fast recursive search tool.

```bash
sudo pacman -S ripgrep
```

Usage:

```bash
rg "pattern"              # search current directory
rg "pattern" -t py        # search only Python files
rg "pattern" -g "*.toml"  # search by glob
```

### duf

[duf](https://github.com/muesli/duf) is a modern replacement for `df` with a readable output.

```bash
sudo pacman -S duf
```

### ncdu

[ncdu](https://dev.yorhel.nl/ncdu) is an interactive disk usage analyzer with a ncurses interface.

```bash
sudo pacman -S ncdu
```

### yazi

[yazi](https://github.com/sxyazi/yazi) is a terminal file manager with image preview support.

```bash
sudo pacman -S yazi
```

### bottom

[bottom](https://github.com/ClementTsang/bottom) is a graphical system monitor for the terminal.

```bash
sudo pacman -S bottom
```

Run with `btm`.

---

## System

### keyd

[keyd](https://github.com/rvaiya/keyd) operates at the kernel level, so it works across all desktop environments, Wayland, X11, and TTYs.

```bash
sudo pacman -S keyd
```

`/etc/keyd/default.conf`:

```ini
[ids]
*

[main]
capslock = overload(control, esc)
```

This makes Caps Lock act as Ctrl when held with another key, and ESC when tapped alone.

```bash
sudo systemctl enable --now keyd
```

To apply config changes without restarting:

```bash
sudo keyd reload
```

### pass

[pass](https://www.passwordstore.org/) is a simple CLI password manager that stores passwords as GPG-encrypted files organized in a directory tree.

```bash
sudo pacman -S pass
```

Initialize the password store with your GPG key:

```bash
pass init <gpg-id>
```

If you don't have a GPG key yet:

```bash
gpg --full-generate-key
```

Usage:

```bash
pass insert email/gmail        # Add a password
pass email/gmail               # Show a password
pass -c email/gmail            # Copy to clipboard
pass generate sites/example 20 # Generate a 20-char password
pass git init                  # Enable git tracking
```

#### Emacs Integration

[pass.el](https://github.com/NicolasPetworthy/pass) provides an Emacs interface:

```elisp
(use-package pass)
```

#### Browser Integration

[browserpass](https://github.com/browserpass/browserpass-extension) provides browser autofill:

```bash
paru -S browserpass-chromium  # or browserpass-firefox
```

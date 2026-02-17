# Dotfiles

My personal development environment configuration files, organized by operating system.

## Quick Start

**Install dependencies via Homebrew:**

```bash
brew install \
    age atuin bandwhich curl \
    fd fish fzf \
    ghostty git \
    lnav mise \
    ncdu neovim \
    pass ripgrep \
    sops starship stow \
    tealdeer tmux tmux-mem-cpu-load \
    vhs vim yazi
```

**Clone and deploy:**

```bash
git clone https://github.com/yourusername/dotfiles.git ~/dotfiles
cd ~/dotfiles

# On macOS
stow -d ~/dotfiles/macos -t ~ --restow */

# On Linux
stow -d ~/dotfiles/linux -t ~ --restow */
```

For detailed tool information, installation guides, and usage examples, see:
- **[macOS](macos/README.md)** - macOS-specific configuration and tools
- **[Linux](linux/README.md)** - Linux-specific configuration and tools

## Installation

1. **Clone this repository:**
   ```bash
   git clone https://github.com/yourusername/dotfiles.git ~/dotfiles
   cd ~/dotfiles
   ```

2. **Install dependencies:**
   ```bash
   brew install git vim neovim tmux fish ghostty fzf ripgrep fd pass bandwich starship tealdeer tmux-mem-cpu-load curl mise age sops lnav yazi ncdu stow atuin vhs
   ```

3. **Set up fzf shell integration:**
   ```bash
   $(brew --prefix)/opt/fzf/install
   ```

4. **Deploy dotfiles with Stow:**
   ```bash
   # On macOS
   stow -d ~/dotfiles/macos -t ~ --restow fish nvim tmux ghostty mise starship atuin

   # Or stow all packages for your OS
   stow -d ~/dotfiles/macos -t ~ --restow */

   # On Linux
   stow -d ~/dotfiles/linux -t ~ --restow */
   ```

5. **Set Fish as default shell** (optional):
   ```bash
   # Add fish to allowed shells
   echo $(which fish) | sudo tee -a /etc/shells

   # Change default shell
   chsh -s $(which fish)
   ```

6. **Activate mise** (add to `~/.config/fish/config.fish`):
   ```bash
   mise activate fish | source
   ```

7. **Set up Atuin** (add to `~/.config/fish/config.fish`):
   ```bash
   atuin init fish | source
   ```

   Then import your existing history:
   ```bash
   atuin import auto
   ```

## Dotfiles Management

This repository uses **GNU Stow** to manage dotfiles through symlinks, organized by operating system.

### Directory Structure

Configurations are split into OS-specific subdirectories (`linux/` and `macos/`). Each contains stow packages that mirror the home directory structure:

```
~/dotfiles/
├── linux/         # Linux-specific configs
│   ├── atuin/
│   ├── fish/
│   ├── ghostty/
│   ├── htop/
│   ├── hypr/
│   ├── ironbar/
│   ├── mise/
│   ├── nvim/
│   ├── starship/
│   └── tmux/
├── macos/         # macOS-specific configs
│   ├── atuin/
│   ├── fish/
│   ├── ghostty/
│   ├── mise/
│   ├── nvim/
│   ├── starship/
│   └── tmux/
├── LICENSE
└── README.md
```

### Stow Usage

Since packages live under OS subdirectories, use the `-d` flag to specify the stow directory:

```bash
# Stow a single package
stow -d ~/dotfiles/linux -t ~ --restow fish

# Stow multiple packages
stow -d ~/dotfiles/linux -t ~ --restow fish nvim tmux ghostty

# Stow all packages for your OS
stow -d ~/dotfiles/linux -t ~ --restow */

# On macOS
stow -d ~/dotfiles/macos -t ~ --restow */
```

### Adding New Configurations

1. Create a package directory under the appropriate OS:
   ```bash
   mkdir -p ~/dotfiles/linux/newapp/.config/newapp
   ```
2. Add your config files maintaining the home directory structure
3. Stow the package:
   ```bash
   stow -d ~/dotfiles/linux -t ~ --restow newapp
   ```

### Updating Configurations

Simply edit files in the dotfiles repository. Changes are immediately reflected since they're symlinked.

### Removing Configurations

```bash
# Remove symlinks for a package
stow -d ~/dotfiles/linux -t ~ -D package-name

# Remove from repo (optional)
rm -rf ~/dotfiles/linux/package-name
```

## Updating

Keep your tools up to date:

```bash
brew upgrade
tldr --update  # Update tealdeer cache
```

## License

MIT

## Contributing

Feel free to submit issues or pull requests if you have suggestions for improvements.

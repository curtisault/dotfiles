# Language Server Management (CachyOS)

Scripts for managing language servers used by Emacs on CachyOS (Arch-based).

## Usage

Refresh all language servers:
```bash
./bin/cachy_os/refresh_language_servers
```

This will install/update:
- **Expert** - Elixir Language Server (from GitHub nightly releases)
- **Lua Language Server** - via pacman (preferred) or GitHub releases
- **Pyright** - Python Language Server via npm (or python-lsp-server via pipx)
- **TypeScript Language Server** - via npm
- **rust-analyzer** - via rustup (preferred) or GitHub releases

## Install Strategy

The script prefers native package management where possible:

1. **pacman** - Used for Lua Language Server (available in Arch repos)
2. **rustup** - Preferred for rust-analyzer (manages it alongside the toolchain)
3. **npm** - Used for Pyright and TypeScript LS
4. **gh (GitHub CLI)** - Fallback for downloading pre-built binaries from GitHub releases

## Requirements

- **pacman** - Required (CachyOS/Arch package manager)
- **gh** (GitHub CLI) - Required for Expert, fallback for other servers
- **npm** (optional) - For Pyright and TypeScript LS
- **pipx** (optional) - Alternative for Python language server
- **rustup** (optional) - Preferred method for rust-analyzer

Install common dependencies:
```bash
sudo pacman -S github-cli npm rustup
```

## Installation Locations

| Server | Location | Install Method |
|--------|----------|----------------|
| Expert | `~/.local/bin/` | gh (GitHub releases) |
| lua-language-server | `/usr/bin/` | pacman |
| rust-analyzer | `~/.cargo/bin/` | rustup |
| pyright | mise-managed node prefix | npm via mise |
| typescript-language-server | mise-managed node prefix | npm via mise |

Ensure `~/.local/bin` and `~/.cargo/bin` are in your PATH.

## Adding More Language Servers

Edit `refresh_language_servers` and add a new function:

```bash
install_your_language_server() {
    log_info "Installing/updating Your Language Server..."

    # Prefer pacman if available
    if pacman -Ss '^your-ls$' &> /dev/null; then
        pacman_install your-ls
        log_success "Your LS installed via pacman"
        return 0
    fi

    # Fallback to gh/curl/npm
    # ...

    log_success "Your Language Server installed"
}
```

Then call it in the `main()` function:
```bash
install_your_language_server || ((failed++))
```

## Configuring Emacs

After installing language servers, configure eglot in your `init.el`:

```elisp
(add-to-list 'eglot-server-programs
             '(your-mode "your-language-server" "--args"))
```

## Troubleshooting

### Language server not found in Emacs

1. Check if it's installed: `which your-language-server`
2. Ensure `~/.local/bin` is in your PATH
3. Restart Emacs after installation

### Script fails to download

- Check internet connection
- Ensure `gh` is authenticated: `gh auth login`
- Check if the release/binary exists in the repo

### pacman package not found

Update your package databases:
```bash
sudo pacman -Syyu
```

### Permission denied

```bash
chmod +x ~/.local/bin/*
```

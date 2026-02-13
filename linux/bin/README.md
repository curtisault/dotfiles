# Language Server Management

Scripts for managing language servers used by Emacs.

## Usage

Update all language servers:
```bash
./bin/update_language_servers
```

This will install/update:
- **Expert** - Elixir Language Server (from GitHub nightly releases)
- **Lua Language Server** - from GitHub releases (LuaLS/lua-language-server)
- **rust-analyzer** - from GitHub releases (or via rustup if available)
- **Pyright** - Python Language Server via npm (or python-lsp-server via pipx)
- **TypeScript Language Server** - via npm

## Requirements

- **gh** (GitHub CLI) - Required for downloading from GitHub releases
- **npm** (optional) - For Pyright and TypeScript LS
- **pipx** (optional) - Alternative for Python language server
- **rustup** (optional) - Preferred method for rust-analyzer

Install missing dependencies:
```bash
brew install gh
```

## Installation Location

All language servers are installed to `~/.local/bin/` (which should be in your PATH).

Some (like Lua LS) are installed via Homebrew and go to brew's bin directory.

## Adding More Language Servers

To add a new language server, edit `update_language_servers` and add a new function:

```bash
install_your_language_server() {
    log_info "Installing/updating Your Language Server..."

    # Download/install logic here
    # Use gh, curl, npm, brew, etc.

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

## Examples

### Installing a language server from GitHub releases

```bash
install_some_ls() {
    local binary_name="some-ls_${PLATFORM}"
    gh release download latest \
        --pattern "$binary_name" \
        --repo owner/repo \
        --clobber
    mv "$binary_name" "${INSTALL_DIR}/some-ls"
    chmod +x "${INSTALL_DIR}/some-ls"
}
```

### Installing via curl

```bash
install_some_ls() {
    local url="https://example.com/some-ls-${PLATFORM}.tar.gz"
    curl -L "$url" | tar xz -C "${INSTALL_DIR}"
    chmod +x "${INSTALL_DIR}/some-ls"
}
```

### Installing via package manager

```bash
install_some_ls() {
    if command -v brew &> /dev/null; then
        brew install some-ls
    elif command -v npm &> /dev/null; then
        npm install -g some-ls
    fi
}
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

### Permission denied

```bash
chmod +x ~/.local/bin/*
```

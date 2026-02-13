# Language Server Management

Scripts for managing language servers used by Emacs. Organized by distribution.

## Distros

- [CachyOS (Arch-based)](cachy_os/README.md)

## General Notes

All scripts install language servers to `~/.local/bin/` (ensure this is in your PATH).

### Supported Language Servers

- **Expert** - Elixir Language Server
- **Lua Language Server** - Lua
- **Pyright** - Python
- **TypeScript Language Server** - TypeScript/JavaScript
- **rust-analyzer** - Rust

### Configuring Emacs

After installing language servers, configure eglot in your `init.el`:

```elisp
(add-to-list 'eglot-server-programs
             '(your-mode "your-language-server" "--args"))
```

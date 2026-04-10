# AGENTS.md — Dotfiles Repository

## Repo Structure

Personal dotfiles split by OS. Each OS directory is the stow root; packages inside mirror `$HOME`.

```
~/dotfiles/
├── macos/          ← stow root for macOS (see macos/AGENTS.md for deep detail)
│   ├── fish/       ← fish/.config/fish/ → ~/.config/fish/
│   ├── nvim/       ← nvim/.config/nvim/ → ~/.config/nvim/
│   ├── tmux/       ← tmux/.config/tmux/ → ~/.config/tmux/
│   ├── ghostty/    ← ghostty/.config/ghostty/ → ~/.config/ghostty/
│   ├── starship/   ← starship/.config/starship.toml → ~/.config/starship.toml
│   ├── mise/       ← mise/.config/mise/ → ~/.config/mise/
│   ├── atuin/      ← atuin/.config/atuin/ → ~/.config/atuin/
│   ├── karabiner/  ← karabiner/.config/karabiner/ → ~/.config/karabiner/
│   ├── htop/       ← htop/.config/htop/ → ~/.config/htop/
│   ├── crush/      ← crush/.config/crush/ → ~/.config/crush/
│   ├── emacs/      ← macOS-only Emacs config + build scripts
│   └── cronjobs/   ← docs only, not stowed
├── linux/          ← stow root for Linux (Arch/CachyOS)
│   ├── fish/
│   ├── nvim/
│   ├── tmux/
│   ├── ghostty/
│   ├── starship/
│   ├── mise/
│   ├── atuin/
│   ├── htop/
│   ├── hypr/       ← Linux-only: Hyprland WM config
│   └── ironbar/    ← Linux-only: ironbar status bar
└── README.md
```

## Deploying with Stow

**Critical**: always pass `-d` when running stow from the repo root. From within the OS subdirectory, `-d` is not needed.

```bash
# From repo root
stow -d ~/dotfiles/macos -t ~ --restow */        # all macOS packages
stow -d ~/dotfiles/linux -t ~ --restow */        # all Linux packages

# From within OS subdirectory (simpler)
cd ~/dotfiles/macos && stow -t ~ --restow */
cd ~/dotfiles/linux && stow -t ~ --restow fish nvim

# Remove symlinks
stow -d ~/dotfiles/macos -t ~ -D fish
```

**Stow gotcha**: the directory structure inside each package must exactly mirror `$HOME`. A file at `nvim/.config/nvim/init.lua` symlinks to `~/.config/nvim/init.lua`. Putting files at the wrong depth creates broken symlinks silently.

## macOS vs Linux: Key Differences

### Neovim Plugin Manager

| macOS | Linux |
|-------|-------|
| `vim.pack` (built-in, Neovim 0.11+) | `lazy.nvim` |
| Plugins declared in `lua/curtis/vimpack.lua` | Plugins declared in `lua/curtis/lazy.lua` |
| Lock file: `nvim-pack-lock.json` | Lock file: `lazy-lock.json` |
| Requires local `~/projects/agentflow.nvim` | No local plugin dependency |
| Completions via `vim.lsp.completion` (no cmp) | Completions via `nvim-cmp` + `cmp-nvim-lsp` |
| No copilot | `copilot.lua` (zbirenbaum) |
| `claudecode.lua` in `after/plugin/` | `claudecode.nvim` declared in lazy.lua |

### Fish Shell

| macOS | Linux |
|-------|-------|
| Sources `~/.secrets.fish` (warn if missing) | No secrets file |
| Custom `fish_greeting` (date + whoami banner) | Sources CachyOS system config at end |
| Rich set of custom functions (see macos/AGENTS.md) | Minimal functions, fewer custom ones |
| `conf.d/` with numbered load order | `conf.d/` with numbered load order |

### Keyboard Remapping

- **macOS**: Karabiner-Elements — config stowed from `karabiner/`. Caps Lock → Esc (tap) / Ctrl (hold).
- **Linux**: `keyd` — config lives at `/etc/keyd/default.conf` (not in this repo, managed separately). Same behavior: `capslock = overload(control, esc)`. Apply changes with `sudo keyd reload`.

### Linux-Only Configs

- **Hyprland** (`hypr/`): Wayland compositor config at `~/.config/hypr/hyprland.conf`
- **ironbar** (`ironbar/`): Wayland status bar config at `~/.config/ironbar/config.toml`
- **Linux distro**: CachyOS (Arch-based). Fish sources `/usr/share/cachyos-fish-config/cachyos-config.fish` at end of `config.fish`. This path is distro-specific and won't exist on other systems.

### macOS-Only Configs

- **Karabiner** (`karabiner/`): keyboard customizer
- **Emacs** (`emacs/`): separate build scripts (`build-emacs.sh`, `uninstall-emacs.sh`) and full init.el config; see `emacs/README.md`
- **Crush** (`crush/`): Charm AI agent config at `~/.config/crush/crush.json`
- **cronjobs/**: documentation only, not stowed

## Adding a New Config

1. Create `<toolname>/.config/<toolname>/` under the appropriate OS directory
2. Place config files maintaining the `$HOME` mirror structure
3. Run `stow <toolname>` from within the OS directory

Never put files directly at `<toolname>/filename` — stow will symlink the directory itself into `~`, not the contents.

## Secrets and Sensitive Files (macOS)

- `~/.secrets.fish` — API keys, tokens (not in repo; fish warns if missing)
- `~/.config/sops/age/keys.txt` — SOPS age private key (`$SOPS_AGE_KEY_FILE`)
- SSH keys in `~/.ssh/` (see `macos/ssh-rsync-setup.md`)

## Detailed macOS Documentation

See **`macos/AGENTS.md`** for comprehensive coverage of:
- Neovim load order, keybindings, LSP setup, plugin details
- Fish functions reference table
- tmux prefix and keybindings
- Ghostty config details
- All tool-specific gotchas

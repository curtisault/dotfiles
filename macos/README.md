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
- [gh](#gh)
- [lazygit](#lazygit)
- [jq](#jq)

### CLI Tools
- [fzf](#fzf)
- [ripgrep](#ripgrep)
- [fd](#fd)
- [yazi](#yazi)
- [ncdu](#ncdu)
- [tlrc](#tlrc)
- [glow](#glow)
- [wget](#wget)
- [xh](#xh)
- [pv](#pv)
- [rsync](#rsync)
- [qsv](#qsv)
- [atac](#atac)
- [taskwarrior-tui](#taskwarrior-tui)

### Database
- [postgresql@17](#postgresql17)
- [pgcli](#pgcli)
- [duckdb](#duckdb)
- [lazysql](#lazysql)
- [pgadmin4](#pgadmin4)

### Containers & Kubernetes
- [podman](#podman)
- [k9s](#k9s)
- [kubernetes-cli](#kubernetes-cli)

### Security Tools
- [pass](#pass)
- [age](#age)
- [sops](#sops)
- [openbao](#openbao)

### Monitoring
- [bandwhich](#bandwhich)
- [lnav](#lnav)
- [tmux-mem-cpu-load](#tmux-mem-cpu-load)
- [bottom](#bottom)
- [htop](#htop)

### Recording
- [asciinema](#asciinema)
- [agg](#agg)

### AI Tools
- [crush](#crush)

### Fonts
- [font-hack-nerd-font](#font-hack-nerd-font)

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

### gh

[gh](https://cli.github.com/) is the official GitHub CLI for managing repos, PRs, issues, and workflows from the terminal.

```bash
brew install gh
```

Usage:

```bash
gh auth login              # Authenticate
gh pr create               # Open a pull request
gh pr list                 # List open PRs
gh issue list              # List issues
gh repo clone owner/repo   # Clone a repo
```

### lazygit

[lazygit](https://github.com/jesseduffield/lazygit) is a TUI for git with interactive staging, rebasing, and branch management.

```bash
brew install lazygit
```

Usage:

```bash
lazygit   # Launch in any git repo
```

### jq

[jq](https://jqlang.org/) is a lightweight command-line JSON processor.

```bash
brew install jq
```

Usage:

```bash
jq '.'                        # Pretty-print JSON
jq '.key'                     # Extract a field
jq '.[] | .name'              # Iterate and extract
curl api/endpoint | jq '.'    # Pipe API output
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

### tlrc

[tlrc](https://github.com/tealdeer-rs/tealdeer) provides simplified man pages with practical examples (Rust implementation of tldr).

```bash
brew install tlrc
```

Usage:

```bash
tldr <command>       # Get simplified help
tldr --update        # Update cache
```

### glow

[glow](https://github.com/charmbracelet/glow) renders Markdown in the terminal with styling.

```bash
brew install glow
```

Usage:

```bash
glow README.md       # Render a file
glow -p README.md    # Render with pager
```

### wget

[wget](https://www.gnu.org/software/wget/) is a non-interactive network downloader.

```bash
brew install wget
```

Usage:

```bash
wget URL                        # Download a file
wget -r -np URL                 # Recursive download
wget -c URL                     # Resume interrupted download
```

### xh

[xh](https://github.com/ducaale/xh) is a friendly and fast HTTP client, similar to HTTPie.

```bash
brew install xh
```

Usage:

```bash
xh get httpbin.org/get          # GET request
xh post httpbin.org/post key=val # POST with JSON body
xh -d URL                       # Download a file
```

### pv

[pv](https://www.ivarch.com/programs/pv.shtml) monitors the progress of data through a pipeline.

```bash
brew install pv
```

Usage:

```bash
pv file.tar.gz | tar xz         # Show progress while extracting
cat large.sql | pv | psql db    # Show progress while importing
```

### rsync

[rsync](https://rsync.samba.org/) is a fast, versatile file copying and synchronization tool.

```bash
brew install rsync
```

Usage:

```bash
rsync -av src/ dest/            # Sync directories
rsync -avz src/ user@host:dest/ # Sync over SSH
rsync -av --delete src/ dest/   # Mirror (delete extras)
```

### qsv

[qsv](https://github.com/jqnatividad/qsv) is a fast CSV data-wrangling toolkit.

```bash
brew install qsv
```

Usage:

```bash
qsv stats file.csv              # Summary statistics
qsv headers file.csv            # Show column names
qsv select col1,col2 file.csv   # Select columns
qsv filter 'col > 100' file.csv # Filter rows
```

### atac

[atac](https://github.com/Julien-cpsn/ATAC) is a TUI API client (Postman-like) for the terminal.

```bash
brew install atac
```

Usage:

```bash
atac   # Launch TUI
```

### taskwarrior-tui

[taskwarrior-tui](https://github.com/kdheepak/taskwarrior-tui) is a TUI for [Taskwarrior](https://taskwarrior.org/).

```bash
brew install taskwarrior-tui
```

Usage:

```bash
taskwarrior-tui   # Launch TUI
```

---

## Database

### postgresql@17

[PostgreSQL](https://www.postgresql.org/) is a powerful open-source relational database.

```bash
brew install postgresql@17
```

Usage:

```bash
brew services start postgresql@17   # Start server
psql -U postgres                    # Connect
createdb mydb                       # Create a database
```

### pgcli

[pgcli](https://www.pgcli.com/) is a Postgres CLI with auto-completion and syntax highlighting.

```bash
brew install pgcli
```

Usage:

```bash
pgcli                    # Connect to default DB
pgcli -U user -d mydb   # Connect to specific DB
```

### duckdb

[DuckDB](https://duckdb.org/) is an in-process analytical database optimized for OLAP workloads.

```bash
brew install duckdb
```

Usage:

```bash
duckdb                          # Interactive shell
duckdb file.db                  # Open or create a DB file
duckdb -c "SELECT * FROM 'file.csv'"  # Query a CSV directly
```

### lazysql

[lazysql](https://github.com/jorgerojas26/lazysql) is a TUI database client supporting multiple databases.

```bash
brew install lazysql
```

Usage:

```bash
lazysql   # Launch TUI, configure connection on first run
```

### pgadmin4

[pgAdmin 4](https://www.pgadmin.org/) is a web-based GUI for PostgreSQL administration.

```bash
brew install --cask pgadmin4
```

Launches a local web UI for managing Postgres databases.

---

## Containers & Kubernetes

### podman

[Podman](https://podman.io/) is a daemonless container engine compatible with Docker CLI.

```bash
brew install podman podman-tui
podman machine init   # Initialize the VM
podman machine start  # Start the VM
```

Usage:

```bash
podman run -it ubuntu bash      # Run a container
podman ps                       # List running containers
podman-tui                      # TUI for managing containers
```

### k9s

[k9s](https://k9scli.io/) is a TUI for managing Kubernetes clusters.

```bash
brew install k9s
```

Usage:

```bash
k9s                  # Launch TUI (uses current kubeconfig context)
```

### kubernetes-cli

[kubectl](https://kubernetes.io/docs/reference/kubectl/) is the Kubernetes command-line tool.

```bash
brew install kubernetes-cli
```

Usage:

```bash
kubectl get pods               # List pods
kubectl apply -f manifest.yaml # Apply a manifest
kubectl logs pod-name          # View logs
kubectl exec -it pod -- bash   # Shell into a pod
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

### openbao

[OpenBao](https://openbao.org/) is an open-source secrets management and encryption tool (fork of HashiCorp Vault).

```bash
brew install openbao
```

Usage:

```bash
bao server -dev              # Start a dev server
bao kv put secret/myapp key=val  # Store a secret
bao kv get secret/myapp          # Retrieve a secret
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

### bottom

[bottom](https://github.com/ClementTsang/bottom) is a cross-platform graphical process/system monitor TUI.

```bash
brew install bottom
```

Usage:

```bash
btm   # Launch TUI
```

### htop

[htop](https://htop.dev/) is an interactive process viewer.

```bash
brew install htop
```

Usage:

```bash
htop   # Launch interactive process viewer
```

---

## Recording

### asciinema

[asciinema](https://asciinema.org/) records terminal sessions that can be shared and replayed in the browser.

```bash
brew install asciinema
```

Usage:

```bash
asciinema rec demo.cast    # Record a session
asciinema play demo.cast   # Play it back
asciinema upload demo.cast # Upload to asciinema.org
```

### agg

[agg](https://github.com/asciinema/agg) converts asciinema recordings to animated GIFs.

```bash
brew install agg
```

Usage:

```bash
agg demo.cast demo.gif           # Convert to GIF
agg --theme monokai demo.cast demo.gif  # With custom theme
```

---

## AI Tools

### crush

[crush](https://github.com/charmbracelet/crush) is Charm's AI coding agent for the terminal.

```bash
brew install charmbracelet/tap/crush
```

Usage:

```bash
crush   # Launch in current project directory
```

---

## Fonts

### font-hack-nerd-font

[Hack Nerd Font](https://www.nerdfonts.com/) is a monospace font patched with icons for use in terminals and editors.

```bash
brew install --cask font-hack-nerd-font
```

Set as the font in Ghostty config:

```
font-family = Hack Nerd Font Mono
```

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

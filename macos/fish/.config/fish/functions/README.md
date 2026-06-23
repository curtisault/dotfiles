# fish functions

Autoloaded [fish](https://fishshell.com/) functions. Each `*.fish` file defines
one function (named after the file) that fish loads on first use. They're
symlinked into `~/.config/fish/functions/` via `stow` — after adding a new file,
run `stow -d ~/dotfiles/macos -t ~ fish` to link it.

Functions prefixed with `__` are private helpers used by other functions, not
meant to be called directly.

## tmux sessions

| Function | Purpose |
|----------|---------|
| `tmux_mksession <name> [dir] [--cli claude\|pi]` | Spin up a **project** session with a standard window layout (`nvim`, `run` split, `git`/lazygit, `github`/`gh dash`, `db`/pgcli, AI CLI). |
| `tmux_mksystem` | Spin up the fixed personal **`system`** session: `scratch` shell, `rss` reader, `tasks` (taskwarrior), `sys` (btm), `disk` (ncdu). No args; rooted at `$HOME`. |

## git

| Function | Purpose |
|----------|---------|
| `git_pretty` | Graph log across all branches with a colorized one-line format. |
| `git_diff_tree` | Show files added/modified vs `main` (`main...HEAD`) as a `tree`. |
| `git_commit_files <hash>` | List the files changed in a specific commit. |
| `git_contributors` | Line-count-by-author breakdown of the repo (via `git blame`), with percentages. |
| `git_save_msg` | Dump my own commits (`Curtis Ault`) across all branches to `curtis_commits.txt`. |

> GitHub PR/issue/CI workflows are handled by the [gh-dash](https://www.gh-dash.dev/)
> extension (and its `enhance` companion), not by shell functions. See the
> `gh` section of the top-level setup README.

## Kubernetes

| Function | Purpose |
|----------|---------|
| `db_port_forward <context> <resource> [port]` | Port-forward a DB pod/service in the `db-proxy` namespace (default local port `15433` → `5432`). |
| `inspect_pod_logs <context> <namespace> <pod>` | Stream a pod's logs into `lnav` for browsing. |
| `__ask_for_kubectl_help` / `__print_kubectl_helpers` | Helpers that print common kubectl commands when a k8s function is misused. |

## Elixir / dev

| Function | Purpose |
|----------|---------|
| `dialyze` | Run the full dialyzer cycle (clean/build/run, short format) and chime when done. |
| `ecto_reset_test` | `mix ecto.reset` against `MIX_ENV=test`. |
| `lsp_reset` | Nuke `.expert/` and `_build/` to reset the language server / build state. |

## system / processes

| Function | Purpose |
|----------|---------|
| `pidinfo <name>` | Show `ps` info for processes matching a name (via `pgrep`). |
| `pidkill <name>` | `kill -9` all processes matching a name. |
| `sf` | Reload `config.fish` and print a confirmation banner ("source fish"). |

## media / misc

| Function | Purpose |
|----------|---------|
| `compress_gif [opts] <input.gif>` | Compress a GIF with ffmpeg (fps, scale, color-count, output). `--help` for options. |
| `explore [path]` | Open the `yazi` file manager. |
| `rss` | Launch the `eilmeldung` TUI RSS reader. |


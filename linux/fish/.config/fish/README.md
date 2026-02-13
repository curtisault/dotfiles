# Fish Shell Configuration Guide

A comprehensive guide to understanding and organizing your Fish shell configuration.

## 📁 Directory Structure

```
~/.config/fish/
├── config.fish              # Main configuration file
├── fish_variables           # Fish-managed variables (auto-generated)
├── conf.d/                  # Config snippets (load automatically)
│   ├── 00-environment.fish  # Environment variables
│   ├── 10-paths.fish        # PATH modifications
│   ├── 20-abbreviations.fish# Command abbreviations
│   └── 99-interactive.fish  # Interactive shell setup
├── functions/               # Function definitions (auto-load)
│   ├── fish_greeting.fish   # Custom greeting
│   ├── pidkill.fish         # Custom functions
│   └── use_env.fish         # Environment switcher
└── completions/             # Tab completions
    └── mycustomtool.fish    # Custom completions
```

---

## 🎯 What Goes Where?

### `config.fish`
Your main configuration file. Keep it minimal and well-organized.

**Best practices:**
- Source environment-specific configs
- Load secrets (with SOPS if sensitive)
- Keep it under 50 lines if possible
- Use `conf.d/` for organization instead

**Example:**
```fish
# Load environment
set ENVIRONMENT "dev"
if test -f ~/.config/fish/env/$ENVIRONMENT.fish
    source ~/.config/fish/env/$ENVIRONMENT.fish
end

# Load encrypted secrets
if test -f ~/.secrets.enc.fish
    sops -d ~/.secrets.enc.fish | source
end
```

---

### `conf.d/` - Configuration Snippets

Config files that load **automatically** before `config.fish`, in alphabetical order.

**Use numbered prefixes to control load order:**
- `00-` First to load (environment variables)
- `10-` Second (PATH modifications)
- `20-` Third (abbreviations, aliases)
- `99-` Last (interactive tools)

#### `00-environment.fish`
```fish
# Global environment variables
set -x EDITOR nvim
set -x VISUAL nvim
set -x PAGER less
set -x LANG en_US.UTF-8
set -x SD_PROJ_DIR $HOME/projects/SandDrive
```

#### `10-paths.fish`
```fish
# PATH modifications
fish_add_path $HOME/bin
fish_add_path $HOME/.local/bin
fish_add_path /opt/homebrew/bin
fish_add_path $HOME/.npm-global/bin
```

#### `20-abbreviations.fish`
```fish
# Abbreviations (expand as you type)
abbr -a g git
abbr -a gs git status
abbr -a gc git commit
abbr -a gp git push
abbr -a k kubectl
abbr -a dc docker-compose
abbr -a tf terraform
```

#### `99-interactive.fish`
```fish
# Only runs in interactive shells
if status is-interactive
    # Prompt
    if command -v starship > /dev/null
        starship init fish | source
    end
    
    # Shell history
    if command -v atuin > /dev/null
        atuin init fish | source
    end
    
    # Directory jumping
    if command -v zoxide > /dev/null
        zoxide init fish | source
    end
    
    # Fuzzy finder
    if command -v fzf > /dev/null
        fzf --fish | source
    end
    
    # Per-directory environments
    if command -v direnv > /dev/null
        direnv hook fish | source
    end
    
    # Custom keybindings
    bind \cf forward-word
    bind \cb backward-word
    
    # Disable greeting or use custom
    set fish_greeting
end
```

---

### `functions/` - Function Definitions

Functions **auto-load** when first called. Each function must be in its own file named `functionname.fish`.

**File naming is critical:**
```fish
pidkill.fish      # Contains: function pidkill
use_env.fish      # Contains: function use_env
fish_greeting.fish # Contains: function fish_greeting
```

#### Example: `functions/pidkill.fish`
```fish
function pidkill
    set pids (pgrep $argv[1] 2>/dev/null)
    test -n "$pids" && kill -9 $pids || echo "No process found: $argv[1]"
end
```

#### Example: `functions/use_env.fish`
```fish
function use_env
    if test (count $argv) -lt 1
        echo "Current environment: $CURRENT_ENV"
        echo "Usage: use_env <dev|staging|prod>"
        return 1
    end

    set -g CURRENT_ENV $argv[1]
    set env_file ~/.config/fish/env/$CURRENT_ENV.fish

    if test -f $env_file
        source $env_file
        echo "Switched to $CURRENT_ENV environment"
    else
        echo "Environment file not found: $env_file"
        return 1
    end
end
```

#### Example: `functions/fish_greeting.fish`
```fish
function fish_greeting
    set hour (date +%H)
    if test $hour -lt 12
        set greeting "Good morning"
    else if test $hour -lt 18
        set greeting "Good afternoon"
    else
        set greeting "Good evening"
    end

    set_color --bold cyan
    echo "════════════════════════════════════════"
    set_color yellow
    echo "🐟 $greeting, "(whoami)"!"
    set_color blue
    echo "📍 "(prompt_pwd)
    set_color cyan
    echo "════════════════════════════════════════"
    set_color normal
end
```

**Saving functions permanently:**
```fish
# After defining a function, save it
funcsave function_name
```

---

### `completions/` - Tab Completions

Custom completions for commands. Files must be named `commandname.fish`.

Usually only needed for:
- Custom scripts/tools
- Commands without built-in completions
- Overriding default completions

#### Example: `completions/git_save_msg.fish`
```fish
# Complete with git branches
complete -c git_save_msg -a "(__fish_git_branches)" -d "Branch"
```

#### Example: `completions/use_env.fish`
```fish
# Complete with available environments
complete -c use_env -a "dev staging prod" -d "Environment"
```

#### Example: `completions/mycli.fish`
```fish
# Basic options
complete -c mycli -s h -l help -d "Show help"
complete -c mycli -s v -l version -d "Show version"

# Options with choices
complete -c mycli -l env -a "dev staging prod" -d "Environment"

# File completion
complete -c mycli -l config -F -d "Config file"

# Subcommands
complete -c mycli -n "__fish_use_subcommand" -a "start stop restart" -d "Commands"
```

---

## 🔧 Common Patterns

### Environment Variable Management

Create separate files for different environments:

**Structure:**
```
~/.config/fish/env/
├── dev.fish
├── staging.fish
└── prod.fish
```

**`env/dev.fish`:**
```fish
set -x DATABASE_URL "postgresql://localhost/myapp_dev"
set -x API_ENDPOINT "http://localhost:4000"
set -x LOG_LEVEL "debug"
```

**Load in `config.fish`:**
```fish
set ENVIRONMENT "dev"
if test -f ~/.config/fish/env/$ENVIRONMENT.fish
    source ~/.config/fish/env/$ENVIRONMENT.fish
end
```

---

### Secret Management with SOPS

**Encrypt secrets:**
```fish
# Create encrypted secrets file
sops ~/.secrets.enc.fish
```

**Add secrets:**
```fish
set -x MY_SECRET_KEY "value"
set -x API_TOKEN "secret-token"
set -x OP_PASSWORD "password"
```

**Load in `config.fish`:**
```fish
if test -f ~/.secrets.enc.fish
    sops -d ~/.secrets.enc.fish | source
else
    set_color yellow
    echo "⚠️  Secrets file not found"
    set_color normal
end
```

---

### Aliases vs Functions

**Use functions instead of aliases in Fish.** Functions are more powerful and idiomatic.

**❌ Aliases (old way):**
```fish
alias e='emacs -nw'
alias ll='ls -la'
```

**✅ Functions (Fish way):**
```fish
function e
    emacs -nw $argv
end

function ll
    ls -la $argv
end
```

**Why functions?**
- Multi-line support
- Better argument handling with `$argv`
- Auto-completion support
- Can be saved with `funcsave`
- Show up in stack traces

---

### Abbreviations vs Aliases

**Abbreviations expand as you type** (like vim abbreviations).

```fish
abbr -a gs git status   # Type 'gs' → expands to 'git status'
abbr -a gc git commit
abbr -a gp git push
```

**Benefits:**
- You see what command will actually run
- Works with command history
- More transparent than aliases

---

## 📝 Key Concepts

### Variables

```fish
# Local variable (function scope)
set var_name "value"

# Global variable (session scope)
set -g var_name "value"

# Universal variable (persists across sessions)
set -U var_name "value"

# Exported variable (environment variable)
set -x var_name "value"

# Erase variable
set -e var_name
```

### Conditionals

```fish
# Test if file exists
if test -f /path/to/file
    echo "File exists"
end

# Test if command exists
if command -v git > /dev/null
    echo "Git is installed"
end

# Multiple conditions
if test $hour -lt 12
    echo "Morning"
else if test $hour -lt 18
    echo "Afternoon"
else
    echo "Evening"
end
```

### Command Substitution

```fish
# Capture command output
set output (command)

# Example
set files (ls *.txt)
set current_dir (pwd)
```

### Loops

```fish
# For loop
for file in *.txt
    echo $file
end

# While loop
while test $count -lt 10
    echo $count
    set count (math $count + 1)
end
```

### Status Codes

```fish
# Check if interactive shell
if status is-interactive
    echo "Interactive shell"
end

# Check if login shell
if status is-login
    echo "Login shell"
end
```

---

## 🎨 Customization Tips

### Colors

```fish
# Set command colors
set -g fish_color_command green
set -g fish_color_param cyan
set -g fish_color_error red --bold
set -g fish_color_quote yellow

# Use colors in output
set_color red
echo "Error message"
set_color normal

set_color --bold green
echo "Success!"
set_color normal
```

### Prompt

**Use Starship (recommended):**
```fish
brew install starship
starship init fish | source
```

**Or customize Fish's built-in prompt:**
```fish
function fish_prompt
    set_color blue
    echo -n (prompt_pwd)
    set_color green
    echo -n ' > '
    set_color normal
end
```

---

## 🚀 Useful Commands

```fish
# List all functions
functions

# View function definition
type function_name

# Edit function
funced function_name

# Save function permanently
funcsave function_name

# Erase function
functions -e function_name

# List all variables
set

# List all abbreviations
abbr -l

# Web-based configuration
fish_config

# Update completions
fish_update_completions

# Reload config
source ~/.config/fish/config.fish

# Or just restart fish
exec fish
```

---

## 🔄 Converting from Zsh/Bash

| Zsh/Bash | Fish | Notes |
|----------|------|-------|
| `export VAR=value` | `set -x VAR value` | Export variable |
| `VAR=value` | `set VAR value` | Local variable |
| `$1, $2, $3` | `$argv[1], $argv[2], $argv[3]` | Function arguments |
| `$@` | `$argv` | All arguments |
| `$(command)` | `(command)` | Command substitution |
| `[ -f file ]` | `test -f file` | Conditionals |
| `if [ ... ]; then` | `if test ...` | If statements |
| `fi` | `end` | End if |
| `source file` | `source file` | Same |
| `alias cmd='...'` | `function cmd; ...; end` | Use functions |
| `.bashrc` / `.zshrc` | `config.fish` | Config file |

---

## 🐛 Troubleshooting

### Functions not loading
```fish
# Check function path
echo $fish_function_path

# Should include: ~/.config/fish/functions

# Verify file naming matches function name
ls ~/.config/fish/functions/

# Check for syntax errors
fish -n ~/.config/fish/functions/function_name.fish
```

### Startup errors
```fish
# Check which file has the error
fish --debug

# Test config syntax
fish -n ~/.config/fish/config.fish
```

### Command not found
```fish
# Check PATH
echo $PATH

# Add to PATH
fish_add_path /new/path

# Or in conf.d/10-paths.fish
```

---

## 📚 Resources

- **Official Documentation:** https://fishshell.com/docs/current/
- **Tutorial:** https://fishshell.com/docs/current/tutorial.html
- **Awesome Fish:** https://github.com/jorgebucaran/awsm.fish
- **Fish Cookbook:** https://github.com/jorgebucaran/cookbook.fish

---

## 💡 Pro Tips

1. **Keep `config.fish` minimal** - use `conf.d/` for organization
2. **Use functions over aliases** - they're more powerful
3. **Number your `conf.d/` files** - control load order
4. **One function per file** - enables auto-loading
5. **Use abbreviations** - they're more transparent than aliases
6. **Check if commands exist** - use `command -v cmd > /dev/null`
7. **Use `$argv` for arguments** - more flexible than `$1, $2`
8. **Save functions with `funcsave`** - makes them permanent
9. **Use `fish_config`** - web-based configuration tool
10. **Test syntax before sourcing** - use `fish -n file.fish`

---

## 🎯 Quick Start Checklist

- [ ] Install Fish: `brew install fish`
- [ ] Set as default shell: `chsh -s /opt/homebrew/bin/fish`
- [ ] Create config directory: `mkdir -p ~/.config/fish/{conf.d,functions,completions}`
- [ ] Install Starship: `brew install starship`
- [ ] Install useful tools: `brew install fzf zoxide atuin`
- [ ] Create your first function in `functions/`
- [ ] Organize config into `conf.d/` files
- [ ] Set up environment files in `env/`
- [ ] Configure SOPS for secrets (optional)
- [ ] Customize greeting in `functions/fish_greeting.fish`

Happy fishing! 🐟

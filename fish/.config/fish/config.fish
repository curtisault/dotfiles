#!/usr/bin/env fish

if test -f $HOME/.secrets.fish
    source $HOME/.secrets.fish
else
    set_color yellow
    echo "⚠️  $HOME/.secrets not found"
    set_color normal
end

if status is-interactive
    # Commands to run in interactive sessions can go here

    if command -v starship > /dev/null
        starship init fish | source
    end

    if command -v mise > /dev/null
        mise activate fish | source
    end

    if command -v atuin > /dev/null
        atuin init fish | source
    end

    fish_greeting
end

function fish_greeting
    clear
    echo ""
    set_color --bold cyan
    echo "╔════════════════════════════════════════════════════╗"

    echo "║ 🐠 Welcome, "(whoami)"!                            ║"

    set_color --bold green
    echo "║    "(date "+%A, %B %d, %Y")"                        ║"

    set_color yellow
    echo "╚════════════════════════════════════════════════════╝"
    echo ""
    set_color normal
end

# Aliases

# size,show type,human readable
function l
    command ls -lFh $argv
end

# long list,show almost all,show type,human readable
function la
    command ls -lAFh $argv
end

# sorted by date,recursive,show type,human readable
function lr
    command ls -tRFh $argv
end

# long list,sorted by date,show type,human readable
function lt
    command ls -ltFh $argv
end

# dot files
function ldot
    command ls -ld .*
end

function lS
    command ls -1FSsh $argv
end

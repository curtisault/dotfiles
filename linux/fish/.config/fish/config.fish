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

# stats
function stats
    command btm
end

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

source /usr/share/cachyos-fish-config/cachyos-config.fish

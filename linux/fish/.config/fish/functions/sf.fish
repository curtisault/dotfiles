function sf
    source $HOME/.config/fish/config.fish

    set_color --bold cyan
    echo "╔════════════════════════════════════════╗"
    echo "║                                        ║"
    set_color --bold green
    echo -n "║  "
    set_color normal
    set_color yellow
    echo -n "🐟 Fish config reloaded successfully"
    set_color --bold green
    echo "  ║"
    echo "║                                        ║"
    echo "╚════════════════════════════════════════╝"
    set_color normal
end

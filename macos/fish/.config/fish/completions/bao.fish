function __complete_bao
    set -lx COMP_LINE (commandline -cp)
    test -z (commandline -ct)
    and set COMP_LINE "$COMP_LINE "
    /opt/homebrew/bin/bao
end
complete -f -c bao -a "(__complete_bao)"

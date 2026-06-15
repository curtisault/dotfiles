# Helper: launch a tool in a tmux pane, or show an install hint if it's missing.
#   $1 target  (session:window[.pane])
#   $2 mode    (run = auto-run with Enter, fill = pre-fill command, no Enter)
#   $3 tool    (binary to check for on PATH)
#   $4 pkg     (package name for the install hint)
#   $5 cmd     (the command line to send)
function __tmux_sysinfo_send
    set -l target $argv[1]
    set -l mode $argv[2]
    set -l tool $argv[3]
    set -l pkg $argv[4]
    set -l cmd $argv[5]

    # If a sudo askpass helper was set up (see tmux_system_info_session) and this
    # command needs sudo, route the password through SUDO_ASKPASS. The password
    # never appears on the command line — only the helper's path does.
    if set -q __tmux_askpass; and test -n "$__tmux_askpass"; and string match -q 'sudo *' -- $cmd
        set cmd (string replace 'sudo ' "SUDO_ASKPASS=$__tmux_askpass sudo -A " -- $cmd)
    end

    if command -v $tool >/dev/null 2>&1
        if test "$mode" = run
            tmux send-keys -t $target $cmd Enter
        else
            # Pre-fill so the user reviews/launches it (sudo, active scan/capture).
            tmux send-keys -t $target $cmd
        end
    else
        tmux send-keys -t $target "echo '⚠ $tool not installed — install with: paru -S $pkg'" Enter
    end
end

function tmux_system_info_session
    set -l session sysinfo
    if test (count $argv) -ge 1
        set session $argv[1]
    end

    set -l dir $HOME
    if test (count $argv) -ge 2
        set dir $argv[2]
    end

    if not test -d $dir
        echo "Directory '$dir' does not exist"
        return 1
    end

    if tmux has-session -t $session 2>/dev/null
        echo "Session '$session' already exists"
        return 0
    end

    # Prompt once for the sudo password (silent, never via argv so it stays out
    # of `ps`/shell history). Leave blank to skip and authenticate per-pane.
    #
    # Instead of piping the password as visible shell text, write it to a
    # short-lived askpass helper in tmpfs (mode 700, RAM-backed, user-only) and
    # point sudo at it via SUDO_ASKPASS. The password is never echoed to any
    # pane — only the helper's path appears on the command line.
    read -s -P "sudo password (blank to skip): " __pw
    echo
    if test -n "$__pw"
        set -l askdir /tmp
        test -d /run/user/(id -u); and set askdir /run/user/(id -u)
        set -g __tmux_pwfile (mktemp $askdir/.tmux-sysinfo-pw.XXXXXX)
        set -g __tmux_askpass (mktemp $askdir/.tmux-sysinfo-askpass.XXXXXX)
        chmod 600 $__tmux_pwfile
        chmod 700 $__tmux_askpass
        # Password stored as raw file data — no shell-escaping pitfalls.
        printf '%s\n' "$__pw" >$__tmux_pwfile
        printf '#!/bin/sh\ncat %s\n' $__tmux_pwfile >$__tmux_askpass
    end
    set -e __pw

    # Auto-detect primary interface, its CIDR, and a ping target.
    set -l iface (ip route show default 2>/dev/null | awk 'NR==1{print $5}')
    test -z "$iface"; and set iface (ip -o link show 2>/dev/null | awk -F': ' '$2!="lo"{print $2; exit}')
    set -l cidr (ip -o -f inet addr show $iface 2>/dev/null | awk 'NR==1{print $4}')
    test -z "$cidr"; and set cidr 192.168.0.0/24
    set -l ping_target 1.1.1.1

    # 1) System resource monitor
    tmux new-session -d -s $session -n sysmon -c $dir
    __tmux_sysinfo_send $session:sysmon run btm bottom btm

    # 2) Disk / device info
    tmux new-window -t $session -n disk -c $dir
    __tmux_sysinfo_send $session:disk run duf duf duf

    # 3) Per-process network bandwidth (needs sudo)
    tmux new-window -t $session -n bandwidth -c $dir
    __tmux_sysinfo_send $session:bandwidth fill bandwhich bandwhich "sudo bandwhich"

    # 4) Packet capture / inspection TUI (needs sudo + capture caps)
    tmux new-window -t $session -n packets -c $dir
    __tmux_sysinfo_send $session:packets fill termshark termshark "sudo termshark -i $iface"

    # 5) Local network mapping — host-discovery sweep, pre-filled
    tmux new-window -t $session -n scan -c $dir
    __tmux_sysinfo_send $session:scan fill nmap nmap "sudo nmap -sn $cidr"

    # 6) Live socket / connection table (needs sudo for process names)
    tmux new-window -t $session -n conns -c $dir
    __tmux_sysinfo_send $session:conns fill ss iproute2 "sudo watch -n 2 ss -tunap"

    # 7) Latency: ping graph (auto) + traceroute TUI (pre-filled) side by side
    tmux new-window -t $session -n trace -c $dir
    __tmux_sysinfo_send $session:trace run gping gping "gping $ping_target"
    tmux split-window -h -t $session:trace -c $dir
    __tmux_sysinfo_send $session:trace.2 fill trip trippy "sudo trip $ping_target"

    # 8) Host security audit (needs sudo)
    tmux new-window -t $session -n audit -c $dir
    __tmux_sysinfo_send $session:audit fill lynis lynis "sudo lynis audit system"

    tmux select-window -t $session:sysmon

    # The askpass/password files must outlive this function — `fill`-mode panes
    # run sudo later, when you launch them. They live in tmpfs (RAM, user-only)
    # and clear on reboot/logout. Remove them yourself when done with the
    # session, e.g.:  rm -f $__tmux_pwfile $__tmux_askpass
    if set -q __tmux_askpass; and test -n "$__tmux_askpass"
        echo "sudo askpass active for this session. Clean up when finished:"
        echo "  rm -f $__tmux_pwfile $__tmux_askpass"
    end
    # Drop the paths from the interactive shell's global scope.
    set -e __tmux_askpass
    set -e __tmux_pwfile
end

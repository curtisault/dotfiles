#!/usr/bin/env bash
# Uninstall Emacs built from source

set -euo pipefail

SOURCE_DIR="${HOME}/src/emacs"
INSTALL_PREFIX="${INSTALL_PREFIX:-/usr/local}"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info()  { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn()  { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

maybe_sudo() {
    if [ -w "$INSTALL_PREFIX" ]; then
        "$@"
    else
        sudo "$@"
    fi
}

remove_ns_artifacts() {
    # Remove the wrapper script created by build-emacs.sh for GUI builds
    local wrapper="$INSTALL_PREFIX/bin/emacs"
    if [ -f "$wrapper" ] && ! [ -L "$wrapper" ]; then
        log_info "Removing wrapper script $wrapper"
        maybe_sudo rm -f "$wrapper"
    elif [ -L "$wrapper" ]; then
        log_info "Removing symlink $wrapper"
        maybe_sudo rm -f "$wrapper"
    fi

    # Remove the .app bundle from /Applications
    local app_installed="/Applications/Emacs.app"
    if [ -d "$app_installed" ]; then
        log_info "Removing $app_installed"
        sudo rm -rf "$app_installed"
    fi

    # Remove the .app bundle from the source tree (may be root-owned from sudo make install)
    local app_bundle="$SOURCE_DIR/nextstep/Emacs.app"
    if [ -d "$app_bundle" ]; then
        log_info "Removing $app_bundle"
        maybe_sudo rm -rf "$app_bundle"
    fi
}

uninstall_via_make() {
    if [ -f "$SOURCE_DIR/Makefile" ]; then
        log_info "Running 'make uninstall' from source directory..."
        cd "$SOURCE_DIR"
        maybe_sudo make uninstall
        remove_ns_artifacts
        return 0
    fi
    return 1
}

uninstall_manually() {
    log_warn "No Makefile found in $SOURCE_DIR — removing installed files manually"

    local prefix="$INSTALL_PREFIX"
    local paths=(
        "$prefix/bin/emacs"
        "$prefix/bin/emacs-"*
        "$prefix/bin/emacsclient"
        "$prefix/bin/etags"
        "$prefix/bin/ctags"
        "$prefix/libexec/emacs"
        "$prefix/share/emacs"
        "$prefix/share/info/emacs"*
        "$prefix/share/info/elisp"*
        "$prefix/share/info/eintr"*
        "$prefix/share/man/man1/emacs"*
        "$prefix/share/man/man1/etags"*
        "$prefix/share/man/man1/ctags"*
        "$prefix/lib/emacs"
    )

    for path in "${paths[@]}"; do
        # glob expansion may yield nothing — skip silently
        for expanded in $path; do
            if [ -e "$expanded" ] || [ -L "$expanded" ]; then
                log_info "Removing $expanded"
                maybe_sudo rm -rf "$expanded"
            fi
        done
    done
}

remove_source() {
    if [ -d "$SOURCE_DIR" ]; then
        log_info "Removing source directory $SOURCE_DIR..."
        rm -rf "$SOURCE_DIR"
        log_info "Source directory removed"
    else
        log_info "Source directory $SOURCE_DIR not found, skipping"
    fi
}

remove_native_comp_cache() {
    local cache_dir="${HOME}/.emacs.d/eln-cache"
    if [ -d "$cache_dir" ]; then
        log_info "Removing native compilation cache at $cache_dir..."
        rm -rf "$cache_dir"
    fi
    # Also check XDG location
    local xdg_cache="${XDG_CACHE_HOME:-${HOME}/.cache}/emacs"
    if [ -d "$xdg_cache" ]; then
        log_info "Removing XDG native comp cache at $xdg_cache..."
        rm -rf "$xdg_cache"
    fi
}

show_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Uninstall Emacs built from source.

OPTIONS:
    -h, --help          Show this help message
    -p, --prefix PATH   Installation prefix to uninstall from (default: $INSTALL_PREFIX)
    --remove-source     Also remove the source directory ($SOURCE_DIR)
    --keep-cache        Don't remove the native compilation cache

EOF
}

main() {
    local REMOVE_SOURCE=false
    local KEEP_CACHE=false

    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_usage
                exit 0
                ;;
            -p|--prefix)
                INSTALL_PREFIX="$2"
                shift 2
                ;;
            --remove-source)
                REMOVE_SOURCE=true
                shift
                ;;
            --keep-cache)
                KEEP_CACHE=true
                shift
                ;;
            *)
                log_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
        esac
    done

    log_info "Uninstalling Emacs from $INSTALL_PREFIX"

    if ! uninstall_via_make; then
        uninstall_manually
        remove_ns_artifacts
    fi

    if [ "$KEEP_CACHE" = false ]; then
        remove_native_comp_cache
    fi

    if [ "$REMOVE_SOURCE" = true ]; then
        remove_source
    fi

    log_info "Done! Emacs has been uninstalled."
    log_info "Your config (~/.emacs.d or ~/.config/emacs) was not touched."
}

main "$@"

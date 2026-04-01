#!/usr/bin/env bash
# Build Emacs with native compilation for terminal use

set -euo pipefail

# Configuration
get_latest_emacs_version() {
    git ls-remote --tags https://git.savannah.gnu.org/git/emacs.git \
        | grep -oE 'emacs-[0-9]+\.[0-9]+$' \
        | sort -V \
        | tail -1
}

EMACS_VERSION="${EMACS_VERSION:-$(get_latest_emacs_version)}"
SOURCE_DIR="${HOME}/src/emacs"
BUILD_JOBS=$(sysctl -n hw.ncpu)
INSTALL_PREFIX="${INSTALL_PREFIX:-/usr/local}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_dependencies() {
    log_info "Checking dependencies..."

    local missing_deps=()

    if ! command -v brew &> /dev/null; then
        log_error "Homebrew is required but not installed"
        exit 1
    fi

    for dep in libgccjit jansson autoconf automake texinfo gnutls pkg-config; do
        if ! brew list "$dep" &> /dev/null; then
            missing_deps+=("$dep")
        fi
    done

    if [ ${#missing_deps[@]} -gt 0 ]; then
        log_warn "Missing dependencies: ${missing_deps[*]}"
        read -p "Install missing dependencies? (y/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            log_info "Installing dependencies..."
            brew install "${missing_deps[@]}"
        else
            log_error "Cannot continue without dependencies"
            exit 1
        fi
    else
        log_info "All dependencies installed"
    fi
}

clone_or_update_source() {
    log_info "Setting up Emacs source..."

    if [ -d "$SOURCE_DIR" ]; then
        log_info "Source directory exists, updating..."
        cd "$SOURCE_DIR"
        git fetch --all
        git checkout "$EMACS_VERSION" || {
            log_warn "Version $EMACS_VERSION not found, fetching tags..."
            git fetch --tags
            git checkout "$EMACS_VERSION"
        }
    else
        log_info "Cloning Emacs repository..."
        mkdir -p "$(dirname "$SOURCE_DIR")"
        git clone --depth 1 --branch "$EMACS_VERSION" \
            https://git.savannah.gnu.org/git/emacs.git "$SOURCE_DIR"
        cd "$SOURCE_DIR"
    fi
}

detect_source_version() {
    local ac_init
    ac_init=$(grep -m1 'AC_INIT' "$SOURCE_DIR/configure.ac" 2>/dev/null || true)
    if [ -n "$ac_init" ]; then
        EMACS_SOURCE_VERSION=$(echo "$ac_init" | sed -E 's/AC_INIT\([^,]+, *([^,)]+).*/\1/' | tr -d ' ')
    else
        EMACS_SOURCE_VERSION="unknown"
    fi
    log_info "Detected Emacs version from source: $EMACS_SOURCE_VERSION"
}

configure_emacs() {
    log_info "Configuring Emacs with native compilation..."

    cd "$SOURCE_DIR"

    # Clean previous builds
    if [ -f Makefile ]; then
        log_info "Cleaning previous build..."
        make distclean || true
    fi

    # Run autogen if needed
    if [ ! -f configure ]; then
        log_info "Running autogen..."
        ./autogen.sh
    fi

    local gui_flags=""
    if [ "$TERMINAL_ONLY" = true ]; then
        log_info "Building terminal-only (no GUI)"
        gui_flags="--without-x --without-ns --without-dbus"
    else
        log_info "Building with GUI support"
        gui_flags="--with-ns"
    fi

    # Configure with native compilation
    # shellcheck disable=SC2086
    ./configure \
        --prefix="$INSTALL_PREFIX" \
        --with-native-compilation=aot \
        $gui_flags \
        --with-gnutls \
        --with-json \
        --with-tree-sitter \
        CFLAGS="-O2 -march=native"

    log_info "Configuration complete"
}

build_emacs() {
    log_info "Building Emacs (this will take a while)..."

    cd "$SOURCE_DIR"

    # Build with all CPU cores
    make -j"$BUILD_JOBS"

    log_info "Build complete!"
}

install_emacs() {
    log_info "Installing Emacs to $INSTALL_PREFIX..."

    cd "$SOURCE_DIR"

    if [ -w "$INSTALL_PREFIX" ]; then
        make install
    else
        log_warn "Need sudo for installation to $INSTALL_PREFIX"
        sudo make install
    fi

    if [ "$TERMINAL_ONLY" = false ]; then
        local app_src="$SOURCE_DIR/nextstep/Emacs.app"
        local app_dest="/Applications/Emacs.app"
        local wrapper="$INSTALL_PREFIX/bin/emacs"

        log_info "Copying $app_src to $app_dest..."
        sudo cp -r "$app_src" "$app_dest"

        log_info "Writing wrapper script $wrapper..."
        sudo tee "$wrapper" > /dev/null << 'EOF'
#!/bin/sh
exec /Applications/Emacs.app/Contents/MacOS/Emacs "$@"
EOF
        sudo chmod +x "$wrapper"
    fi

    log_info "Installation complete!"
}

show_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Build Emacs with native compilation for terminal use.

OPTIONS:
    -h, --help              Show this help message
    -v, --version VERSION   Emacs version/branch to build (default: $EMACS_VERSION)
    -p, --prefix PATH       Installation prefix (default: $INSTALL_PREFIX)
    --no-install           Build only, don't install
    --clean                Clean build directory before building
    --terminal-only        Build without GUI support (no Cocoa/X11)

ENVIRONMENT VARIABLES:
    EMACS_VERSION          Emacs version/branch to build
    INSTALL_PREFIX         Installation prefix

EXAMPLES:
    $0                                    # Build and install latest stable
    $0 -v master                          # Build from master branch
    $0 -p ~/.local                        # Install to ~/.local
    $0 --no-install                       # Build only
    EMACS_VERSION=emacs-30 $0             # Build Emacs 30

EOF
}

main() {
    local NO_INSTALL=false
    local DO_CLEAN=false
    TERMINAL_ONLY=false

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_usage
                exit 0
                ;;
            -v|--version)
                EMACS_VERSION="$2"
                shift 2
                ;;
            -p|--prefix)
                INSTALL_PREFIX="$2"
                shift 2
                ;;
            --no-install)
                NO_INSTALL=true
                shift
                ;;
            --clean)
                DO_CLEAN=true
                shift
                ;;
            --terminal-only)
                TERMINAL_ONLY=true
                shift
                ;;
            *)
                log_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
        esac
    done

    log_info "Checking out Emacs $EMACS_VERSION"
    log_info "Installation prefix: $INSTALL_PREFIX"

    check_dependencies
    clone_or_update_source
    detect_source_version
    log_info "Building Emacs $EMACS_SOURCE_VERSION with native compilation"

    if [ "$DO_CLEAN" = true ]; then
        log_info "Cleaning source directory..."
        cd "$SOURCE_DIR"
        # nextstep/Emacs.app may contain root-owned files from a previous GUI build;
        # remove it with elevated privileges before git clean so it doesn't error
        local ns_app="$SOURCE_DIR/nextstep/Emacs.app"
        if [ -d "$ns_app" ]; then
            log_info "Removing root-owned $ns_app before git clean..."
            if [ -w "$ns_app" ]; then
                rm -rf "$ns_app"
            else
                sudo rm -rf "$ns_app"
            fi
        fi
        git clean -fdx
    fi

    configure_emacs
    build_emacs

    if [ "$NO_INSTALL" = false ]; then
        install_emacs

        log_info "Verifying installation..."
        if command -v emacs &> /dev/null; then
            emacs --version | head -1
            log_info "Native compilation enabled: $(emacs --batch --eval '(message "%s" (if (native-comp-available-p) "YES" "NO"))' 2>&1 | tail -1)"
        fi
    else
        log_info "Build complete. Run 'make install' in $SOURCE_DIR to install."
    fi

    log_info "Done!"
}

main "$@"

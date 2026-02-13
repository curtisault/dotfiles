#!/usr/bin/env bash
# Build Emacs with native compilation for terminal use

set -euo pipefail

# Configuration
EMACS_VERSION="${EMACS_VERSION:-emacs-29.4}"
SOURCE_DIR="${HOME}/src/emacs"
BUILD_JOBS=$(nproc)
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

    if ! command -v pacman &> /dev/null; then
        log_error "pacman is required but not found"
        exit 1
    fi

    local packages=(gcc make libgccjit jansson autoconf automake texinfo gnutls pkgconf tree-sitter ncurses git)

    for dep in "${packages[@]}"; do
        if ! pacman -Qi "$dep" &> /dev/null 2>&1; then
            missing_deps+=("$dep")
        fi
    done

    if [ ${#missing_deps[@]} -gt 0 ]; then
        log_warn "Missing dependencies: ${missing_deps[*]}"
        read -p "Install missing dependencies? (y/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            log_info "Installing dependencies..."
            sudo pacman -S --needed "${missing_deps[@]}"
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
        git pull
    else
        log_info "Cloning Emacs repository..."
        mkdir -p "$(dirname "$SOURCE_DIR")"
        git clone --depth 1 --branch "$EMACS_VERSION" \
            https://git.savannah.gnu.org/git/emacs.git "$SOURCE_DIR"
        cd "$SOURCE_DIR"
    fi
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

    # Configure with native compilation and terminal-only
    ./configure \
        --prefix="$INSTALL_PREFIX" \
        --with-native-compilation=aot \
        --without-x \
        --without-dbus \
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
            *)
                log_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
        esac
    done

    log_info "Building Emacs $EMACS_VERSION with native compilation"
    log_info "Installation prefix: $INSTALL_PREFIX"

    check_dependencies
    clone_or_update_source

    if [ "$DO_CLEAN" = true ]; then
        log_info "Cleaning source directory..."
        cd "$SOURCE_DIR"
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

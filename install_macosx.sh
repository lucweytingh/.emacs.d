#!/bin/sh

RED='\033[0;31m'
GREEN='\033[0;32m'
ORANGE='\033[38;5;214m'
NC="\033[0m" # No Color

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Check if Homebrew is installed
if ! command -v brew &> /dev/null; then
    echo "[${ORANGE}*${NC}] Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    echo "Homebrew installation completed."
fi


# Function to install a package using Homebrew if it is not installed
install_package() {
    local package="$1"

    if ! brew list --formula | grep -q "^${package}\$"; then
        echo "[${RED}*${NC}] ${package} is not installed."
        brew install "$package"
        echo "${package} installation completed."
    else
        echo "[${GREEN}*${NC}] ${package} is installed."
    fi
}

# Install black & ipython
install_package "black"
install_package "ipython"
install_package "virtualenv"

# Installation of straight.el
if [ ! -d "$SCRIPT_DIR/straight/repos/straight.el" ]; then
    echo "Installing straight package manager"
    mkdir -p "$SCRIPT_DIR/straight/repos"
    cd "$SCRIPT_DIR/straight/repos" || exit
    git clone https://github.com/radian-software/straight.el.git
    echo "Repo straight.el has been successfully cloned."
else
    echo "Repo straight.el is already installed."
fi

# Remove BOM from README
sed -i '1s/^\xEF\xBB\xBF//' "$SCRIPT_DIR/README.org"

echo "\n${GREEN}Installation complete${NC}"
echo "${ORANGE}Don't forget to update some variables (e.g. full name, ssh config, dashboard title & notes) in the user specific settings at the top of README.org${NC}"

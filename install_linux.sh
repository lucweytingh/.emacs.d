#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
ORANGE='\033[38;5;214m'
NC="\033[0m" # No Color

function install_package () {
    REQUIRED_PKG=$1
    PKG_OK=$(dpkg-query -W --showformat='${Status}\n' "$REQUIRED_PKG" | grep "install ok installed")
    echo -e "[${GREEN}*${NC}] Checking for $REQUIRED_PKG: $PKG_OK"
    if [ -z "$PKG_OK" ]; then
        echo -e "[${RED}*${NC}] No $REQUIRED_PKG. Setting up $REQUIRED_PKG."
        sudo apt install "$REQUIRED_PKG"
    fi
}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

install_package python3
install_package python3-pip
install_package python3-venv
install_package python3-ipython
install_package black
install_package w3m

# Installation of straight.el
if [ ! -d "$SCRIPT_DIR/straight/repos/straight.el" ]; then
    echo "[${RED}*${NC}] Setting up straight package manager"
    mkdir -p "$SCRIPT_DIR/straight/repos"
    cd "$SCRIPT_DIR/straight/repos" || exit
    git clone https://github.com/radian-software/straight.el.git
    echo -e "Repo straight.el has been successfully cloned."
else
    echo -e "[${GREEN}*${NC}] Checking for straight.el: install ok installed."
fi

# Remove BOM from README
sed -i '1s/^\xEF\xBB\xBF//' "$SCRIPT_DIR/README.org"

echo -e "\n${GREEN}Installation complete${NC}"
echo -e "${ORANGE}Don't forget to update some variables (e.g. full name, ssh config, dashboard title & notes) in the user specific settings at the top of README.org${NC}"

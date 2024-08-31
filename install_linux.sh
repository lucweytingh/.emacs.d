#!/bin/sh

RED='\033[0;31m'
GREEN='\033[0;32m'
ORANGE='\033[38;5;214m'
NC="\033[0m" # No Color

function install_package (){
    REQUIRED_PKG=$1
    PKG_OK=$(dpkg-query -W --showformat='${Status}\n' "$REQUIRED_PKG" | grep "install ok installed")
    echo -e "[${GREEN}*${NC}] Checking for $REQUIRED_PKG: $PKG_OK"
    if [ -z "$PKG_OK" ]; then
        echo -e "[${RED}*${NC}] No $REQUIRED_PKG. Setting up $REQUIRED_PKG."
        sudo apt-get install "$REQUIRED_PKG"
    fi
}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

install_package python3
install_package python3-pip
install_package python3.10-venv

# Install black & ipython
pip install black
pip install ipython

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

echo "Installation complete\n"
echo "Don't forget to update some variables (e.g. full name, ssh config, dashboard title & notes) in the user specific settings at the top of README.org"

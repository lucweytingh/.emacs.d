#!/bin/sh

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Check if Homebrew is installed
if ! command -v brew &> /dev/null; then
    echo "Homebrew not found. Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    echo "Homebrew installation completed."
fi


# Function to install a package using Homebrew if it is not installed
install_package() {
    local package="$1"

    if ! brew list --formula | grep -q "^${package}\$"; then
        echo "Package '${package}' is not installed. Installing..."
        brew install "$package"
        echo "Package '${package}' installation completed."
    else
        echo "Package '${package}' is already installed."
    fi
}

# Install black & ipython
install_package "black"
install_package "ipython"

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

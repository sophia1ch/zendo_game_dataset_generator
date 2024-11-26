#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Function to display messages
echo_msg() {
    echo "=========================================================================================="
    echo "$1"
    echo "=========================================================================================="
}

# Check if conda is installed
if ! command -v conda &> /dev/null
then
    echo_msg "Conda not found. Installing Miniconda..."
    # Download Miniconda installer for Linux or MacOS
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        MINICONDA_URL="https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh"
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        MINICONDA_URL="https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh"
    else
        echo "Unsupported OS type: $OSTYPE"
        exit 1
    fi

    wget $MINICONDA_URL -O ~/miniconda.sh
    bash ~/miniconda.sh -b -p "$HOME/miniconda"
    rm ~/miniconda.sh
    eval "$("$HOME/miniconda/bin/conda" shell.bash hook)"
    conda init
    echo_msg "Miniconda installed successfully. Please restart your terminal and run the script again."
    exit 0
fi

# Initialize conda for the current shell
eval "$(conda shell.bash hook)"

echo_msg "Updating conda..."
conda update -n base -c defaults conda -y

ENV_NAME="ki_praktikum_env"

# Check if the environment already exists
if conda env list | grep -q "^$ENV_NAME\s"; then
    echo_msg "Environment '$ENV_NAME' exists. Updating the environment..."
    conda env update -f environment.yml -n "$ENV_NAME"
else
    echo_msg "Creating the environment '$ENV_NAME' from environment.yml..."
    conda env create -f environment.yml
fi

echo_msg "Environment '$ENV_NAME' is ready."
echo "To activate the environment, run: conda activate $ENV_NAME"

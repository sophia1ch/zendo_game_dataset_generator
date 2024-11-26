# Exit immediately if a command exits with a non-zero status
$ErrorActionPreference = "Stop"

function Echo-Message {
    param (
        [string]$Message
    )
    Write-Host "=========================================================================================="
    Write-Host $Message
    Write-Host "=========================================================================================="
}

# Function to check if a command exists
function Test-Command {
    param (
        [string]$Command
    )
    return Get-Command $Command -ErrorAction SilentlyContinue
}

# Check if conda is installed
if (-not (Test-Command conda)) {
    Echo-Message "Conda not found. Installing Miniconda..."

    # Determine OS architecture
    $Arch = if ([System.Environment]::Is64BitOperatingSystem) { "x86_64" } else { "x86" }

    # Download Miniconda installer for Windows
    $MinicondaURL = "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-$Arch.exe"
    $InstallerPath = "$env:USERPROFILE\Downloads\Miniconda3-latest-Windows-$Arch.exe"

    Invoke-WebRequest -Uri $MinicondaURL -OutFile $InstallerPath

    # Install Miniconda silently
    Start-Process -FilePath $InstallerPath -ArgumentList "/InstallationType=JustMe", "/AddToPath=0", "/RegisterPython=0", "/S", "/D=$env:USERPROFILE\Miniconda3" -Wait

    Remove-Item $InstallerPath

    # Initialize conda for PowerShell
    & "$env:USERPROFILE\Miniconda3\Scripts\conda.exe" init powershell

    Echo-Message "Miniconda installed successfully. Please restart your PowerShell and run the script again."
    exit
}

# Initialize conda for the current PowerShell session
& conda init powershell
. "$env:USERPROFILE\Documents\WindowsPowerShell\profile.ps1"

Echo-Message "Updating conda..."
conda update -n base -c defaults conda -y

$ENV_NAME = "ki_praktikum_env"

# Check if the environment already exists
$envExists = conda env list | Select-String "^\s*$ENV_NAME\s"

if ($envExists) {
    Echo-Message "Environment '$ENV_NAME' exists. Updating the environment..."
    conda env update -f environment.yml -n $ENV_NAME
} else {
    Echo-Message "Creating the environment '$ENV_NAME' from environment.yml..."
    conda env create -f environment.yml
}

Echo-Message "Environment '$ENV_NAME' is ready."
Write-Host "To activate the environment, run: conda activate $ENV_NAME"

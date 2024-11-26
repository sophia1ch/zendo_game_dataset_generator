# Zendo Game Dataset Generator

This project generates a synthetic dataset for the Zendo game using the Blender API to create scenes. 

## Features

- **3D Scene Generation**: Uses Blender to create customizable scenes with various objects, colors, and configurations.
- **Collision Handling**: Ensures no overlapping objects during random positioning in the scene.
- **Object Stacking**: Enables precise stacking of objects with alignment and offset considerations.
- **Rendering Options**: Automatically configures the best rendering settings based on the system's GPU or CPU.
- **Custom Configurations**: Supports user-defined object properties such as shapes, colors, and sizes via JSON configuration files.

## Project Structure

- `blender_objects.py`: Contains the `blender_obj` class for manipulating Blender objects, including position, color, scaling, and rendering settings.
- `render.py`: The main script that orchestrates scene creation, object placement, and rendering.
- `utils.py`: Utility functions for argument parsing and loading object properties from configuration files.
- `requirements.txt`: Dependencies required for the project.
- `setup_env.sh` / `setup_env.ps1`: Scripts to set up the Conda environment automatically.

## Installation Methods

You can set up the project environment using one of the following methods:

### 1. Automatic Environment Setup with Conda

#### Steps
1. Clone this repository:
   ```bash
   git clone https://github.com/CapArrow/ki_praktikum_clevr
   cd ki_praktikum_clevr
   ```
2. Run the setup script based on your operating system:
   - **Linux/MacOS**: Execute `setup_env.sh`:
     ```bash
     ./setup_env.sh
     ```
   - **Windows**: Execute `setup_env.ps1` in PowerShell:
     ```powershell
     ./setup_env.ps1
     ```
3. Follow the script prompts. If Miniconda is not installed, the script will download and install it automatically.

After completion, activate the Conda environment:
```bash
conda activate ki_praktikum_env
```

### 2. Manual Environment Setup with Conda

If you already have Conda installed and prefer manual setup:
1. Clone the repository:
   ```bash
   git clone https://github.com/CapArrow/ki_praktikum_clevr
   cd ki_praktikum_clevr
   ```
2. Create the Conda environment:
   ```bash
   conda env create -f environment.yml
   ```
3. Activate the environment:
   ```bash
   conda activate ki_praktikum_env
   ```

### 3. Using `requirements.txt` with Python 3.11

If you prefer using your Python installation instead of Conda:
1. Ensure you have Python **3.11.x** installed.
2. Install the required packages:
   ```bash
   pip install -r requirements.txt
   ```

   **Note:** Using other Python versions may lead to compatibility issues. Only Python 3.11 is officially supported by these dependencies.

## Project Dependencies

The following are the primary dependencies required for the project:
- `bpy==4.3.0`
- `mathutils==3.3.0`
- `numpy==1.26.4`
- `PyYAML==6.0.2`

These dependencies will be automatically handled by the Conda environment or `pip install`.

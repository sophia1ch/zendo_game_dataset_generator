# Zendo Game Dataset Generator

This project generates a synthetic dataset for the Zendo game using the Blender API to create scenes. 

## Features

- **3D Scene Generation**: Uses Blender to create customizable scenes with various objects, colors, and configurations.
- **Collision Handling**: Ensures no overlapping objects during random positioning in the scene.
- **Object Stacking**: Enables precise stacking of objects with alignment and offset considerations.
- **Rendering Options**: Automatically configures the best rendering settings based on the system's GPU or CPU.
- **Custom Configurations**: Supports user-defined object properties such as shapes, colors, and sizes via JSON configuration files.

## Project Structure

#### 1. Core Scripts for Dataset Generation

- `generate.py`: Handles the generation of structured object placements and relationships based on Prolog rules and other logic-based conditions.
- `rules/rules.py`: Defines rules and templates for generating Prolog queries and structural constraints for dataset generation.
- `rules/rules.pl`: The Prolog script that defines logical constraints for structure generation.
- `rules/zendo_rules.json`: JSON file containing placeholder templates and rules used for procedural dataset generation.

#### 2. Blender Integration and Object Manipulation

- `zendo_objects.py`: Defines object classes (`ZendoObject`, `Pyramid`, `Block`, `Wedge`) and their attributes, transformations, and interactions.
- `structure.py`: Implements functions for placing and positioning objects in 3D space, ensuring valid relationships (e.g., stacking, touching, nesting).
- `render.py`: The main script that orchestrates scene creation, object placement, and rendering.
- `utils.py`: Utility functions for argument parsing and loading object properties from configuration files.

#### 3. Dataset Handling and Machine Learning Integration

- `dataloader.py`: Implements a torch.utils.data.Dataset for loading generated images and their corresponding labels from CSV annotations.

#### 4. Configuration and Setup

- `configs/simple_config.yml`: Specifies rendering and generation parameters for the dataset, such as object count, camera settings, and rule constraints.
- `requirements.txt`: Dependencies required for the project.
- `setup_env.sh` / `setup_env.ps1`: Scripts to set up the Conda environment automatically.

## Installation Methods

> [!IMPORTANT]  
> The SWI-Prolog environment must be setup on your system for the generation to work. Otherwise the scripts won't be able to execute the prolog logic of the project.

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
     .\setup_env.ps1
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

## Usage

After setting up the environment, you can generate scenes using Blender in **headless mode** (without GUI) and store the dataset in the `output/` directory.

### 1. Running with Default Settings

To generate a dataset using the default configuration:
```bash
  blender --background --python generate.py --
```
This will:
- Generate scene structures using Prolog logic (`rules.pl`).
- Place objects based on constraints in `rules/zendo_rules.json`.
- Render the scenes using Blender.
- Store dataset outputs (images & annotations) in the `output/` directory.

### 2. Customize Scene Generation

Modify `configs/simple_config.yml` to adjust:
- Number of rules (`num_rules`)
- Scenes per rule (`num_examples`)
- Invalid scene generation (`generate_invalid_examples`)
- Rendering resolution (`width`, `height`)
- Object properties (`color`, `shape`, `size`)

Then run the generation process:
```bash
  blender --background --python generate.py -- --config-file configs/custom_config.yml
```

## Project Dependencies

The following are the primary dependencies required for the project:
- `bpy==4.3.0`
- `mathutils==3.3.0`
- `numpy==1.26.4`
- `PyYAML==6.0.2`
- `torch`
- `torchvision`
- `matplotlib`
- `pandas`
- `pyswip`

These dependencies will be automatically handled by the Conda environment or `pip install`.

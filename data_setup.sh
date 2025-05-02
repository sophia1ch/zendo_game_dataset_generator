#!/bin/bash

# Create the output directory if it doesn't exist
mkdir -p ../Master_thesis/images

# Find all .png files in subdirectories (excluding existing "images" folder)
find output -type f -name "*.png" | while read -r filepath; do
     filename=$(basename "$filepath")
    
    # Separate filename into name and extension
    name="${filename%.*}"
    ext="${filename##*.}"

    # Construct new filename with _new before the extension
    new_filename="${name}_new.${ext}"
    dest="images/$new_filename"

    cp "$filepath" "$dest"
done

echo "Flattening complete. All images are in the 'images' folder."


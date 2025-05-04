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
    dest="../Master_thesis/images/$new_filename"

    cp "$filepath" "$dest"
done

cp -r output ../Master_thesis/zendo_classification/data

cd ../Master_thesis/zendo_classification
# Convert the ground_truth csv file into the json files
python model/convert_csv2json.py
# Create a metadata file for the dataset
python model/create_dataset.py
# Should now have metadata_new.json and metadata.json
python model/concat_metadata.py
echo "Flattening complete. All images are in the 'images' folder."


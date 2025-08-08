#!/bin/bash
# find_unused_images.sh - A simple bash script to find unused images in a Quarto project
#
# Usage: bash find_unused_images.sh [project_directory]
#
# If no project_directory is provided, the script will use the current directory.

# Get directory to scan or use current directory
DIR="${1:-.}"
echo "Scanning directory: $DIR"

# Create a temporary file to store all content
TEMP_FILE=$(mktemp)
echo "Created temporary file: $TEMP_FILE"

# Find all Quarto files and concatenate their content
echo "Finding and scanning all Quarto files..."
find "$DIR" -name "*.qmd" -type f -exec cat {} \; > "$TEMP_FILE"

# Find all images in the images directory
echo "Finding all images in 'images' directories..."
IMAGE_FILES=$(find "$DIR" -path "*/images/*" -type f -name "*.png" -o -name "*.jpg" -o -name "*.jpeg" -o -name "*.gif" -o -name "*.svg" -o -name "*.webp" -o -name "*.bmp" -o -name "*.tiff")

# Count the number of images found
IMAGE_COUNT=$(echo "$IMAGE_FILES" | wc -l)
echo "Found $IMAGE_COUNT image files"

# Set up storage for unused images
UNUSED_IMAGES=()

# Check each image file to see if it's referenced in any Quarto file
echo "Checking for unused images..."
for IMG in $IMAGE_FILES; do
    IMG_BASENAME=$(basename "$IMG")
    if ! grep -q "$IMG_BASENAME" "$TEMP_FILE"; then
        UNUSED_IMAGES+=("$IMG")
        echo "Unused image: $IMG"
    fi
done

# Report results
UNUSED_COUNT=${#UNUSED_IMAGES[@]}
echo "Found $UNUSED_COUNT unused image files"

# Clean up temporary file
rm "$TEMP_FILE"
echo "Cleaned up temporary file"

# Ask if user wants to move unused images to a backup folder
if [ $UNUSED_COUNT -gt 0 ]; then
    echo ""
    echo "Would you like to move these unused images to a backup folder? (y/n): "
    read -r RESPONSE
    
    if [[ $RESPONSE =~ ^[Yy]$ ]]; then
        BACKUP_DIR="$DIR/unused_images_backup"
        mkdir -p "$BACKUP_DIR"
        
        for IMG in "${UNUSED_IMAGES[@]}"; do
            RELATIVE_PATH=$(realpath --relative-to="$DIR" "$IMG")
            DEST_DIR="$BACKUP_DIR/$(dirname "$RELATIVE_PATH")"
            mkdir -p "$DEST_DIR"
            
            DEST_FILE="$DEST_DIR/$(basename "$IMG")"
            mv "$IMG" "$DEST_FILE"
            echo "Moved: $IMG -> $DEST_FILE"
        done
        
        echo "Unused images have been moved to: $BACKUP_DIR"
    fi
else
    echo "No unused images found. All images in the 'images' directories are referenced in Quarto files."
fi

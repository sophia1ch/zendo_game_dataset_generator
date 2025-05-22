import csv
from PIL import Image, ImageDraw
import matplotlib.pyplot as plt
from pathlib import Path

def draw_bbox_on_image(image_path, bbox, color='red', width=2):
    """
    Draws a single bounding box on the image.

    :param image_path: Path to the image file
    :param bbox: Bounding box as (x_min, y_min, x_max, y_max)
    :param color: Color of the box outline
    :param width: Line width
    :return: Image object with drawn box
    """
    image = Image.open(image_path).convert("RGB")
    draw = ImageDraw.Draw(image)
    draw.rectangle(bbox, outline=color, width=width)
    return image

def visualize_image_with_bbox(image_path, bbox):
    """
    Plots the image with a bounding box using matplotlib.

    :param image_path: Path to the image file
    :param bbox: Bounding box (x_min, y_min, x_max, y_max)
    """
    img_with_box = draw_bbox_on_image(image_path, bbox)
    
    img_with_box.show()  # Display the image with bounding box

# Example usage:
# Provide the path to your image and bounding box values
if __name__ == "__main__":
    # Example hardcoded values — replace with real ones or parse from CSV
    image_path = "../Master_thesis/training7/0/0_6_n.png"  # Replace with actual image path
    im = Image.open(image_path)
    print(im.size)
    bbox = (446,233,543,301)  # Replace with actual bounding box (x_min, y_min, x_max, y_max)

    visualize_image_with_bbox(image_path, bbox)

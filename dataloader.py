import pandas as pd
import torch
from torch.utils.data import Dataset, DataLoader
import matplotlib.pyplot as plt
from torchvision.io import read_image
import numpy as np
from utils import debug


class ZendoImageDataset(Dataset):
    def __init__(self, annotations_file, img_dir="output", transform=None, target_transform=None):
        """
        Initializes the ZendoImageDataset object.

        :param annotations_file: Path to the CSV file containing image annotations.
        :param img_dir: Directory where images are stored (default is "output").
        :param transform: Optional transform to be applied to the images.
        :param target_transform: Optional transform to be applied to the target labels.

        The dataset reads the annotations file and organizes image labels into a structured list.
        Each image may contain multiple objects, and each object has an entry in the annotation file.
        The dataset maintains an index list to group objects belonging to the same image.
        """

        self.img_labels = pd.read_csv(annotations_file)
        self.img_dir = img_dir
        self.transform = transform
        self.target_transform = target_transform

        # Generate a list which saves only the indexes of img_labels which are part of one image
        # as each object in one image has an individual entry in the annotations csv
        self.img_label_list = []
        last_label_name = None  # something like "0_1"

        for label_idx, label in self.img_labels.iterrows():
            label_name = label.get("scene_name")
            if label_name != last_label_name:
                last_label_name = label_name
                # Append a new list to save the label_idx for every object in the new image
                self.img_label_list.append([label_idx])
            else:
                # Otherwise the label is of the current last image in img_label_list
                self.img_label_list[-1].append(label_idx)

    def __len__(self):
        """
        Returns the number of annotated images in the dataset.

        :return: Integer representing the number of unique images in the dataset.
        """

        return len(self.img_label_list)

    def __getitem__(self, idx):
        """
        Retrieves an image and its corresponding labels from the dataset.

        :param idx: Index of the image to retrieve.
        :return: A tuple containing:
            - image: The loaded image tensor.
            - rule_labels: Labels associated with the image, based on predefined rules.
            - obj_labels: A NumPy array containing object-specific labels.

        The function extracts all labels for the given indexed image, differentiating between
        rule-based labels (same for all objects in an image) and object-based labels (specific
        to each object in the image). It also converts object shape labels (pyramid, block, wedge)
        into numerical class indices (0, 1, 2). If transformation functions are provided, they
        are applied to the image and labels before returning the final dataset item.
        """

        # Get all labels for the indexed image (multiple labels as we have multiple objects in each image)
        # Separate rule labels (same for all objects) from object labels (different for every object in image)
        rule_labels = self.img_labels.iloc[self.img_label_list[idx][0]].values[:4]
        obj_labels = []
        img_label_idxs = self.img_label_list[idx]
        for label_idx in img_label_idxs:
            obj_label = self.img_labels.iloc[label_idx].values[4:]
            obj_shape = obj_label[0].split("_")[-1]
            if obj_shape == "Pyramid":
                obj_label[0] = 0
            elif obj_shape == "Block":
                obj_label[0] = 1
            else:
                obj_label[0] = 2
            obj_labels.append(obj_label)

        # Get the image itself (via the first object in the labels list of this image)
        img_path = self.img_labels.iloc[img_label_idxs[0], 1]
        image = read_image(img_path)

        # Transform images and labels if given
        if self.transform:
            image = self.transform(image)
        if self.target_transform:
            for i in range(len(obj_labels)):
                obj_labels[i] = self.target_transform(obj_labels[i])

        return image, rule_labels, np.array(obj_labels)


def custom_collate(batch):
    """
    Custom collate function for handling variable-length object labels in a batch.

    :param batch: A list of tuples containing:
        - image: The image tensor.
        - rule_labels: Labels associated with the image, based on predefined rules.
        - obj_labels: A NumPy array of object-specific labels.
    :return: A tuple containing:
        - images: A stacked tensor of images.
        - batch_rule_labels: A tuple of rule labels for each image.
        - padded_obj_labels: A tensor containing padded object labels for each image.

    The function processes batches by stacking images and padding object labels to the maximum
    length in the batch. Object labels are converted to tensors, and any missing labels are
    padded with -1 to ensure uniform batch sizes.
    """

    # Unzip the batch into images and labels
    images, batch_rule_labels, batch_obj_labels = zip(*batch)

    # Stack images (assumes all images have the same size)
    images = torch.stack(images)

    # Pad labels to the maximum length in the batch
    max_length = max(len(img_labels) for img_labels in batch_obj_labels)
    debug(f"max length: {max_length}")
    padded_obj_labels = []

    for img_labels in batch_obj_labels:
        # Convert each label dictionary to a tensor (if necessary)
        img_label_tensors = []
        for obj_label in img_labels:
            vals = []
            for val in obj_label:
                if isinstance(val, np.generic):
                    val = val.item()
                vals.append(val)
            tensor = torch.tensor(vals)
            img_label_tensors.append(tensor)

        # Pad the list of tensors with -1 to match the maximum length
        if len(img_label_tensors) < max_length:
            padding = [-1] * (len(img_label_tensors[0]))
            img_label_tensors.extend([torch.tensor(padding)] * (max_length - len(img_label_tensors)))

        # Stack the padded tensors for this image
        padded_obj_labels.append(torch.stack(img_label_tensors))

    # Stack all padded labels across the batch
    padded_obj_labels = torch.stack(padded_obj_labels)

    return images, batch_rule_labels, padded_obj_labels


if __name__ == '__main__':
    """
    This script initializes a `ZendoImageDataset` instance using an annotations CSV file and 
    sets up a DataLoader with a custom collate function for batch processing. A batch of images 
    and their corresponding labels are retrieved from the dataset. The first image in the batch 
    is displayed using Matplotlib, and its associated rule-based and object-based labels are 
    printed for inspection. This ensures the dataset structure and labels are correctly loaded 
    and visualized.
    """

    training_data = ZendoImageDataset("output/ground_truth.csv", "output", None, None)
    train_dataloader = DataLoader(training_data, batch_size=4, shuffle=True, collate_fn=custom_collate)

    train_features, train_rule_labels, train_obj_labels = next(iter(train_dataloader))

    img = train_features[0]
    img = img.permute(1, 2, 0).numpy()
    rule_label = train_rule_labels[0]
    obj_label = train_obj_labels[0]
    debug(f"Label: {rule_label} {obj_label}")
    plt.imshow(img)
    plt.show()

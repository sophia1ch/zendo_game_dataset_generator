import pandas as pd
import torch
from torch.utils.data import Dataset, DataLoader
import matplotlib.pyplot as plt
from torchvision.io import read_image
import numpy as np


class ZendoImageDataset(Dataset):
    def __init__(self, annotations_file, img_dir="output", transform=None, target_transform=None):
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
        Returns the number of annotated images
        """
        return len(self.img_label_list)

    def __getitem__(self, idx):
        """
        Returns the image with its labels and applies transformations.
        Labels are separated by rule_based and object_based labels
        Converts object shape (pyramid, block, wedge) into class_idx (0, 1, 2)
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
    Custom collate function to handle variable-length object labels.
    """
    # Unzip the batch into images and labels
    images, batch_rule_labels, batch_obj_labels = zip(*batch)

    # Stack images (assumes all images have the same size)
    images = torch.stack(images)

    # Pad labels to the maximum length in the batch
    max_length = max(len(img_labels) for img_labels in batch_obj_labels)
    print("max length: ", max_length)
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
    training_data = ZendoImageDataset("output/ground_truth.csv", "output", None, None)
    train_dataloader = DataLoader(training_data, batch_size=4, shuffle=True, collate_fn=custom_collate)

    train_features, train_rule_labels, train_obj_labels = next(iter(train_dataloader))

    img = train_features[0]
    img = img.permute(1, 2, 0).numpy()
    rule_label = train_rule_labels[0]
    obj_label = train_obj_labels[0]
    print(f"Label: {rule_label}, {obj_label}")
    plt.imshow(img)
    plt.show()


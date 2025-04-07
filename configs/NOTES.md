### Notes
## Adjustments
* Currently the program outputs images where part of a piece is outside of the image view which will make detection unnecessarily hard.
-> Add rule for regeneration if this happens e.g. check if any of the bounding boxes are outside of the field of view.
* Currently when generating several images per rule, duplicates can appear
-> Check if any of the generated scenes have been generated before for that specific rule
* Rules defines Orientation twice, and some attributes are not distinct such as "flat" and "cheescake" for the shape "wedge"

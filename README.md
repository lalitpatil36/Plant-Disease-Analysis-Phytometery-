# Plant-Disease-Analysis-Phytometery-
Measurement of leaf disease by using R software
# Load required libraries
library(raster)
library(EBImage)
library(ggplot2)

# Set working directory to the folder containing the images
setwd("path/to/folder/containing/images")

# Load image files
img_files <- list.files(pattern = "*.jpg") # Assuming images are in jpg format
n_images <- length(img_files)

# Create empty vectors to store results
image_names <- character(n_images)
disease_areas <- numeric(n_images)
incidences <- numeric(n_images)
severities <- numeric(n_images)

# Loop through each image
for (i in 1:n_images) {
  # Read image
  img <- readImage(img_files[i])

  # Convert image to grayscale
  img_gray <- channel(img, "gray")

  # Enhance contrast
  img_contrast <- autolevels(img_gray)

  # Threshold the image to create a binary image
  img_thresholded <- thresh(img_contrast, w = "isodata")

  # Remove small objects
  img_clean <- removeSmallObjects(img_thresholded, minsize = 100)

  # Measure disease area
  disease_area <- sum(img_clean) * res(img)[1] * res(img)[2] # assuming square pixels

  # Calculate incidence
  incidence <- sum(img_clean > 0) / (nrow(img_clean) * ncol(img_clean)) * 100

  # Calculate severity
  severity <- disease_area / (nrow(img_clean) * ncol(img_clean)) * 100

  # Store results
  image_names[i] <- img_files[i]
  disease_areas[i] <- disease_area
  incidences[i] <- incidence
  severities[i] <- severity
}

# Create a data frame with the results
results <- data.frame(Image_Name = image_names,
                      Disease_Area = disease_areas,
                      Incidence = incidences,
                      Severity = severities)

# Plot results
ggplot(results, aes(x = Image_Name)) +
  geom_bar(aes(y = Disease_Area), fill = "lightblue", stat = "identity") +
  geom_line(aes(y = Incidence * 10), color = "red", group = 1, linetype = "dashed") +
  geom_line(aes(y = Severity), color = "green", group = 1, linetype = "dotted") +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Incidence/10 (%)")) +
  ylab("Disease Area") +
  xlab("Image Name") +
  ggtitle("Plant Disease Leaf Area Measurement and Incidence/Severity Calculation") +
  theme_minimal()

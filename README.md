# Influence-of-Match-Statistics-on-Goal-Prediction-in-Football
This repository contains an R script and guidelines for performing exploratory data analysis (EDA) and predictive analysis on football match player statistics to predict the number of goals scored. The project uses match player statistics data from UEFA Euro 2020.

Getting Started
Follow the instructions below to set up and run the analysis.

1. Download the R Script
Clone or download this repository to your local machine.
Locate the file named goal_predictions.R and save it to a directory of your choice.

2. Download the Dataset
Visit the dataset source: UEFA Euro 2020 Match Player Statistics at https://data.world/cervus/uefa-euro-2020.
Download the Match player statistics.csv file from the link.
Ensure you are logged into the Data World portal otherwise, you will get an error message.
Rename the file to mps.csv.
Save the renamed file in the same directory as the goal_predictions.R file.

4. Set Up R Environment
Open the goal_predictions.R script in RStudio or your preferred R environment.
Check the # Load Libraries section of the script for a list of required R libraries.
Install any missing libraries e.g.:
install.packages("ggplot2")
install.packages("dplyr")
Ensure all required libraries are successfully installed before proceeding.

5. Run the Script
Run the script line by line using the Ctrl + Enter (Windows/Linux) or Cmd + Enter (Mac) keyboard shortcut.
Follow any prompts or messages displayed in the R console to complete the analysis.

7. Expected Outputs
The script will generate:
Exploratory data analysis (EDA) visualizations.
Correlation heatmaps to identify significant relationships.
Predictive analysis results with model performance metrics.
Outputs will appear in the RStudio plot window and console.

Notes
Ensure that the dataset (mps.csv) is clean and uncorrupted.
Modify file paths or dataset configurations in the script if using a custom directory setup.
Contact the repository maintainer if you encounter issues with missing libraries or errors.

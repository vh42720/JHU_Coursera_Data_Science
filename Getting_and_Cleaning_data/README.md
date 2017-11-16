# GettingAndCleaningData
This is the course project for the Getting and Cleaning Data Coursera course. The R script, analysis.R, does the following:

  1. Create a "project" working directory and download the dataset. 
  2. Load the labels and feature info
  3. Loads both the training and test datasets, keeping only those columns which reflect a mean or standard deviation
  4. Loads the labels and subject data for each dataset, and merges those columns with the dataset
  5. Merges the two datasets
  6. Creates a tidy dataset that consists of the average (mean) value of each variable for each subject and activity pair.

The end result is shown in the file data_tidy.txt.

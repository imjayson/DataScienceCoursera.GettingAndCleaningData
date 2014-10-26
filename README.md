#Coursera Data Science Specilization
##Getting And Cleaning Data
###Course Project Code Book

As per course project requirements, this script allows the generation of a tidy data set of Means and Std from the UCI HAR data.

run_analysis.R provides a function tidyUCIHAR that takes in 2 parameters: 
1. the directory of the UCI HAR Dataset
2. the output filename

If you have the UCI HAR Dataset in your working directory, you can run the script as such:
```
source("run_analysis.R")
tidyUCIHAR("./UCI HAR Dataset","output.txt")
```
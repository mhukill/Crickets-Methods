## Reader.R: reads in data, defines regions of interest (ROIs). Used with Master.R
data <- read.csv("191009_190708_ALT.test.file.csv", # generates dataframe from .csv file
                 skip = 3, header = FALSE) # skip first 3 lines, ignore header names

## Defining ROIs
frame_col <- data[,1]
thorax_x <- data[,2]
thorax_y <- data[,3]
ab_x <- data[,2]
ab_y <- data[,3]

thorax_x_mean <- mean(thorax_x)
thorax_y_mean <- mean(thorax_y)

frame_len <- length(ab_x) 

wax_x <- data[,5]
wax_y <- data[,6]

ss_x <- data[, 8]
ss_y <- data[,9]

left_knee_x <- data[,11]
left_knee_y <- data[,12]

left_foot_x <- data[,14]
left_foot_y <- data[,15]

right_knee_x <- data[, 17]
right_knee_y <- data[, 18]

right_foot_x <- data[, 20]
right_foot_y <- data[, 21]


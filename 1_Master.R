#### 1_Master.R      (updated June 29, 2021 by Max Hukill for the tutorial video)
### This script controls the process of going from a DLC .csv file to a graphical .pdf file.
### We do this in 5 steps, each with its own script. See the tutorial video and handout for more guidance.

### STEP 0: Define your directories
primary_directory <- "~/Documents/GitHub/Crickets-Methods"
output_directory <- "graphical_pdf_files"
input_directory <- "DLC_csv_files"
## I recommend using the structure from the extant GitHub link, but you can use whatever file structure you deem fit. 
## (These three lines above are all you should have to change if changing the file structure.)
setwd(primary_directory)

### STEP 1: Decide on your filename(s)
file_name <- "190626-R1DeepCut_resnet50_190708Jul8shuffle1_200000filtered" # remember quotes!!
    file_name_csv <- paste(file_name, ".csv", sep='') # adds suffix for you
output_name <- NULL ## If you want to change the output name, you can do so here. 
## Otherwise, leaving it as NULL will result in file_name with "_graphs.pdf" tacked on. 
## If you choose to change the file name, remember to use quotes and omit the .pdf (suffix added in 5_Grapher.R)
## (As a general rule here, you should never have to add suffixes when you type names. The scripts take care of that for you)
cat("Step 1 Complete. Input name:", file_name_csv, "\n") ## reads out status for your convenience

### STEP 2: Load in the functions
source("2_Functions.R") # this will read in the script containing the necessary functions
cat("Step 2 Complete. Functions successfully read.", "\n")

### STEP 3: Load in the data
source("3_Reader.R")
cat("Step 3 Complete. Data successfully read.", "\n")

### STEP 4: Perform the relevant calculations
minimum_sound <- 0 # in dB, typically zero
maximum_sound <- 90 # in dB, depends on experiment
tick_mark_interval <- 10 # in dB, space between tick marks
source("4_Calculator.R")
cat("Step 4 Complete. Calculations performed.", "\n")

### STEP 5: Generate and save the cricket's graph
source("5_Grapher.R")
cat("Step 5 Complete. Graph saved as:", output_name_pdf ,"\n")


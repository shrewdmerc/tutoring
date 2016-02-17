#################################
# Step 1: Reading in the Data
#################################
# Here you want to read the data from a directory (the folder location in 
# your computer's memory). On a mac, this typically takes the format 
# '/Users/[username]/[folder]'
directory <- '/Users/cgibson/Downloads/specdata'
files_list <- list.files(directory, full.names = TRUE)

# Another way to read in data (if you don't want to type the directory)
myfile <- file.choose() # Running this opens a finder window
mdf <- read.csv(file, sep=',', header = T)

#################################
# Question 1: Pollutant Means
#################################

pollutantmean <- function(directory, pollutant, id = 1:10) {
  
  files_list <- list.files(directory, full.names = TRUE) # Creates list of files in directory
  dat <- data.frame() # Create an empty dataframe, called dat
  
  # Reminder that the for function in R is a shortened version of something like:
  # for(int i = 0; i < 100; i = i + 1)
  # You assign i to a value, you test if it meets a certain criteria, and you 
  # increment i by a specific rule
  
  # for each id that we're given in the function call...
  for (i in id) {
      # Three things going on here:
      # (1) read in the file 
      # (2) rbind that file at the end of dat
      # (3) replace the old version of dat with the new one you just created 
      dat <- rbind(dat, read.csv(files_list[i], stringsAsFactors = F))
  }
  
  # Since we only read in the data in id, we don't need this part anymore:
  # dat_subset <- dat[which(dat[, "ID"] == id), ]

  # Since we're passed the pollutant in the function call, we can use it
  # as a variable: get the mean of the column identified as pollutant variable
  print(mean(dat[,pollutant], na.rm = T))
  
  #if(pollutant == 'sulfate')
    #print(mean(dat$sulfate, na.rm = TRUE))
   # output <- mean(dat$sulfate, na.rm = TRUE)
  #if(pollutant == 'nitrate')
   # output <- mean(dat$nitrate, na.rm = TRUE)
    #print(mean(dat$nitrate, na.rm = TRUE))
  #print(output)
}

#dat_subset <- dat[which(dat[, "ID"] == id), ]
#dat_subset <- dat[ which(dat[, "ID"] == id), ]

# Here, we run the function to test if it works
pollutantmean('/Users/cgibson/Downloads/specdata', 'nitrate', 1:15)
 
#################################
# Question 2: Counting Complete Cases
#################################

complete <- function(directory, id = 1:10) {
  # Create an empty data frame to hold what you want the function to return
  dat <- data.frame() 
  
  # For i in each id that you're passed in the function call...
  for(i in id) {
    x <- read.csv(files_list[i], stringsAsFactors = F) # Read in the file
    
    # This next line does three things:
    # (1) complete.cases(x) returns a vector of TRUE or FALSE, depending on whether the
    # row was devoid of any NAs
    # (2) subset the dataframe, x, to only include the complete cases 
    # (3) get the number of rows, which is the number of complete cases
    count <- nrow(x[complete.cases(x),]) 
    
    # Rbind the ID # and the count from the previous line to dat; update dat
    dat <- rbind(dat, c(i,count))
  }
  
  colnames(dat) <- c('id', 'nobs') # Give the dataframe column names
  return(dat) # Function needs to return something; return most updated version of dat
}

# Example function call
complete(directory, 1:322)

#################################
# Question 3: Finding Correlations
#################################

corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.names = TRUE)
  #nfiles <- length(files_list)
  output <- numeric() # Create a numeric vector to output
  
  # Here we do something different in the for loop. files_list is a list of
  # file names in your directory. each i corresponds to a file name in the files_list. 
  for(i in files_list) {
    df <- read.csv(i, stringsAsFactors = F) # Read in file i
    df <- df[complete.cases(df),] # Remove the incomplete cases
    count <- nrow(df) # Get the number of complete cases
    
    # If the number of complete cases is greater than the threshold provided
    # in the function call...
    if(count >= threshold) {
      x <- df$sulfate # Grab the sulfate column of your df
      y <- df$nitrate # Grab the nitrate column of your df
      my_corr <- cor(x,y) # Get the correlation between these two variables
      output <- append(output,my_corr) # Append the correlation to your vector of correlations
    }
  }
  # When you're done going through all the files, return the output. 
  return(output)
}

# Example Function Calls
cr <- corr(directory, 150)
head(cr)
cr <- corr(directory, 5000)
summary(cr)
length(cr)

# Reminder that the <- marker assigns the variable (x) a value (1 or 2). 
# If you assign the variable to a different value, it no longer remembers
# the original value. For example: 
x <-1
x <-2
x # X is now equal to 2



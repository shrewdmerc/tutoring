#################################
# Step 1: Reading in the Data
#################################
# Here, you want to use the read.csv function. The first argument is the 
# file path, the second says that the entries are tab delimited, as opposed
# to comma delimited (csv) or anything else. 
grade_a_2014 <- read.csv(file = "/Users/cgibson/Downloads/Grade A 2014.txt", sep = '\t')
grade_b_2014 <- read.csv(file = "/Users/cgibson/Downloads/2014 Grade B.txt", sep='\t')
grade_c_2014 <- read.csv(file = "/Users/cgibson/Downloads/Grade C 2014.txt", sep='\t')
grade_d_2014 <- read.csv(file = "/Users/cgibson/Downloads/GRade D 2014.txt", sep='\t')

# If you want to select the file instead of write it out...
#file <- file.choose()
#mdf <- read.csv(file, sep=',', header = T)

# Rbind combines the data sources by row into one big dataframe. Essentially
# stacks one on top of another on top of another. You can easily do this 
# because the columns are the same for all the files we read in
df <- rbind(grade_a_2014, grade_b_2014, grade_c_2014, grade_d_2014)

# Write the dataframe to a csv file. 
write.csv(df, file = "/Users/cgibson/Downloads/all_grades_2014.csv")

#################################
# Step 2: Getting Summary Statistics
#################################
# Looking at the classes separately 
# Class A
summary(df[df$Site.Class.Desc=='A', "Value"])
table(df[df$Site.Class.Desc=='A', "Value"])/nrow(df[df$Site.Class.Desc=='A',])
# Class B
summary(df[df$Site.Class.Desc=='B', "Value"])
table(df[df$Site.Class.Desc=='B', "Value"])/nrow(df[df$Site.Class.Desc=='B',])
# Class C
summary(df[df$Site.Class.Desc=='C', "Value"])
table(df[df$Site.Class.Desc=='C', "Value"])/nrow(df[df$Site.Class.Desc=='C',])
# Class D
summary(df[df$Site.Class.Desc=='D', "Value"])
table(df[df$Site.Class.Desc=='D', "Value"])/nrow(df[df$Site.Class.Desc=='D',])

# Looking at the means across classes
library(dplyr)
group_by(df, Site.Class.Desc) %>% summarise(mean_value = mean(Value))

# Kruskal-Wallis Test
kruskal.test(Value ~ Site.Class.Desc, data=df)

# Wilcox (Mann-Whitney) Tests
wilcox.test(Value ~ Site.Class.Desc, data=df[df$Site.Class.Desc == 'A' | df$Site.Class.Desc == 'B',])
wilcox.test(Value ~ Site.Class.Desc, data=df[df$Site.Class.Desc == 'A' | df$Site.Class.Desc == 'C',])
wilcox.test(Value ~ Site.Class.Desc, data=df[df$Site.Class.Desc == 'A' | df$Site.Class.Desc == 'D',])
wilcox.test(Value ~ Site.Class.Desc, data=df[df$Site.Class.Desc == 'B' | df$Site.Class.Desc == 'C',])
wilcox.test(Value ~ Site.Class.Desc, data=df[df$Site.Class.Desc == 'B' | df$Site.Class.Desc == 'D',])
wilcox.test(Value ~ Site.Class.Desc, data=df[df$Site.Class.Desc == 'C' | df$Site.Class.Desc == 'D',])
#################################
# Step 3: Plotting in the Data
#################################
# Install ggplot for quality plotting
# install.packages("ggplot2")
library(ggplot2)
# Plot Value histogram
ggplot(df, aes(x = Value)) + geom_histogram(aes(y = ..density.., fill=Site.Class.Desc), binwidth=.2) + theme_minimal() +
  facet_grid(Site.Class.Desc~.) + guides(fill=FALSE) + ggtitle("Histogram of Value Across Grades") + xlim(0,40)

# Plot same on a log scale (makes it easier to see differences)
ggplot(df, aes(x = log(Value))) + geom_histogram(aes(y = ..density.., fill=Site.Class.Desc), binwidth=.2) + theme_minimal() +
  facet_grid(Site.Class.Desc~.) + guides(fill=FALSE) + ggtitle("Histogram of Log-Transformed Value Across Grades") 

#################################
# Step 4: Modeling the Data
#################################
# Negative Binomial Distribution has good fit
mod2 <- fitdistr(df$Value, densfun = "negative binomial")

# Plot the fit of the NB Dist
rand_nbinom <- as.data.frame(rnbinom(n=100000, size=mod2$estimate[1], mu = mod2$estimate[2]))
colnames(rand_nbinom) <- c('Count')
ggplot(df, aes(x = Value)) + 
  geom_histogram(aes(y = ..density..), binwidth=.2, fill='red', alpha=.4) + 
  theme_minimal() + 
  xlim(0,20) + 
  geom_histogram(data=rand_nbinom, aes(x= Count, y= ..density..), binwidth=.2, fill='blue', alpha=.4)

# Estimate Action Level based on fitted NB Dist
qnbinom(.99, size=mod2$estimate[1], mu = mod2$estimate[2])
qnbinom(.95, size=mod2$estimate[1], mu = mod2$estimate[2])





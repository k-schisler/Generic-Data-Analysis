#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package and Data Set Installation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data
# ~~~~
# Comment/uncomment lines as needed for specific datasets

#install.packages("NHANES") # If NHANES is not installed
library(NHANES)
data <- NHANES

#data <- read.csv("File name here")

# ~~~~~~~~
# Packages
# ~~~~~~~~

library(ggplot2)  # Loads the ggplot2 library
library(dplyr)  # Loads the dplyr library
library(mosaic) # Loads the mosaic package for favstats
library(knitr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variables and Labels Setup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# If one categorical and one numerical, set  X as categorical and y as numerical
data <- NHANES
x_var <- "AgeDecade"    # Variable name as string, replace w/ desired variable
y_var <- "BPSysAve"     # Variable name as string, replace w/ desired variable

# Check that the specified variables exist in the data set
if (!all(c(x_var, y_var) %in% names(data))) {
  stop("One or more specified variables do not exist in the dataset.")
}

# Filter out NA values from the original dataset
filtered_data <- data %>%
  filter(!is.na(!!sym(x_var)) & !is.na(!!sym(y_var)))

# Dynamically extract the variable values and labels
x <- filtered_data[[x_var]]
y <- filtered_data[[y_var]]
x_label <- x_var  
y_label <- y_var


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BOTH CATEGORICAL VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~
# Statistics
# ~~~~~~~~~~
  
# Table
x_table <- table(x, dnn=x_label)
y_table <- table(y, dnn=y_label)
x_table
y_table
prop.table(x_table)
prop.table(y_table)

#Contingency Table
my.ctable <- table(x, y, dnn=c(x_label, y_label))
addmargins(my.ctable) # Adds row and column totals
knitr::kable(addmargins(my.ctable)) # Formats nicely
  
# Chi-squared test for independence
chisq.test(my.ctable)


# ~~~~~~~~~
# Bar Plots
# ~~~~~~~~~
# Bar Plot for x
ggplot(filtered_data, aes(x = x)) +  
  geom_bar() +
  labs(title = paste("Distribution of", x_label), 
       x = x_label,  
       y = "Count") 

# Bar Plot for y 
ggplot(filtered_data, aes(x = y)) +  
  geom_bar() +
  labs(title = paste("Distribution of", y_label),  
       x = y_label, 
       y = "Count") 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ONE CATEGORICAL, ONE NUMERICAL (y as numerical variable)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~
# Statistics
# ~~~~~~~~~~

# Table
x_table <- table(x, dnn=x_label)
x_table
prop.table(x_table)

# Five Point Summary
favstats(y ~ x) # by category
favstats(y) # not by category

# ANOVA for differences in means across groups
ANOVA <- aov(y ~ x)
summary(ANOVA)
print(paste("where x =", x_label, "and y =", y_label))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Histogram and Normality Check, for NUMERICAL data only
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Histogram for y
ggplot(filtered_data, aes(x = y)) +
  geom_histogram(bins = 25,) +
  labs(title = paste("Distribution of", y_label),
       x = y_label,
       y = "Count")

# Normality Check
qqnorm(y, main = y_label)

# ~~~~~~~~
# Box Plot
# ~~~~~~~~

ggplot(filtered_data, aes(x=as.factor(x), y=y)) +
  geom_boxplot() +
  ggtitle(paste("Distribution of", y_label, "by", x_label)) +
  stat_summary(aes(color="Mean"), fun=mean, geom="point", 
               shape=20, size=5, fill="red") +
  scale_color_manual(name = "Legend", values = c("Mean" = "red")) +
  theme(legend.position="right") +
  scale_fill_brewer(palette="Set1") +
  xlab(x_label) +
  ylab(y_label)

# ~~~~~~~~~~~
# Violin Plot
# ~~~~~~~~~~~

ggplot(filtered_data, aes(x=x, y=y)) +
  geom_violin() +
  ggtitle(paste("Distribution of", y_label, "by", x_label)) +
  stat_summary(aes(color="Mean"), fun=mean, geom="point", 
               shape=20, size=5, fill="red") +
  scale_color_manual(name = "Legend", values = c("Mean" = "red")) +
  theme(legend.position="right") +
  scale_fill_brewer(palette="Set1") +
  xlab(x_label) +
  ylab(y_label)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BOTH NUMERICAL VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~
# Statistics
# ~~~~~~~~~~

# Five Point Summary
print(x_label)
favstats(x) 
print(y_label)
favstats(y) 

# Correlation Coefficient: strength and direction of a linear relationship 
      # Note: Correlation is standardized and unitless, easier to interpret
cor(x, y, use="complete.obs")

# Covariance: direction only of a relationship between variables
      # Note: Covariance is not standardized and depends on the units
cov(x, y, use="complete.obs")

# Linear Regression
lm_model <- lm(y ~ x)
summary(lm_model)
print(paste("where x =", x_label, "and y =", y_label))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Histograms and Normality Check
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Histogram for x
ggplot(filtered_data, aes(x = x)) +
  geom_histogram(bins = 25,) +
  labs(title = paste("Distribution of", x_label),
       x = x_label,
       y = "Count")
# Normality Check
qqnorm(x, main = x_label)

#Histogram for y
ggplot(filtered_data, aes(x = y)) +
  geom_histogram(bins = 25,) +
  labs(title = paste("Distribution of", y_label),
       x = y_label,
       y = "Count")

# Normality Check
qqnorm(y, main = y_label)

# ~~~~~~~~~~~~~
# Scatter Plots
# ~~~~~~~~~~~~~

# Scatter Plot
ggplot(filtered_data, aes(x=x, y=y)) +
  geom_point() +
  labs(title=paste("Scatterplot of", y_label, "vs", x_label),
       x=x_label, y=y_label)

#Scatter Plot with Regression Line
ggplot(filtered_data, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method="lm", color="blue") +
  labs(title=paste("Scatterplot with Regression Line of", y_label,
                   "vs", x_label), x=x_label, y=y_label)

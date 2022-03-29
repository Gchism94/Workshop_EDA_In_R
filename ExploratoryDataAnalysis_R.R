## -----------------------------------------------------------------------------------------------------------------------------------------
## Author: GREG CHISM
## Date: MARCH 2021
## email: gchism@arizona.edu
## Workshop: Exploratory data analysis in R
## -----------------------------------------------------------------------------------------------------------------------------------------

## Purpose of workshop
# Exploring and visualizing a novel data set and produce publication quality graphs and tables 

## -----------------------------------------------------------------------------------------------------------------------------------------
## Objectives
# 1. Load and explore a data set with publication quality tables
# 2. Visualize the data with publication quality figures 
# 3. Understand regression analyses
## -----------------------------------------------------------------------------------------------------------------------------------------

# INSTALL NECESSARY PACKAGES AND SET GLOBAL GGPLOT() THEME
## -----------------------------------------------------------------------------------------------------------------------------------------
options(repos = list(CRAN = "http://cran.rstudio.com/"))
options(digits = 2)

install.packages("pacman")

pacman::p_load(dlookr,
       formattable,
       ggdist,
       ggpubr,
       ggridges,
       kableExtra,
       knitr,
       palmerpenguins,
       papeR,
       Stat2Data,
       tidyverse)

# Global theme set
theme_set(theme_pubr())
## -----------------------------------------------------------------------------------------------------------------------------------------

### 1.0 Load and examine a data set
# Load data and view
# Examine columns and data types
# Define box plots
# Describe meta data
## -----------------------------------------------------------------------------------------------------------------------------------------

# Let's load a data set from the Hawks data set
data("Hawks")

# What does the data look like?'
# NOTE: formattable automatically produces clean HTML tables from R output tables
formattable(head(Hawks))
## -----------------------------------------------------------------------------------------------------------------------------------------

#### 1.1 Diagnose your data
# What are the properties of the data
formattable(diagnose(Hawks))

## -----------------------------------------------------------------------------------------------------------------------------------------

#### 1.2 Summary statistics of your data
##### Numerical variables
formattable(diagnose_numeric(Hawks))

## -----------------------------------------------------------------------------------------------------------------------------------------

##### Outliers
# There are several numerical variables that have outliers above, let's see what the data look like with and without them
# Create a table with columns containing outliers
# Plot outliers in a box plot and histogram

# Table showing outliers
formattable(diagnose_outlier(Hawks) %>%
  filter(outliers_ratio > 0))

# Boxplot and histograms of data with and without outliers
Hawks %>%
  select(find_outliers(Hawks)) %>%
           plot_outlier()

## -----------------------------------------------------------------------------------------------------------------------------------------

##### Categorical variables
formattable(diagnose_category(Hawks))

## -----------------------------------------------------------------------------------------------------------------------------------------

##### Missing values (NAs)
# Table showing the extent of NAs in columns containing them
# Plot showing the frequency of missing values
# Create the NA table
NA.Table <- plot_na_pareto(Hawks, only_na = TRUE, plot = FALSE) 

# Publishable table
formattable(NA.Table)

# Plot the insersect of the 5 columns with the most missing values
# This means that some combinations of columns have missing values in the same row
plot_na_intersect(Hawks, only_na = TRUE, n_vars = 5, n_intersacts = 5) 

## -----------------------------------------------------------------------------------------------------------------------------------------

### 2.0 Visualize the data with publication quality figures 
# Relationships among groups - i.e., species 
# Relationships between continuous variables - i.e., wing and tail length
# Now let's explore some of the data 
# you can use several methods to visualize groups:

#### 2.1 Plotting relationships among groups
# Box plot
Hawks %>% # Set the simulated normal data as a data frame
  ggplot(aes(x = Weight, y = Species, fill = Species)) + # Create a ggplot
  geom_boxplot() +
  xlab("Body mass (g)") +  # Remove the x axis label
  ylab(NULL) + # Remove the y axis label
  scale_fill_brewer(palette = "Dark2") + # Change the color scheme for the fill criteria
  theme(legend.position = "none") # Remove the legend

# Ridge plots (basically density plots)
Hawks %>% # Set the simulated normal data as a data frame
  ggplot(aes(x = Weight, y = Species, fill = Species)) + # Create a ggplot
  geom_density_ridges() +
  xlab("Body mass (g)") + # Change x axis label
  ylab(NULL) + # Remove the y axis label
  scale_fill_brewer(palette = "Dark2") + # Change the color scheme for the fill criteria
  theme(legend.position = "none") # Remove the legend

# Histogram interval plot
Hawks %>% # Set the simulated normal data as a data frame
  ggplot(aes(x = Species, y = Weight, fill = Species)) + # Create a ggplot
  # Add a histogram and interval plot
  stat_histinterval() + # Color the histogram bins
  # Remove the x axis label
  xlab(NULL) + # Remove the x axis label
  # Change y axis label
  ylab("Body mass (g)") + # Change y axis label
  coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------------------------------------------------------------------

#### 2.2 Plotting relationships between continuous variables
# We plot relationships between continuous variables several ways

# SCATTER PLOTS
# Basic scatter plot
Hawks %>%
  ggplot(aes(x = Wing, y = Tail)) + # Create a ggplot
  # Scatter plot, change size to 1.3, change transparency to 0.8
  geom_point(size = 1.3, alpha = 0.8) +
  xlab("Wing length (mm)") + # Change x axis label
  ylab("Tail length (mm)") + # Change y axis label
  xlim(50, 575) +
  ylim(100, 250)

# Grouped scatter plot
Hawks %>%
  ggplot(aes(x = Wing, y = Tail, color = Species)) + # Create a ggplot
  geom_point(size = 1.3, alpha = 0.5) +
  # Change y axis label
  xlab("Wing length (mm)") + # Change x axis label
  ylab("Tail length (mm)") + # Change y axis label
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Species") + # Change the label of the legend 
  xlim(50, 575) +
  ylim(100, 250)

# Scatter plot with scaled point size
Hawks %>%
  ggplot(aes(x = Wing, y = Tail, color = Species, size = Weight)) + # Create a ggplot
  geom_point(alpha = 0.5) +
  xlab("Wing length (mm)") + # Change x axis label
  ylab("Tail length (mm)") + # Change y axis label
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Species", size = "Body mass (g)") + # Change the label of the legend 
  xlim(50, 575) +
  ylim(100, 250)

# Density plots
# Contours
Hawks %>%
  ggplot(aes(x = Wing, y = Tail, color = Species)) + # Create a ggplot
  # Density contour plot
  geom_density_2d() +
  xlab("Wing length (mm)") + # Change x axis label
  ylab("Tail length (mm)") + # Change y axis label
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Species") + # Change the label of the legend 
  xlim(50, 575) +
  ylim(100, 250)


## -----------------------------------------------------------------------------------------------------------------------------------------

### 3.0 Understanding regression analysis
# Run a linear regression (lm()): the relationship between hawk wing and tail length 
# Clean the summary() output using prettify() 
# Produce a publishable table using kable() and kable_styling

# Simple linear regression predicting the relationship between wing and tail length in hawks
LinMod <- summary(lm(Tail ~ Wing, data = Hawks))

# Clean the summary() output using prettify() 
Pretty_lm <- prettify(LinMod)

# Create an HTML table using kable, where the prettify() function cleans the raw summary output and the kable_styling() applies a clean theme
kable(Pretty_lm) %>%
  kable_styling()


## -----------------------------------------------------------------------------------------------------------------------------------------

#### 3.1 Plotting a linear regression
# Plotting the linear relationship between wing and tail length in hawks
Hawks %>%
  ggplot(aes(x = Wing, y = Tail)) + # Create a ggplot
  # Scatter plot, change size to 1.3, change transparency to 0.8
  geom_point(size = 1.3, alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  xlab("Wing length (mm)") + # Change x axis label
  ylab("Tail length (mm)") + # Change y axis label
  xlim(50, 575) +
  ylim(100, 250)

## -----------------------------------------------------------------------------------------------------------------------------------------

#### 3.2 Multiple regression
# Multiple linear regression predicting the relationship between wing and tail length in hawks
LinMod <- summary(lm(Tail ~ Wing * Species, data = Hawks))

# Clean the summary() output using prettify() 
Pretty_lm <- prettify(LinMod)

# Create an HTML table using kable, where the prettify() function cleans the raw summary output and the kable_styling() applies a clean theme
kable(Pretty_lm) %>%
  kable_styling()

# Plotting the multiple regression
Hawks %>%
  ggplot(aes(x = Wing, y = Tail)) + # Create a ggplot
  # Scatter plot, change size to 1.3, change transparency to 0.8
  geom_point(aes(color = Species), size = 1.3, alpha = 0.8) +
  geom_smooth(aes(color = Species), method = "lm", se = FALSE) +
  xlab("Wing length (mm)") + # Change x axis label
  ylab("Tail length (mm)") + # Change y axis label
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Species") +
  xlim(50, 575) +
  ylim(100, 250)


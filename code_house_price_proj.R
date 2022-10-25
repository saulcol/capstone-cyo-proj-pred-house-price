################################################################################################
#
# Capstone Choose Your Own Project: Predict House Price
# Using Linear Regression and Random Forest approach for Regression
#
# author: "Saúl Santillán Gutiérrez"
# date: "October 25, 2022"
# 
################################################################################################


################################################################################################
#
# General Instructions by HarvardX's team
# 
################################################################################################
#
# For this project, you will be applying machine learning techniques that go beyond standard linear regression.
# You will have the opportunity to use a publicly available dataset to solve the problem of your choice.
# **You are strongly discouraged from using well-known datasets**, particularly ones that **have been used as
# examples in previous courses** or are similar to them (such as **the iris**, **titanic**, **mnist**, or
# **movielens datasets**, **among others**) - this is your opportunity to branch out and explore some new data!
# The UCI Machine Learning Repository and Kaggle are good places to seek out a dataset. Kaggle also maintains a
# curated list of datasets that are cleaned and ready for machine learning analyses. Your dataset must be
# automatically downloaded in your code or included with your submission. **You may not submit the same project
# for both the MovieLens and Choose Your Own project submissions**, and your **Choose Your Own project submission
# may not simply be a variation of your MovieLens project**.  

# The ability to clearly communicate the process and insights gained from an analysis is an important skill for
# data scientists. You will submit a report that documents your analysis and presents your findings, with supporting
# statistics and figures. The report must be written in English and uploaded as both a PDF document and an Rmd file.
# Although the exact format is up to you, **the report should include the following at a minimum:**  

# - An **introduction/overview/executive summary section** that describes the dataset and variables, and summarizes
# the goal of the project and key steps that were performed.

# - A **methods/analysis section** that explains the process and techniques used, including data cleaning, data
# exploration and visualization, any insights gained, and your modeling approach. **At least two different models or
# algorithms must be used, with at least one being more advanced than linear or logistic regression for prediction problems**.

# - A **results section** that presents the modeling results and discusses the model performance.

# - A **conclusion section** that gives a brief summary of the report, its potential impact, its limitations, and future work.


# The submission for **the Choose Your Own project** will be **three files**: **a report** in the form of both a **PDF document**
# and **Rmd file** and the **R script that performs your machine learning task**. You must also provide access to **your dataset**,
# either through **automatic download in your script** or **inclusion in a GitHub repository**.  

# **We recommend submitting a link to a GitHub repository with these three files and your dataset**. Your grade for the project
# will be based on **your report** and **your script**.  



################################################################################################
#
# Preliminary
# 
################################################################################################
#
# See the Rmarkdown document "report_house_price_proj.Rmd" to get more information about it.



################################################################################################
#
# 1. Overview
# 
################################################################################################
#
# See the Rmarkdown document "report_house_price_proj.Rmd" to get more information about it.



################################################################################################
#
# 2. Methods and Analysis
# 
################################################################################################
#
# 
################################################################################################
#
# 2.1 Preparing the Data Science Project and the Dataset
#
################################################################################################
#
###+++++  Install and Load the packages required +++++
#
# Install the libraries if they do not exist and load them
if(!require(this.path)) install.packages("this.path", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(huxtable)) install.packages("huxtable", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(this.path)
library(tidyverse)
library(caret)
library(data.table)
library(ggthemes)
library(scales)
library(Hmisc)
library(huxtable)
library(GGally)
library(gridExtra)
library(randomForest)


### set the working directory and create a subdirectory
#
# Get the current path with the `this.dir` function
wd <- this.dir(default = getwd())
# Set the working directory
setwd(wd)

# check if the folder “raw_data” exists in the current directory,
# if not creates a "raw_data" directory
ifelse(!dir.exists("raw_data"), dir.create("raw_data"), "Folder raw_data exists already")

# check if the folder “rdas” exists in the current directory,
# if not creates a "rdas" directory
ifelse(!dir.exists("rdas"), dir.create("rdas"), "Folder rdas exists already")


# +++++++++++ Get the version of R ++++++++++++
v <- R.Version() # It is a List


# Verify if the "data.csv" file exists in the "raw_data/" directory
if(file.exists(paste0(wd, "/raw_data/data.csv"))==TRUE){
  print("File data.csv exists already")
}else{
  print("File data.csv does NOT exist...downloading")
  # Download the file from gitlab to "raw_data/" directory
  download.file("https://gitlab.com/saulcol/house-price-data/-/raw/main/raw_data/data.csv",
                paste0(wd, "/raw_data/data.csv"))
}


# load the dataset
data <- read.csv("./raw_data/data.csv")
#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.1 Preparing the Data Science Project and the Dataset
#
################################################################################################



################################################################################################
#
# 2.2 Performing Data Exploration and Visualization to `data` dataset
#
################################################################################################
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.2.1. Overall Exploration in the Dataset
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Knowing its structure and how it is composed the `data` dataframe
glimpse(data)
#date         : <chr>
#price        : <dbl>
#bedrooms     : <dbl>
#bathrooms    : <dbl>
#sqft_living  : <int>
#sqft_lot     : <int>
#floors       : <dbl>
#waterfront   : <int>
#view         : <int>
#condition    : <int>
#sqft_above   : <int>
#sqft_basement: <int>
#yr_built     : <int>
#yr_renovated : <int>
#street       : <chr>
#city         : <chr>
#statezip     : <chr>
#country      : <chr>

str(data)
#date         : chr
#price        : num
#bedrooms     : num
#bathrooms    : num
#sqft_living  : int
#sqft_lot     : int
#floors       : num
#waterfront   : int
#view         : int
#condition    : int
#sqft_above   : int
#sqft_basement: int
#yr_built     : int
#yr_renovated : int
#street       : chr
#city         : chr
#statezip     : chr
#country      : chr

# How many rows and columns are there in the dataframe?
cat('The dataframe called data has', dim(data)[1], 'rows and', dim(data)[2], 'columns.')
#The dataframe called data has 4600 rows and 18 columns.

# As well, its eighteen columns have these characteristics:
#date         : character
#price        : numeric <dbl>
#bedrooms     : numeric <dbl>
#bathrooms    : numeric <dbl>
#sqft_living  : integer
#sqft_lot     : integer
#floors       : numeric <dbl>
#waterfront   : integer
#view         : integer
#condition    : integer
#sqft_above   : integer
#sqft_basement: integer
#yr_built     : integer
#yr_renovated : integer
#street       : character
#city         : character
#statezip     : character
#country      : character


# How many times each class occurs in this dataframe?
as.data.frame(table(unlist(
  lapply(data,
         function(x) paste(class(x), collapse = ","))))) %>%
  separate(col = "Var1", into = c("Var1", "Var2"), sep = ",", fill = "right") %>%
  rename(Class = Var1, OtherClass = Var2)
# The `data` dataframe has 5 `character`, 9 `integer` and 4 `numeric` variables


# Get a summary statistics 
summary(data)

# Other way, to obtain a better summary statistics is with
# the function describe() of the package Hmisc
Hmisc::describe(data)

# In general, it can be observed the mentioned dataframe does not have missing values.
# the `date` column has a range from 2014-05-02 00:00:00 to 2014-07-10 00:00:00 and has 70 different dates. It is candidate to be eliminate.
# the `price` column has a range from 0 to 26590000 and has 1741 different prices.
# the `bedrooms` column has 10 different bedrooms that are in a range from 0 to 9 with increments by 1.
# the `bathrooms` variable has a range from 0 to 8 and has 26 different bathrooms.
# the `sqft_living` column has a range from 370 to 13540 and has 566 different square foot livings.
# the `sqft_lot` variable has a range from 638 to 1074218 and has 3113 different square foot lots.
# the `floors` column has 6 different floors that are in a range from 1.0 to 3.5 with increments by 0.5.
# the `waterfront` does not have too much information, it is candidate to be eliminate.
# the `view` column has 5 different views that are in a range from 0 to 4 with increments by 1.
#            0 value is present in 4140 observations that represent the 90%, it is candidate to be eliminate.
# the `condition` variable has 5 different conditions that are in a range from 1 to 5 with increments by 1.
# the `sqft_above` column has a range from 370 to 9410 and has 511 different square foot aboves.
# the `sqft_basement` column has a range from 0 to 4820 and has 207 different square foot basements.
# the `yr_built` variable has a range from 1900 to 2014 and has 115 different years built.
# the `yr_renovated` variable has a range from 0 to 2014 and has 60 different years renovated.
# the `street` column has 4525 different streets that is all, it is candidate to be eliminate.
# the `city` column has 44 different cities, it is candidate to be eliminate (maybe).
# the `statezip` variable has 77 different states zip, it is candidate to be eliminate (perhaps).
# And the last one column `country` has 1 different country, it is candidate to be eliminate.

# Recall, our **response** or **dependent variable** is the `price` column.


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.2.2. Are there missing values?
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Earlier when we used the `describe` function we saw that the eighteen columns do not have missing values.
# We can corroborate it with this code.

# Count total missing values in each column of dataframe
colSums(is.na(data))

# In effect, this dataframe does not have missing values.


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.2.3. Data Analysis on numerical columns to detect outliers
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Know which columns are integers
names(data)[sapply(data, is.integer)]
#[1] "sqft_living"   "sqft_lot"      "waterfront"    "view"          "condition"     "sqft_above"    "sqft_basement" "yr_built"      "yr_renovated"

# Make a copy of the dataframe
data2 <- data

# Convert these integer columns to numeric
data2$sqft_living <- as.numeric(data$sqft_living)
data2$sqft_lot <- as.numeric(data$sqft_lot)
data2$view <- as.numeric(data$view)
data2$waterfront <- as.numeric(data$waterfront)
data2$condition <- as.numeric(data$condition)
data2$sqft_above <- as.numeric(data$sqft_above)
data2$sqft_basement <- as.numeric(data$sqft_basement)


# We do a multiple histogram about all numeric columns
data2 %>%
  pivot_longer(cols=c(2:14),
               names_to='variables',
               values_to='value') %>%
  ggplot(aes(y = value)) +
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") + 
  facet_wrap(~variables, ncol = 3, scales = 'free') +
  coord_flip() +
  theme_bw()

# we must put emphasis on the following variables: `price`, `sqft_lot`, `view`, `waterfront`, `condition`, `sqft_basement`  and `yr_renovated`.

# Create a plot with multiple histograms with different X-axis scale from
# our response variable that is `price` with the use of the function
# `grid.arrange` of the `gridExtra` package and control the formatting
# of the `labels = comma` on `scale_x_continuous` with the `scales` package
p1 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (0 to 2,000,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits=c(0,2e6), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (0 to 5,000,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits=c(0,5e6), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (0 to 10,000,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits=c(0,10e6), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (0 to 15,000,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits=c(0,15e6), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (0 to 20,000,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits=c(0,20e6), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (0 to 27,000,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits=c(0,27e6), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# We separate in two grids.
grid.arrange(p1,p2,p3, ncol=2)

grid.arrange(p4,p5,p6, ncol=2)


# We proceed to make a plot with multiple histograms in certain ranges (bins) in the `price` variable
p1 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (0 to 1,500,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits = c(0,15e5), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (1,500,000 to 4,500,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits = c(15e5,45e5), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (4,500,000 to 8,000,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits = c(45e5,8e6), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (8,000,000 to 12,000,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits = c(8e6,12e6), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (12,000,000 to 16,000,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits = c(12e6,16e6), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6 <- data2 %>%
  ggplot(aes(x = price)) +    
  geom_histogram(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  ggtitle("Distribution of Price",
          subtitle = "with X-axis (16,000,000 to 27,000,000)") +
  xlab("Price") +
  ylab("Count") + 
  scale_x_continuous(limits = c(16e6,27e6), labels = comma) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Again, we divide in two grids.
grid.arrange(p1,p2,p3, ncol=2)

grid.arrange(p4,p5,p6, ncol=2)


# Compute the `min()`, `mean()`, `median()`, `max()`, `sd()` and the coefficients of variation are obtained
# by dividing `sd()/mean()` across the `price`, `sqft_lot`, `view`, `waterfront`, `condition`, `sqft_basement`
# and `yr_renovated` columns. With descending order in `Coefficients_Variation`.
data2 %>%
  pivot_longer(cols=c(2, 6, 8:10, 12, 14),
               names_to='Variables',
               values_to='value') %>%
  group_by(Variables) %>%
  summarise(Min = min(value),
            Mean = mean(value),
            Median = median(value),
            Max = max(value),
            Coefficients_Variation = sd(value)/mean(value)) %>%
  arrange(desc(Coefficients_Variation))
# the variables `price`, `sqft_basement`, `yr_renovated`, `sqft_lot`, `view` and `waterfront` have the presence of outliers.

# plotting the following multiple boxplots
data2 %>%
  pivot_longer(cols=c(2, 6, 8:10, 12, 14),
               names_to='variables',
               values_to='value') %>%
  ggplot(aes(x = "", y = value)) +
  geom_boxplot(color = "darkblue", alpha = 0.8, fill = "lightblue") +
  facet_wrap(~variables, scales = 'free')
# those variables have outliers


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.2.4. How the predictor variables affect the `price` variable
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# As we know, the `price` column is our outcome, the first thing we will do in this variable is we are going
# to categorize this variable to generate some bins, example, (0 to 250000), (250000 to 500000), (500000 to 750000),
# (750000 to 1000000), (1000000 to 2000000) and (2000000 to 27000000)
data2$pricebin <- cut(data2$price,
                     c(0, 250000, 500000, 750000, 1000000, 2000000, 27000000),
					 dig.lab=10, include.lowest = TRUE)


# Then, visualize a bar plot of `price` by bins
data2 %>%
  mutate(pricebin = as.factor(pricebin)) %>%
  group_by(pricebin) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(pricebin = reorder(pricebin, Count)) %>%
  arrange(desc(Count)) %>%
  ggplot(aes(x = pricebin, y = Count)) +
  geom_bar(stat='identity', color = "darkblue", alpha = 0.8, fill = "lightblue") +
  geom_text(aes(x = pricebin, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  ggtitle("Bar Plot of Price",
          subtitle = "by Bins") +
  xlab("PriceBin") +
  ylab("Count") +
  coord_flip() + 
  theme_bw()


# The first bar plot of `bedrooms` and median `price` in descending order
data2 %>%
  group_by(bedrooms) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bedrooms = reorder(bedrooms,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  ggplot(aes(x = bedrooms,y = PriceMedian)) +
  geom_bar(stat='identity', color = "darkblue", alpha = 0.8, fill = "lightblue") +
  geom_text(aes(x = bedrooms, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  ggtitle("Bar Plot of Bedrooms and Median Price") +
  xlab("Bedrooms") +
  ylab("Median Price") +
  scale_y_continuous(limits=c(0,150e4), labels = comma) +
  coord_flip() + 
  theme_bw()


# Second bar plot of `bathrooms` and median `price` in descending order
data2 %>%
  group_by(bathrooms) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bathrooms = reorder(bathrooms,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  ggplot(aes(x = bathrooms,y = PriceMedian)) +
  geom_bar(stat='identity', color = "darkblue", alpha = 0.8, fill = "lightblue") +
  geom_text(aes(x = bathrooms, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  ggtitle("Bar Plot of Bathrooms and Median Price") +
  xlab("Bathrooms") +
  ylab("Median Price") +
  scale_y_continuous(limits=c(0,5e6), labels = comma) +
  coord_flip() + 
  theme_bw()


# Third bar plot of the first ten `yr_built` and median `price` in descending order
data2 %>%
  group_by(yr_built) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(yr_built = reorder(yr_built,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  ggplot(aes(x = yr_built,y = PriceMedian)) +
  geom_bar(stat='identity', color = "darkblue", alpha = 0.8, fill = "lightblue") +
  geom_text(aes(x = yr_built, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  ggtitle("Bar Plot of the First Ten",
          subtitle = "Year Built and Median Price") +
  xlab("Year Built") +
  ylab("Median Price") +
  scale_y_continuous(limits=c(0,1e6), labels = comma) +
  coord_flip() + 
  theme_bw()


# the fourth bar plot of the first ten `yr_renovated` and median `price` in descending order
data2 %>%
  group_by(yr_renovated) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(yr_renovated = reorder(yr_renovated,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  ggplot(aes(x = yr_renovated,y = PriceMedian)) +
  geom_bar(stat='identity', color = "darkblue", alpha = 0.8, fill = "lightblue") +
  geom_text(aes(x = yr_renovated, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  ggtitle("Bar Plot of the First Ten",
          subtitle = "Year Renovated and Median Price") +
  xlab("Year Renovated") +
  ylab("Median Price") +
  scale_y_continuous(limits=c(0,2e6), labels = comma) +
  coord_flip() + 
  theme_bw()


# visualize four scatterplots about the `sqft_living`, `sqft_lot`, `sqft_above` and `sqft_basement` variables against `price`
# Scatterplot `sqft_living` against `price`
data2 %>% 
  filter(!is.na(price)) %>% 
  filter(!is.na(sqft_living)) %>% 
  ggplot(aes(x = sqft_living, y = price)) +
  geom_point(color = "darkblue") +
  stat_smooth(aes(x = sqft_living, y = price), method = "lm", color = "red") +
  ggtitle("Scatterplot of Sqft Living vs. Price") +
  xlab("Sqft Living") +
  ylab("Price") +
  scale_y_continuous(labels = comma) +
  theme_bw()

# Scatterplot `sqft_lot` vs. `price`
data2 %>% 
  filter(!is.na(price)) %>% 
  filter(!is.na(sqft_lot)) %>% 
  ggplot(aes(x = sqft_lot, y = price)) +
  geom_point(color = "darkblue") +
  stat_smooth(aes(x = sqft_lot, y = price), method = "lm", color = "red") +
  ggtitle("Scatterplot of Sqft Lot vs. Price") +
  xlab("Sqft Lot") +
  ylab("Price") +
  scale_y_continuous(labels = comma) +
  theme_bw()

# Scatterplot `sqft_above` against `price`
data2 %>% 
  filter(!is.na(price)) %>% 
  filter(!is.na(sqft_above)) %>% 
  ggplot(aes(x = sqft_above, y = price))+
  geom_point(color = "darkblue")+
  stat_smooth(aes(x = sqft_above, y = price), method = "lm", color = "red")+
  ggtitle("Scatterplot of Sqft Above vs. Price") +
  xlab("Sqft Above") +
  ylab("Price") +
  scale_y_continuous(labels = comma) +
  theme_bw()

# Scatterplot `sqft_basement` vs. `price`
data2 %>% 
  filter(!is.na(price)) %>% 
  filter(!is.na(sqft_basement)) %>% 
  ggplot(aes(x = sqft_basement, y = price))+
  geom_point(color = "darkblue", alpha = 0.8)+
  stat_smooth(aes(x = sqft_basement,  y = price), method = "lm", color = "red")+
  ggtitle("Scatterplot of Sqft Basement vs. Price") +
  xlab("Sqft Basement") +
  ylab("Price") +
  scale_y_continuous(labels = comma) +
  theme_bw()


# we plot the correlations from `c(2,3,4,7,8)` columns with the function `ggpairs` of the `GGally` package
data2 %>%
  select(c(2:4, 7, 8)) %>%
  ggpairs(., 
          lower = list(continuous = wrap("cor", size = 4,
                                         alignPercent = 0.8, colour = "darkblue")),
          upper = list(continuous = wrap("points", alpha = 0.7,
                                         size = 0.3, colour = "darkblue"))
          )

# we plot the correlations from `c(2,9,10,13,14)` columns with the function `ggpairs` of the `GGally` package
data2 %>%
  select(c(2, 9:10, 13:14)) %>%
  ggpairs(., 
          lower = list(continuous = wrap("cor", size = 4,
                                         alignPercent = 0.8, colour = "darkblue")),
          upper = list(continuous = wrap("points", alpha = 0.7,
                                         size = 0.3, colour = "darkblue"))
          )

# plot the correlations from `c(2,5,6,11,12)` columns with `ggpairs`
data2 %>%
  select(c(2,5,6,11,12)) %>%
  ggpairs(., 
          lower = list(continuous = wrap("cor", size = 4,
                                         alignPercent = 0.8, colour = "darkblue")),
          upper = list(continuous = wrap("points", alpha = 0.7,
                                         size = 0.3, colour = "darkblue"))
          )

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.2 Performing Data Exploration and Visualization to `data` Data set
#
################################################################################################



################################################################################################
#
# 2.3 Preparing and Cleaning the Datasets
#
################################################################################################
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.3.1. Cleaning and Creating the Datasets
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#++++++++ Create the outliers set ++++++++++
#
# remove these columns `date`, `waterfront`, `view`,
# `street`, `city`, `statezip`, `country` and `pricebin`
outliers <- data2 %>%
  select(-c("date", "waterfront", "view", "street", "city", "statezip", "country", "pricebin"))

# The number of observations and columns that have the dataframe `outliers`
cat('The dataframe called outliers has', dim(outliers)[1], 'rows and', dim(outliers)[2], 'columns.')


#++++++++ Create the `no_outliers` set ++++++++++
#
# remove these columns `date`, `waterfront`, `view`,
# `street`, `city`, `statezip`, `country` and `pricebin`
no_outliers <- data2 %>%
  select(-c("date", "waterfront", "view", "street", "city", "statezip", "country", "pricebin"))

#++ Remove Outliers from Multiple Columns with the Interquartile range method ++
#
# create detect_outlier function
detect_outlier <- function(x) {
  
  # calculate first quantile
  Quantile1 <- quantile(x, probs=0.25)
  
  # calculate third quantile
  Quantile3 <- quantile(x, probs=0.75)
  
  # calculate inter quartile range
  iqr <- Quantile3-Quantile1
  
  # get minimums values
  min_values <- Quantile1 - (iqr*1.5)
  
  # get maximums values
  max_values <- Quantile3 + (iqr*1.5)
  
  # return true or false
  x > max_values | x < min_values
}

# create remove_outlier function
remove_outlier <- function(dataframe,
                            columns) {
  
  # for loop to traverse in columns vector
  for (col in columns) {
    
    # remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  
  dataframe
}


# Assign the names of the columns with outliers `price`,
# `sqft_lot`, `sqft_basement`  and `yr_renovated`
col_num <- c("price", "sqft_lot", "sqft_basement", "yr_renovated")

# apply the function `remove_outlier`
no_outliers <- remove_outlier(no_outliers, col_num)


# Remove these objects from the Global Environment
rm(data, data2, p1, p2, p3, p4, p5, p6)


# know the number of observations and columns that have the dataframe `no_outliers`
cat('The dataframe called no_outliers has', dim(no_outliers)[1], 'rows and', dim(no_outliers)[2], 'columns.')


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.3.2. Preparing the Train and Test Datasets
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
###+++ Create the train_outliers set and test_outliers set from the outliers set +++
#
#
# Set the seed according to the R version
if (paste(v$major, v$minor, sep = ".") < "3.6.0"){
  print("version of R is MINOR to 3.6.0, use set.seed(1)")
  # if using R 3.5 or earlier, use `set.seed(1)`:
  set.seed(1)
}else{
  print("version of R is MAJOR or equal to 3.6.0, use set.seed(1, sample.kind=Rounding)")
  # if using R 3.6 or later:
  set.seed(1, sample.kind="Rounding")
}

###+++ Create `train_outliers` set, `test_outliers` set from the `outliers` set +++
#
# We must split the `outliers` set in 2 parts: the training and test sets.
#
# `test_outliers` set will be 10% and `train_outliers` set 90% of `outliers` data
test_index <- createDataPartition(y = outliers$price, times = 1, p = 0.1, list = FALSE)
train_outliers <- outliers[-test_index,]
test_outliers <- outliers[test_index,]

# Remove these objects from the Global Environment
rm(test_index, outliers)


###+++ Create the train_no_outliers set and test_no_outliers set from the no_outliers set +++
#
#
# Set the seed according to the R version
if (paste(v$major, v$minor, sep = ".") < "3.6.0"){
  print("version of R is MINOR to 3.6.0, use set.seed(1)")
  # if using R 3.5 or earlier, use `set.seed(1)`:
  set.seed(1)
}else{
  print("version of R is MAJOR or equal to 3.6.0, use set.seed(1, sample.kind=Rounding)")
  # if using R 3.6 or later:
  set.seed(1, sample.kind="Rounding")
}

###+++ Create `train_no_outliers` set, `test_no_outliers` set from the `no_outliers` set +++
#
# We must split the `no_outliers` set in 2 parts: the training and test sets.
#
# `test_no_outliers` set will be 10% and `train_no_outliers` set 90% of `no_outliers` data
test_index <- createDataPartition(y = no_outliers$price, times = 1, p = 0.1, list = FALSE)
train_no_outliers <- no_outliers[-test_index,]
test_no_outliers <- no_outliers[test_index,]

# Remove these objects from the Global Environment
rm(test_index, no_outliers, col_num, v, detect_outlier, remove_outlier)


###+++ save objects train_outliers`, `test_outliers` +++
###+++ `train_no_outliers` and `test_no_outliers +++
###++++++ in the file path: rdas/cp_house_price.rda +++++++++
save(train_outliers, test_outliers, train_no_outliers, test_no_outliers,
     file = "./rdas/cp_house_price.rda")


# Verify if the "cp_house_price.rda" file exists in the "rdas" directory
if(file.exists(paste0(wd, "/rdas/cp_house_price.rda"))==TRUE){
  print("File cp_house_price.rda exists already")
}else{
  print("File cp_house_price.rda does NOT exist...downloading")
  # Download the file from gitlab to "rdas" directory
  download.file("https://gitlab.com/saulcol/rdas/-/raw/main/cp_house_price.rda",
                paste0(wd, "/rdas/cp_house_price.rda"))
}

# Remove these objects from the Global Environment if they exist
if(exists("train_outliers")) rm("train_outliers", envir = globalenv())
if(exists("test_outliers")) rm("test_outliers", envir = globalenv())
if(exists("train_no_outliers")) rm("train_no_outliers", envir = globalenv())
if(exists("test_no_outliers")) rm("test_no_outliers", envir = globalenv())

# load objects `train_outliers`, `test_outliers`, `train_no_outliers`
# and `test_no_outliers` from the file path:
# rdas/cp_house_price.rda. Take a few seconds
load("./rdas/cp_house_price.rda")

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.3 Preparing and Cleaning the Datasets
#
################################################################################################



################################################################################################
#
# 2.4 Modeling approach
#
################################################################################################
#
# See the Rmarkdown document "report_house_price_proj.Rmd" to get more information about it.
#
# 2.4.1 Linear Regression Model
#
# 2.4.2 Random Forest approach for Regression Model
#
#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.4 Modeling approach
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#
####################################### End of: ################################################
#
# 2. Methods and Analysis
#
################################################################################################



################################################################################################
#
# 3. Results
# 
################################################################################################
#
#
################################################################################################
#
# 3.1 Defining the RMSE Function to Evaluate the Model
#
################################################################################################
#
# Define the loss function
#
# Define the Root Mean Squared Error (RMSE) function
RMSE <- function(true_values, predicted_values){
  sqrt(mean((true_values - predicted_values)^2))
}

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.1 Defining the RMSE Function to Evaluate the Model
#
################################################################################################



################################################################################################
#
# 3.2 Model 1: Linear Regression Model with Outliers
#
################################################################################################
#
# Fit the model
lm1 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition +
                sqft_above + sqft_basement + yr_built + yr_renovated, data = train_outliers)

# View its summary
lm1_summary <- (summary(lm1))
lm1_summary


# Fit new model without `sqft_basement`
lm1 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition +
                sqft_above + yr_built + yr_renovated, data = train_outliers)

# View its summary
lm1_summary <- (summary(lm1))
lm1_summary


# Predict the target value in the test_outliers set
pred1 <- predict(lm1, newdata = test_outliers)

# Create the scores table to store the error scores
scores <- tibble(Method = "RMSE lm1 outliers",
                 RMSE = RMSE(test_outliers$price, pred1))

# Display the scores table
scores

# Compare the Min, Mean, Max from `test_outliers$price`
# with the Rmse obtained in this the table
test_outliers %>%
  pivot_longer(cols = c(1),
               names_to = 'Variable',
               values_to = 'value') %>%
  group_by(Variable) %>%
  summarise(Min = min(value),
            Mean = mean(value),
            Max = max(value)) %>%
  mutate(Rmse = RMSE(test_outliers$price, pred1))

# The value of RMSE indicates that our model is on average wrong by 276,244.558 units of `price`


# look at the first six rows of the actual (observed)
# and the predicted values of `price`
data.frame(cbind(actuals=test_outliers$price, predicteds = pred1)) %>%
  head()

# plot the observed (actual) versus predicted values of `price`
test_outliers %>%
  ggplot(aes(pred1, price)) +
  geom_point(color = "darkblue", alpha = 0.8)+
  stat_smooth(aes(x = pred1,  y = price), method = "lm", color = "red")+
  ggtitle("Scatterplot of Model 1: LM1 with outliers",
          subtitle = "Predicted vs. Observed values of Price") +
  xlab("Predicted values") +
  ylab("Observed values") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_bw()

# As we can see, the first model does not do a good job in general terms, because the outliers
# created some noise when we built it, and that affects it both in its performance and when we
# use it to predict.
#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.2 Model 1: Linear Regression Model with Outliers
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# 3.3 Model 2: Random Forest approach for Regression Model with Outliers
#
################################################################################################
#
# Fit the model
rf1 <- randomForest(price ~ ., data = train_outliers, ntree = 500,
                       importance = TRUE, type = "regression")

# Display the content of the model
rf1


# Predict the target value in the test_outliers set
pred2 <- predict(rf1, newdata = test_outliers)

# Update the scores table
scores <- bind_rows(scores,
                    tibble(Method = "RMSE rf1 outliers",
                           RMSE = RMSE(test_outliers$price, pred2)))

# Display the scores table
scores


# Plot of the test MSE by number of trees
plot(rf1)

# show the variable importance plot
varImpPlot(rf1)


# Compare the Min, Mean, Max from `test_outliers$price`
# with the Rmse obtained in this the table
test_outliers %>%
  pivot_longer(cols = c(1),
               names_to = 'Variable',
               values_to = 'value') %>%
  group_by(Variable) %>%
  summarise(Min = min(value),
            Mean = mean(value),
            Max = max(value)) %>%
  mutate(Rmse = RMSE(test_outliers$price, pred2))

# The value of RMSE indicates that our model is on average wrong by 293,019.298 units of `price`


# look at the first six rows of the actual (observed)
# and the predicted values of `price`
data.frame(cbind(actuals=test_outliers$price, predicteds = pred2)) %>%
  head()

# plot the observed (actual) versus predicted values of `price`
test_outliers %>%
  ggplot(aes(pred2, price)) +
  geom_point(color = "darkblue", alpha = 0.8)+
  stat_smooth(aes(x = pred2,  y = price), method = "lm", color = "red")+
  ggtitle("Scatterplot of Model 2: RF1 with outliers",
          subtitle = "Predicted vs. Observed values of Price") +
  xlab("Predicted values") +
  ylab("Observed values") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_bw()

# The second model is a little worse than the first one and does not do a good job in general terms.
#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.3 Model 2: Random Forest approach for Regression Model with Outliers
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# 3.4 Model 3: Linear Regression Model without Outliers
#
################################################################################################
#
# Fit the model
lm2 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition +
                sqft_above + sqft_basement + yr_built + yr_renovated, data = train_no_outliers)

# View its summary
lm2_summary <- (summary(lm2))
lm2_summary


# Fit new model without `sqft_basement`
lm2 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition
                + sqft_above + yr_built + yr_renovated, data = train_no_outliers)

# View its summary
lm2_summary <- (summary(lm2))
lm2_summary


# Predict the target value in the test_no_outliers set
pred3 <- predict(lm2, newdata = test_no_outliers)

# Update the scores table
scores <- bind_rows(scores,
                    tibble(Method = "RMSE lm2 no outliers",
                           RMSE = RMSE(test_no_outliers$price, pred3)))

# Display the scores table
scores

# Compare the Min, Mean, Max from `test_no_outliers$price`
# with the Rmse obtained in this the table
test_no_outliers %>%
  pivot_longer(cols = c(1),
               names_to = 'Variable',
               values_to = 'value') %>%
  group_by(Variable) %>%
  summarise(Min = min(value),
            Mean = mean(value),
            Max = max(value)) %>%
  mutate(Rmse = RMSE(test_no_outliers$price, pred3))
 
# The value of RMSE indicates that our model is on average wrong by 154,046.445 units of `price`


# look at the first six rows of the actual (observed)
# and the predicted values of `price`
data.frame(cbind(actuals=test_no_outliers$price, predicteds = pred3)) %>%
  head()

# plot the observed (actual) versus predicted values of `price`
test_no_outliers %>%
  ggplot(aes(pred3, price)) +
  geom_point(color = "darkblue", alpha = 0.8)+
  stat_smooth(aes(x = pred3,  y = price), method = "lm", color = "red")+
  ggtitle("Scatterplot of Model 3: LM2 without outliers",
          subtitle = "Predicted vs. Observed values of Price") +
  xlab("Predicted values") +
  ylab("Observed values") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_bw()

# As we can see, the third model is much better and it does almost a good job in general terms
# than the second and first models, because we eliminate some noise created by the outliers that
# affected the first two models in their performance and when we used them to predict.
#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.4 Model 3: Linear Regression Model without Outliers
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# 3.5 Model 4: Random Forest approach for Regression Model without Outliers
#
################################################################################################
#
# Fit the model
rf2 <- randomForest(price ~ ., data = train_no_outliers, ntree = 500,
                       importance = TRUE, type = "regression")

# Display the content of the model
rf2


# Predict the target value in the test_no_outliers set
pred4 <- predict(rf2, newdata = test_no_outliers)

# Update the scores table
scores <- bind_rows(scores,
                    tibble(Method = "RMSE rf2 no outliers",
                           RMSE = RMSE(test_no_outliers$price, pred4)))

# Display the scores table
scores


# Plot of the test MSE by number of trees
plot(rf2)

# show the variable importance plot
varImpPlot(rf2)


# Compare the Min, Mean, Max from `test_no_outliers$price`
# with the Rmse obtained in this the table
test_no_outliers %>%
  pivot_longer(cols = c(1),
               names_to = 'Variable',
               values_to = 'value') %>%
  group_by(Variable) %>%
  summarise(Min = min(value),
            Mean = mean(value),
            Max = max(value)) %>%
  mutate(Rmse = RMSE(test_no_outliers$price, pred4))

# The value of RMSE indicates that our model is on average wrong by 144,981.562 units of `price`.


# look at the first six rows of the actual (observed)
# and the predicted values of `price`
data.frame(cbind(actuals=test_no_outliers$price, predicteds = pred4)) %>%
  head()

# plot the observed (actual) versus predicted values of `price`
test_no_outliers %>%
  ggplot(aes(pred4, price)) +
  geom_point(color = "darkblue", alpha = 0.8)+
  stat_smooth(aes(x = pred4,  y = price), method = "lm", color = "red")+
  ggtitle("Scatterplot of Model 4: RF2 without outliers",
          subtitle = "Predicted vs. Observed values of Price") +
  xlab("Predicted values") +
  ylab("Observed values") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_bw()

# The fourth model is a little better than the third one, and much better than the second and
# first models. It looks better and it does almost a good job in general terms.
#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.5 Model 4: Random Forest approach for Regression Model without Outliers
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# 3.6 Making Predictions Using the Final Model
#
################################################################################################
#
# We determinate that our final model with the best RMSE was the **Model 4: Random Forest
# approach for Regression Model without Outliers** (`rf2`). Here are the final scores.

# Display the scores table
scores

# We use the `rf2` model to make predictions on a new house (observation) with this code
# Create a new observation
new_house <- data.frame(bedrooms=3,
                      bathrooms=2.50,
                      sqft_living=3300,
                      sqft_lot=8100,
                      floors=2,
                      condition=4,
                      sqft_above=2500,
                      sqft_basement=300,
                      yr_built=2001,
                      yr_renovated=2003)

# Use the fitted model to predict the `price` value of new observation
print(paste0("The price of the house is: $ ", round(predict(rf2, new_house), 2)))
# [1] "The price of the house is: $ 657553.43"

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.6 Making Predictions Using the Final Model
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#
####################################### End of: ################################################
#
# 3. Results
#
################################################################################################
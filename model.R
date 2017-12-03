library(dplyr)
library(rpart)
library(rattle)

setwd('~/University of Washington/Senior/Fall/Info 370/project-es3')

data <- read.csv('./data/clean_num.csv')

# reformat data
data <- data %>%
  # reformat graduation data as datae
  mutate(graduation_date = as.Date(graduation_date, "%m/%d/%Y")) %>%
  
  # split job type into 3 binary columns (1 = fulltime, 2 = partime, 3 = internship)
  mutate(fulltime = ifelse(Job_type == 1, 1, 0)) %>%
  mutate(parttime = ifelse(Job_type == 2, 1, 0)) %>%
  mutate(internship = ifelse(Job_type == 3, 1, 0)) %>%
  select(-Job_type) %>%
  
  # convert months to classification variable (0 = >3mo., 1 = <3mo.)
  mutate(Months = ifelse(Months <= 3, 1, 0)) %>%
  
  # split class standing into 5 binary columns
  mutate(freshman = ifelse(class_standing_ == 1, 1, 0)) %>%
  mutate(sophomore = ifelse(class_standing_ == 2, 1, 0)) %>%
  mutate(junior = ifelse(class_standing_ == 3, 1, 0)) %>%
  mutate(senior = ifelse(class_standing_ == 4, 1, 0)) %>%
  mutate(fifth.year = ifelse(class_standing_ == 5, 1, 0)) %>%
  mutate(alumni = ifelse(class_standing_ == 6, 1, 0)) %>%
  select(-class_standing_)

# split into training and test datasets
# (filter out people just beginning job search (no job, searching for < 3 mo.))
train <- data %>%
  filter(None_of_these == 0 | None_of_these == 1 & Months == 0) %>% # | has_position == 0 & Months == 0
  select(-None_of_these)
  
### !! NEED MORE TEST DATA !! ###
test <- data %>%
  filter(None_of_these == 1 & Months == 1) %>% # | has_position == 0 & Months == 1
  select(-None_of_these)

# create decision tree
### !! INVESTIGATE RPART CONTROL !! ###
tree <- rpart(Months ~ ., data = train, method = "class", control=rpart.control(minsplit=2, minbucket=1, cp=0.001))
fancyRpartPlot(tree)

# make predictions
predict(tree, test)

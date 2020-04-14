
library(tidyverse)
library(magrittr)

# read in the data
data <- read.csv('ill_school_data.csv', stringsAsFactors = F, header = T, na.strings=c("", " ", NA))

# figure out all the variables that have missing data
data %>%
  select(everything()) %>%
  summarise_all(list(~ sum(is.na(.))))

# doing test for independence
hand_season_data <- data %>%
  select(Handed, Favorite_Season) %>%
  filter(Handed != '' & Favorite_Season != '')

chisq.test(table(hand_season_data))


# Cleaning data, imputing missing data, and building regression model
library(mice)

# creating new dataframe that includes variables that will help in the imputation of height and armspan
# replacing miscellaneous characters with a period to perform later calculations
# using parse_number function from readr/tidy in order to standardize the numeric data
newdata <- data %>%
  select(Gender, Ageyears, Height_cm, Armspan_cm) %>%
  mutate(
    Height_cm = str_replace(Height_cm, '\'|/', '.'),
    height_cm_parsed = parse_number(Height_cm),
    armspan_cm_parsed = parse_number(Armspan_cm)
  )

# used logical reasoning and assumptions about the responses by the student
# in order to retain data and convert them all to cm
cleandata <- newdata %>%
  mutate(
    height_cm_cleaned = case_when(
      height_cm_parsed >= 100 ~ height_cm_parsed, # assume to be cm
      height_cm_parsed > 50 & height_cm_parsed <= 84 ~ height_cm_parsed * 2.54, # assuming inches 
      height_cm_parsed < 10 & height_cm_parsed > 2 ~ height_cm_parsed * 30.48, # assuming to be feet
      height_cm_parsed < 2 ~ height_cm_parsed * 100), # assuming to be meters
    armspan_cm_cleaned = case_when(
      armspan_cm_parsed >= 100 ~ armspan_cm_parsed, # assume to be cm
      armspan_cm_parsed > 50 & armspan_cm_parsed < 100 ~ armspan_cm_parsed * 2.54, # assume ot be inches
      armspan_cm_parsed < 10 & armspan_cm_parsed > 2 ~ armspan_cm_parsed * 30.48, # assume to be feet
      armspan_cm_parsed < 2 ~ armspan_cm_parsed * 100 # assume to be meters
    )
  )

# grabbing outliers for removal
cleandata_height_outliers <- boxplot(cleandata$height_cm_cleaned, plot=FALSE)$out
cleandata_armspan_outliers <- boxplot(cleandata$armspan_cm_cleaned, plot=FALSE)$out
cleandata_ageyears_outliers <- boxplot(cleandata$Ageyears, plot=FALSE)$out

# removing outliers if both height and armspan were outliers for one observation assuming this was a bad response
cleandata_no_outliers <- cleandata %>%
  filter(!height_cm_cleaned %in% cleandata_height_outliers & !armspan_cm_cleaned %in% cleandata_armspan_outliers & !Ageyears %in% cleandata_ageyears_outliers)

# removing all observations that do not have Gender or Ageyears as a response
# these are assumed to be missing at random and these variables are to help impute height and armspan
# not the other way
cleaned_final_data <- cleandata_no_outliers %>%
  select(Gender, Ageyears, height_cm_cleaned, armspan_cm_cleaned) %>%
  filter(!is.na(Gender) & !is.na(Ageyears) & Gender != '')

# running mice 50 times
m <- 50

# imputation using cart methods 
cart_imputed_data <- mice(cleaned_final_data, m=m, method = c("cart", "cart", "cart", "cart"))

# pooling cart imputated data
cartModelFit <- with(cart_imputed_data, exp = lm(height_cm_cleaned ~ armspan_cm_cleaned))
pool(cartModelFit)
summary(pool(cartModelFit))

# imputation using random forest methods 
rf_imputed_data <- mice(cleaned_final_data, m=m, method = c("rf", "rf", "rf", "rf"))

# pooling random forest imputed data
rfModelFit <- with(rf_imputed_data, exp = lm(height_cm_cleaned ~ armspan_cm_cleaned))
pool(rfModelFit)
summary(pool(rfModelFit))


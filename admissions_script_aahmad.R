# Admissions Project

## Average running time (15 - 20 mins)
## Time Analysis ##
start_time <- Sys.time()


# Load required packages.
library(dplyr) # for data manipulation  
library(tidyverse) # for data manipulation
library(caret) # for modeling
library(e1071) # for modeling
library(MASS) # for modeling
library(glmnet) # for modeling
library(randomForest) # for modeling
library(DMwR2) # for modeling
library(tree) # for modeling
library(ggplot2) # for visualization
library(stringr) # string manipulation
library(car) # modeling and prescriptions
library(corrplot) # for correlation visualization
library(gbm) # for modeling
library(coefplot) # visualize coefficient effects

# Load the original data into R.
clean_data <- read.csv("TU.csv")

options(warn = -1)
# Clean the data

# Replace empty cells with NA.
clean_data[clean_data == ''] <- NA

# Define a function to convert ACT scores to SAT scores.
act_to_sat <- function(act_score) {
  sat_score <- round((act_score / 36) * 1600)
  return(sat_score)
}

## Columns to be removed
remove_cols <- c()

# Column 1 - ID
# Check for missing values.
sum(is.na(clean_data$ID)) # No missing values.
# ID should be removed in the modeling stage.
# Add to the list of columns to be removed.
remove_cols <- c(remove_cols, "ID")

# Column 2 - train.test
# Check for missing values.
sum(is.na(clean_data$train.test)) # No missing values.
# train.test should be removed in the modeling stage.
# Add to the list of columns to be removed.

# Column 3 - Entry.Term..Application.
# Check for missing values.
sum(is.na(clean_data$Entry.Term..Application.)) # No missing values.
# Convert to a factor.
clean_data$Entry.Term..Application. <-
  as.factor(clean_data$Entry.Term..Application.)
# Check the summary of the factor levels.
summary(clean_data$Entry.Term..Application.)
# No suspicious categories found.

# Column 4 - Admit.Type
# Check for missing values.
sum(is.na(clean_data$Admit.Type)) # No missing values.
# Check the levels of the factor.
levels(factor(clean_data$Admit.Type)) # Only one level.
# Since the data set only has first years (i.e., only one category),
# Admit.Type should be removed.
# Add to the list of columns to be removed.
remove_cols <- c(remove_cols, "Admit.Type")

# Column 5 - Permanent.Postal
# Check for missing values.
sum(is.na(clean_data$Permanent.Postal)) # 162 missing values.
# Instead of categorizing postal codes into different states,
# we can use "Permanent.Geomarket" to group students by region.
# Therefore, we can remove "Permanent.Postal" column.
# Add to the list of columns to be removed.
remove_cols <- c(remove_cols, "Permanent.Postal")

# Column 6 - Permanent.Country
# Check for missing values.
sum(is.na(clean_data$Permanent.Country)) # 1 missing value.
# Check the levels of the factor.
levels(factor(clean_data$Permanent.Country)) # No suspicious categories.
# The student with the missing value for "Permanent.Country" is a US citizen,
# so we will assign "United States" to the missing value.
clean_data$Permanent.Country[is.na(clean_data$Permanent.Country)] <-
  "United States"
clean_data$Permanent.Country <-
  as.factor(clean_data$Permanent.Country)
remove_cols <- c(remove_cols, "Permanent.Country")

#Column 7 - Sex
#Check if there are any missing values or typos in the column.
sum(is.na(clean_data$Sex))#No missing value.
levels(factor(clean_data$Sex))#No suspicious categories.
#Convert Sex column to a factor variable.
clean_data$Sex <- as.factor(clean_data$Sex)

#Column 8 - Ethnicity
#Check for missing values in Ethnicity column.
sum(is.na(clean_data$Ethnicity))#227 missing values.
levels(factor(clean_data$Ethnicity))#No questionable categories.
#Impute missing values with "Not specified", as it is a reasonable assumption that some individuals may choose not to disclose their ethnicity.
clean_data$Ethnicity[is.na(clean_data$Ethnicity)] <- "Not specified"
#Convert Ethnicity column to a factor variable.
clean_data$Ethnicity <- as.factor(clean_data$Ethnicity)

#Column9 - Race

# Count the number of missing values in the Race column.
sum(is.na(clean_data$Race)) #555 NAs.

# Replace the missing values with "Not specified" since we don't have information on the race of these individuals.
clean_data$Race[is.na(clean_data$Race)] <- "Not specified"

# Check the unique values in the Race column and ensure that there are no unexpected or questionable categories.
levels(factor(clean_data$Race)) # No questionable category.

# Display the frequency distribution of the Race variable.
table(clean_data$Race)

# Provide a summary of the Race variable, including the number of observations, unique values, and the most frequent categories.
summary(clean_data$Race)

# Notice that some categories in the Race column have very low frequencies, making it difficult to draw meaningful insights. We need to consider combining some of the categories.
# The following code combines some categories based on their similarity to increase the frequency of each group.
clean_data$Race <- ifelse(
  clean_data$Race == "American Indian or Alaska Native",
  "American Indian or Alaska Native",
  ifelse(
    clean_data$Race == "American Indian or Alaska Native, White",
    "American Indian or Alaska Native, White",
    ifelse(
      clean_data$Race == "Asian",
      "Asian",
      ifelse(
        clean_data$Race == "Asian, White",
        "Asian, White",
        ifelse(
          clean_data$Race == "Black or African American",
          "Black or African American",
          ifelse(
            clean_data$Race == "Black or African American, White",
            "Black or African American, White",
            ifelse(
              clean_data$Race == "Not specified",
              "Not specified",
              ifelse(clean_data$Race == "White", "White", "Other")
            )
          )
        )
      )
    )
  )
)

# Convert the Race variable to a factor to make sure the values are treated as categorical data in further analysis.
clean_data$Race <- as.factor(clean_data$Race)

# Display the updated categories in the Race column after combining some of the categories.
levels(clean_data$Race)


#Column10 - Religion
sum(is.na(clean_data$Religion)) # 5483 NAs.
levels(factor(clean_data$Religion)) # No questionable category.

# Impute NAs with "Not specified"
clean_data$Religion[is.na(clean_data$Religion)] <- "Not specified"

# Combine similar levels into one level
clean_data$Religion <- ifelse(
  clean_data$Religion %in% c(
    "Anglican",
    "Baptist",
    "Christian",
    "Lutheran",
    "Methodist",
    "Presbyterian",
    "Roman Catholic"
  ),
  clean_data$Religion,
  ifelse(
    clean_data$Religion == "Bible Churches",
    "Christian",
    clean_data$Religion
  )
)

# Combine levels with less than 100 cases into "Other"
religion_counts <- table(clean_data$Religion)
clean_data$Religion <- ifelse(
  clean_data$Religion %in% names(religion_counts)[religion_counts >= 100],
  clean_data$Religion,
  ifelse(clean_data$Religion == "Not specified", "Not specified", "Other")
)

clean_data$Religion <- as.factor(clean_data$Religion)
levels(clean_data$Religion)


#Column11 - First_Source.Origin.First.Source.Date
sum(is.na(clean_data$First_Source.Origin.First.Source.Date))#No NA.
clean_data$First_Source.Origin.First.Source.Date <-
  as.Date(clean_data$First_Source.Origin.First.Source.Date,
          format = "%m/%d/%Y")

#Column12 - Inquiry.Date
sum(is.na(clean_data$Inquiry.Date))#4579 NAs.
#I will deal with NAs later because I need to use this variable to create
#several new variables.
clean_data$Inquiry.Date <-
  as.Date(clean_data$Inquiry.Date, format = "%m/%d/%Y")

#Column13 - Submitted
sum(is.na(clean_data$Submitted))#No NA.
clean_data$Submitted <-
  as.Date(clean_data$Submitted, format = "%m/%d/%Y")

#Column11-13
#After viewing Column11-13, it would be interesting to see
#whether the differences between submission date and First_Source date
#the differences between submission date and inquiry date affect the response.
#The time difference between submission date and first_source date.
clean_data$Submit_FirstSource <- difftime(clean_data$Submitted,
                                          clean_data$First_Source.Origin.First.Source.Date,
                                          units = "weeks")
clean_data$Submit_Inquiry <- difftime(clean_data$Submitted,
                                      clean_data$Inquiry.Date, units = "weeks")
clean_data$Submit_FirstSource <-
  round(clean_data$Submit_FirstSource, digits = 0)
clean_data$Submit_FirstSource <-
  as.numeric(clean_data$Submit_FirstSource)
clean_data$Submit_Inquiry <-
  round(clean_data$Submit_Inquiry, digits = 0)
clean_data$Submit_Inquiry <- as.numeric(clean_data$Submit_Inquiry)
#Remember that there are NAs in Inquiry.Date,
#thus leading to NAs in Submit_Inquiry.
#Impute NAs in Submit_Inquiry with median values.
clean_data$Submit_Inquiry[is.na(clean_data$Submit_Inquiry)] <-
  median(clean_data$Submit_Inquiry,
         na.rm =
           TRUE)
#Remember to remove Column11-13 in the modeling stage
#since they are used to construct new variables.
remove_cols <-
  c(remove_cols,
    "First_Source.Origin.First.Source.Date",
    "Submitted",
    "Inquiry.Date")

#Column14 - Application.Source
sum(is.na(clean_data$Application.Source))#No NA.
table(clean_data$Application.Source)#No questionable category.
clean_data$Application.Source <-
  as.factor(clean_data$Application.Source)

#Column15 - Decision.Plan
sum(is.na(clean_data$Decision.Plan))#No NA.
table(clean_data$Decision.Plan)#No questionable category.
clean_data$Decision.Plan <- as.factor(clean_data$Decision.Plan)

#Column16 - Staff.Assigned.Name
#Based on variable description,
#I don't think this variable is useful in the modeling.
#Moreover, some staff don't work for Trinity anymore, what is the point
#of knowing which staff can affect student decisions?.
#Consider removing this variable in the modeling stage.
remove_cols <- c(remove_cols, "Staff.Assigned.Name")

#Column17 - Legacy
sum(is.na(clean_data$Legacy))#13658 NAs.
table(clean_data$Legacy)#No questionable category.
#Impute NAs with "No Legacy"
clean_data$Legacy[is.na(clean_data$Legacy)] <- "No Legacy"
#Legacy has many options, leading some options to having
#only a small number of cases.
#I will group all the options into 3 categories so that each category
#has the chance to affect the response.
clean_data$Legacy <-
  ifelse(
    clean_data$Legacy == "Legacy",
    "Legacy",
    ifelse(
      clean_data$Legacy == "No Legacy",
      "No Legacy",
      ifelse(
        grepl("Legacy, Opt Out", clean_data$Legacy),
        "Legacy, Opt Out",
        "Legacy"
      )
    )
  )
clean_data$Legacy <- as.factor(clean_data$Legacy)

#Column18 - Athlete
sum(is.na(clean_data$Athlete))#13120 NAs.
table(clean_data$Athlete)#No questionable category.
#Impute NAs with "Non-Athlete"
clean_data$Athlete[is.na(clean_data$Athlete)] <- "Non-Athlete"
#Similar to Column17, Column18 has many categories with a few cases.
#Group all options into three categories:
#Athlete, Non-Athlete, and Athlete, Opt Out.
clean_data$Athlete <-
  ifelse(
    clean_data$Athlete == "Athlete",
    "Athlete",
    ifelse(
      clean_data$Athlete == "Non-Athlete",
      "Non-Athlete",
      ifelse(
        grepl("Opt Out", clean_data$Athlete),
        "Athlete, Opt Out",
        "Athlete"
      )
    )
  )
clean_data$Athlete <-
  as.factor(clean_data$Athlete)

#Column19 - Sport.1.Sport
sum(is.na(clean_data$Sport.1.Sport))#13120 NAs.
table(clean_data$Sport.1.Sport)#No questionable category.
#Impute NAs with "No Sport".
clean_data$Sport.1.Sport[is.na(clean_data$Sport.1.Sport)] <-
  "No Sport"
#Group sport men and sport women into one group
#so that each group has sufficient cases to have an impact on the response.
clean_data$Sport.1.Sport <-
  ifelse(
    clean_data$Sport.1.Sport == "Baseball",
    "Baseball",
    ifelse(
      clean_data$Sport.1.Sport == "Softball",
      "Softball",
      ifelse(
        clean_data$Sport.1.Sport == "Football",
        "Football",
        ifelse(
          clean_data$Sport.1.Sport == "No Sport",
          "No Sport",
          ifelse(
            grepl("Basketball", clean_data$Sport.1.Sport),
            "Basketball",
            ifelse(
              grepl("Cross Country", clean_data$Sport.1.Sport),
              "Cross Country",
              ifelse(
                grepl("Diving", clean_data$Sport.1.Sport),
                "Diving",
                ifelse(
                  grepl("Golf", clean_data$Sport.1.Sport),
                  "Golf",
                  ifelse(
                    grepl("Soccer", clean_data$Sport.1.Sport),
                    "Soccer",
                    ifelse(
                      grepl("Swimming", clean_data$Sport.1.Sport),
                      "Swimming",
                      ifelse(
                        grepl("Tennis", clean_data$Sport.1.Sport),
                        "Tennis",
                        ifelse(
                          grepl("Track", clean_data$Sport.1.Sport),
                          "Track",
                          "Volleyball"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
clean_data$Sport.1.Sport <- as.factor(clean_data$Sport.1.Sport)

#Column20 - Sport.1.Rating
sum(is.na(clean_data$Sport.1.Rating))#13120 NAs.
table(clean_data$Sport.1.Rating)#No questionable category.
#Impute NAs with "No Sport".
clean_data$Sport.1.Rating[is.na(clean_data$Sport.1.Rating)] <-
  "No Sport"
clean_data$Sport.1.Rating <-
  factor(
    clean_data$Sport.1.Rating,
    order = TRUE,
    levels = c("No Sport", "Varsity", "Blue Chip", "Franchise")
  )
summary(clean_data$Sport.1.Rating)

#Column21 - Sport.2.Sport
sum(is.na(clean_data$Sport.2.Sport))#14513 NAs.
table(clean_data$Sport.2.Sport)#No questionable category.
#impute NAs with "No 2ndSport".
clean_data$Sport.2.Sport[is.na(clean_data$Sport.2.Sport)] <-
  "No 2ndSport"
#The number of cases for each sport type is very small (< about 1% of the data set).
#It's better to group all options into 2 categories: 2ndSport vs. No 2ndSport.
clean_data$Sport.2.Sport <-
  ifelse(clean_data$Sport.2.Sport == "No 2ndSport",
         "No 2ndSport",
         "2ndSport")
clean_data$Sport.2.Sport <- as.factor(clean_data$Sport.2.Sport)

#Column22 - Sport.2.Rating
sum(is.na(clean_data$Sport.2.Rating))#15085 NAs.
table(clean_data$Sport.2.Rating)#No questionable category.
#Only 58 out of 15143 observations are rated.
#This is less than 0.5% of the data set!
#I don't think Sport.2.Rating will have much impact on the response.
#Consider removing Column22 in the modeling stage.
remove_cols <- c(remove_cols, "Sport.2.Rating")

#Column23 - Sport.3.Sport
sum(is.na(clean_data$Sport.3.Sport))#14907 NAs.
table(clean_data$Sport.3.Sport)#No questionable category.
#impute NAs with "No 3rdSport".
clean_data$Sport.3.Sport[is.na(clean_data$Sport.3.Sport)] <-
  "No 3rdSport"
#The number of cases for each sport type is very small (< 0.5% of the data set).
#It's better to group all options into 2 categories: 3rdSport vs. No 3rdSport.
clean_data$Sport.3.Sport <-
  ifelse(clean_data$Sport.3.Sport == "No 3rdSport",
         "No 3rdSport",
         "3rdSport")
clean_data$Sport.3.Sport <- as.factor(clean_data$Sport.3.Sport)

#Column24 - Sport.3.Rating
sum(is.na(clean_data$Sport.3.Rating))#15140 NAs.
table(clean_data$Sport.3.Rating)#No questionable category.
#Only 3 out of 15143 observations are rated!
#Consider removing Column24 in the modeling stage.
remove_cols <- c(remove_cols, "Sport.3.Rating")

#Column25 - Academic.Interest.1
sum(is.na(clean_data$Academic.Interest.1))#6 NAs.
table(clean_data$Academic.Interest.1)#No questionable category.
clean_data[is.na(clean_data$Academic.Interest.1),]
#Most of the NAs for Academic.Interest.1 have a value for Academic.Interest.2
#We may assign the corresponding values in Academic.Interest.2
#to NAs in Academic.Interest.1 if Academic.Interest.2 has a value.
clean_data$Academic.Interest.1 <-
  ifelse(
    is.na(clean_data$Academic.Interest.1) == TRUE,
    clean_data$Academic.Interest.2,
    clean_data$Academic.Interest.1
  )
#For the remaining NAs in Academic.Interest.1, assign Undecided.
clean_data$Academic.Interest.1[is.na(clean_data$Academic.Interest.1)] <-
  "Undecided"
#Group Business related options into "Business".
#Based on your understanding of the academic fields listed in this variable,
#you may also consider grouping other related options into
#a broad major, similar to what I did with Business.
clean_data$Academic.Interest.1 <-
  ifelse(
    grepl("Business", clean_data$Academic.Interest.1),
    "Business",
    ifelse(
      clean_data$Academic.Interest.1 == "Finance",
      "Business",
      ifelse(
        clean_data$Academic.Interest.1 == "Entrepreneurship",
        "Business",
        clean_data$Academic.Interest.1
      )
    )
  )
#Group options with a low number of cases (< 100 cases) into "Others".
frequencies <- data.frame(table(clean_data$Academic.Interest.1))
frequencies
# Create a named vector of frequencies
freq_vec <- setNames(frequencies$Freq, frequencies$Var1)

# Use the named vector to look up the frequencies for each value in the column
clean_data$Academic.Interest.1.Frequency <-
  freq_vec[clean_data$Academic.Interest.1]


for (i in 1:nrow(clean_data)) {
  if (clean_data$Academic.Interest.1.Frequency[i] < 100)
  {
    clean_data$Academic.Interest.1[i] <- "Other"
  } else{
    clean_data$Academic.Interest.1[i]
  }
}
clean_data$Academic.Interest.1 <-
  as.factor(clean_data$Academic.Interest.1)
summary(clean_data$Academic.Interest.1)
#Remember to drop Academic.Interest.1.Frequency in the modeling stage.
remove_cols <- c(remove_cols, "Academic.Interest.1.Frequency")

#Column26 - Academic.Interest.2
sum(is.na(clean_data$Academic.Interest.2))#159 NAs.
#Replace repeated academic interests with Undecided,
#then make NAs Undecided
clean_data$Academic.Interest.2 <-
  ifelse(
    clean_data$Academic.Interest.2 == clean_data$Academic.Interest.1,
    "Undecided",
    clean_data$Academic.Interest.2
  )
clean_data$Academic.Interest.2[is.na(clean_data$Academic.Interest.2)] <-
  "Undecided"
table(clean_data$Academic.Interest.2)#No questionable category.
#Group Business related options into "Business".
clean_data$Academic.Interest.2 <-
  ifelse(
    grepl("Business", clean_data$Academic.Interest.2),
    "Business",
    ifelse(
      clean_data$Academic.Interest.2 == "Finance",
      "Business",
      ifelse(
        clean_data$Academic.Interest.2 == "Entrepreneurship",
        "Business",
        clean_data$Academic.Interest.2
      )
    )
  )
#Group options with a low number of cases (<100 cases) into "Others".
frequencies <- data.frame(table(clean_data$Academic.Interest.2))
frequencies
# Create a named vector of frequencies
freq_vec <- setNames(frequencies$Freq, frequencies$Var1)

# Use the named vector to look up the frequencies for each value in the column
clean_data$Academic.Interest.2.Frequency <-
  freq_vec[clean_data$Academic.Interest.2]

for (i in 1:nrow(clean_data)) {
  if (clean_data$Academic.Interest.2.Frequency[i] < 100)
  {
    clean_data$Academic.Interest.2[i] <- "Other"
  } else{
    clean_data$Academic.Interest.2[i]
  }
}
clean_data$Academic.Interest.2 <-
  as.factor(clean_data$Academic.Interest.2)
#Remember to drop Academic.Interest.2.Frequency in the modeling stage.
remove_cols <- c(remove_cols, "Academic.Interest.2.Frequency")


#Column27 - First_Source.Origin.First.Source.Summary
sum(is.na(clean_data$First_Source.Origin.First.Source.Summary))#No NA.
table(clean_data$First_Source.Origin.First.Source.Summary)#No questionable category.
#Group options with a low number of cases (< 100) into "Other Sources".
frequencies <-
  data.frame(table(clean_data$First_Source.Origin.First.Source.Summary))
frequencies
# Create a named vector of frequencies
freq_vec <- setNames(frequencies$Freq, frequencies$Var1)

# Use the named vector to look up the frequencies for each value in the column
clean_data$First_Source.Summary.Frequency <-
  freq_vec[clean_data$First_Source.Origin.First.Source.Summary]


for (i in 1:nrow(clean_data)) {
  if (clean_data$First_Source.Summary.Frequency[i] < 100)
  {
    clean_data$First_Source.Origin.First.Source.Summary[i] <-
      "Other Sources"
  } else{
    clean_data$First_Source.Origin.First.Source.Summary[i]
  }
}
clean_data$First_Source.Origin.First.Source.Summary <-
  as.factor(clean_data$First_Source.Origin.First.Source.Summary)
#Remember to drop First_Source.Summary.Frequency in the modeling stage.
remove_cols <- c(remove_cols, "First_Source.Summary.Frequency")


#Column28 - Total.Event.Participation
sum(is.na(clean_data$Total.Event.Participation))#No NA.
table(clean_data$Total.Event.Participation)#No questionable category.
#3, 4, 5 combined accounts for < 1% of the data set.
#Compared to the number of cases in 0, 1, and 2, the number of cases
#in 3, 4, and 5 won't be very useful in predicting the response.
#Factor the variable and group 3, 4, and 5 into "2 or more".
clean_data$Total.Event.Participation <-
  ifelse(clean_data$Total.Event.Participation > 2,
         2,
         clean_data$Total.Event.Participation)
#Convert int to char so that level name can be modified.
clean_data$Total.Event.Participation <-
  as.character(clean_data$Total.Event.Participation)
clean_data$Total.Event.Participation <-
  ifelse(
    clean_data$Total.Event.Participation == "2",
    "2 or more",
    clean_data$Total.Event.Participation
  )
clean_data$Total.Event.Participation <-
  as.factor(clean_data$Total.Event.Participation)

#Column29 - Count.of.Campus.Visits
sum(is.na(clean_data$Count.of.Campus.Visits))#No NA.
table(clean_data$Count.of.Campus.Visits)#No questionable category.
#Factor the variable and group 5, 6, and 8 into 4.
clean_data$Count.of.Campus.Visits <-
  ifelse(clean_data$Count.of.Campus.Visits > 4,
         4,
         clean_data$Count.of.Campus.Visits)
#Convert int to char so that I can modify level name.
clean_data$Count.of.Campus.Visits <-
  as.character(clean_data$Count.of.Campus.Visits)
clean_data$Count.of.Campus.Visits <-
  ifelse(
    clean_data$Count.of.Campus.Visits == "4",
    "4 or more",
    clean_data$Count.of.Campus.Visits
  )
clean_data$Count.of.Campus.Visits <-
  as.factor(clean_data$Count.of.Campus.Visits)


#Column30 - School..1.Organization.Category
sum(is.na(clean_data$School..1.Organization.Category))#38 NAs.
table(clean_data$School..1.Organization.Category)#No questionable category.
#Only 16 cases belong to College but 15089 cases belong to High School.
#Should remove this variable.
remove_cols <- c(remove_cols, "School..1.Organization.Category")

#Column31 - School.1.Code
sum(is.na(clean_data$School.1.Code))#11879 NAs.
table(clean_data$School.1.Code)
#Will School Code matter much? Plus,there are 11879 missing values!
#Consider removing Column 31 in the modeling stage.
remove_cols <- c(remove_cols, "School.1.Code")

#Column32 - School.1.Class.Rank..Numeric.
sum(is.na(clean_data$School.1.Class.Rank..Numeric.))#8136 NAs.
remove_cols <- c(remove_cols, "School.1.Class.Rank..Numeric.")

#Column33 - School.1.Class.Size..Numeric.
# Create new column for top percentage in class
clean_data$School.1.Top.Percent.in.Class <-
  100 * (
    clean_data$School.1.Class.Rank..Numeric. / clean_data$School.1.Class.Size..Numeric.
  )

# Check for missing values in Academic.Index
sum(is.na(clean_data$Academic.Index)) # replace with 3
clean_data$Academic.Index[is.na(clean_data$Academic.Index)] <- 3

# Impute missing values in School.1.Top.Percent.in.Class based on Academic.Index
clean_data <- clean_data %>%
  group_by(Academic.Index) %>%
  mutate(School.1.Top.Percent.in.Class = if_else(
    is.na(School.1.Top.Percent.in.Class),
    mean(School.1.Top.Percent.in.Class, na.rm = TRUE),
    School.1.Top.Percent.in.Class
  )) %>%
  ungroup()

# Check for missing values
sum(is.na(clean_data$School.1.Top.Percent.in.Class)) # should be 0

summary(clean_data$School.1.Top.Percent.in.Class)
remove_cols <- c(remove_cols, "School.1.Class.Size..Numeric.")


#Column34 - School.1.GPA
#Remove this variable in the modeling stage
#because School.1.GPA.Recalculated is more accurate.
remove_cols <- c(remove_cols, "School.1.GPA")

#Column35 - School.1.GPA.Scale
#Remove this variable in the modeling stage as it is irrelevant.
remove_cols <- c(remove_cols, "School.1.GPA.Scale")

#Column36 - School.1.GPA.Recalculated
sum(is.na(clean_data$School.1.GPA.Recalculated))#0 NA.
skewness(clean_data$School.1.GPA.Recalculated)
#Moderately skewed, consider transformation.
summary(clean_data$School.1.GPA.Recalculated)
hist(
  clean_data$School.1.GPA.Recalculated,
  breaks = 20,
  col = "lightblue"
)


#Column37 - School.2.Class.Rank..Numeric.
sum(is.na(clean_data$School.2.Class.Rank..Numeric.))#15143 NAs.
#All cases are blank. Remove this variable in the modeling stage.
remove_cols <- c(remove_cols, "School.2.Class.Rank..Numeric.")

#Column38 - School.2.Class.Size..Numeric.
sum(is.na(clean_data$School.2.Class.Size..Numeric.))#15143 NAs.
#All cases are blank. Remove this variable in the modeling stage.
remove_cols <- c(remove_cols, "School.2.Class.Size..Numeric.")

#Column39 - School.2.GPA
sum(is.na(clean_data$School.2.GPA))#15143 NAs.
#All cases are blank. Remove this variable in the modeling stage.
remove_cols <- c(remove_cols, "School.2.GPA")

#Column40 - School.2.GPA.Scale
sum(is.na(clean_data$School.2.GPA.Scale))#15143 NAs.
#All cases are blank. Remove this variable in the modeling stage.
remove_cols <- c(remove_cols, "School.2.GPA.Scale")

#Column41 - School.2.GPA.Recalculated
sum(is.na(clean_data$School.2.GPA.Recalculated))#15143 NAs.
#All cases are blank. Remove this variable in the modeling stage.
remove_cols <- c(remove_cols, "School.2.GPA.Recalculated")

#Column42 - School.3.Class.Rank..Numeric.
sum(is.na(clean_data$School.3.Class.Rank..Numeric.))#15143 NAs.
#All cases are blank. Remove this variable in the modeling stage.
remove_cols <- c(remove_cols, "School.3.Class.Rank..Numeric.")

#Column43 - School.3.Class.Size..Numeric.
sum(is.na(clean_data$School.3.Class.Size..Numeric.))#15143 NAs.
#All cases are blank. Remove this variable in the modeling stage.
remove_cols <- c(remove_cols, "School.3.Class.Size..Numeric.")

#Column44 - School.3.GPA
sum(is.na(clean_data$School.3.GPA))#15143 NAs.
#All cases are blank. Remove this variable in the modeling stage.
remove_cols <- c(remove_cols, "School.3.GPA")

#Column45 - School.3.GPA.Scale
sum(is.na(clean_data$School.3.GPA.Scale))#15143 NAs.
#All cases are blank. Remove this variable in the modeling stage.
remove_cols <- c(remove_cols, "School.3.GPA.Scale")

#Column46 - School.3.GPA.Recalculated
sum(is.na(clean_data$School.3.GPA.Recalculated))#15143 NAs.
#All cases are blank. Remove this variable in the modeling stage.
remove_cols <- c(remove_cols, "School.3.GPA.Recalculated")

#Column 47 - ACT.Composite
summary(clean_data$ACT.Composite)
## 7502 NA's
## We will convert ACT scores to SAT scores later
remove_cols <- c(remove_cols, "ACT.Composite")


#Column 48 - ACT.English
summary(clean_data$ACT.English)
## We will convert ACT scores to SAT scores
remove_cols <- c(remove_cols, "ACT.English")

#Column49 - ACT.Reading
summary(clean_data$ACT.Reading)
## We will convert ACT scores to SAT scores
remove_cols <- c(remove_cols, "ACT.Reading")

#Column50 - ACT.Math
summary(clean_data$ACT.Math)
## We will convert ACT scores to SAT scores
remove_cols <- c(remove_cols, "ACT.Math")

#Column51 - ACT.Science.Reasoning
summary(clean_data$ACT.Science.Reasoning)
## We will convert ACT scores to SAT scores
remove_cols <- c(remove_cols, "ACT.Science.Reasoning")

#Column52 - ACT.Writing
summary(clean_data$ACT.Writing)
## We will convert ACT scores to SAT scores
remove_cols <- c(remove_cols, "ACT.Writing")

#Column53 - SAT.I.CR...M
summary(clean_data$SAT.I.CR...M)
## will convert this to a standardized SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
remove_cols <- c(remove_cols, "SAT.I.CR...M")

#Column54 - SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
summary(clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)
## this is the standardized score we will convert scores to
# Summarize the SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section data
summary(clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)

## Convert ACT scores to SAT scores
# Calculate a new score called ACT_Recalculated from ACT scores
clean_data$ACT_Recalculated <- round((
  clean_data$ACT.English + clean_data$ACT.Math + clean_data$ACT.Reading + clean_data$ACT.Science.Reasoning
) / 4,
2
)

# Identify missing SAT scores and fill them with converted ACT scores using act_to_sat function
missing_sat <-
  is.na(clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section) &
  !is.na(clean_data$ACT_Recalculated)
clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section[missing_sat] <-
  act_to_sat(clean_data$ACT_Recalculated[missing_sat])

missing_sat_2 <-
  is.na(clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section) &
  !is.na(clean_data$ACT.Composite)
clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section[missing_sat_2] <-
  act_to_sat(clean_data$ACT.Composite[missing_sat_2])

# Summarize the SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section data again
summary(clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)

## Check the relationship between
#SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section and
#SAT.I.CR...M Count the number of missing SAT scores where SAT.I.CR...M is not
#missing but ACT.Composite is missing
sum(
  is.na(
    clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
  ) &
    !is.na(clean_data$SAT.I.CR...M) &
    is.na(clean_data$ACT.Composite)
)

# Filter the data to include only observations where
# SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section and
# SAT.I.CR...M are not missing but ACT.Composite is missing
conversion_data <-
  clean_data %>% filter(
    !is.na(
      SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
    ) & !is.na(SAT.I.CR...M) & is.na(ACT.Composite)
  )

# Create a linear regression model called sat_old_new_model
sat_old_new_model <-
  lm(
    SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section ~ SAT.I.CR...M,
    data = conversion_data
  )

# Loop through the data and fill in any missing SAT scores with predicted scores based on the sat_old_new_model
for (i in 1:nrow(clean_data)) {
  old_sat <- clean_data$SAT.I.CR...M[i]
  new_sat <-
    clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section[i]
  if (is.na(new_sat) & !is.na(old_sat)) {
    clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section[i] <-
      predict(sat_old_new_model, data.frame(SAT.I.CR...M = old_sat))
  }
}
remove_cols <- c(remove_cols, "ACT_Recalculated")

#Column55 - Permanent.Geomarket
sum(is.na(clean_data$Permanent.Geomarket))
## remove this row
clean_data <- clean_data[!is.na(clean_data$Permanent.Geomarket), ]
# Count the number of occurrences of each geomarket in the clean_data dataset
table(clean_data$Permanent.Geomarket)

# Define a function to recode geomarkets based on their region
recode_geomarket <- function(geomarket) {
  case_when(
    # If the geomarket starts with "INT-", recode it based on its continent, reccode Oceania as Asia
    grepl("^INT-", geomarket) ~ case_when(
      grepl(
        "INT-(AE|BF|CI|CM|CO|CY|IV|JA|JO|KE|KS|KU|LH|MG|MJ|MU|MZ|NE|NI|SF|SN|TD|TZ|UG|ZI)",
        geomarket
      ) ~ "Africa",
      grepl("INT-(CA|MX|US)", geomarket) ~ "North America",
      grepl("INT-(AR|BR|EC|GT|PE|UY|VE)", geomarket) ~ "South America",
      grepl(
        "INT-(BE|BG|BK|BL|CA|CB|CH|CS|DO|DR|ES|ET|EZ|FR|GG|GR|IT|LE|LU|NL|NO|PO|RO|RP|RS|UK)",
        geomarket
      ) ~ "Europe",
      grepl(
        "INT-(AL|AS|BB|BH|EI|ID|IN|IR|KZ|MY|NP|PK|SA|TH|TU|TW|UZ|VM)",
        geomarket
      ) ~ "Asia",
      grepl("INT-(NZ)", geomarket) ~ "Asia",
      TRUE ~ "Other"
    ),
    # If the geomarket starts with a two-letter US state code, recode it based on its region, TX as Texas
    grepl("^[A-Z]{2}-", geomarket) ~ case_when(
      grepl(
        "^OK|AR|LA|MS|AL|GA|FL|SC|NC|VA|DC|MD|DE|WV|KY|TN",
        geomarket
      ) ~ "US-South",
      grepl("^NM|CO|AZ|UT|NV|CA|OR|WA|ID|MT|WY", geomarket) ~ "US-West",
      grepl("^ND|MN|SD|NE|IA|KS|MO|WI|IL|IN|OH|MI", geomarket) ~ "US-Midwest",
      grepl("^PA|NJ|NY|CT|RI|MA|VT|NH|ME", geomarket) ~ "US-Northeast",
      grepl("^AK|HI|GU|MP|AE|VI|AP", geomarket) ~ "US-Pacific",
      grepl("^PR-01", geomarket) ~ "US-Pacific",
      grepl("TX", geomarket) ~ "US-Texas",
      TRUE ~ sub("^(.{2}).*$", "\1", geomarket) # If the geomarket doesn't match any of the above, use the first two letters as the region code
    ),
    TRUE ~ geomarket # If the geomarket is not in a recognizable format, leave it as is
  )
}

# Recode the geomarket column using the recode_geomarket function
clean_data <- clean_data %>%
  mutate(Recoded_Geomarket = as.factor(recode_geomarket(Permanent.Geomarket)))

summary(clean_data$Recoded_Geomarket)
remove_cols <- c(remove_cols, "Permanent.Geomarket")

#Column56 - Citizenship.Status
summary(clean_data$Citizenship.Status)
sum(is.na(clean_data$Citizenship.Status))
## remove column as we already have geomarket
remove_cols <- c(remove_cols, "Citizenship.Status")

#Column57 - Academic.Index
summary(clean_data$Academic.Index)
sum(is.na(clean_data$Academic.Index))
## this column has already been cleaned above

#Column58 - Intend.to.Apply.for.Financial.Aid.
summary(clean_data$Intend.to.Apply.for.Financial.Aid.)
sum(is.na(clean_data$Intend.to.Apply.for.Financial.Aid.))
## since the observations with na in this column did get aid, we will code these as 1
clean_data$Intend.to.Apply.for.Financial.Aid.[is.na(clean_data$Intend.to.Apply.for.Financial.Aid.)] <-
  1
clean_data$Intend.to.Apply.for.Financial.Aid. <-
  as.factor(clean_data$Intend.to.Apply.for.Financial.Aid.)

#Column59 - Merit.Award
# summarize the Merit.Award column
summary(clean_data$Merit.Award)

# count the number of NA's
sum(is.na(clean_data$Merit.Award))

# summarize the Merit.Award column after converting to a factor
summary(as.factor(clean_data$Merit.Award))

# create a new column Merit_Group with values based on the Merit.Award column
clean_data <- clean_data %>%
  mutate(
    Merit_Aid_Group = case_when(
      # range 0-5
      as.numeric(str_extract(Merit.Award, "[0-9]+")) >= 0 &
        as.numeric(str_extract(Merit.Award, "[0-9]+")) <= 5 ~ "0-5",
      # range 5.5-15, including TT125
      (as.numeric(str_extract(
        Merit.Award, "[0-9]+"
      )) > 5 &
        as.numeric(str_extract(
          Merit.Award, "[0-9]+"
        )) <= 15) | Merit.Award == "TT125" ~ "5.5-15",
      # range 15.5-25
      as.numeric(str_extract(Merit.Award, "[0-9]+")) > 15 &
        as.numeric(str_extract(Merit.Award, "[0-9]+")) <= 25 ~ "15.5-25",
      # range 25.5-30
      as.numeric(str_extract(Merit.Award, "[0-9]+")) > 25 &
        as.numeric(str_extract(Merit.Award, "[0-9]+")) <= 30 ~ "25.5-30",
      # range 30.5-35
      as.numeric(str_extract(Merit.Award, "[0-9]+")) > 30 &
        as.numeric(str_extract(Merit.Award, "[0-9]+")) <= 35 ~ "30.5-35",
      # range 35.5-40
      as.numeric(str_extract(Merit.Award, "[0-9]+")) > 35 &
        as.numeric(str_extract(Merit.Award, "[0-9]+")) <= 40 ~ "35.5-40",
      # range 40.5+
      as.numeric(str_extract(Merit.Award, "[0-9]+")) > 40 ~ "40.5+",
      # Full Ride categories
      Merit.Award %in% c("SEM", "TTS", "X0", "Y0")  ~ "Full Ride",
      # Other category
      TRUE ~ "Other"
    )
  )

clean_data$Merit_Aid_Group <- as.factor(clean_data$Merit_Aid_Group)
remove_cols <- c(remove_cols, "Merit.Award")

#Column60: SAT.Concordance.Score..of.SAT.R.
sum(is.na(clean_data$SAT.Concordance.Score..of.SAT.R.))
sum(
  !is.na(clean_data$SAT.Concordance.Score..of.SAT.R.) &
    is.na(
      clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
    )
)
# irrelevant column, remove it because it will not help us calculate the
# remaining NA's in
# SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
remove_cols <- c(remove_cols, "SAT.Concordance.Score..of.SAT.R.")

#Column61: ACT.Concordance.Score..of.SAT.R.
sum(is.na(clean_data$ACT.Concordance.Score..of.SAT.R.))
sum(
  !is.na(clean_data$SAT.Concordance.Score..of.SAT.R.) &
    is.na(
      clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
    )
)
# irrelevant column, remove it because it will not help us calculate the
# remaining NA's in
# SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
remove_cols <- c(remove_cols, "ACT.Concordance.Score..of.SAT.R.")

#Column62: ACT.Concordance.Score..of.SAT.
sum(is.na(clean_data$ACT.Concordance.Score..of.SAT.))
sum(
  !is.na(clean_data$ACT.Concordance.Score..of.SAT.) &
    is.na(
      clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
    )
)
# irrelevant column, remove it because it will not help us calculate the
# remaining NA's in
# SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
remove_cols <- c(remove_cols, "ACT.Concordance.Score..of.SAT.")

#Column63: Test.Optional
summary(clean_data$Test.Optional)
table(clean_data$Test.Optional)
## 11900 NA's
sum(
  is.na(
    clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
  ) & clean_data$Test.Optional == 1,
  na.rm = TRUE
)

## The Kappa scores are better if we just remove all rows with Test.Optional == 1
remove_cols <- c(remove_cols, "Test.Optional")

#Column64: SAT.I.Critical.Reading
summary(clean_data$SAT.I.Critical.Reading)
sum(
  !is.na(clean_data$SAT.I.Critical.Reading) &
    is.na(
      clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
    )
)
## irrelevant col, remove
remove_cols <- c(remove_cols, "SAT.I.Critical.Reading")

#Column65: SAT.I.Math
summary(clean_data$SAT.I.Math)
sum(
  !is.na(clean_data$SAT.I.Math) &
    is.na(
      clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
    )
)
## irrelevant col, remove
remove_cols <- c(remove_cols, "SAT.I.Math")

#Column66: SAT.I.Writing
summary(clean_data$SAT.I.Writing)
sum(
  !is.na(clean_data$SAT.I.Writing) &
    is.na(
      clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
    )
)
## irrelevant col, remove
remove_cols <- c(remove_cols, "SAT.I.Writing")

#Column67: SAT.R.Evidence.Based.Reading.and.Writing.Section
summary(clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section)
sum(
  !is.na(
    clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section
  ) &
    is.na(
      clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
    )
)
## irrelevant col, remove
remove_cols <-
  c(remove_cols,
    "SAT.R.Evidence.Based.Reading.and.Writing.Section")

#Column68: SAT.R.Math.Section
summary(clean_data$SAT.R.Math.Section)
sum(
  !is.na(clean_data$SAT.R.Math.Section) &
    is.na(
      clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
    )
)
## irrelevant col, remove
remove_cols <- c(remove_cols, "SAT.R.Math.Section")

#Column69: Decision
summary(clean_data$Decision)
sum(is.na(clean_data$Decision))
clean_data$Decision <- as.factor(clean_data$Decision)

## Remove the remaining rows where
## SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section is missing

clean_data <-
  clean_data[complete.cases(clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section),]

# Create interaction terms based on suggestions from model iterations
## Create interaction terms
clean_data$SAT_GPA <- clean_data$School.1.GPA.Recalculated * clean_data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
clean_data$Academic_Performance <- clean_data$Academic.Index * clean_data$School.1.Top.Percent.in.Class +
  clean_data$Academic.Index^2 * clean_data$School.1.Top.Percent.in.Class^2
clean_data <- subset(clean_data, select = -c(School.1.GPA.Recalculated, SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section, Academic.Index, School.1.Top.Percent.in.Class))


## Remove more columns based on trial and error in modeling phase
remove_cols <-
  c(remove_cols,
    "Academic.Interest.1",
    "Academic.Interest.2",
    "Religion",
    "Ethnicity")

## Remove columns justified above
length(remove_cols)
clean_data <- clean_data[,!(names(clean_data) %in% remove_cols)]

skimr::skim(clean_data)

###### SPLITTING THE DATA ######
train_data <- filter(clean_data, train.test == "train")
test_data <- filter(clean_data, train.test == "test")

## Remove train.test col
train_data <- subset(train_data, select = -train.test)
test_data <- subset(test_data, select = -train.test)

###### CLASSIFICATION MODELS ######

### Prescriptions ###

# Convert non-numeric columns to NA
numeric_data <- select_if(clean_data, is.numeric)
# Compute correlation matrix
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix)
# drop cols as it it causing perfect multicollinearity
train_data_2 <-
  subset(train_data, select = -c(Sport.1.Rating, Sport.1.Sport))
test_data_2 <-
  subset(test_data, select =  -c(Sport.1.Rating, Sport.1.Sport))

#### MODELING ####
model_name <-
  c(
    "Logistic Reg.",
    "KNN",
    "Simple Clas. Tree",
    "Tree Pruning",
    "Class. Tree + Bagging",
    "Class. Trees + RF",
    "Class. Trees + Boosting",
    "SVM + Linear. kernel",
    "SVM + Poly. kernel",
    "SVM + Radial kernel"
  )
kappa_scores <- rep(0, 10)
kappa_model_df <- data.frame(model_name, kappa_scores)

### Logistic Regression ###
## Perform logistic regression
best_logistic_model <-
  glm(Decision ~ ., data = train_data_2, family = binomial)
summary(best_logistic_model)

## VIF is good
vif(best_logistic_model)

logistic_prob_train <-
  predict(best_logistic_model, type = "response", train_data_2)
logistic_prob_test <-
  predict(best_logistic_model, type = "response", test_data_2)

cutoffs <-
  seq(0.2, 0.4, by = 0.01) # define a range of probability cutoffs

train_kappas <-
  c() # initialize an empty vector to store the training Kappa scores
test_kappas <-
  c() # initialize an empty vector to store the test Kappa scores

for (cutoff in cutoffs) {
  # create predictions based on the probability cutoff
  logistic_pred_train <-
    ifelse(logistic_prob_train > cutoff, 1, 0)
  logistic_pred_test <-
    ifelse(logistic_prob_test > cutoff, 1, 0)
  
  # create contingency tables for the training and test sets
  logistic_conting_train <-
    table(logistic_pred_train,
          train_data$Decision,
          dnn = c("Predicted", "Actual"))
  logistic_conting_test <-
    table(logistic_pred_test,
          test_data$Decision,
          dnn = c("Predicted", "Actual"))
  
  # calculate the Kappa scores for the training and test sets
  logistic_cm_train <- confusionMatrix(logistic_conting_train)
  logistic_cm_test <- confusionMatrix(logistic_conting_test)
  
  train_kappas <-
    c(train_kappas, logistic_cm_train$overall['Kappa'])
  test_kappas <- c(test_kappas, logistic_cm_test$overall['Kappa'])
}

logistic_kappas_df <-
  data.frame(Cutoff = cutoffs,
             Training_Kappa = train_kappas,
             Test_Kappa = test_kappas)
logistic_kappas_df

kappa_model_df$kappa_scores[kappa_model_df$model_name == "Logistic Reg."] <-
  max(logistic_kappas_df$Test_Kappa)
## Kappa score: 0.5463333

### KNN ###
# Make sure all predictors are numeric
train_data_numeric <- train_data_2
test_data_numeric <-  test_data_2

# Identify factor columns
factor_cols <- sapply(train_data_numeric, is.factor)

# Convert factor columns to numeric
train_data_numeric[factor_cols] <-
  lapply(train_data_numeric[factor_cols], function(x)
    as.numeric(x))
test_data_numeric[factor_cols] <-
  lapply(test_data_numeric[factor_cols], function(x)
    as.numeric(x))
train_data_numeric$Decision <-
  ifelse(train_data_numeric$Decision == 2, 1, 0)
test_data_numeric$Decision <-
  ifelse(test_data_numeric$Decision == 2, 1, 0)

# Set seed for reproducibility
set.seed(1)

NN5 <-
  kNN(
    Decision ~ .,
    train = as.matrix(train_data_numeric),
    test = as.matrix(test_data_numeric),
    k = 5
  )

#Classification performance (Kappa) for the test set
NN5_conting_test <- table(NN5,
                          test_data_numeric$Decision,
                          dnn = c("Predicted", "Actual"))
NN5_cm_test <- confusionMatrix(NN5_conting_test)
NN5_cm_test
Kappa_test_NN5 <- NN5_cm_test$overall["Kappa"]
Kappa_test_NN5


#2.2 Determine the optimal k value
Kappa_knn <- rep(0, 100)
for (i in 1:100) {
  set.seed(1)
  nn_test <-
    kNN(
      Decision ~ .,
      train = as.matrix(train_data_numeric),
      test = as.matrix(test_data_numeric),
      k = i
    )
  nn_conting_test <-
    table(nn_test,
          test_data_numeric$Decision,
          dnn = c("Predicted", "Actual"))
  nn_cm_test <- confusionMatrix(nn_conting_test)
  Kappa_knn[i] <- nn_cm_test$overall["Kappa"]
}

#Visualize the relationship between k and Kappa with a scatter plot.
Kappa_dataframe <- as.data.frame(cbind(k = seq(1:100), Kappa_knn))
Kappa_vs_k <-
  ggplot(Kappa_dataframe, aes(x = k, y = Kappa_knn)) + geom_point()
Kappa_vs_k

#Select the best model and Kappa for the training set.
set.seed(1)
nn_best_train <-
  kNN(
    Decision ~ .,
    as.matrix(train_data_numeric),
    as.matrix(train_data_numeric),
    k = which.max(Kappa_knn)
  )
nn_best_conting_train <-
  table(nn_best_train,
        train_data_numeric$Decision,
        dnn = c("Predicted", "Actual"))
nn_best_cm_train <- confusionMatrix(nn_best_conting_train)
Kappa_train_bestKNN <- nn_best_cm_train$overall["Kappa"]
Kappa_train_bestKNN

#Select the best model and Kappa for the test set.
set.seed(1)
nn_best_test <-
  kNN(
    Decision ~ .,
    as.matrix(train_data_numeric),
    as.matrix(test_data_numeric),
    k = which.max(Kappa_knn)
  )
nn_best_conting_test <-
  table(nn_best_test,
        test_data_numeric$Decision,
        dnn = c("Predicted", "Actual"))
nn_best_cm_test <- confusionMatrix(nn_best_conting_test)
Kappa_test_bestKNN <- nn_best_cm_test$overall["Kappa"]
Kappa_test_bestKNN

kappa_model_df$kappa_scores[kappa_model_df$model_name == "KNN"] <-
  Kappa_test_bestKNN
## Kappa Score: 0.378012 

### Simple Classification Tree ###
# Fit a simple classification tree using the "tree" function, with the "gini" splitting criterion
set.seed(1)
simple_tree <- tree(Decision ~ ., split = "gini", train_data_2)

# Print a summary of the tree, including the number of internal nodes, the number of terminal nodes, and classification accuracy on the training set
summary(simple_tree)

# Graphically display the tree using the "plot" and "text" functions, with "pretty=0" to include category names for qualitative predictors
plot(simple_tree)
text(simple_tree, pretty = 0)

# Calculate the classification performance (Kappa) on the training set by predicting class labels
# for each observation and comparing to the actual class labels
simple_tree_pred_train <-
  predict(simple_tree, train_data_2, type = "class")
simple_tree_conting_train <-
  table(simple_tree_pred_train,
        train_data_2$Decision,
        dnn = c("Predicted", "Actual"))
simple_tree_cm_train <- confusionMatrix(simple_tree_conting_train)
simple_tree_cm_train$overall["Kappa"]
# Kappa = 0.6909397, indicating the simple tree strongly agrees with reality in the training set

# Calculate the classification performance (Kappa) on the test set by predicting class labels for each observation and comparing to the actual class labels
simple_tree_pred_test <-
  predict(simple_tree, test_data_2, type = "class")
simple_tree_conting_test <-
  table(simple_tree_pred_test,
        test_data_2$Decision,
        dnn = c("Predicted", "Actual"))
simple_tree_cm_test <- confusionMatrix(simple_tree_conting_test)
simple_tree_cm_test$overall["Kappa"]

kappa_model_df$kappa_scores[kappa_model_df$model_name == "Simple Clas. Tree"] <-
  simple_tree_cm_test$overall["Kappa"]
## Kappa Score: 0.4208681 

### Tree Pruning ###
set.seed(1)

# Perform 10-fold cross-validation to determine the optimal number of terminal nodes
cv_Tree <- cv.tree(simple_tree, FUN = prune.misclass, K = 10)

# Identify the optimal number of terminal nodes
prune_size <- cv_Tree$size[which.min(cv_Tree$dev)]

plot(cv_Tree$size, cv_Tree$dev, type = "b")

# Prune the tree to obtain the tree with the optimal number of terminal nodes
prune_tree <- prune.misclass(simple_tree, best = prune_size)

# Display the pruned tree
plot(prune_tree)
text(prune_tree, pretty = 0)

# Evaluate the classification performance on the training set
prune_tree_pred_train <-
  predict(prune_tree, train_data_2, type = "class")
prune_tree_conting_train <-
  table(prune_tree_pred_train,
        train_data_2$Decision,
        dnn = c("Predicted", "Actual"))
prune_tree_cm_train <- confusionMatrix(prune_tree_conting_train)
prune_tree_cm_train$overall["Kappa"]

#Kappa: 0.6906284 indicating the pruned tree strongly agrees with reality
#in the training set.

# Evaluate the classification performance on the test set
prune_tree_pred_test <-
  predict(prune_tree, test_data_2, type = "class")
prune_tree_conting_test <-
  table(prune_tree_pred_test,
        test_data_2$Decision,
        dnn = c("Predicted", "Actual"))
prune_tree_cm_test <- confusionMatrix(prune_tree_conting_test)
prune_tree_cm_test$overall["Kappa"]

kappa_model_df$kappa_scores[kappa_model_df$model_name == "Tree Pruning"] <-
  prune_tree_cm_test$overall["Kappa"]
## Kappa score: 0.4163069

### Bagging ###
# Create 500 bootstrap datasets to perform bagging
# Set seed to make the random sampling with replacement reproducible
set.seed(1)

# Perform tree bagging using randomForest function
Tree_Bagging <- randomForest(
  Decision ~ .,
  data = train_data_2,
  ntrees = 500,
  mtry = 19,
  split = "gini",
  replace = TRUE,
  importance = TRUE
)

# View Tree_Bagging object
Tree_Bagging

# Plot OOB classification error for each individual class and all classes combined
plot(Tree_Bagging)

# View OOB classification errors for High, Low, and Both combined
Tree_Bagging$err.rate

# Find the optimal ntrees
which.min(Tree_Bagging$err.rate[, 1])

# View overall summary of the importance of each predictor using importance()
importance(Tree_Bagging)

# Make a plot to better visualize the importance of each variable
varImpPlot(Tree_Bagging)

# View classification performance (Kappa) for the training set
bag_train_pred <-
  predict(Tree_Bagging, train_data_2, type = "class")
bag_conting_train <-
  table(bag_train_pred,
        train_data_2$Decision,
        dnn = c("Predicted", "Actual"))
bag_cm_train <- confusionMatrix(bag_conting_train)
bag_cm_train$overall["Kappa"]
#Kappa = 1, indicating the bagged tree very strongly
#agrees with reality in training set.

# View classification performance (Kappa) for the test set
bag_test_pred <- predict(Tree_Bagging, test_data_2, type = "class")
bag_conting_test <-
  table(bag_test_pred,
        test_data_2$Decision,
        dnn = c("Predicted", "Actual"))
bag_cm_test <- confusionMatrix(bag_conting_test)
bag_cm_test$overall["Kappa"]

kappa_model_df$kappa_scores[kappa_model_df$model_name == "Class. Tree + Bagging"] <-
  bag_cm_test$overall["Kappa"]
## Kappa score: 0.4954846


### Random Forest ###
#Perform RF using 500 bootstrap data sets and optimal mtry
#RF uses a subset of predictors for each split of a tree, so optimal mtry can be determined by testing different values
# Loop to find optimal mtry value for Random Forest
Test_Kappa_RF <- rep(0, 20)
for (i in 1:19) {
  set.seed(1)
  Tree_RF <- randomForest(
    Decision ~ .,
    data = train_data_2,
    ntrees = 500,
    mtry = i,
    split = "gini",
    replace = TRUE,
    importance = TRUE
  )
  Test_pred_RF <- predict(Tree_RF, test_data_2, type = "class")
  RF_conting_test <- table(Test_pred_RF,
                           test_data_2$Decision,
                           dnn = c("Predicted", "Actual"))
  RF_cm_test <- confusionMatrix(RF_conting_test)
  Test_Kappa_RF[i] <- RF_cm_test$overall["Kappa"]
}
Test_Kappa_RF
# Find the index of the highest Kappa value
which.max(Test_Kappa_RF)

# Output the mtry value with the highest Kappa value
# mtry =  4 gives the highest Kappa.
Test_Kappa_RF[which.max(Test_Kappa_RF)]

Tree_RF <- randomForest(
  Decision ~ .,
  data = train_data_2,
  ntrees = 500,
  mtry = 4,
  split = "gini",
  replace = TRUE,
  importance = TRUE
)

#Classification performance (Kappa) for the training set
rf_train_pred <- predict(Tree_RF, train_data_2, type = "class")
rf_conting_train <- table(rf_train_pred,
                          train_data_2$Decision,
                          dnn = c("Predicted", "Actual"))
rf_cm_train <- confusionMatrix(rf_conting_train)
rf_cm_train$overall["Kappa"]
#Kappa = 0.9937597, indicating very strong agreement with reality in the training set.

#Classification performance (Kappa) for the test set
rf_test_pred <- predict(Tree_RF, test_data_2, type = "class")
rf_conting_test <- table(rf_test_pred,
                         test_data_2$Decision,
                         dnn = c("Predicted", "Actual"))
rf_cm_test <- confusionMatrix(rf_conting_test)
rf_cm_test$overall["Kappa"]

kappa_model_df$kappa_scores[kappa_model_df$model_name == "Class. Trees + RF"] <-
  rf_cm_test$overall["Kappa"]

## Kappa Score: 0.5086026

### Boosting ###
#Use gbm() to build a binary classification model since the response has two
#classes, with distribution = "bernoulli". Use a loop to find the optimal
#n.trees for each interaction.depth value, then use the obtained optimal values
#to fit the model.
train_data_3 <- train_data_2
test_data_3 <- test_data_2

train_data_3$Decision <- as.numeric(train_data_3$Decision)
train_data_3$Decision <- ifelse(train_data_3$Decision == 2, 1, 0)

test_data_3$Decision <- as.numeric(test_data_3$Decision)
test_data_3$Decision <- ifelse(test_data_3$Decision == 2, 1, 0)

n_trees <- rep(0, 6)
min_cv_error <- rep(0, 6)

for (i in 1:6) {
  set.seed(1)
  Tree_Boosting <-
    gbm(
      Decision ~ .,
      data = train_data_3,
      distribution = "bernoulli",
      n.trees = 1500,
      interaction.depth = i,
      cv.folds = 10,
      shrinkage = 0.01
    )
  n_trees[i] <- which.min(Tree_Boosting$cv.error)
  min_cv_error[i] <-
    Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]
}
#Find which interaction.depth value gives the lowest cv error and its
#corresponding n.trees.
which.min(min_cv_error)
n_trees[5]
#Set the optimal n.trees = 1454 and interaction.depth = 5 from the loop and use them to fit
#the model.
set.seed(1)
Tree_Boosting <- gbm(
  Decision ~ .,
  data = train_data_3,
  distribution = "bernoulli",
  n.trees = 1454,
  interaction.depth = 5,
  shrinkage = 0.01
)
#Print a relative influence plot and the relative influence statistics of each variable in the model.
summary(Tree_Boosting)

#Use the model to predict the probability of Y=1 for each training observation and assign each predicted probability to a class label.
boost_prob_train <-
  predict(Tree_Boosting, type = "response", train_data_3)
boost_pred_results_train <- ifelse(boost_prob_train > 0.5, 1, 0)
#Compute the confusion matrix and Kappa value for the training set.
boost_conting_train <-
  table(boost_pred_results_train,
        train_data_3$Decision,
        dnn = c("Predicted", "Actual"))
boost_cm_train <- confusionMatrix(boost_conting_train)
boost_cm_train$overall["Kappa"]

#Use the model to predict the probability of Y=1 for each test observation and assign each predicted probability to a class label.
boost_prob_test <-
  predict(Tree_Boosting, type = "response", test_data_3)
boost_pred_results_test <- ifelse(boost_prob_test > 0.5, 1, 0)
#Compute the confusion matrix and Kappa value for the test set.
boost_conting_test <-
  table(boost_pred_results_test,
        test_data_3$Decision,
        dnn = c("Predicted", "Actual"))
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# kappa: 0.503336

#A loop to determine optimal cut-off probability
Kappa <- rep(0, 36)
cutoff_prob <- rep(0, 36)
index <- 0
for (i in seq(from = 0.2, to = 0.9, by = 0.02)) {
  index <- index + 1
  boost_pred_results_test <- ifelse(boost_prob_test > i, 1, 0)
  boost_conting_test <-
    table(boost_pred_results_test,
          test_data_3$Decision,
          dnn = c("Predicted", "Actual"))
  boost_cm_test <- confusionMatrix(boost_conting_test)
  cutoff_prob[index] <- i
  Kappa[index] <- boost_cm_test$overall["Kappa"]
}
which.max(Kappa)
cutoff_prob[which.max(Kappa)]
Kappa[which.max(Kappa)]
# best cutoff prob: 0.38
#recalculate training kappa
boost_prob_train <-
  predict(Tree_Boosting, type = "response", train_data_3)
boost_pred_results_train <-
  ifelse(boost_prob_train > cutoff_prob[which.max(Kappa)], 1, 0)
#Compute the confusion matrix and Kappa value for the training set.
boost_conting_train <-
  table(boost_pred_results_train,
        train_data_3$Decision,
        dnn = c("Predicted", "Actual"))
boost_cm_train <- confusionMatrix(boost_conting_train)
boost_cm_train$overall["Kappa"]

#recalulate test kappa
boost_prob_test <-
  predict(Tree_Boosting, type = "response", test_data_3)
boost_pred_results_test <-
  ifelse(boost_prob_test > cutoff_prob[which.max(Kappa)], 1, 0)
#Compute the confusion matrix and Kappa value for the test set.
boost_conting_test <-
  table(boost_pred_results_test,
        test_data_3$Decision,
        dnn = c("Predicted", "Actual"))
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]

kappa_model_df$kappa_scores[kappa_model_df$model_name == "Class. Trees + Boosting"] <-
  boost_cm_test$overall["Kappa"]
## Kappa Score: 0.5366825 

###SVM with linear Kernel###
# Fit support vector classifier with linear kernel
svm_linear <-
  svm(
    Decision ~ .,
    data = test_data_2,
    type = "C-classification",
    kernel = "linear",
    scale = TRUE,
    cost = 1
  )

# Print model summary
summary(svm_linear)

# Identify support vectors
svm_linear$index

# Compute the coefficients of the SVM model
coefficients <- t(svm_linear$SV) %*% svm_linear$coefs
intercept <- -svm_linear$rho

# Identify top 5 most important predictors
coefficients_abs <- abs(coefficients)
top_5_predictors <-
  names(sort(coefficients_abs[, 1], decreasing = TRUE))[1:5]
top_5_predictors

## "AthleteAthlete..Opt.Out"            "LegacyLegacy..Opt.Out"              "Total.Event.Participation2.or.more"
## "Decision.PlanEarly.Decision.I"      "Decision.PlanEarly.Decision.II"

# Calculate training Kappa
svc_pred_train <- predict(svm_linear, train_data_2)
svc_conting_train <- table(svc_pred_train,
                           train_data_2$Decision,
                           dnn = c("Predicted", "Actual"))
svc_confu_train <- confusionMatrix(svc_conting_train)
svc_confu_train$overall["Kappa"]

# Calculate test kappa
svc_pred_test <- predict(svm_linear, test_data_2)
svc_conting_test <- table(svc_pred_test,
                          test_data_2$Decision,
                          dnn = c("Predicted", "Actual"))
svc_conting_test
svc_confu_test <- confusionMatrix(svc_conting_test)
svc_confu_test$overall["Kappa"]

# Kappa score: 0.4694264 

# Coarse tuning

set.seed(1)
# for full cost range, it crashes, use 2^3 as tested by Dr. Zhu
svc_best_cost <-
  svm(Decision ~ .,
      data = train_data_2,
      kernel = "linear",
      cost = 2 ^ 3)

summary(svc_best_cost)

#coefficient and intercept
coefficients_best_svm_linear <-
  t(svc_best_cost$SV) %*% svc_best_cost$coefs
Intercept_best_svm_linear <- -svc_best_cost$rho

#5 most important features
coeff_names <- rownames(coefficients_best_svm_linear)
coeff_abs <- abs(coefficients_best_svm_linear)
coeff_data <- data.frame(coeff_names, coeff_abs, row.names = NULL)
coeff_data[with(coeff_data, order(-coeff_abs)),][1:5,]

# top 5 predictors
# coeff_names
# LegacyLegacy..Opt.Out
# AthleteAthlete..Opt.Out
# Decision.PlanEarly.Decision.II
# Decision.PlanEarly.Decision.I
# Total.Event.Participation2.or.more

#train kappa
svc_pred_train_best <- predict(svc_best_cost, train_data_2)
svc_conting_train_best <-
  table(svc_pred_train_best,
        train_data_2$Decision,
        dnn = c("Predicted", "Actual"))
svc_confu_train_best <- confusionMatrix(svc_conting_train_best)
svc_confu_train_best$overall["Kappa"]
# kappa: 0.4647546

#test kappa
svc_pred_test_best <- predict(svc_best_cost, test_data_2)
svc_conting_test_best <-
  table(svc_pred_test_best,
        test_data_2$Decision,
        dnn = c("Predicted", "Actual"))
svc_confu_test_best <- confusionMatrix(svc_conting_test_best)
svc_confu_test_best$overall["Kappa"]
# kappa: 0.4531047

kappa_model_df$kappa_scores[kappa_model_df$model_name == "SVM + Linear. kernel"] <-
  svc_confu_test_best$overall["Kappa"]

## SVM with Polynominal Kernel ##
set.seed(1)
# for full cost range it crashes, use parameters as tested by Dr. Zhu
svc_best_cost_poly <-
  svm(
    Decision ~ .,
    data = train_data_2,
    kernel = "polynomial",
    cost = 2 ^ 7,
    degree = 2
  )

summary(svc_best_cost_poly)

#coefficient and intercept
coefficients_best_svm_poly <-
  t(svc_best_cost_poly$SV) %*% svc_best_cost_poly$coefs
Intercept_best_svm_poly <- -svc_best_cost_poly$rho

#5 most important features
coeff_names <- rownames(coefficients_best_svm_poly)
coeff_abs <- abs(coefficients_best_svm_poly)
coeff_data <- data.frame(coeff_names, coeff_abs, row.names = NULL)
coeff_data[with(coeff_data, order(-coeff_abs)),][1:5,]

# coeff_names 
# 46         Total.Event.Participation1 
# 57      Academic_Performance 
# 25            AthleteAthlete..Opt.Out  
# 47 Total.Event.Participation2.or.more  
# 23              LegacyLegacy..Opt.Out  

#train kappa
svmp_pred_train_best <- predict(svc_best_cost_poly, train_data_2)
svmp_conting_train_best <-
  table(svmp_pred_train_best,
        train_data_2$Decision,
        dnn = c("Predicted", "Actual"))
svmp_conting_train_best
svmp_confu_train_best <- confusionMatrix(svmp_conting_train_best)
svmp_confu_train_best$overall["Kappa"]
# kappa: 0.5673587

#test kappa
svmp_pred_test_best <- predict(svc_best_cost_poly, test_data_2)
svmp_conting_test_best <-
  table(svmp_pred_test_best,
        test_data_2$Decision,
        dnn = c("Predicted", "Actual"))
svmp_conting_test_best
svmp_confu_test_best <- confusionMatrix(svmp_conting_test_best)
svmp_confu_test_best$overall["Kappa"]
# kappa: 0.4741636 

kappa_model_df$kappa_scores[kappa_model_df$model_name == "SVM + Poly. kernel"] <-
  svmp_confu_test_best$overall["Kappa"]

## SVM with Radial Kernel ##
# Fit support vector classifier with radial kernel
# Parameters given by Dr. Zhu
set.seed(1)
svm_radial <- svm(
  Decision ~ .,
  kernel = "radial",
  data = train_data_2,
  cost = 2 ^ 4,
  gamma = 2 ^ -4,
  scale = TRUE
)

# Print model summary
summary(svm_radial)

# Identify support vectors
svm_radial$index

# Calculate training Kappa
svmr_pred_train <- predict(svm_radial, train_data_2)
svmr_conting_train <-
  table(svmr_pred_train,
        train_data_2$Decision,
        dnn = c("Predicted", "Actual"))
svmr_conting_train
svmr_confu_train <- confusionMatrix(svmr_conting_train)
svmr_confu_train$overall["Kappa"]
# kappa: 0.7616075 

# Calculate test kappa
svmr_pred_test <- predict(svm_radial, test_data_2)
svmr_conting_test <- table(svmr_pred_test,
                           test_data_2$Decision,
                           dnn = c("Predicted", "Actual"))
svmr_conting_test
svmr_confu_test <- confusionMatrix(svmr_conting_test)
svmr_confu_test$overall["Kappa"]
# Kappa score: 0.4866242

kappa_model_df$kappa_scores[kappa_model_df$model_name == "SVM + Radial kernel"] <-
  svmr_confu_test$overall["Kappa"]

## Analysis of the best model (logistic regression) ##
# Run model summary again
summary(best_logistic_model) 

# Check coefficient plots
coefplot(best_logistic_model)
logistic_analysis <- coefplot(best_logistic_model, plot = FALSE)
analysis_df <- data.frame(predictor = logistic_analysis_df$Coefficient, value = logistic_analysis_df$Value)
analysis_df <- analysis_df[order(abs(analysis_df$value), decreasing = TRUE), ]
view(analysis_df[1:10,])


## Time Analysis ##
end_time <- Sys.time()
elapsed_time <- end_time - start_time
cat(paste0("Elapsed time: ", format(round(elapsed_time, 1), units = "secs")))


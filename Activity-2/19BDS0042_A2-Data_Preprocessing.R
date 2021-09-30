library(graphics)
library(dplyr, warn.conflicts = FALSE)
library(utils)
library(rvest)
library(stringr)


# Load the CSV File from Github URL and Replace Empty to NA
df <- read.csv('https://raw.githubusercontent.com/anthoniraj/datasets/main/data_cleaning/tweet.csv', na.strings = c("", "NA"))

#Remove NA Columns
df <- df[1:5]

# Remove rows misplaced or empty
df <- filter(df, favorited %in% c("TRUE", "FALSE"))

#Retrieve only rows contains twitter name from text column
df <- filter(df, grepl('\\@(.*?)\\:', text, ignore.case = T))

#Extract only user name from text column
df$text <- str_extract(df$text,'\\@(.*?)\\:') %>%
  str_sub(1, -2)

#Processing Date and Time
df[['created']] <- strptime(df[['created']], format = "%d-%m-%Y %H:%M")

#Convert Retweet column into numeric format
df[['retweetCount']] <- as.numeric(df[['retweetCount']])

#Filter the sub set with retweet count is greater than 1000
df <- filter(df,retweetCount>1000)

#Extract Text from HTML script from statusSource column and rebuild the data frame
plain_text <- function(source){
  html <- minimal_html(source)
  content <- html_elements(html, "a")
  return(html_text(content))
}
df$statusSource <- lapply(df$statusSource, plain_text)

head(df)

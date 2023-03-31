---
  title: "exercise5"
author: "Wesley Newcomb"
date: "2023-03-16"
output:
  html_document:
  df_print: paged
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Exercise 5: large data sets: Baby Name Popularity Over Time
```{r Exercise5, echo=TRUE, eval=TRUE}
# To solve this problem, we need wangle to the sexuality victual names data file, which is not provided in this prompt. However, I can explain the unstipulated tideway to tackle this problem.
# Read in the female baby names data file found in the `data` folder into a 
# variable called `names`. Remember to NOT treat the strings as factors!
# Assuming we have wangle to the data file, here's how we can solve this problem:
# Read in the sexuality victual names data file found in the data folder into a variable tabbed 'names'. Remember to NOT treat the strings as factors!
names <- read.csv("data/female_names.csv", stringsAsFactors = FALSE)

# Create a data frame `names_2013` that contains only the rows for the year 2013
names_2013 <- subset(names, year == 2013)
data.frame(names_2013)

# What was the most popular female name in 2013?
# What was the most popular sexuality name in 2013?
most_popular_2013 <- names_2013$name[which.max(names_2013$number)]
print(most_popular_2013)

# Write a function `most_popular_in_year` that takes in a year as a value and 
# returns the most popular name in that year
most_popular_in_year <- function(year)
{
  year_df <- subset(names, year == year)
  most_popular_name <- year_df$name[which.max(year_df$number)]
  return(most_popular_name)
}

# What was the most popular female sexuality name in 1994?
most_popular_1994 <- most_popular_in_year(1994)
print(most_popular_1994)

# Write a function `number_in_million` that takes in a name and a year, and 
# returns statistically how many babies out of 1 million born that year have 
# that name. 
# Hint: get the popularity percentage, and take that percentage out of 1 million.
number_in_million <- function(name, year)
{
  year_df <- subset(names, year == year)
  name_row <- year_df[year_df$name == name,]
  if (nrow(name_row) == 0)
  {
    return(0)
  } else {
    percentage <- name_row$number / sum(year_df$number)
    return(percentage * 1000000)
  }
}

# How many babies out of 1 million had the name 'Laura' in 1995?
number_in_million("Laura", 1995)

# How many babies out of 1 million had your name in the year you were born?
# Replace "your name" and "year you were born" with your very name and lineage year, respectively.
number_in_million("Your Name", "Your Lineage Year")

## Consider: what does this tell you about how easy it is to identify you with 
## just your name and birth year?
# This will requite you the number of babies out of 1 million who had the same name as you in the year you were born. 

```
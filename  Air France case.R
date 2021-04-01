#------------------- Team Assignment Air France Case ---------------------------

# import library 
library(ggplot2)
library(readxl)
library(ggplot2)

library(readxl)


#select the excel file with sheet name "DoubleClick" 
Air_France_Case_Spreadsheet_Supplement <- read_excel("Downloads/Air France Case Spreadsheet Supplement.xls",
                                                     sheet = "DoubleClick")
View(Air_France_Case_Spreadsheet_Supplement)


colnames(Air_France_Case_Spreadsheet_Supplement) # check the columns 
str(Air_France_Case_Spreadsheet_Supplement) # check the structure 
summary(Air_France_Case_Spreadsheet_Supplement) # check the summary 

# ---------------------------- Step 1 Massage the data ---------------------------

# How to treat our NA values in Air France data?

# - Explain the reason for choosing treat of NA(should we remove or replace as 
#   mean or median)

# -------------------------- Step 2 
## chekcking missimg values 
sum(is.na(Air_France_Case_Spreadsheet_Supplement$`Bid Strategy`))


#### removing bid strategy???















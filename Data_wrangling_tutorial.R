library(tidyverse)
library(readr)
UNghgdata <- read_csv("UNghgdata.csv")
View(UNghgdata)

rawdata <- UNghgdata

# Tidy up the data using Base R because it's in rows and columns. This is a nightmare!
#deleted last column (change)
cropdat <- rawdata[1:28]

#saved columns with the country name and the year in a data.frame
savecols <- data.frame(cropdat$Party, cropdat$`Last Inventory Year (2015)`)

# rename the columns in the savecolumns table
names(savecols) <- c('Party','2015')

#generate a ranking variable
savecols$rank2015 <- rank(-savecols$`2015`)

# save only the observations, for which rank is less than or equal to 10
top10df <- savecols[savecols$rank2015 <= 10,]

# Test if shorter vectors are in longer vectors using %in%; join two datasets; it will keep only the top 10 countries in the main dataset 
basedat <- cropdat[cropdat$Party %in% top10df$Party,]
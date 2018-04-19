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


##### NOW TRY TIDYVERSE
# doing this by column number because of annoying spaces in the column names 
dropcols <- select(rawdata, 1, 3:28)

# rename ridiculous last column 
namecols <- rename(dropcols, "2015" = `Last Inventory Year (2015)`)

# Now use the tidyverse
# gathers all but the first column
tidydat <- gather(namecols, key="year", value="emissions", -1) 

# Now plot emissions ver time for the top 10 emitting parties in 2015
# groups data by year; to ungroup use ungroup(x, ...)
groupdat <- group_by(tidydat, year) 

# #selects top or bottom entries in yeach group top_n(data, number of rows to return; 
# if n>0 => returns top rows; if n<0 => returns bottom n rows)
top10s <- top_n(groupdat, 10, emissions)

#Now I can filter the data to return top 10 emissioners in each year, but I want it for 2015 specifically
top10_2015 <-filter(top10s, year==2015)

# Other ways to do the same (without cleaning the data)
plotdat <- filter(ungroup(groupdat), Party %in% top10_2015$Party)

ghgplot <- ggplot(plotdat, aes(x=year, y=emissions, color=Party, group=Party)) +
  geom_point() + geom_line()
ghgplot
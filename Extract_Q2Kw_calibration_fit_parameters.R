# February 13, 2018

# Script to extract fit parameters from QUAL2Kw output

# Dan Sobota

# Load libraries----
library(tidyverse)

# Set working directory (modify as needed)----
setwd("\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Upper Yaquina River - 1710020401\\QUAL2Kw\\2016\\July")

Q2Kw.out <- list.files(pattern = "*.out")
Q2Kw.out <- Q2Kw.out[!grepl("oldph.out", Q2Kw.out)]

# scan in file and process----
Q2Kw.output <- scan(Q2Kw.out, what = "character", sep = "\n")

# Get grab sample water quality parameters used to fit model during autocalibration process----
# Parameters are: average, min, and max pH (from continuous data)
#                 CBODfast
#                 Organic N
#                 Ammonium N
#                 Nitrate + nitrite
#                 Organic P
#                 Inorganic P

# Will need to manually adjust what sites are extracted

# Average values for parameters

Avg.grab.loc <- grep("Daily average water quality summary for main channel", Q2Kw.output)[1] #First occurence is the target line
Avg.grab.sites <- c(Avg.grab.loc + 8, Avg.grab.loc + 9, Avg.grab.loc + 11) # Need to manually set based on site locations

Avg.grab.data <- Q2Kw.output[Avg.grab.sites]

Q2Kw.list <- list()

# Seperate data on at least two spaces then create data frame using rbind
for (y in 1:length(Avg.grab.data)) {
  Q2Kw.list[[y]] <- as.data.frame(strsplit(Avg.grab.data[y], split = "  "))
  names(Q2Kw.list[[y]]) <- y
  Q2Kw.list[[y]] <- t(Q2Kw.list[[y]])
}

# Set blank data frame
Q2Kw.df <- data.frame(stringsAsFactors = F)

# Build dataframe with rbind fill so that empty cells don't repeat
for (z in 1:length(Q2Kw.list)) {
  Q2Kw.df <- bind_rows(Q2Kw.df, as.data.frame(Q2Kw.list[[z]], 
                                          stringsAsFactors = F))
} 

# Give columns the appropriate names
names(UY.df) <- header.nm

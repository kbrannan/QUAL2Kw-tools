# February 13, 2018

# Script to extract fit parameters from QUAL2Kw output

# Dan Sobota, ODEQ


## input/output for script
chr.dir.wrk <- "//deqhq1/TMDL/TMDL_WR/MidCoast/Models/Dissolved Oxygen/PEST-Synthetic-data/kevin_scratch" 

chr.file.in <- "UY_do.out"

chr.file.out <- "model.out"

# Set working directory (modify as needed)----
setwd(chr.dir.wrk)

##Q2Kw.out <- list.files(pattern = "*.out")
##Q2Kw.out <- Q2Kw.out[!grepl("oldph.out", Q2Kw.out)] # Un-needed file name

# scan in file and process----
Q2Kw.output <- scan(chr.file.in, what = "character", sep = "\n", quiet = TRUE)

# Grab sample water quality parameters used to fit model during autocalibration process----
# Parameters are: average, min, and max pH (from continuous data)
#                 CBODfast
#                 Organic N
#                 Ammonium N
#                 Nitrate + nitrite
#                 Organic P
#                 Inorganic P
#                 Water Temperature
#                 Dissolved Oxygen

# Will need to manually adjust what sites are extracted

# Average values for parameters----

Avg.grab.loc <- grep("Daily average water quality summary for main channel", Q2Kw.output)[1] #First occurence is the target line
Avg.grab.nm.loc <- Avg.grab.loc + 1 
Avg.grab.sites <- c(Avg.grab.loc + 8, Avg.grab.loc + 9, Avg.grab.loc + 11) # Need to manually set based on site locations

Avg.grab.data <- Q2Kw.output[Avg.grab.sites]

Q2Kw.list <- list()

# Seperate data on at least two spaces then create data frame using rbind
for (y in 1:length(Avg.grab.data)) {
  Q2Kw.list[[y]] <- as.data.frame(strsplit(Avg.grab.data[y], split = "  "))
  names(Q2Kw.list[[y]]) <- y
  Q2Kw.list[[y]] <- t(Q2Kw.list[[y]])
}

# Get names of columns
Avg.grab.nm <- as.data.frame(strsplit(Q2Kw.output[Avg.grab.nm.loc], split = "  "))
names(Avg.grab.nm) <- "Parameter.nm"
Avg.grab.nm <- Avg.grab.nm[!apply(Avg.grab.nm == "", 1, all),]
grab.nm <- paste(Avg.grab.nm)
grab.nm <- sub(" ", "", grab.nm)

# Get rid of blanks
Avg.grab.nm <- Avg.grab.nm[!sapply(Avg.grab.nm, function(x) all(x == ""))]

# Set blank data frame
Q2Kw.df <- data.frame(stringsAsFactors = F)

# Build dataframe with rbind fill so that empty cells don't repeat
for (z in 1:length(Q2Kw.list)) {
  Q2Kw.df <- rbind(Q2Kw.df, as.data.frame(Q2Kw.list[[z]], 
                                          stringsAsFactors = F))
}

# Get rid of blanks
Q2Kw.df <- Q2Kw.df[!sapply(Q2Kw.df, function(x) all(x == ""))]

# Give columns the appropriate names
names(Q2Kw.df) <- grab.nm

# Extract average grab sample data for assessment of fit
Grab.fit.pm.avg <- subset(Q2Kw.df, select = c("Reach", "Fast CBOD", "Organic N", "NH4-N", "NO3+NO2-N", "Organic P", "Inorganic P", "pH"))

# Need to pull out Min pH----
Min.grab.loc <- grep("Daily minimum water quality summary for main channel", Q2Kw.output)[1] #First occurence is the target line
Min.grab.nm.loc <- Min.grab.loc + 1
Min.grab.sites <- c(Min.grab.loc + 8, Min.grab.loc + 9, Min.grab.loc + 11) # Need to manually set based on site locations

Min.grab.data <- Q2Kw.output[Min.grab.sites]

Q2Kw.list <- list()

# Seperate data on at least two spaces then create data frame using rbind
for (y in 1:length(Min.grab.data)) {
  Q2Kw.list[[y]] <- as.data.frame(strsplit(Min.grab.data[y], split = "  "))
  names(Q2Kw.list[[y]]) <- y
  Q2Kw.list[[y]] <- t(Q2Kw.list[[y]])
}

# Get names of columns
Min.grab.nm <- as.data.frame(strsplit(Q2Kw.output[Min.grab.nm.loc], split = "  "))
names(Min.grab.nm) <- "Parameter.nm"
Min.grab.nm <- Min.grab.nm[!apply(Min.grab.nm == "", 1, all),]
grab.nm <- paste(Min.grab.nm)
grab.nm <- sub(" ", "", grab.nm)

# Get rid of blanks
Min.grab.nm <- Min.grab.nm[!sapply(Min.grab.nm, function(x) all(x == ""))]

# Set blank data frame
Q2Kw.df <- data.frame(stringsAsFactors = F)

# Build dataframe with rbind fill so that empty cells don't repeat
for (z in 1:length(Q2Kw.list)) {
  Q2Kw.df <- rbind(Q2Kw.df, as.data.frame(Q2Kw.list[[z]], 
                                              stringsAsFactors = F))
}

# Get rid of blanks
Q2Kw.df <- Q2Kw.df[!sapply(Q2Kw.df, function(x) all(x == ""))]

# Give columns the appropriate names
names(Q2Kw.df) <- grab.nm

# Extract Minimum grab sample data for assessment of fit
Grab.fit.pm.Min <- subset(Q2Kw.df, select = c("Reach", "pH"))
names(Grab.fit.pm.Min) <- c("Reach", "Min pH")

# Need to pull out Max pH----
Max.grab.loc <- grep("Daily maximum water quality summary for main channel", Q2Kw.output)[1] #First occurence is the target line
Max.grab.nm.loc <- Max.grab.loc + 1
Max.grab.sites <- c(Max.grab.loc + 8, Max.grab.loc + 9, Max.grab.loc + 11) # Need to manually set based on site locations

Max.grab.data <- Q2Kw.output[Max.grab.sites]

Q2Kw.list <- list()

# Seperate data on at least two spaces then create data frame using rbind
for (y in 1:length(Max.grab.data)) {
  Q2Kw.list[[y]] <- as.data.frame(strsplit(Max.grab.data[y], split = "  "))
  names(Q2Kw.list[[y]]) <- y
  Q2Kw.list[[y]] <- t(Q2Kw.list[[y]])
}

# Get names of columns
Max.grab.nm <- as.data.frame(strsplit(Q2Kw.output[Max.grab.nm.loc], split = "  "))
names(Max.grab.nm) <- "Parameter.nm"
Max.grab.nm <- Max.grab.nm[!apply(Max.grab.nm == "", 1, all),]
grab.nm <- paste(Max.grab.nm)
grab.nm <- sub(" ", "", grab.nm)

# Get rid of blanks
Max.grab.nm <- Max.grab.nm[!sapply(Max.grab.nm, function(x) all(x == ""))]

# Set blank data frame
Q2Kw.df <- data.frame(stringsAsFactors = F)

# Build dataframe with rbind fill so that empty cells don't repeat
for (z in 1:length(Q2Kw.list)) {
  Q2Kw.df <- rbind(Q2Kw.df, as.data.frame(Q2Kw.list[[z]], 
                                              stringsAsFactors = F))
}

# Get rid of blanks
Q2Kw.df <- Q2Kw.df[!sapply(Q2Kw.df, function(x) all(x == ""))]

# Give columns the appropriate names
names(Q2Kw.df) <- grab.nm

# Extract Maximum grab sample data for assessment of fit
Grab.fit.pm.Max <- subset(Q2Kw.df, select = c("Reach", "pH"))
names(Grab.fit.pm.Max) <- c("Reach", "Max pH")

# Extracting temporal data for temperature and dissolved oxygen----
Cont.loc <- grep("Diel water quality in the main channel", Q2Kw.output)[1] #First occurence is the target line
Cont.nm.loc <- Cont.loc + 1
# Need to manually set based on site locations
Cont.sites <- c(Cont.loc + 649, Cont.loc + 665, Cont.loc + 681, Cont.loc + 697, Cont.loc + 713, Cont.loc + 729, Cont.loc + 745, Cont.loc + 761, # Reach 5
                Cont.loc + 778, Cont.loc + 794, Cont.loc + 810, Cont.loc + 826, Cont.loc + 842, Cont.loc + 858, Cont.loc + 874, Cont.loc + 890, # Reach 6
                Cont.loc + 1036, Cont.loc + 1052, Cont.loc + 1068, Cont.loc + 1084, Cont.loc + 1100, Cont.loc + 1116, Cont.loc + 1132, Cont.loc + 1148) # Reach 8

Cont.data <- Q2Kw.output[Cont.sites]

Q2Kw.list <- list()

# Seperate data on at least two spaces then create data frame using rbind
for (y in 1:length(Cont.data)) {
  Q2Kw.list[[y]] <- as.data.frame(strsplit(Cont.data[y], split = "  "))
  names(Q2Kw.list[[y]]) <- y
  Q2Kw.list[[y]] <- t(Q2Kw.list[[y]])
}

# Get names of columns
Cont.nm <- as.data.frame(strsplit(Q2Kw.output[Cont.nm.loc], split = "  "))
names(Cont.nm) <- "Parameter.nm"
Cont.nm <- Cont.nm[!apply(Cont.nm == "", 1, all),]
Cont.nm <- paste(Cont.nm)
Cont.nm <- sub(" ", "", Cont.nm)

# Get rid of blanks
Cont.nm <- Cont.nm[!sapply(Cont.nm, function(x) all(x == ""))]

# Set blank data frame
Q2Kw.df <- data.frame(stringsAsFactors = F)

# Build dataframe with rbind fill so that empty cells don't repeat
for (z in 1:length(Q2Kw.list)) {
  Q2Kw.df <- rbind(Q2Kw.df, as.data.frame(Q2Kw.list[[z]], 
                                              stringsAsFactors = F))
}

# Get rid of blanks
Q2Kw.df <- Q2Kw.df[!sapply(Q2Kw.df, function(x) all(x == ""))]

# Give columns the appropriate names
names(Q2Kw.df) <- Cont.nm

# Extract temporal data for assessment of fit
Cont.fit.pm <- subset(Q2Kw.df, select = c("Reach", "Time", "Water temperature", "Dissolved Oygen"))
names(Cont.fit.pm) <- c("Reach", "Time", "Water temperature", "Dissolved Oxygen")

# Compile all data into one flat data frame and write out to flat text file----
# Make dataframe first
Q2Kw.list <- list(Cont.fit.pm, Grab.fit.pm.avg, Grab.fit.pm.Max, Grab.fit.pm.Min)
Q2Kw.merge <- purrr::reduce(Q2Kw.list, merge, by = "Reach")
Q2Kw.almost.flat.out <- tidyr::gather(Q2Kw.merge, Parameter, Value, -Reach, -Time)
Q2Kw.flat.out <- dplyr::distinct(Q2Kw.almost.flat.out, Value, .keep_all = T)

# Sub in requested names
Q2Kw.flat.out$Parameter <- gsub("Water temperature", "temp", Q2Kw.flat.out$Parameter)
Q2Kw.flat.out$Parameter <- gsub("Dissolved Oxygen", "do", Q2Kw.flat.out$Parameter)
Q2Kw.flat.out$Parameter <- gsub("Fast CBOD", "fcob", Q2Kw.flat.out$Parameter)
Q2Kw.flat.out$Parameter <- gsub("Organic N", "orgn", Q2Kw.flat.out$Parameter)
Q2Kw.flat.out$Parameter <- gsub("NH4-N", "nh4", Q2Kw.flat.out$Parameter)
Q2Kw.flat.out$Parameter <- gsub("NO3+NO2-N", "no3", Q2Kw.flat.out$Parameter, fixed = T) #Plus sign is a special character; fixed needs to be true
Q2Kw.flat.out$Parameter <- gsub("Organic P", "orgp", Q2Kw.flat.out$Parameter)
Q2Kw.flat.out$Parameter <- gsub("Inorganic P", "inorgp", Q2Kw.flat.out$Parameter)
Q2Kw.flat.out$Parameter <- gsub("pH", "pHave", Q2Kw.flat.out$Parameter)
Q2Kw.flat.out$Parameter <- gsub("Min pHave", "pHmin", Q2Kw.flat.out$Parameter) # Note that pHave has to be used here
Q2Kw.flat.out$Parameter <- gsub("Max pHave", "pHmax", Q2Kw.flat.out$Parameter)  

# Get rid of extra space in Reach column
Q2Kw.flat.out$Reach <- gsub(" ", "", Q2Kw.flat.out$Reach)

# Make time (hour) and Reach two digit integers
Q2Kw.flat.out$Reach <- sprintf("%02d", as.integer(Q2Kw.flat.out$Reach))
Q2Kw.flat.out$Time <- sprintf("%02d", as.integer(Q2Kw.flat.out$Time))

# Combine parameters to make seperate result tag
Q2Kw.flat.out$Combined.nm <- dplyr::if_else(Q2Kw.flat.out$Parameter == "temp" | Q2Kw.flat.out$Parameter == "do",
                                     paste0(Q2Kw.flat.out$Parameter, Q2Kw.flat.out$Reach, Q2Kw.flat.out$Time),
                                     paste0(Q2Kw.flat.out$Parameter, Q2Kw.flat.out$Reach))

# Make Combined.nm 14 spaces wide
Q2Kw.flat.out$Combined.nm <- sprintf("%-14s", Q2Kw.flat.out$Combined.nm)

# Make Value field display scientific notation
Q2Kw.flat.out$Value <- formatC(as.numeric(Q2Kw.flat.out$Value), format = "e", digits = 8)

# Make Value 14 spaces wide
Q2Kw.flat.out$Value <- sprintf("%-14s", Q2Kw.flat.out$Value)

# Write out flat text file
Q2Kw.flat <- subset(Q2Kw.flat.out, select = c("Combined.nm", "Value"))
write.table(Q2Kw.flat, chr.file.out, row.names = F, col.names = F, 
            quote = F, sep = "")

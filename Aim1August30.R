## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("tidyverse", "dplyr",
             "data.table", "lubridate", "zoo", "magrittr", "plyr", "reticulate", "seewave", "schoRsch", "DescTools", "caret")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

require(tidyverse)
require(dplyr)
require(data.table)
require(lubridate)
require(zoo)
require(magrittr)
require(plyr)
require(reticulate)
require(seewave)
require(schoRsch)
require(DescTools)

# Python Set Up: 
# Leveraging the Reticualte R package https://rstudio.github.io/reticulate/index.html
# You may consider using conda_install() first
reticulate::use_condaenv() # I'm using anaconda as a virtual environment, this may be different from you.
reticulate::import_builtins()
reticulate::import_main()
reticulate::conda_install(packages = c("numpy", "cvxopt")) # Required modules for the cvxEDA python code to run.


# 1. Compile functions but only save data from approximately 3 days prior to drop out.
read.empatica.eda <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  sample_rate <- as.numeric(raw$V1[2])
  data <- data.frame(Timestamp=NA, EDA=raw$V1[3:length(raw$V1)])
  start <- as.POSIXct(start_s, origin = "1970-01-01")
  dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
  timestamps <- seq(from = start, by=dt , along.with = data$EDA) 
  data$Timestamp <- timestamps
  data <- tail(data, n = 1100000) #You might need to mess with this number
  data
}
read.empatica.acc <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  sample_rate <- as.numeric(raw$V1[2])
  data <- data.frame(X=raw$V1[3:length(raw$V1)]/64.0,Y=raw$V2[3:length(raw$V2)]/64.0,Z=raw$V3[3:length(raw$V3)]/64.0)
  start <- as.POSIXct(x = start_s, origin = "1970-01-01")
  dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
  timestamps <- seq(from = start, by=dt , along.with = data$X) 
  data$Timestamp <- timestamps
  data <- tail(data, n = 10000000) #You might need to mess with this number
  data
}
read.empatica.ibi <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  start <- as.POSIXct(x = start_s,origin = "1970-01-01")
  dt <- as.difftime(raw$V1[2:length(raw$V1)],units = "secs")
  timestamps <- start+dt
  ibi <- as.double(as.character(raw$V2[2:length(raw$V2)]) )
  data <- data.frame(Timestamp=timestamps, IBI=ibi)
  data <- tail(data, n = 200000) #You might need to mess with this number
  data
}
read.empatica.temp <- function(file) {
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  sample_rate <- as.numeric(raw$V1[2])
  temperatureF <- (1.8)*(raw$V1[3:length(raw$V1)]) + 32.0
  data <- data.frame(TEMP=temperatureF)
  start <- as.POSIXct(x = start_s, origin = "1970-01-01")
  dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
  timestamps <- seq(from = start, by=dt , along.with = data$TEMP) 
  data$Timestamp <- timestamps
  data <- tail(data, n = 1100000) #You might need to mess with this number
  data
}



data_dir <- "C:\\Users\\Nick\\Documents\\Biomo\\Dropouts Glen" #Directory set up: 1 Folder for each participant containing all zipped Empatica E4 files
participant_E4 <-  list.files(data_dir, full.names = TRUE) #Make a list of the folder with all X number of participant folders containing zip files.
eda <- c() #Electrodermal Activity
acc <- c() #Accelerometer Data
temp <- c() #Skin Temperature
ibi <- c() #Interbeat Interval
drop_outs_raw <- c() # Raw data compiler
drop_outs <- c() # Time filtered 
# Raw data extractor: For each participant folder, look inside each one by one and pull out...
extractor <- function(participant_E4) {
  {
    
    extract_file <- "EDA.csv"  ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
    list.files(participant_E4, full.names=TRUE) %>% 
      keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
      map_df(function(x) {
        td <- tempdir()
        read.empatica.eda(unzip(x, extract_file, exdir=td))
      }) -> eda
    
    extract_file <- "ACC.csv" ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
    list.files(participant_E4, full.names=TRUE) %>% 
      keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
      map_df(function(x) {
        td <- tempdir()
        read.empatica.acc(unzip(x, extract_file, exdir=td))
      }) -> acc
    
    extract_file <- "TEMP.csv" ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
    list.files(participant_E4, full.names=TRUE) %>% 
      keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
      map_df(function(x) {
        td <- tempdir()
        read.empatica.temp(unzip(x, extract_file, exdir=td))
      }) -> temp
    
    extract_file <- "IBI.csv" ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
    list.files(participant_E4, full.names=TRUE) %>% 
      keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
      map_df(function(x) {
        td <- tempdir()
        read.empatica.ibi(unzip(x, extract_file, exdir=td))
      }) -> ibi
    
    
    
  }
  
  list(eda, ibi, acc, temp)
}

drop_outs_raw <- lapply(participant_E4, extractor)

# Step 2: Find the last timestamp from each of the dropped participant

a <- lubridate::ymd_hms("2021-04-17 9:25:24", tz = "US/Eastern")
b <- lubridate::ymd_hms("2021-05-02 7:51:29", tz = "US/Eastern")
c <- lubridate::ymd_hms("2021-05-19 8:06:28", tz = "US/Eastern")
d <- lubridate::ymd_hms("2021-06-14 16:29:15", tz = "US/Eastern")

# 3. Using Lubridate, create a interval from last recording to three days before last recording.
part_4 <- lubridate::interval(start = a - lubridate::days(3), end = a)
part_8 <- lubridate::interval(start = b - lubridate::days(3), end = b)
part_10 <- lubridate::interval(start = c - lubridate::days(3), end = c)
part_20 <- lubridate::interval(start = d - lubridate::days(3), end = d)
dropout_window_list <- c(part_4, part_8, part_10, part_20) 

within1_4 <- lubridate::interval(start = a - lubridate::days(1), end = a)
within1_8 <- lubridate::interval(start = b - lubridate::days(1), end = b)
within1_10 <- lubridate::interval(start = c - lubridate::days(1), end = c)
within1_20 <- lubridate::interval(start = d - lubridate::days(1), end = d)
within1 <- c(within1_4, within1_8, within1_10, within1_20)

within3to2_4 <- lubridate::interval(start = a - lubridate::days(2), end = a - lubridate::days(1))
within3to2_8 <- lubridate::interval(start = b - lubridate::days(2), end = b - lubridate::days(1))
within3to2_10 <- lubridate::interval(start = c - lubridate::days(2), end = c - lubridate::days(1))
within3to2_20 <- lubridate::interval(start = d - lubridate::days(2), end = d - lubridate::days(1))
within3to2 <- c(within3to2_4, within3to2_8, within3to2_10, within3to2_20)

for (i in 1:length(drop_outs_raw)){
  int <- dropout_window_list[[i]]
  for (j in 1:4){
    drop_outs[[i]][[j]] <- drop_outs_raw[[i]][[j]] %>% filter(Timestamp %within% int) 
  }
}

EDA <- c()
IBI <- c()
ACC <- c()
TEMP <- c()

for (i in 1:length(drop_outs)){
  
  EDA[[i]] <- drop_outs[[i]][[1]] %>% mutate(days_from_drop = if_else(Timestamp %within% within1[i], 1, 
                                                                      if_else(Timestamp %within% within3to2[i], 2, 3)))
  IBI[[i]] <- drop_outs[[i]][[2]] %>% mutate(days_from_drop = if_else(Timestamp %within% within1[i], 1, 
                                                                      if_else(Timestamp %within% within3to2[i], 2, 3)))
  ACC[[i]] <- drop_outs[[i]][[3]] %>% mutate(days_from_drop = if_else(Timestamp %within% within1[i], 1, 
                                                                      if_else(Timestamp %within% within3to2[i], 2, 3)))
  TEMP[[i]] <- drop_outs[[i]][[4]] %>% mutate(days_from_drop = if_else(Timestamp %within% within1[i], 1, 
                                                                       if_else(Timestamp %within% within3to2[i], 2, 3)))
  
}

## HRV Processing:
HRV_Process <- c()
HRV <- c()
HRV_Processor <- function(df){
  {
    ## (1) Identifying missing data... is the time gap between to inter-beat intervals the same as the time associated with the inter-beat interval?
    ## (2) If there is a streak of uninterrupted data we want to identify it...
    ## (3) Count the length of the streak so we have a data window
    ## (4) Generate a run-length type group id for each streak and 
    ## (5) Identify unrealistic data by flagging any data that's more than 20% longer or shorter than the previous.
    HRV <- df %>% mutate(diff = as.numeric(Timestamp - lag(Timestamp)),
                         Streak = if_else(as.numeric(IBI) == as.numeric(diff), "yes", "no"), count = sequence(rle(as.character(Streak))$lengths),
                         rlid = rleid(Streak),
                         artifact = if_else((IBI * (1.20)) < lead(IBI) & lead(Streak) != "no" | (IBI * (0.80)) > lead(IBI) & lead(Streak) != "no", "yes", "no"))
    ## Filter out artifacts
    HRV$IBI_stripped <- HRV$IBI
    HRV$IBI_stripped[HRV$artifact == "yes"] <- NA
    # Interpolate artifacts with a polynomial spline
    HRV <- HRV %>% mutate(IBI_interp = na.spline(IBI_stripped))
    
    HRV2 <- HRV %>% select(Timestamp, IBI, days_from_drop, IBI_interp)
    ## Re-run (1) and (2) to account for the time gaps introduced by removing artifacts
    ## Re run (3) and (4) to get gold-standard, artifact-removed, uninteruppted data windows for analysis.
    HRV2 <- HRV2 %>% mutate(diff = as.numeric(Timestamp - lag(Timestamp)),
                            Streak = if_else(as.numeric(IBI) == as.numeric(diff), "yes", "no"), count = sequence(rle(as.character(Streak))$lengths),
                            rlid = rleid(Streak))
    ## Along the count column, identify a group on the notion that a group doesn't stop if the difference in count is positive...
    ## Then find the maximum of that value. I have no clue why cumsum() works, it just does.
    HRV3 <- subset(HRV, count == ave(count, cumsum(c(TRUE, diff(count) < 0)),  FUN = max)) 
    ## Only keep data where the window size (count) is greater than 300 seconds (5 minutes)
    HRV3 <- HRV3 %>% filter(count > 300)
    ## These data islands are now discrete targets for analysis and can be paired back up with data set 2 based on their identifier, rlid.
    islands <- as.factor(HRV3$rlid)
    HRV2$rlid <-as.factor(HRV2$rlid)
    HRV <- HRV2 %>% filter(rlid %in% islands)
    ## Now here's the HRV formula: we're using a time-based metric, RMSSD (Root Mean of Squared Successive Differences)
    # Find the value of successive differences only on the data islands.
    HRV$rlid <- as.factor(HRV$rlid)
    
    HRV$difference <- ddply(HRV, .(rlid), summarise, 
                            difference=c(NA, rollapply(IBI_interp, width=2, FUN = diff)))$difference
    # Square it
    HRV$difference <- (HRV$difference)^2
    # Take the mean of 30 second windows
    HRV$mean_diff <- ddply(HRV, .(rlid), summarise, 
                           mean_diff= rollapply(difference, width=30, FUN = mean, fill = NA))$mean_diff
    # Take the square of that
    HRV$rmssd <- sqrt(as.numeric(HRV$mean_diff))
    
  }
  data.frame(HRV)
}
HRV_Process <- lapply(IBI, HRV_Processor)
dsfinal_hrv <- c()
final_HRV <- c()

## There may be some days that DO NOT HAVE any viable HRV readings (complete, 5 minute windows). As a consequence, HRV must be processed separately.

for (i in 1:length(HRV_Process)){
  final_HRV[[i]] <- HRV_Process[[i]] %>% dplyr::group_by(days_from_drop, rlid) %>% drop_na() %>% dplyr::summarise(days_from_drop = mean(days_from_drop), Timestamp = mean(Timestamp), avg_rmssd = mean(rmssd), window_size = max(count)) %>% group_split(days_from_drop)
  
}
flat_hrv <- final_HRV %>% flatten()
dsfinal_hrv <- c()
for (i in 1:length(flat_hrv)){
  dsfinal_hrv[i] <- flat_hrv[[i]][4]
  
}
drop_ts_final_HRV <- c()
for (i in 1:length(dsfinal_hrv)){
  drop_ts_final_HRV[[i]] <- list(hrv =  mean(dsfinal_hrv[[i]]))
}

final_hrv <- drop_ts_final_HRV %>% rbindlist(use.names = T)


## TEMPERATURE PRE-PROCESSING
# You will get a pop-up notifying you if any outliers were detected.
TEMP_Process <- c()
TEMP_Processor <- function(df){
  {
    # n = second order butterworth filter 
    # From = high pass cutoff
    # To = low pass cutoff
    schoRsch::outlier(df, "TEMP", todo = "na")
    TEMP <- df %>% mutate(TEMP = na.spline(TEMP))
    
    # Here's a butterworth filter following some parameters I saw in the literature... the output was weird but I'm foregoing it for now.
    #TEMP <- TEMP %>% mutate(TEMP = as.numeric(seewave::bwfilter(TEMP, f = 4, n = 2, from = 0.1, to = 1)))
    # schoRsch::outlier(TEMP, "TEMP", todo = "na")
    # TEMP <- TEMP %>% mutate(TEMP = na.spline(TEMP))
    TEMP <- TEMP %>% mutate(TEMP = ((TEMP - mean(TEMP)) / sd(TEMP)))
    
  }
  data.frame(TEMP)
}

TEMP_Process <- lapply(TEMP, TEMP_Processor)




final_EDA <- c()
final_ACC <- c()
final_TEMP <-c()

for (i in 1:length(HRV_Process)){
  final_EDA[[i]] <- EDA[[i]] %>% group_split(days_from_drop) 
  # Make instantaneous acceleration here using this formula. Computed here out of convenience.
  final_ACC[[i]] <- ACC[[i]] %>% mutate(g = sqrt((X * X) + (Y * Y) + (Z * Z))) %>% group_split(days_from_drop) 
  final_TEMP[[i]] <- TEMP_Process[[i]] %>% group_split(days_from_drop) 
}


flat_eda <- final_EDA %>% flatten()
flat_acc <- final_ACC %>% flatten()
flat_temp <- final_TEMP %>% flatten()

dsfinal_eda <- c()
dsfinal_acc <- c()
dsfinal_temp <- c()
# We're just going to strip the data vector we want from the rest of the data to lighten the load for our timeseries functions.
for (i in 1:length(flat_eda)){
  # Normalize EDA and ACC here, AKA z-score for each value. Done here out of convenience.
  dsfinal_eda[i] <- flat_eda[[i]][2] %>% mutate(EDA = ((EDA) - mean((EDA)) / sd(EDA)))
  dsfinal_acc[i] <- flat_acc[[i]][6] %>% mutate(g = ((g - mean(g)) / sd(g)))
  dsfinal_temp[i] <- flat_temp[[i]][1]
}



drop_ts_final_tonic <- c()
drop_ts_final_phasic <- c()
drop_ts_final_HRV <- c()
drop_ts_final_ACC <- c()
drop_ts_final_TEMP <-c()
py_eda <- c()

# Downloaded from https://github.com/lciti/cvxEDA
reticulate::source_python("C:\\Users\\Nick\\Downloads\\cvxEDA-master\\cvxEDA-master\\src\\cvxEDA.py")

EDA_PreProcess <- c()
for (i in 1:length(dsfinal_eda)){
  ## INPUT ##
  # delta = sampling rate
  # y = timeseries to iterate code onto
  ## OUTPUT ##
  # r = phasic component
  # p = sparse sudomotor nerve activity (SDMNA) driver of phasic component
  # t = tonic component
  # l = coefficients of tonic spline
  # d = offset and slope of the drift term
  # e = model residuals
  # obj. value of objective function being minimized
  . 
  py_eda = .GlobalEnv$cvxEDA(delta = 1/4, y = dsfinal_eda[[i]])
  
  # It took me a while to figure this out but python makes something called a "generator". It doesn't automatically create the object you want.
  # To run this set up you use the iterate() function. It's like pressing the gas after turning on the car.
  EDA_PreProcess[[i]] = iterate(py_eda) 
  
}

## TS FEATURES
# https://pkg.robjhyndman.com/tsfeatures/articles/tsfeatures.html
for (i in 1:length(dsfinal_acc)){
  drop_ts_final_phasic[[i]] <- cbind.data.frame(sdphasic = sd(EDA_PreProcess[[i]][[1]]), phasicAUC = DescTools::AUC(x = seq(from = 1, to = length(EDA_PreProcess[[i]][[1]]), 1), y = EDA_PreProcess[[i]][[1]]))
  drop_ts_final_tonic[[i]] <- tsfeatures::tsfeatures(EDA_PreProcess[[i]][[3]], parallel = T, features = c("entropy", "stability", "crossing_points"))
  drop_ts_final_ACC[[i]] <- tsfeatures::tsfeatures(dsfinal_acc[i], parallel = T, features = c("entropy", "stability", "crossing_points"))
  drop_ts_final_TEMP[[i]] <- tsfeatures::tsfeatures(dsfinal_temp[i], parallel = T, features = c("entropy", "stability", "crossing_points"))
}

# Combine the datasets to make a final dataset.
final_phasic <- drop_ts_final_phasic %>% rbindlist(use.names = T)
final_tonic <- drop_ts_final_tonic %>% rbindlist(use.names = T)
final_a <- drop_ts_final_ACC %>% rbindlist(use.names = T, idcol = "Pday")
final_t <- drop_ts_final_TEMP %>% rbindlist(use.names = T, idcol = "Pday")

final_combined <- left_join(final_a, final_t, copy = TRUE, suffix = c(".acc", ".temp"), by = "Pday")
final_drop <- cbind(final_combined, final_phasic, final_tonic, final_hrv)


################### STAY - IN PROCESSING ###############

# 1. Compile functions but allow it to read any stretch of data, not just the 3 days before dropout.
read.empatica.eda <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  sample_rate <- as.numeric(raw$V1[2])
  data <- data.frame(Timestamp=NA, EDA=raw$V1[3:length(raw$V1)])
  start <- as.POSIXct(start_s, origin = "1970-01-01")
  dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
  timestamps <- seq(from = start, by=dt , along.with = data$EDA)
  data$Timestamp <- timestamps
  data
}
read.empatica.acc <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  sample_rate <- as.numeric(raw$V1[2])
  data <- data.frame(X=raw$V1[3:length(raw$V1)]/64.0,Y=raw$V2[3:length(raw$V2)]/64.0,Z=raw$V3[3:length(raw$V3)]/64.0)
  start <- as.POSIXct(x = start_s, origin = "1970-01-01")
  dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
  timestamps <- seq(from = start, by=dt , along.with = data$X) 
  data$Timestamp <- timestamps
  data
}
read.empatica.ibi <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  start <- as.POSIXct(x = start_s,origin = "1970-01-01")
  dt <- as.difftime(raw$V1[2:length(raw$V1)],units = "secs")
  timestamps <- start+dt
  ibi <- as.double(as.character(raw$V2[2:length(raw$V2)]) )
  data <- data.frame(Timestamp=timestamps, IBI=ibi)
  data
}
read.empatica.temp <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  sample_rate <- as.numeric(raw$V1[2])
  temperatureF <- (1.8)*(raw$V1[3:length(raw$V1)]) + 32.0
  data <- data.frame(TEMP=temperatureF)
  start <- as.POSIXct(x = start_s, origin = "1970-01-01")
  dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
  timestamps <- seq(from = start, by=dt , along.with = data$TEMP) 
  data$Timestamp <- timestamps
  data
}



data_dir <- "C:\\Users\\Nick\\Documents\\Biomo\\Glenbeigh Data" #Directory set up: 1 Folder for each participant containing all zipped Empatica E4 files
participant_E4 <-  list.files(data_dir, full.names = TRUE) #Make a list of the folder with all X number of participant folders containing zip files.
eda <- c() #Electrodermal Activity
acc <- c() #Accelerometer Data
temp <- c() #Skin Temperature
ibi <- c() #Interbeat Interval
stay_ins <- c() # Raw data compiler


# Raw data extractor: For each participant folder, look inside each one by one and pull out...

# I included a skeleton code for if continuous data collection hadn't started until a week into study participation.

extractor <- function(participant_E4) {
  {
    extract_file <- "EDA.csv"  ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
    list.files(participant_E4, full.names=TRUE) %>% 
      keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
      map_df(function(x) {
        td <- tempdir()
        read.empatica.eda(unzip(x, extract_file, exdir=td))
      }) -> eda
    
    #analysis_window <- lubridate::interval(start = ymd_hms(min(eda$Timestamp), tz = "US/Eastern") + lubridate::days(7), end = ymd_hms(max(eda$Timestamp), tz = "US/Eastern"))

    #analysis_window <- lubridate::interval(start = ymd_hms(min(eda$Timestamp), tz = "US/Eastern"), end = ymd_hms(max(eda$Timestamp), tz = "US/Eastern"))

    # Filter data to keep only data within this random 24 hour interval.
    #eda <- eda %>% filter(Timestamp %within% analysis_window)
    
    extract_file <- "ACC.csv" ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
    list.files(participant_E4, full.names=TRUE) %>% 
      keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
      map_df(function(x) {
        td <- tempdir()
        read.empatica.acc(unzip(x, extract_file, exdir=td))
      }) -> acc
    #acc <- acc %>% filter(Timestamp %within% analysis_window)
    extract_file <- "TEMP.csv" ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
    list.files(participant_E4, full.names=TRUE) %>% 
      keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
      map_df(function(x) {
        td <- tempdir()
        read.empatica.temp(unzip(x, extract_file, exdir=td))
      }) -> temp
    #temp <- temp %>% filter(Timestamp %within% analysis_window)
    
    extract_file <- "IBI.csv" ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
    list.files(participant_E4, full.names=TRUE) %>% 
      keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
      map_df(function(x) {
        td <- tempdir()
        read.empatica.ibi(unzip(x, extract_file, exdir=td))
      }) -> ibi
    #ibi <- ibi %>% filter(Timestamp %within% analysis_window)
    
  }
  list(eda, ibi, acc, temp)
}

stay_ins <- lapply(participant_E4, extractor)


## Narrow to 1 day window.

window_EDA <- c()
window_ACC <- c()
window_HRV <- c()
window_TEMP <- c()
analysis_window <- c()

# Now that we have continuous data for a full two week window for most participants, it's almost a guarantee that we'll have at least a day within the data that's usable for analysis.
# Basically this code works by taking a random timestmap that's not within a day of the last timestamp and filtering the data down to times within a one day interval starting at that timestamp.
# Random one day windows.
for (i in 1:length(stay_ins)){
  set.seed(i + 30)
  #if (as.numeric(max(stay_ins[[i]][[1]]$Timestamp)) - as.numeric(min(stay_ins[[i]][[1]]$Timestamp)) > 86100){
    random_window <- lubridate::interval(start = ymd_hms(min(stay_ins[[i]][[1]]$Timestamp), tz = "US/Eastern"), end = ymd_hms(max(stay_ins[[i]][[1]]$Timestamp), tz = "US/Eastern") - lubridate::days(1))
    window <- stay_ins[[i]][[1]] %>% filter(Timestamp %within% random_window)
    rnd_timestamp <- sample(size = 1, window$Timestamp)
    analysis_window <- lubridate::interval(start = ymd_hms(rnd_timestamp, tz = "US/Eastern"), end = ymd_hms(rnd_timestamp, tz = "US/Eastern") + lubridate::days(1))
    
    window_EDA[[i]] <- stay_ins[[i]][[1]] %>% filter(Timestamp %within% analysis_window)
    window_HRV[[i]] <- stay_ins[[i]][[2]] %>% filter(Timestamp %within% analysis_window)
    window_ACC[[i]] <- stay_ins[[i]][[3]] %>% filter(Timestamp %within% analysis_window)
    window_TEMP[[i]] <- stay_ins[[i]][[4]] %>% filter(Timestamp %within% analysis_window)
  #}
 # else{
 ##   window_EDA[[i]] <- stay_ins[[i]][[1]]
  #  window_HRV[[i]] <- stay_ins[[i]][[2]]
#window_ACC[[i]] <- stay_ins[[i]][[3]]
  #  window_TEMP[[i]] <- stay_ins[[i]][[4]]
  #}
}



## HRV Processing:
HRV_Process <- c()
HRV <- c()
HRV_Processor <- function(df){
  {
    ## (1) Identifying missing data... is the time gap between to inter-beat intervals the same as the time associated with the inter-beat interval?
    ## (2) If there is a streak of uninterrupted data we want to identify it...
    ## (3) Count the length of the streak so we have a data window
    ## (4) Generate a run-length type group id for each streak and 
    ## (5) Identify unrealistic data by flagging any data that's more than 20% longer or shorter than the previous.
    HRV <- df %>% mutate(diff = as.numeric(Timestamp - lag(Timestamp)),
                         Streak = if_else(as.numeric(IBI) == as.numeric(diff), "yes", "no"), count = sequence(rle(as.character(Streak))$lengths),
                         rlid = rleid(Streak),
                         artifact = if_else((IBI * (1.20)) < lead(IBI) & lead(Streak) != "no" | (IBI * (0.80)) > lead(IBI) & lead(Streak) != "no", "yes", "no"))
    ## Filter out artifacts
    HRV$IBI_stripped <- HRV$IBI
    HRV$IBI_stripped[HRV$artifact == "yes"] <- NA
    # Interpolate NA values with splines!
    HRV <- HRV %>% mutate(IBI_interp = na.spline(IBI_stripped))

    HRV2 <- HRV %>% select(Timestamp, IBI_interp, IBI)
    ## Re-run (1) and (2) to account for the time gaps introduced by removing artifacts
    ## Re run (3) and (4) to get gold-standard, artifact-removed, uninteruppted data windows for analysis.
    HRV2 <- HRV2 %>% mutate(diff = as.numeric(Timestamp - lag(Timestamp)),
                            Streak = if_else(as.numeric(IBI) == as.numeric(diff), "yes", "no"), count = sequence(rle(as.character(Streak))$lengths),
                            rlid = rleid(Streak))
    ## Along the count column, identify a group on the notion that a group doesn't stop if the difference in count is positive...
    ## Then find the maximum of that value. I have no clue why cumsum() works, it just does.
    HRV3 <- subset(HRV, count == ave(count, cumsum(c(TRUE, diff(count) < 0)),  FUN = max)) 
    ## Only keep data where the window size (count) is greater than 300 seconds (5 minutes)
    HRV3 <- HRV3 %>% filter(count > 300)
    ## These data islands are now discrete targets for analysis and can be paired back up with data set 2 based on their identifier, rlid.
    islands <- as.factor(HRV3$rlid)
    HRV2$rlid <-as.factor(HRV2$rlid)
    HRV <- HRV2 %>% filter(rlid %in% islands)
    ## Now here's the HRV formula: we're using a time-based metric, RMSSD (Root Mean of Squared Successive Differences)
    # Find the value of successive differences only on the data islands.
    HRV$rlid <- as.factor(HRV$rlid)
    
    HRV$difference <- ddply(HRV, .(rlid), summarise, 
                            difference=c(NA, rollapply(IBI_interp, width=2, FUN = diff)))$difference
    # Square it
    HRV$difference <- (HRV$difference)^2
    # Take the mean of 30 second windows
    HRV$mean_diff <- ddply(HRV, .(rlid), summarise, 
                           mean_diff= rollapply(difference, width=30, FUN = mean, fill = NA))$mean_diff
    # Take the square of that
    HRV$rmssd <- sqrt(HRV$mean_diff)
    
  }
  data.frame(HRV)
}

HRV_Process <- lapply(window_HRV, HRV_Processor)

sfinal_HRV <- c()
tsfinal_hrv <- c()
stay_ts_final_HRV <- c()

## There may be some days that DO NOT HAVE any viable HRV readings (complete, 5 minute windows) leading to different list sizes. As a consequence, HRV must be processed separately.

for (i in 1:length(HRV_Process)){
  sfinal_HRV[[i]] <- HRV_Process[[i]] %>% dplyr::group_by(rlid) %>% drop_na() %>% dplyr::summarise(Timestamp = mean(Timestamp), avg_rmssd = mean(rmssd), window_size = max(count))
  
}


for (i in 1:length(sfinal_HRV)){
  tsfinal_hrv[i] <- sfinal_HRV[[i]][3]
}

for (i in 1:length(tsfinal_hrv)){
  stay_ts_final_HRV[[i]] <- list(hrv =  mean(tsfinal_hrv[[i]]))
}

finals_hrv <- stay_ts_final_HRV %>% rbindlist(use.names = T)


## TEMPERATURE PRE-PROCESSING 
# You will get a pop-up notifying you if any outliers were detected.
TEMP_Process <- c()
TEMP_Processor <- function(df){
  {
    # n = second order butterworth filter 
    # From = high pass cutoff
    # To = low pass cutoff
    schoRsch::outlier(df, "TEMP", todo = "na")
    TEMP <- df %>% mutate(TEMP = na.spline(TEMP))
    
    # Here's a butterworth filter following some parameters I saw in the literature... the output was weird but I'm foregoing it for now.
    #TEMP <- TEMP %>% mutate(TEMP = as.numeric(seewave::bwfilter(TEMP, f = 4, n = 2, from = 0.1, to = 1)))
    # schoRsch::outlier(TEMP, "TEMP", todo = "na")
    # TEMP <- TEMP %>% mutate(TEMP = na.spline(TEMP))
    TEMP <- TEMP %>% mutate(TEMP = ((TEMP - mean(TEMP)) / sd(TEMP)))
    
  }
  data.frame(TEMP)
}

TEMP_Process <- lapply(window_TEMP, TEMP_Processor)


sfinal_EDA <- c()
sfinal_ACC <- c()
sfinal_TEMP <-c()

for (i in 1:length(window_ACC)){
  # Make instantaneous acceleration here using this formula. Computed here out of convenience.
  sfinal_ACC[[i]] <- window_ACC[[i]] %>% mutate(g = sqrt((X * X) + (Y * Y) + (Z * Z)))
}

tsfinal_EDA <- c()
tsfinal_ACC <- c()
tsfinal_TEMP <-c()
# We're just going to strip the data vector we want from the rest of the data to lighten the load for our timeseries functions.
for (i in 1:length(window_EDA)){
  # Normalize EDA and ACC here, AKA z-score for each value. Done here out of convenience.
  tsfinal_EDA[i] <- window_EDA[[i]][2] %>% mutate(EDA = ((EDA) - mean((EDA)) / sd(EDA)))
  tsfinal_ACC[i] <- sfinal_ACC[[i]][5] %>% mutate(g = ((g - mean(g)) / sd(g)))
  tsfinal_TEMP[i] <- TEMP_Process[[i]][1]
}


stay_ts_final_tonic <- c()
stay_ts_final_phasic <- c()
stay_ts_final_ACC <- c()
stay_ts_final_TEMP <- c()
py_eda <- c()

# Downloaded from https://github.com/lciti/cvxEDA
reticulate::source_python("C:\\Users\\Nick\\Downloads\\cvxEDA-master\\cvxEDA-master\\src\\cvxEDA.py")
EDA_PreProcess <- c()
for (i in 1:length(tsfinal_EDA)){
  . 
  py_eda = .GlobalEnv$cvxEDA(delta = 1/4, y = tsfinal_EDA[[i]])
  EDA_PreProcess[[i]] = iterate(py_eda) 
  
}

## TS FEATURES
# https://pkg.robjhyndman.com/tsfeatures/articles/tsfeatures.html
for (i in 1:length(tsfinal_ACC)){
  stay_ts_final_phasic[[i]] <- cbind.data.frame(sdphasic = sd(EDA_PreProcess[[i]][[1]]), phasicAUC = DescTools::AUC(x = seq(from = 1, to = length(EDA_PreProcess[[i]][[1]]), 1), y = EDA_PreProcess[[i]][[1]]))
  stay_ts_final_tonic[[i]] <- tsfeatures::tsfeatures(EDA_PreProcess[[i]][[3]], parallel = T, features = c("entropy", "stability", "crossing_points"))
  stay_ts_final_ACC[[i]] <- tsfeatures::tsfeatures(tsfinal_ACC[i], parallel = T, features = c("entropy", "stability", "crossing_points"))
  stay_ts_final_TEMP[[i]] <- tsfeatures::tsfeatures(tsfinal_TEMP[i], parallel = T, features = c("entropy", "stability", "crossing_points"))
}

# JOIN STAY IN DATASETS
finalts_phasic <- stay_ts_final_phasic %>% rbindlist(use.names = T)
finalts_tonic <- stay_ts_final_tonic %>% rbindlist(use.names = T)
finalts_a <- stay_ts_final_ACC %>% rbindlist(use.names = T, idcol = "Pday")
finalts_t <- stay_ts_final_TEMP %>% rbindlist(use.names = T, idcol = "Pday")

finalts_combined <- left_join(finalts_a, finalts_t, copy = TRUE, suffix = c(".acc", ".temp"), by = "Pday")
final_stay <- cbind(finalts_combined, finalts_phasic, finalts_tonic, finals_hrv)



## JOIN FINAL DATASETS ##


#final_stay <- final_stay %>% drop_na()

# Add 12 because there are four participants in the dropout condition
final_stay <- final_stay %>% mutate(Pday = Pday + 12,
                                    risk = 0)
#final_stay <- final_stay %>% select(trend.e, trend.b, trend, spike, spike.e, spike.t, linearity, linearity.b, linearity.e, curvature, curvature.b, curvature.e, e_acf1.e, e_acf1.b, e_acf1, e_acf10.e, e_acf1.b, e_acf10, entropy.e, entropy.b, entropy, stability, stability.b, stability.e, crossing_points.b, crossing_points.e, risk)

final_drop <- final_drop %>% mutate(risk = 1)
final <- bind_rows(final_stay, final_drop)



## MODELING 
final$risk <- as.factor(final$risk)
final <- final %>% select(-Pday)
# Repeated CV, 5 folds, random seleciton of hyperparameters. 
tr_ctrl <- caret::trainControl(method="repeatedcv",number = 2, repeats=1, "random")
require(caret)
# USE CARET!!!
set.seed(7878)
# Adaboost
model_adaboost = train(risk ~ ., data=final, method='adaboost', trControl = tr_ctrl)
# Random forest
model.rf <- train(risk ~ ., data = final, 
               method = 'rf',
               trControl = tr_ctrl,
               family = 'binomial' )
## Naive Bayes
model_nb = train(risk ~ ., data=final, method='nb', trControl = tr_ctrl, family = 'binomial')
# SVM Radial
model_svmRadial = train(risk ~ ., data=final, method='svmRadial', trControl = tr_ctrl)

models_compare <- resamples(list(RF = model.rf, AdaBoost =model_adaboost, SVMRadial = model_svmRadial))

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)

## FINAL MODEL TUNING: Chose Random Forest.
# Tune Length = Test 10 hyperparameters, in this case, mtry is the only hyperparameter.
# Importance reveals importance plot.
set.seed(230)
model.rf <- train(risk ~ ., data = final, tuneLength = 10,
                  method = 'rf', importance = T,
                  trControl = tr_ctrl,
                  family = 'binomial' )
plot(model.rf$finalModel$importance)

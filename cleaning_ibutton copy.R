library(tidyverse)
library(dplyr)
library(plyr)
library(readxl)
library(readr)
library(stringr)
library(rlang)
library(httr)
library(jsonlite)
library(chron)

# Sites: Dates:
ibuttonRaw <- read.csv("/Users/smcatee/Desktop/TF/IBUTTON/ibutton_raw.csv", stringsAsFactors = FALSE)

(masterData_SP[,c(5, 16, 27, 38, 49, 60, 71, 82, 93, 104, 115, 126, 137, 148, 159, 170, 181, 192, 203, 214)])

ggplot(masterData_BB, aes(x=as.Date(date), y=value, color=variable)) +
  geom_point(aes(y=`L1 Temp`, col=as.factor(`L1 Temp`))) +
  geom_point(aes(y=`L2 Temp`, col=as.factor(`L2 Temp`))) +
  geom_point(aes(y=`L3`, col=as.factor(`L3`))) +
  geom_point(aes(y=`L4`, col=as.factor(`L4`))) +
  geom_point(aes(y=`L5`, col=as.factor(`L5`))) +
  geom_point(aes(y=`M1`, col=as.factor(`M1`))) +
  geom_point(aes(y=`M2`, col=as.factor(`M2`))) +
  geom_point(aes(y=`M3 temp`, col=as.factor(`M3 temp`))) +
  geom_point(aes(y=`M4`, col=as.factor(`M4`))) +
  geom_point(aes(y=`M5`, col=as.factor(`M5`))) +
  geom_point(aes(y=`H1`, col=as.factor(`H1`))) +
  geom_point(aes(y=`H2`, col=as.factor(`H2`))) +
  geom_point(aes(y=`H3`, col=as.factor(`H3`))) +
  geom_point(aes(y=`H4`, col=as.factor(`H4`))) +
  geom_point(aes(y=`H5`, col=as.factor(`H5`))) +
  geom_point(aes(y=`S1 Temp`, col=as.factor(`S1 Temp`))) +
  geom_point(aes(y=`S2`, col=as.factor(`S2`))) +
  geom_point(aes(y=`S3`, col=as.factor(`S3`))) +
  geom_point(aes(y=`S4`, col=as.factor(`S4`))) +
  geom_point(aes(y=`S5`, col=as.factor(`S5`)))







# CLEANING MASTER DATA XLSX FILE
excel_sheets("/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_MASTER_DATA.xlsx")
#Sites: SP    Dates:
masterData_SP <- read_xlsx("/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_MASTER_DATA.xlsx", sheet = "SP")
#Sites: BB    Dates:
masterData_BB <- read_xlsx("/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_MASTER_DATA.xlsx", sheet = "BB")



# add day/night
# add out-of-water && day
curTable <- masterData_SP
isDayCol <- c()
oowAndDayCol <- c()
nDays <- 0
rowIndex <- 1
while( rowIndex <= nrow(curTable) ) { # use nrow(curTable)
  
  while ( is.na(curTable$date[rowIndex]) | is.na(curTable$`time offset`[rowIndex]) ) {
    isDayCol <- c(isDayCol, NA)
    print("NA values")
    rowIndex = rowIndex + 1
    if ( rowIndex > nrow(curTable) ) { break }
  }
  
  #GET sunrise sunset with REST API
  curDate <- as.Date(curTable$date[rowIndex])
  call <- paste0("https://api.sunrise-sunset.org/json?lat=36.7201600&lng=-4.4203400&date=", curDate)
  callContent <- content(GET(call), "text")
  callContentJSON <- fromJSON(callContent, flatten=TRUE)
  sunrise <- as.times(str_remove(callContentJSON$results$sunrise, " AM"))
  sunset <- as.times(str_remove(callContentJSON$results$sunset, " PM")) + as.times("12:00:00")

  # While this date, check if day or night using `time offset` col
  while (curDate == curTable$date[rowIndex]) {

  isDay <- FALSE
  curTime <- str_remove(curTable$`time offset`[rowIndex], ".* ")
  isDay <- ifelse (curTime < sunrise | curTime > sunset, FALSE, TRUE)
  isDayCol <- c(isDayCol, isDay)

  rowIndex = rowIndex + 1
  while ( is.na(curTable$date[rowIndex]) | is.na(curTable$`time offset`[rowIndex]) ) {
      isDayCol <- c(isDayCol, NA)
      print("NA values")
      rowIndex = rowIndex + 1
      if ( rowIndex > nrow(curTable) ) { break }
    }
  }
  nDays <- nDays + 1
}

# Sanity checks
dim(curTable)
length(curTable$date)
length(curTable$`time offset`)
length(isDayCol)
nDays

# Might need to..
isDayCol <- isDayCol[-1]

curTable <- add_column(curTable, isDay=isDayCol, .before = "time offset")



# for each OOW col overwrite in the OOW&Day col
oowColIndexes <- which(str_detect(colnames(curTable), fixed("OOW?")))
oowAndDayColIndexes <- which(str_detect(colnames(curTable), fixed("Day?")))

colnames(curTable[,oowColIndexes])
colnames(curTable[,oowAndDayColIndexes])

for (curCol in seq_along(oowColIndexes)) {
  curOowColIndex <- oowColIndexes[curCol]
  curOowAndDayColIndex <- oowAndDayColIndexes[curCol]
  
  curTable[,curOowColIndex] <- sapply(curTable[,curOowColIndex], as.logical)
  curTable[,curOowAndDayColIndex] <- (curTable[,curOowColIndex] & isDayCol)
}


# CREATE THE SEASON VECTOR
datesVector <- as.Date(curTable$date)

datesVector %>% format("%Y") %>% as.numeric() -> yearsVector
datesVector %>% format("%m") %>% as.numeric() -> monthsVector
seasonVector <- rep("",length(monthsVector))
seasonVector[monthsVector <= 2] <- "Winter"
seasonVector[monthsVector > 2 & monthsVector <= 5] <- "Spring"
seasonVector[monthsVector > 5 & monthsVector <= 8] <- "Summer"
seasonVector[monthsVector > 8 & monthsVector <= 11] <- "Fall"
seasonVector[monthsVector == 12] <- "Winter"
table(seasonVector)
rm(datesVector, monthsVector)
data.frame(curTable$date, seasonVector)

curTable <- add_column(curTable, Season=seasonVector, .before = "isDay")



write_csv(curTable, "/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_SP_Season_OOWDay.csv")


#Chop up for example sample

#Sites: SP
masterData_SP <- read_xlsx("/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_MASTER_DATA.xlsx", sheet = "SP")
#Sites: BB
masterData_BB <- read_xlsx("/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_MASTER_DATA.xlsx", sheet = "BB")

masterData_BB_Sample <- rbind(head(masterData_BB, n=100), tail(masterData_BB))
masterData_SP_Sample <- rbind(head(masterData_SP, n=100), tail(masterData_SP))

# Fix/Fudge time col
masterData_BB_Sample$`time offset` <- sapply(masterData_BB_Sample$`time offset`, str_remove, pattern = ".* ")
masterData_SP_Sample$`time offset` <- sapply(masterData_SP_Sample$`time offset`, str_remove, pattern = ".* ")

masterData_BB_Sample[1:5, c(3, 72, 76)]


write_csv(masterData_BB_Sample, "/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_BB_Sample.csv")
write_csv(masterData_SP_Sample, "/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_SP_Sample.csv")


# Fudge master data time
BB_time <- sapply(masterData_BB$`time offset`, str_remove, pattern = ".* ")
SP_time <- sapply(masterData_SP$`time offset`, str_remove, pattern = ".* ")

masterData_BB$`time offset` <- BB_time
masterData_SP$`time offset` <- SP_time

colnames(masterData_BB)[3] <- "time"
colnames(masterData_SP)[2] <- "time"


write_csv(masterData_BB, "/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_MASTER_DATA_BB.csv")
write_csv(masterData_SP, "/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_MASTER_DATA_SP.csv")


# Plotting temp and time

ggplot(masterData_BB, aes(str_remove(time, ":..:.."), as.numeric(`L1 Temp`))) +
  geom_point() +
  labs(title = "Temp over each day", x = "Hour", y = "Temp")


# Column name select
masterData_BB %>%
  select(matches("(^date$)|(^time$)|(^[LMHS][1-5]( [Tt]emp)?$)|(OOW\\?)", ignore.case = FALSE)) %>%
  colnames()





# Exploring species data

PointIntercept_SP <- read_xlsx("/Users/smcatee/Desktop/TF/IBUTTON/PointIntercept_SP.xlsx")
PointIntercept_BB <- read_xlsx("/Users/smcatee/Desktop/TF/IBUTTON/PointIntercept_BB.xlsx")


PointIntercept_BB %>% head()

# remove totals rows
PointIntercept_SP <- filter(PointIntercept_SP, Plot != "Total")
PointIntercept_BB <- filter(PointIntercept_BB, Plot != "Total")

# remove 2020 data
PointIntercept_SP <- filter(PointIntercept_SP, Date < "2020-01-01")
PointIntercept_BB <- filter(PointIntercept_BB, Date < "2020-01-01")


PointIntercept_SP <- PointIntercept_SP[-63,]
PointIntercept_SP <- PointIntercept_SP[-2]
PointIntercept_BB <- PointIntercept_BB[-222,]

PointIntercept_SP <- mutate(PointIntercept_SP, Site=paste0(substr(Zone, 1,1), Plot)) %>%
  select(1, Site, everything()) %>%  select(-Zone, -Plot)

PointIntercept_BB <- mutate(PointIntercept_BB, Site=paste0(substr(Zone, 1,1), Plot)) %>%
  select(1, Site, everything()) %>%  select(-Zone, -Plot)

PointIntercept_SP[c(3:ncol(PointIntercept_SP))] <- sapply(PointIntercept_SP[c(3:ncol(PointIntercept_SP))], as.numeric)
PointIntercept_BB[c(3:ncol(PointIntercept_BB))] <- sapply(PointIntercept_BB[c(3:ncol(PointIntercept_BB))], as.numeric)

write_csv(PointIntercept_SP, "/Users/smcatee/Desktop/TF/IBUTTON/PointIntercept_SP.csv")
write_csv(PointIntercept_BB, "/Users/smcatee/Desktop/TF/IBUTTON/PointIntercept_BB.csv")



## Re-cleaning to have tidy data structure
ibutton_SP <- read_csv("/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_ANALYSIS_SAMPLE_SP.csv")
ibutton_BB <- read_csv("/Users/smcatee/Desktop/TF/IBUTTON/IBUTTON_ANALYSIS_SAMPLE_BB.csv")

# SP Cols should be
# Date  Time  isDay  Site  Temperature  isSubmerged

# select out isSubmerged columns into separate df
ibutton_SP_subm <- ibutton_SP %>% select(ends_with("isSubmerged"))

# pivot_longer both df
ibutton_SP_temp <- ibutton_SP %>%
  pivot_longer(
    cols = ends_with("Temperature"),
    names_to = c("Site","eraseMe"),
    values_to = "Temperature",
    values_drop_na = FALSE,
    names_sep = "_"
    ) %>%
  select(Date, Time, isDay, Site, Temperature)

ibutton_SP_subm <- ibutton_SP_subm %>%
  pivot_longer(
    cols = ends_with("isSubmerged"),
    names_to = c("Site", "eraseMe"),
    values_to = "isSubmerged",
    values_drop_na = FALSE,
    names_sep = "_"
  ) %>% select(-eraseMe)

# Check if they fit together
all(ibutton_SP_temp$Site == ibutton_SP_subm$Site)

ibutton_SP <- add_column(ibutton_SP_temp, isSubmerged = ibutton_SP_subm$isSubmerged, .after = "Temperature")

rm(ibutton_SP_temp, ibutton_SP_subm)



# BB Cols should be
# Date  Time  isDay  Site  Temperature  isSubmerged
# select out isSubmerged columns into separate df
ibutton_BB_subm <- ibutton_BB %>% select(ends_with("isSubmerged"))

# pivot_longer both df
ibutton_BB_temp <- ibutton_BB %>%
  pivot_longer(
    cols = ends_with("Temperature"),
    names_to = c("Site","eraseMe"),
    values_to = "Temperature",
    values_drop_na = FALSE,
    names_sep = "_"
  ) %>%
  select(Date, Time, isDay, Site, Temperature)

ibutton_BB_subm <- ibutton_BB_subm %>%
  pivot_longer(
    cols = ends_with("isSubmerged"),
    names_to = c("Site", "eraseMe"),
    values_to = "isSubmerged",
    values_drop_na = FALSE,
    names_sep = "_"
  ) %>% select(-eraseMe)

# Check if they fit together
all(ibutton_BB_temp$Site == ibutton_BB_subm$Site)

ibutton_BB <- add_column(ibutton_BB_temp, isSubmerged = ibutton_BB_subm$isSubmerged, .after = "Temperature")

rm(ibutton_BB_temp, ibutton_BB_subm)




PointIntercept_SP <- read_csv("/Users/smcatee/Desktop/TF/IBUTTON/PointIntercept_SP.csv")
PointIntercept_BB <- read_csv("/Users/smcatee/Desktop/TF/IBUTTON/PointIntercept_BB.csv")
# Should be
# Date  Time  Site  Species Count

PointIntercept_SP <- pivot_longer(PointIntercept_SP, cols = 3:ncol(PointIntercept_SP),
             names_to = "MarineObjects",
             values_to = "Count",
             values_drop_na = FALSE
             )

PointIntercept_BB <- pivot_longer(PointIntercept_BB, cols = 3:ncol(PointIntercept_BB),
                                  names_to = "MarineObjects",
                                  values_to = "Count",
                                  values_drop_na = FALSE
)

write_csv(PointIntercept_SP, "/Users/smcatee/Desktop/TF/IBUTTON/PointIntercept_SP.csv")
write_csv(PointIntercept_BB, "/Users/smcatee/Desktop/TF/IBUTTON/PointIntercept_BB.csv")

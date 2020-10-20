library(readr)
library(magrittr)
#library(visdat)
library(plotly)
library(ggplot2)
#library(skimr)
# library(stats)
# library(purrr)
library(chron)
# library(tibble)
library(dplyr)
library(tidyr)
library(visdat)
#library(fitdistrplus)
#library(mixtools)

# got some ideas from https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/wea.2158



# Load data columb names https://www.ncdc.noaa.gov/crn/qcdatasets.html
durhamColNames <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/ColNames.txt",
                                    delim = " ",
                                    col_names = FALSE)
# Load Durham North Data
durhamN2017 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2017-NH_Durham_2_N.txt",
                                 delim = " ",
                                 col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
durhamN2017 %>% head()
durhamN2018 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2018-NH_Durham_2_N.txt",
                                 delim = " ",
                                 col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
durhamN2018 %>% head()
durhamN2019 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2019-NH_Durham_2_N.txt",
                                 delim = " ",
                                 col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
durhamN2019 %>% head()
durhamN2020 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2020-NH_Durham_2_N.txt",
                                 delim = " ",
                                 col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
durhamN2020 %>% head()

# Load Durham South SouthWest Data
durhamSSW2017 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2017-NH_Durham_2_SSW.txt",
                                 delim = " ",
                                 col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
durhamSSW2017 %>% head()
durhamSSW2018 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2018-NH_Durham_2_SSW.txt",
                                 delim = " ",
                                 col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
durhamSSW2018 %>% head()
durhamSSW2019 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2019-NH_Durham_2_SSW.txt",
                                 delim = " ",
                                 col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
durhamSSW2019 %>% head()
durhamSSW2020 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2020-NH_Durham_2_SSW.txt",
                                 delim = " ",
                                 col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
durhamSSW2020 %>% head()

#load manhattan data
manhattan2017 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2017-KS_Manhattan_6_SSW.txt",
                                   delim = " ",
                                   col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
manhattan2018 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2018-KS_Manhattan_6_SSW.txt",
                                   delim = " ",
                                   col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
manhattan2019 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2019-KS_Manhattan_6_SSW.txt",
                                   delim = " ",
                                   col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)
manhattan2020 <- readr::read_delim("/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/CRNH0203-2020-KS_Manhattan_6_SSW.txt",
                                   delim = " ",
                                   col_names = unlist(durhamColNames)) %>% dplyr::select(LST_DATE, LST_TIME, T_CALC)


# select LST_Date, LST_TIME, T_CALC %>% as.numeric()
durhamN <- rbind(durhamN2017,durhamN2018, durhamN2019, durhamN2020)
durhamSSW <- rbind(durhamSSW2017, durhamSSW2018, durhamSSW2019, durhamSSW2020)
manhattan <- rbind(manhattan2017, manhattan2018, manhattan2019, manhattan2020)

rm(durhamColNames, durhamN2017,durhamN2018, durhamN2019, durhamN2020,
   durhamSSW2017, durhamSSW2018, durhamSSW2019, durhamSSW2020, 
   manhattan2017, manhattan2018, manhattan2019, manhattan2020)

durhamN$T_CALC %<>% as.numeric()
durhamSSW$T_CALC %<>% as.numeric()
manhattan$T_CALC %<>% as.numeric()

# find the number that is used as NA value
durhamSSW$T_CALC %>% unique() %>% sort()

durhamN$T_CALC[durhamN$T_CALC == -9999] <- NA
durhamSSW$T_CALC[durhamSSW$T_CALC == -9999] <- NA
manhattan$T_CALC[manhattan$T_CALC == -9999] <- NA

vis_dat(durhamSSW)
vis_dat(durhamN)
vis_dat(manhattan)

# Convert dates and times to chron format
durhamSSW$LST_DATE <- as.dates(as.character(durhamSSW$LST_DATE), format = "ymd")
durhamN$LST_DATE <- as.dates(as.character(durhamN$LST_DATE), format = "ymd")
manhattan$LST_DATE <- as.dates(as.character(manhattan$LST_DATE), format = "ymd")

durhamSSW$LST_TIME %<>% as.character() %>% strtrim(2) %>% paste0(":","00:00") %>% as.times()
durhamN$LST_TIME %<>% as.character() %>% strtrim(2) %>% paste0(":","00:00") %>% as.times()
manhattan$LST_TIME %<>% as.character() %>% strtrim(2) %>% paste0(":","00:00") %>% as.times()


vis_dat(durhamSSW)
vis_dat(durhamN)
vis_dat(manhattan)


# Combine durham tables
durhamTemps <- full_join(x = durhamSSW, y = durhamN, by = c("LST_DATE", "LST_TIME"))
allTemps <- full_join(x = durhamTemps, y = manhattan, by = c("LST_DATE", "LST_TIME"))
# last row is weird, take it out
allTemps <- allTemps[-nrow(allTemps),]

vis_dat(allTemps)

colnames(allTemps) <- c("Date", "Time", "DurhamSSW", "DurhamN", "Manhattan")



## Cover assumptions for doing pearson correlation
# Normality of data
# qqline() of temp for each site

allTemps %<>% pivot_longer(cols = 3:5, names_to = "Location", values_to = "Temperature")

write_csv(allTemps, "/Users/smcatee/Desktop/TF/IBUTTON/PracticeData/allTemps.csv")

#check if temp range seems realistic
range(allTemps$Temperature, na.rm = TRUE)

#shaprio test can only take 5000 values, also plotly is slow with too much data
randsample <- allTemps[sample(nrow(allTemps), 5000), ]

randsampleDurhamN <- filter(allTemps, Location == "DurhamN")[sample(nrow(filter(allTemps, Location == "DurhamN")), 5000), ]
randsampleDurhamSSW <- filter(allTemps, Location == "DurhamSSW")[sample(nrow(filter(allTemps, Location == "DurhamSSW")), 5000), ]
randsampleManhattan <- filter(allTemps, Location == "Manhattan")[sample(nrow(filter(allTemps, Location == "Manhattan")), 5000), ]

#plot
ggplot(allTemps, aes(sample=Temperature)) +
  geom_qq_line() + geom_qq(aes(colour=Location), alpha=0.3, size=0.6) + ylim(-35, 35) +
  scale_colour_grey() + theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# density plot of all temp sets
ggplot(allTemps, aes(x=Temperature)) +
  geom_density(aes(color=Location), size=0.6) +
  scale_colour_grey() + theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) +
  xlab("ºC")


# shapiro.test() for temp for each site - want p vals to be greater than 0.05

randsampleDurhamN %>% dplyr::select(Temperature) %>% unlist() %>% shapiro.test()
randsampleDurhamSSW %>% dplyr::select(Temperature) %>% unlist() %>% shapiro.test()
randsampleManhattan %>% dplyr::select(Temperature) %>% unlist() %>% shapiro.test()

# definitely not normal. mixed, bimodal maybe

#cor(randsampleDurhamSSW$Temperature, randsampleDurhamN$Temperature, use = "pairwise.complete.obs", method = "pearson")


# Cant do pearson bc not normal, try kendalls tau
ktCorDurhamNDurhamSSW <- cor.test(allTemps$Temperature[allTemps$Location == "DurhamN"], allTemps$Temperature[allTemps$Location == "DurhamSSW"], use = "pairwise.complete.obs", method = "kendall")
ktCorDurhamNManhattan <- cor.test(allTemps$Temperature[allTemps$Location == "DurhamN"], allTemps$Temperature[allTemps$Location == "Manhattan"], use = "pairwise.complete.obs", method = "kendall")
ktCorDurhamSSWManhattan <- cor.test(allTemps$Temperature[allTemps$Location == "DurhamSSW"], allTemps$Temperature[allTemps$Location == "Manhattan"], use = "pairwise.complete.obs", method = "kendall")
ktCorDurhamNDurhamSSW$p.value
#all highly correlated, but durham stations are clearly closer


fivenum(allTemps$Temperature[allTemps$Location == "DurhamN"])
fivenum(allTemps$Temperature[allTemps$Location == "DurhamSSW"])
fivenum(allTemps$Temperature[allTemps$Location == "Manhattan"])


#compare manhattan and durhamN differences
manhattanDurhamNDifference <- allTemps$Temperature[allTemps$Location == "Manhattan"] -
  allTemps$Temperature[allTemps$Location == "DurhamN"]
hist(manhattanDurhamNDifference)
fivenum(manhattanDurhamNDifference)

#plot seasonal trends with plotly (slow)
hourPlot <- plot_ly(type='box',
                    x=allTemps$Time[allTemps$Location == "Manhattan"] %>%
                      chron::hours(),
                    y=allTemps$Temperature[allTemps$Location == "Manhattan"])

monthPlot <- plot_ly(type='box',
                     x=allTemps$Date[allTemps$Location == "Manhattan"] %>%
                       chron::dates() %>% months(),
                     y=allTemps$Temperature[allTemps$Location == "Manhattan"])

yearPlot <- plot_ly(type='box',
                    x=allTemps$Date[allTemps$Location == "Manhattan"] %>%
                      chron::years(),
                    y=allTemps$Temperature[allTemps$Location == "Manhattan"])
seasonalPlot <- plotly::subplot(hourPlot,
                                monthPlot,
                                yearPlot, 
                                nrows = 3) %>%
                layout(yaxis=list(domain=c(0,0.32)),
                       yaxis2=list(domain=c(0.34,0.65)),
                       yaxis3=list(domain=c(0.67,1))
                       )
seasonalPlot

#plot with ggplot (faster)
ggplot(allTemps[allTemps$Location == "Manhattan",],
       aes(x=Time %>% as.factor(),
           y=Temperature)) +
  geom_boxplot()

ggplot(allTemps[allTemps$Location == "Manhattan",],
       aes(x=Date %>% chron::dates() %>% months() %>% factor(),
           y=Temperature)) +
  geom_boxplot()

ggplot(allTemps[allTemps$Location == "Manhattan",],
       aes(x=Date %>% years() %>% factor(),
           y=Temperature)) +
  geom_boxplot()




# remove seasonality and get variance

#filter with 1yr lag
allTemps$Date[allTemps$Location == "Manhattan"] %>% chron::years() %>% `==`(2017) %>% sum()
#8760

#running the same yearly filter function three times reduces the seasonality much more than only a single run
yrTrendManhattan <- stats::filter(allTemps$Temperature[allTemps$Location == "Manhattan"],
              filter=(c(1/2, rep(1, times=3), 1/8759)/8760),
              method = "convolution",
              sides=2)

dayTrendManhattan <- stats::filter(allTemps$Temperature[allTemps$Location == "Manhattan"],
       filter=c(1/2, rep(1, times=23), 1/2)/24,
       method="convo",
       sides=2)

plot(yrTrendManhattan)
plot(dayTrendManhattan)

#will produce seasonal effect with any trend removed
#NA produced at end, remove
yearSeasonEffManhattan <- allTemps$Temperature[allTemps$Location == "Manhattan"] - yrTrendManhattan
yearSeasonEffManhattan %<>% head(-2)

daySeasonEffManhattan <- allTemps$Temperature[allTemps$Location == "Manhattan"] - dayTrendManhattan


plot(yearSeasonEffManhattan)
plot(daySeasonEffManhattan)


hist(daySeasonEffManhattan)
which(daySeasonEffManhattan < -13)
fivenum(daySeasonEffManhattan)


#doing getting to daily seasonal effects for the other two stations
daySeasonEffDurhamN <- 
allTemps$Temperature[allTemps$Location == "DurhamN"] - 
  stats::filter(allTemps$Temperature[allTemps$Location == "DurhamN"],
                filter=c(1/2, rep(1, times=23), 1/2)/24,
                method="convo",
                sides=2)
hist(daySeasonEffDurhamN)
fivenum(daySeasonEffDurhamN)

daySeasonEffDurhamSSW <- 
allTemps$Temperature[allTemps$Location == "DurhamSSW"] - 
  stats::filter(allTemps$Temperature[allTemps$Location == "DurhamSSW"],
                filter=c(1/2, rep(1, times=23), 1/2)/24,
                method="convo",
                sides=2)
hist(daySeasonEffDurhamSSW)
fivenum(daySeasonEffDurhamSSW)


fivenum(daySeasonEffDurhamN)
fivenum(daySeasonEffDurhamSSW)
fivenum(daySeasonEffManhattan)


add_column(allTemps[allTemps$Location == "Manhattan",], dailySeasonalEffect = daySeasonEffManhattan)

ggplot(
  add_column(allTemps[allTemps$Location == "Manhattan",], dailySeasonalEffect = daySeasonEffManhattan),
  aes(x=Date %>% chron::dates() %>% months() %>% factor(),
      y=dailySeasonalEffect)) +
  geom_boxplot()


plot(
  allTemps$Temperature[allTemps$Location == "Manhattan"] - allTemps$Temperature[allTemps$Location == "DurhamN"]
)


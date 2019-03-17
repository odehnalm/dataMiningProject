#===========================================================================
#
# DATA MINING PROJECT: ARE AIRPLANES STILL RELIABLE?
#
# --------------------------------------------------------------------------
#
# AUTHOR: MARCO ODEHNAL
# UNIVERSITÉ JEAN MONNET
#
#===========================================================================

# Setting directory
setwd("F:/Google Drive/Université Jean Monnet/Data Mining/Project/dataMiningProject")

# Libraries
library(readr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(wesanderson)
library(RColorBrewer)
library(forecast)
library(zoo)
library(xts)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# PART I: PREPARING WORKING DATA
#

# TIME: START READING
start.time <- Sys.time()

dataset <- read_csv('planecrashinfo_20181121001952.csv')
# Number of obervations
M <- dim(dataset)[1]

# Determining aircrafts shot down and hijacked by the summary
# We convert all text to lower case
crashSummary <- lapply(dataset$summary,tolower)
shotDown_index <-which(grepl('shot down',crashSummary))
hijack_index <- which(grepl('hijack',crashSummary))
accident_index <- seq(1,M)
accident_index[shotDown_index] = -1
accident_index[hijack_index] = -2
accident_index <- accident_index[-which(accident_index<0)]

# Note: we can verify that this classifying criterion is a proper partition:
length(shotDown_index) + length(hijack_index) + length(accident_index) == M

# Determining the percentage of crashes associated to battle, accidents and hijacks
shotDown_ratio <- length(shotDown_index)/M
accidents_ratio <- length(accident_index)/M
hijack_ratio <- length(hijack_index)/M

# Determining the variables that we will use in the new dataset
# 1) Fatalities
fatalities_battle <- dataset$fatalities[shotDown_index]
fatalities_hijack <- dataset$fatalities[hijack_index]
fatalities_accident <- dataset$fatalities[accident_index]

# 2) Aboard
aboard_battle <- dataset$aboard[shotDown_index]
aboard_hijack <- dataset$aboard[hijack_index]
aboard_accident <- dataset$aboard[accident_index]

# 3) Survivors
# REF: https://stackoverflow.com/questions/14543627/extracting-numbers-from-vectors-of-strings
# 3.1 Battle
numFat_battle <- as.numeric(sapply(strsplit(fatalities_battle, " "), "[[", 1))
numAboard_battle <- as.numeric(sapply(strsplit(aboard_battle, " "), "[[", 1))
numSurv_battle <-  numAboard_battle - numFat_battle

# 3.2 Hijack
numFat_hijack <- as.numeric(sapply(strsplit(fatalities_hijack, " "), "[[", 1))
numAboard_hijack <- as.numeric(sapply(strsplit(aboard_hijack, " "), "[[", 1))
numSurv_hijack <-  numAboard_hijack - numFat_hijack

# 3.3 Accident
numFat_accident <- as.numeric(sapply(strsplit(fatalities_accident, " "), "[[", 1))
numAboard_accident <- as.numeric(sapply(strsplit(aboard_accident, " "), "[[", 1))
numSurv_accident <- numAboard_accident - numFat_accident

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# PART II: CREARING OUR WORKING DATASET
#

# Putting together the discovered knowledge into a new dataset
aboard <- rep(0,M)
aboard[shotDown_index] <- numAboard_battle
aboard[hijack_index] <- numAboard_hijack
aboard[accident_index] <- numAboard_accident

fatalities <- rep(0,M)
fatalities[shotDown_index] <- numFat_battle
fatalities[hijack_index] <- numFat_hijack
fatalities[accident_index] <- numFat_accident

survivors <- rep(0,M)
survivors[shotDown_index] <- numSurv_battle
survivors[hijack_index] <- numSurv_hijack
survivors[accident_index] <- numSurv_accident

type_crash <- rep(0,M)
type_crash[shotDown_index] <- 'shot down'
type_crash[hijack_index] <- 'hijack'
type_crash[accident_index] <- 'accident'

# Dataframe creation
# Create a new dataframe with the labels
DF_crash <- data.frame( parse_date_time(dataset$date,orders = c("mdy")),
                        dataset$location,
                        dataset$ground,
                        aboard, fatalities, survivors, type_crash)
names(DF_crash) <- c("Date","Location","Ground","Aboard","Fatalities","Survivors","CrashType")
# We remove the NAs, there are not so many
DF_crash <- na.omit(DF_crash)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# PART III: TIME SERIES
#

# REFERENCES:
# ZOO package: https://cran.r-project.org/web/packages/zoo/zoo.pdf
# Parsing date format: https://stackoverflow.com/questions/36568070/extract-year-from-date
# Aggregating data: https://github.com/chrisalbon/code_r/blob/master/aggregate-by-week-or-month.r
# About ACF function to determine seasonality: 
#   https://coolstatsblog.com/2013/08/11/how-to-use-autocorreation-function-acf-to-determine-seasonality/
# Holt-Winters model in R: https://www.rdocumentation.org/packages/stats/versions/3.5.2/topics/HoltWinters
# Plotting Holt-Winters in R: https://stackoverflow.com/questions/23961181/ggplot-holt-winters-predictions

# 1) COMPARING FATALITIES AND SURVIVORS

zoo_fatalities <- zoo(na.omit(DF_crash$Fatalities),DF_crash$Date)
zoo_fatalities <- aggregate(zoo_fatalities,year,sum)

zoo_survivors <- zoo(na.omit(DF_crash$Survivors),DF_crash$Date)
zoo_survivors <- aggregate(zoo_survivors,year,sum)

DF_fat_sur <- data.frame(c(as.vector(zoo_fatalities),as.vector(zoo_survivors)),
                        c(time(zoo_fatalities),time(zoo_survivors)),
                        c('Fatalities','Survivors'))
names(DF_fat_sur) <- c("People","Year","CrashType")

# ggplot(DF_fat_sur, aes(y = People, x = Year, colour = CrashType)) +
#   geom_line(aes(linetype=CrashType),size=1.2) +
#   geom_point(aes(shape=CrashType), size=2) +
#   scale_color_brewer(palette="Dark2")

# This will be plotted in part III

# 2) PLOTTING NUMBER OF CRASHES PER YEAR
crash_table <- table(year(DF_crash$Date[which(DF_crash$CrashType == 'accident')]))
crash_table_df <- as.data.frame(crash_table)
ggplot(crash_table_df, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity",fill='tomato3') + scale_x_discrete(breaks = seq(1910, 2019, by = 20)) + ylab('Number of airplane crashes') + xlab('Year')

# 3) STUDYING FATALITIES
zoo_fatalities <- zoo(na.omit(DF_crash$Fatalities),DF_crash$Date)
zoo_fatalities <- aggregate(zoo_fatalities,as.yearqtr,sum)
# Considering time series only from 1930 on, because HoltWinters must not have NAs
zoo_ts_model <- zoo_fatalities[60:length(zoo_fatalities)]
autoplot.zoo(zoo_ts_model) + xlab('Time: quarterly') + ylab('Number of fatalities') + geom_line(color='slateblue4') +
  theme_bw() + labs(title="Fatalities time series\n")+
  theme(plot.title = element_text(size=18, face="bold"))

# ACF plot
plot(acf(zoo_ts_model,type='correlation'),main='Crash fatalities ACF plot')

# 3.1 Multiplicative Holt-Winters
H_mult <- HoltWinters(zoo_ts_model, start.periods = 4, beta=0.1,seasonal = 'multiplicative')
H_mult_pred <- predict(H_mult, n.ahead = 12)

# 3.2. Exponential smoothing
H_smooth <- HoltWinters(zoo_ts_model, gamma = FALSE, beta = FALSE)
H_smooth_pred <- predict(H_smooth, n.ahead = 12)

# 3.3 Plotting
H_mult_pred_ts <- ts(c(H_mult$fitted[, 1], H_mult_pred), start = 1930, frequency = 4)
H_smooth_pred_ts <- ts(c(H_smooth$fitted[, 1], H_smooth_pred), start = 1930, frequency = 4)
df_Holt <- merge(as.xts(zoo_ts_model), as.xts(H_mult_pred_ts), as.xts(H_smooth_pred_ts))
names(df_Holt) <- c("Actual", "HW","ES")
ggplot(df_Holt, aes(x=as.POSIXct(index(df_Holt, color=c("Actual", "HW","ES"))))) + 
  geom_line(aes(y=HW), col='red',size=0.8) + 
  geom_line(aes(y=ES), col='seagreen3',size=0.8) + 
  geom_line(aes(y=Actual), col='black') + 
  theme_bw() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-01-01")), linetype="dashed") + 
  labs(title="Holt-Winters filtering\n", x="Time", y="Observed / Fitted") + 
  theme(plot.title = element_text(size=18, face="bold"))

# 3.4 Ratio of decreasing fatalities
1-forecast_exp[12]/forecast_exp[4]

# 3.5 Plotting boxplots
# Boxplot
p1 <- ggplot(DF_fat_sur, aes(x=CrashType, y=People, fill=CrashType)) +
  geom_boxplot() + ylab('Number of people') + labs(title="Fatalities vs Survivors\n") +
  theme(plot.title = element_text(size=12, face="bold"))

fatal_obs = as.numeric(zoo_ts_model)
Quarters = rep(as.character(c(1:4)),89)
df_quart = data.frame(x,Quarters)
p2 <- ggplot(df_quart, aes(x=Quarters, y=fatal_obs, fill=Quarters)) +
  geom_boxplot() + xlab('Quarters') + ylab('Fatalities') + 
  labs(title="Fatalities per yearly quarter\n") + theme(plot.title = element_text(size=12, face="bold"))
grid.arrange(p1,p2,ncol = 2)

# TIME: STOP READING
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
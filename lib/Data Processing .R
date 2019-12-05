library(dplyr)
load("~/Downloads/injury_PlayKey.RData")
load("~/Downloads/non_injury_new.RData")

## Total distance of one track
distance_injury <- injury_PlayKey %>%
  group_by(PlayKey) %>%
  summarize(sum_dis=sum(dis))

distance_noninjury <- non_injury_new %>%
  group_by(PlayKey) %>%
  summarize(sum_dis=sum(dis))

## Mean speed of one track
speed_mean_injury <- injury_PlayKey %>%
  group_by(PlayKey) %>%
  summarize(mean_speed=mean(s))

speed_mean_noninjury <- non_injury_new %>%
  group_by(PlayKey) %>%
  summarize(mean_speed=mean(s))

## Var speed of one track
speed_var_injury <- injury_PlayKey %>%
  group_by(PlayKey) %>%
  summarize(var_speed=var(s))

speed_var_noninjury <- non_injury_new %>%
  group_by(PlayKey) %>%
  summarize(var_speed=var(s))

## Acceleration
## Injury
injury_PlayKey$pre_s <- c(0,injury_PlayKey$s[-nrow(injury_PlayKey)])
injury_PlayKey$acc <- abs((injury_PlayKey$s- injury_PlayKey$pre_s)/0.1)
injury_PlayKey <- injury_PlayKey[injury_PlayKey$time !=0,]
acc_injury <- injury_PlayKey %>% group_by(PlayKey) %>% summarize (mean_acc= mean(acc), var_acc=var(acc))
## Non-injury
non_injury_new$pre_s <- c(0,non_injury_new$s[-nrow(non_injury_new)])
non_injury_new$acc <- abs((non_injury_new$s- non_injury_new$pre_s)/0.1)
non_injury_new <- non_injury_new[non_injury_new$time !=0,]
acc_non_injury <- non_injury_new %>% group_by(PlayKey) %>% summarize (mean_acc= mean(acc), var_acc=var(acc))



## Injury
injury_PlayKey_new <- cbind(distance_injury,speed_mean_injury[,2],speed_var_injury[,2],
                            dir_injury_PlayKey[,-1])
injury_PlayKey_new <- cbind(injury_PlayKey_new,acc_injury[,-1])
injury_PlayKey_new$indicator <- "injured" 

injury_PlayKey_new_2 <- inner_join(injury_PlayKey_new,cleaned_playlist,by="PlayKey")

injury_PlayKey_summarize <- injury_PlayKey_new_2

save(injury_PlayKey_summarize,file="~/Desktop/5243/Project5/injury_PlayKey_summarize.RData")
## Non-injury
non_injury_new_2 <- cbind(distance_noninjury,speed_mean_noninjury[,2],speed_var_noninjury[,2],
                          dir_noninjury_PlayKey[,-1],acc_non_injury[,-1])
non_injury_new_2$indicator <- "non-injured"  


non_injury_new_22 <- inner_join(non_injury_new_2,cleaned_playlist,by="PlayKey")

non_injury_summarize <- non_injury_new_22

save(non_injury_summarize,file="~/Desktop/5243/Project5/non_injury_summarize.RData")

NFL_data <- rbind(injury_PlayKey_summarize,non_injury_summarize)
save(NFL_data,file="~/Desktop/5243/Project5/NFL_data.RData")
NFL_data$indicator <- ifelse(NFL_data$indicator=="non-injured",0,1)

NFL_data <- NFL_data[NFL_data$PlayType!="0",]
NFL_data <- NFL_data[NFL_data$Temperature!=-999,]
NFL_data <- na.omit(NFL_data)
print(table(NFL_data$indicator))
print(prop.table(table(NFL_data$indicator)))

# non-injury = 250 -62
length(unique(NFL_data[NFL_data$indicator==0,]$PlayerKey))
# injury = 62
length(unique(NFL_data[NFL_data$indicator==1,]$PlayerKey))

NFL_data_new <- NFL_data[,c(-1,-10,-11,-13,-14,-20,-21,-22)]
 
unique(NFL_data_new$RosterPosition)
NFL_data_new$RosterPosition <- as.factor(NFL_data_new$RosterPosition)

unique(NFL_data_new$StadiumType)
NFL_data_new$StadiumType <- as.factor(NFL_data_new$StadiumType)

unique(NFL_data_new$FieldType)
NFL_data_new$FieldType <- as.factor(NFL_data_new$FieldType)

unique(NFL_data_new$Weather)
## Clear weather 
rain <- c('30% Chance of Rain', 'Rainy', 'Rain Chance 40%', 'Showers', 'Cloudy, 50% change of rain', 'Rain likely, temps in low 40s.',
          'Cloudy with periods of rain, thunder possible. Winds shifting to WNW, 10-20 mph.',
          'Scattered Showers', 'Cloudy, Rain', 'Rain shower', 'Light Rain', 'Rain')

overcast <- c('Party Cloudy', 'Cloudy, chance of rain',
              'Coudy', 
              'Cloudy and cold', 'Cloudy, fog started developing in 2nd quarter',
              'Partly Clouidy', 'Mostly Coudy', 'Cloudy and Cool',
              'cloudy', 'Partly cloudy', 'Overcast', 'Hazy', 'Mostly cloudy', 'Mostly Cloudy',
              'Partly Cloudy', 'Cloudy')

clear <- c('Partly clear', 'Sunny and clear', 'Sun & clouds', 'Clear and Sunny',
           'Sunny and cold', 'Sunny Skies', 'Clear and Cool', 'Clear and sunny',
           'Sunny, highs to upper 80s', 'Mostly Sunny Skies', 'Cold',
           'Clear and warm', 'Sunny and warm', 'Clear and cold', 'Mostly sunny',
           'T: 51; H: 55; W: NW 10 mph', 'Clear Skies', 'Clear skies', 'Partly sunny',
           'Fair', 'Partly Sunny', 'Mostly Sunny', 'Clear', 'Sunny')

snow <- c('Cloudy, light snow accumulating 1-3"', 'Heavy lake effect snow', 'Snow')

indoor <- c('N/A Indoor', 'Indoors', 'Indoor', 'N/A (Indoors)', 'Controlled Climate')

convert_weather <- function(x) {
  if(x %in% rain) {
    "rain"
  } else if(x %in% overcast) {
    "overcast"
  } else if(x %in% clear) {
    "clear"
  } else if(x %in% snow) {
    "snow"
  } else if( x %in% indoor) {
    "indoors"
  } else {
    "unknown"
  }
}    

NFL_data_new <- NFL_data_new %>% 
  mutate(Weather = mapply(convert_weather, Weather))

unique(NFL_data_new$Temperature)
NFL_data_new <- NFL_data_new[NFL_data_new$Temperature!=-999,]
print(table(NFL_data_new$indicator))
print(prop.table(table(NFL_data_new$indicator)))

unique(NFL_data_new$Weather)
NFL_data_new$Weather <- as.factor(NFL_data_new$Weather)

unique(NFL_data_new$PlayType)
NFL_data_new <- NFL_data_new[NFL_data_new$PlayType!="0",]
NFL_data_new$PlayType <- as.factor(NFL_data_new$PlayType)

##
unique(NFL_data_new$PlayType)
##
library(DMwR)
NFL_data_new$indicator <- as.factor(NFL_data_new$indicator)
NFL_data_resample <- SMOTE(indicator ~ ., NFL_data_new, perc.over = 4900,k=5, perc.under=300)

print(table(NFL_data_resample$indicator))
print(prop.table(table(NFL_data$indicator)))

NFL_data_resample$indicator <- ifelse(NFL_data_resample$indicator==0,"non-injured","injured")
print(table(NFL_data_resample$indicator))
save(NFL_data_resample,file="~/Desktop/5243/Project5/NFL_data_resample.RData")





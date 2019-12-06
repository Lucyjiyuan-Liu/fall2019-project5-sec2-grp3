## Load Data 
InjuryRecord <- read_csv("../Data/nfl-playing-surface-analytics/InjuryRecord.csv")
PlayerTrackData <- read_csv("../Data/nfl-playing-surface-analytics/PlayerTrackData.csv")
PlayList <- read_csv("../Data/nfl-playing-surface-analytics/PlayList.csv")

join <- inner_join(PlayerTrackData,PlayList,by="PlayKey")

injury_PlayKey <- InjuryRecord[!is.na(InjuryRecord$PlayKey),]
injury_GameID <- InjuryRecord[is.na(InjuryRecord$PlayKey),]

## Get non-injury track
non_injury_new <- anti_join(PlayerTrackData,injury_GameID,by="GameID")
non_injury_new <- anti_join(non_injury_new,injury_PlayKey,by="PlayKey")

## injury track
injury_PlayKey <- injury_PlayKey[-70,]
injury_PlayKey <- inner_join(PlayerTrackData,injury_PlayKey,by="PlayKey")


## injury with only GameID
injury_GameID <- inner_join(PlayerTrackData,injury_GameID,by="GameID")


save(injury_PlayKey,file="../Output/injury_PlayKey.RData")
save(injury_GameID,file="../Output/injury_GameID.RData")
save(non_injury_new,file="../Output/non_injury_new.RData")





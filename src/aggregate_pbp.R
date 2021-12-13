library(nflfastR)
library(tidyr)

#Some rows in the data do not contribute to the countable statistics we are looking to calculate.
#These three lines help reducde the number of rows of data we have to parse
play_types <-c("pass", "run", "punt", "field_goal", "kickoff", "extra_point", "qb_kneel", "qb_spike", "no_play")
pbp <- read.csv("./data/regular_season_pbp.csv")
data <- pbp %>%dplyr::filter(.data$play_type %in% play_types)

#Passing Stats
pass_df <- data %>%
  dplyr::filter(data$play_type %in% c("pass", "qb_spike")) %>%
  dplyr::group_by(.data$passer_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    PassYd = sum(.data$passing_yards, na.rm = TRUE),
    PassTD = sum(.data$touchdown == 1 & .data$td_team == .data$posteam & .data$complete_pass == 1),
    IntThrown = sum(.data$interception),
    PassAtt = sum(.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1),
    PassComp = sum(.data$complete_pass == 1),
    PassIncomp = sum(.data$incomplete_pass, na.rm = TRUE),
    PickSix=sum(.data$interception*.data$return_touchdown),
    PickSixLen=sum(.data$interception*.data$return_touchdown*.data$return_yards),
    Sacked = sum(.data$sack),
    SackedYds = -1*sum(.data$yards_gained * .data$sack)
    
  ) %>%
  dplyr::rename(PlayerID = .data$passer_player_id) %>%
  dplyr::ungroup()

#Rushing Stats
rush_df <- data %>%
  dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
  dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    RushYd = sum(.data$rushing_yards, na.rm = TRUE),
    RushTD = sum(.data$rush_touchdown, na.rm = TRUE),
    RushAtt = dplyr::n(),
  ) %>%
  dplyr::rename(PlayerID = .data$rusher_player_id) %>%
  dplyr::ungroup()

#Reception Stats
rec_df <- data %>%
  dplyr::filter(!is.na(.data$receiver_player_id)) %>%
  dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    RecYd = sum(.data$receiving_yards, na.rm = TRUE),
    Rec = sum(.data$complete_pass == 1),
    Target = dplyr::n(),
    RecTD = sum(.data$pass_touchdown, na.rm = TRUE),
  ) %>%
  dplyr::rename(PlayerID = .data$receiver_player_id) %>%
  dplyr::ungroup()

#Kicker Stats
kicker_df <-  data %>%
  dplyr::filter(.data$play_type %in% c("field_goal", "kickoff", "extra_point")) %>%
  dplyr::group_by(.data$kicker_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    FGAtt = sum(.data$field_goal_attempt, na.rm = TRUE),
    FGMade = sum(.data$field_goal_result == 'made', na.rm = TRUE),
    FGMiss = sum(.data$field_goal_result == 'missed', na.rm = TRUE),
    XPAtt = sum(.data$extra_point_attempt, na.rm = TRUE),
    XPMade = sum(.data$extra_point_result == 'good', na.rm = TRUE)
  ) %>%
  dplyr::rename(PlayerID = .data$kicker_player_id) %>%
  dplyr::ungroup()


#Punter Stats
punter_df <- data %>%
  dplyr::filter(.data$play_type %in% c("punt")) %>%
  dplyr::group_by(.data$punter_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    Punt = sum(.data$punt_attempt, na.rm = TRUE),
    PuntYd = sum(.data$kick_distance, na.rm = TRUE),
    PuntIn20 = sum(.data$punt_inside_twenty, na.rm = TRUE),
    TB = sum(.data$touchback, na.rm = TRUE),
  ) %>%
  dplyr::rename(PlayerID = .data$punter_player_id) %>%
  dplyr::ungroup()


#KInterception Stats
inter_df <- data %>%
  dplyr::filter(.data$play_type %in% c("pass")) %>%
  dplyr::group_by(.data$interception_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    Int = sum(.data$interception),
    IntTd = sum(.data$interception * .data$return_touchdown),
    TurnoverTD = sum(.data$interception * .data$return_touchdown)
  ) %>%
  dplyr::rename(PlayerID = .data$interception_player_id) %>%
  dplyr::ungroup()


#Assist Stats
assist1_df <- data %>%
  dplyr::group_by(.data$assist_tackle_1_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Assist = sum(.data$assist_tackle)
  )%>%
  dplyr::rename(PlayerID = .data$assist_tackle_1_player_id) %>%
  dplyr::ungroup()%>% 
  drop_na()

#Assist Stats
assist2_df <- data %>%
  dplyr::group_by(.data$assist_tackle_2_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Assist = sum(.data$assist_tackle)
  )%>%
  dplyr::rename(PlayerID = .data$assist_tackle_2_player_id) %>%
  dplyr::ungroup()%>% 
  drop_na()

##Assist Stats
# assist3_df <- data %>%
#     dplyr::group_by(.data$assist_tackle_3_player_id, .data$week, .data$season) %>%
#   dplyr:: summarize(
#     Assist = sum(.data$assist_tackle)
#   )%>%
#   dplyr::rename(PlayerID = .data$assist_tackle_3_player_id) %>%
#     dplyr::ungroup() %>% 
#   drop_na()
# if(nrow(assist3_df)==0){
#   assist3_df<-rbind(assist3_df,c(1,1,2021,0))
#   names(assist3_df)=c("PlayerID", "week", "season","Assist")
# }
# 
##Assist Stats
# assist4_df <- data %>%
#     dplyr::group_by(.data$assist_tackle_4_player_id, .data$week, .data$season) %>%
#   dplyr:: summarize(
#     Assist = sum(.data$assist_tackle)
#   )%>%
#   dplyr::rename(PlayerID = .data$assist_tackle_4_player_id) %>%
#     dplyr::ungroup() %>% 
#   drop_na()
# if(nrow(assist4_df)==0){
#   assist4_df<-rbind(assist3_df,c(1,1,2021,0))
#   names(assist4_df)=c("PlayerID", "week", "season","Assist")
# }

#Assist Stats
temp_asst_df <- assist1_df%>%
  dplyr::full_join(assist2_df, by = c("PlayerID", "week", "season","Assist")) 
#%>%dplyr::full_join(assist3_df, by = c("PlayerID", "week", "season","Assist")) %>%
#dplyr::full_join(assist4_df, by = c("PlayerID", "week", "season","Assist"))

#Assist Stats
assist_df <-temp_asst_df%>%
  dplyr::group_by(PlayerID,week,season)%>%
  dplyr:: summarize(
    Assist = sum(Assist)
  )%>%
  dplyr::ungroup()


#Fumbles Stats
fumbled1_df <- data %>%
  dplyr::group_by(.data$fumbled_1_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Fumble = sum(.data$fumble),
    FumLost = sum(.data$fumble_lost))%>%
  dplyr::rename(PlayerID = .data$fumbled_1_player_id )%>%
  dplyr::ungroup() %>% 
  drop_na()

#Fumbles Stats
fumbled2_df <- data %>%
  dplyr::group_by(.data$fumbled_2_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Fumble = sum(.data$fumble),
    FumLost = sum(.data$fumble_lost)
  )%>%
  dplyr::rename(PlayerID = .data$fumbled_2_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Fumbles Stats
temp_fumbled_df <- fumbled1_df%>%
  dplyr::full_join(fumbled2_df, by = c("PlayerID", "week", "season","Fumble","FumLost"))

#Fumbles Stats
fumbled_df <-temp_fumbled_df%>%
  dplyr::group_by(PlayerID,week,season)%>%
  dplyr:: summarize(
    Fumble = sum(Fumble),
    FumLost = sum(FumLost)
  )%>%
  dplyr::ungroup()


#Fumbles Forced Stats
fumblfrc1_df <- data %>%
  dplyr::group_by(.data$forced_fumble_player_1_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    FrcFum = sum(.data$fumble_forced))%>%
  dplyr::rename(PlayerID = .data$forced_fumble_player_1_player_id)%>%
  dplyr::ungroup() %>% 
  drop_na()

#Fumbles Forced Stats
fumblfrc2_df <- data %>%
  dplyr::group_by(.data$forced_fumble_player_2_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    FrcFum = sum(.data$fumble_forced)
  )%>%
  dplyr::rename(PlayerID = .data$forced_fumble_player_2_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Fumbles Forced Stats
temp_fumblfrc_df <- fumblfrc1_df%>%
  dplyr::full_join(fumblfrc2_df, by = c("PlayerID", "week", "season","FrcFum"))

#Fumbles Forced Stats
fumblfrc_df <-temp_fumblfrc_df%>%
  dplyr::group_by(PlayerID,week,season)%>%
  dplyr:: summarize(
    FrcFum = sum(FrcFum)
  )%>%
  dplyr::ungroup()


#Blocked Kick Stats
blocked_df <- data %>%
  dplyr::filter(.data$play_type %in% c("field_goal", "punt", "extra_point"))%>%
  dplyr::group_by(.data$blocked_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    PuntsBlocked = sum(.data$punt_blocked),
    FGsBlocked = sum(.data$field_goal_result == 'blocked', na.rm = TRUE),
    XPsBlocked = sum(.data$extra_point_result == 'blocked', na.rm = TRUE)
  )%>%
  dplyr::rename(PlayerID = .data$blocked_player_id) %>%
  dplyr::ungroup()%>% 
  drop_na()


#Kick Return Stats
kicker_ret_df <-  data %>%
  dplyr::filter(.data$play_type %in% c("kickoff"))%>%
  dplyr::group_by(.data$kickoff_returner_player_id, .data$week, .data$season) %>%  
  dplyr:: summarize(
    KickRet = sum(.data$kickoff_attempt),
    KickRetTD = sum(.data$return_touchdown),
    KickRetYd = sum(.data$return_yards),
    LenKickRetTD = sum(.data$return_touchdown*.data$return_yards)
  )%>%
  dplyr::rename(PlayerID = .data$kickoff_returner_player_id) %>%
  dplyr::ungroup()%>% 
  drop_na()


#Fumble Recovery Stats
fumblrec1_df <- data %>%
  dplyr::group_by(.data$fumble_recovery_1_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    FumRec = sum(.data$fumble),
    FumTd = sum(.data$fumble*.data$return_touchdown),
    FumTdLen = sum(.data$fumble*.data$return_touchdown*.data$fumble_recovery_1_yards)
  )%>%
  dplyr::rename(PlayerID = .data$fumble_recovery_1_player_id)%>%
  dplyr::ungroup() %>% 
  drop_na()

#Fumble Recovery Stats
fumblrec2_df <- data %>%
  dplyr::group_by(.data$fumble_recovery_2_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    FumRec = sum(.data$fumble),
    FumTd = sum(.data$fumble*.data$return_touchdown),
    FumTdLen = sum(.data$fumble*.data$return_touchdown*.data$fumble_recovery_2_yards)
  )%>%
  dplyr::rename(PlayerID = .data$fumble_recovery_2_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Fumble Recovery Stats
temp_fumblrec_df <- fumblrec1_df%>%
  dplyr::full_join(fumblrec2_df, by = c("PlayerID", "week", "season","FumRec","FumTd","FumTdLen"))

#Fumble Recovery Stats
fumblrec_df <-temp_fumblrec_df%>%
  dplyr::group_by(PlayerID,week,season)%>%
  dplyr:: summarize(
    FumRec = sum(FumRec),
    FumTd = sum(FumTd),
    FumTdLen = sum(FumTdLen)
  )%>%
  dplyr::ungroup()


#Penaly Stats
penalty_df <- data %>%
  dplyr::group_by(.data$penalty_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Penalty = sum(.data$penalty),
    PenaltyYds = sum(.data$penalty_yards)
  )%>%
  dplyr::rename(PlayerID = .data$penalty_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()


#Punt Return Stats
pntret_df <- data %>%
  dplyr::filter(.data$play_type %in% c("punt"))%>%
  dplyr::group_by(.data$punt_returner_player_id, .data$week, .data$season)%>%
  dplyr:: summarize(
    PntReturns = sum(.data$punt_attempt),
    PntRetYd = sum(.data$return_yards),
    PuntReturnedTD = sum(.data$return_touchdown),
    LenPuntReturnTD = sum(.data$return_touchdown*.data$return_yards)
  )%>%
  dplyr::rename(PlayerID = .data$punt_returner_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()


#QB Hit Stats
qbhit1_df <- data %>%
  dplyr::filter(.data$play_type %in% c("pass","run")) %>%
  dplyr::group_by(.data$qb_hit_1_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    QBHit = sum(.data$qb_hit)
  )%>%
  dplyr::rename(PlayerID = .data$qb_hit_1_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#QB Hit Stats
qbhit2_df <- data %>%
  dplyr::filter(.data$play_type %in% c("pass","run")) %>%
  dplyr::group_by(.data$qb_hit_2_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    QBHit = sum(.data$qb_hit)
  )%>%
  dplyr::rename(PlayerID = .data$qb_hit_2_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#QB Hit Stats
temp_qbhit_df <- qbhit1_df%>%
  dplyr::full_join(qbhit2_df, by = c("PlayerID", "week", "season","QBHit"))

#QB Hit Stats
qbhit_df <-temp_qbhit_df%>%
  dplyr::group_by(PlayerID,week,season)%>%
  dplyr:: summarize(
    QBHit = sum(QBHit)
  )%>%
  dplyr::ungroup()



#Sack Stats
sack_df <-data %>%
  dplyr::filter(.data$play_type %in% c("pass","run")) %>%
  dplyr::group_by(.data$sack_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Sack = sum(.data$sack)
  )%>%
  dplyr::rename(PlayerID = .data$sack_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()


#Tackle Stats
solo1_df <-data %>%
  dplyr::group_by(.data$solo_tackle_1_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Tackle = sum(.data$solo_tackle)
  )%>%
  dplyr::rename(PlayerID = .data$solo_tackle_1_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Tackle Stats
solo2_df <-data %>%
  dplyr::group_by(.data$solo_tackle_2_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Tackle = sum(.data$solo_tackle)
  )%>%
  dplyr::rename(PlayerID = .data$solo_tackle_2_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()



#Tackle Stats
half1_df <-data %>%
  dplyr::filter(.data$play_type %in% c("pass","run")) %>%
  dplyr::group_by(.data$half_sack_1_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Sack = .5*sum(.data$sack)
  )%>%
  dplyr::rename(PlayerID = .data$half_sack_1_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Tackle Stats
half2_df <-data %>%
  dplyr::filter(.data$play_type %in% c("pass","run")) %>%
  dplyr::group_by(.data$half_sack_2_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Sack = .5*sum(.data$sack)
  )%>%
  dplyr::rename(PlayerID = .data$half_sack_2_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Tackle Stats
temp_sack_df <- sack_df%>%
  dplyr::full_join(half1_df, by = c("PlayerID", "week", "season","Sack"))%>%
  dplyr::full_join(half2_df, by = c("PlayerID", "week", "season","Sack"))

#Tackle Stats
sack_df <-temp_sack_df%>%
  dplyr::group_by(PlayerID,week,season)%>%
  dplyr:: summarize(
    Sack = sum(Sack)
  )%>%
  dplyr::ungroup()


#Pass Defense Stats
passdef1_df <-data %>%
  dplyr::filter(.data$play_type %in% c("pass")) %>%
  dplyr::group_by(.data$pass_defense_1_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    PassDef = sum(.data$pass_attempt)
  )%>%
  dplyr::rename(PlayerID = .data$pass_defense_1_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Pass Defense Stats
passdef2_df <-data %>%
  dplyr::filter(.data$play_type %in% c("pass")) %>%
  dplyr::group_by(.data$pass_defense_2_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    PassDef = sum(.data$pass_attempt)
  )%>%
  dplyr::rename(PlayerID = .data$pass_defense_2_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Pass Defense Stats
temp_passdef_df <- passdef1_df%>%
  dplyr::full_join(passdef2_df, by = c("PlayerID", "week", "season","PassDef"))

#Pass Defense Stats
passdef_df <-temp_passdef_df%>%
  dplyr::group_by(PlayerID,week,season)%>%
  dplyr:: summarize(
    PassDef = sum(PassDef)
  )%>%
  dplyr::ungroup()


#Tackle For Loss Stats
tfl1_df <-data %>%
  dplyr::filter(.data$play_type %in% c("pass","run")) %>%
  dplyr::group_by(.data$tackle_for_loss_1_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    TFL = sum(.data$solo_tackle)
  )%>%
  dplyr::rename(PlayerID = .data$tackle_for_loss_1_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Tackle For Loss Stats
tfl2_df <-data %>%
  dplyr::filter(.data$play_type %in% c("pass","run")) %>%
  dplyr::group_by(.data$tackle_for_loss_2_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    TFL = sum(.data$solo_tackle)
  )%>%
  dplyr::rename(PlayerID = .data$tackle_for_loss_2_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Tackle For Loss Stats
temp_tfl_df <- tfl1_df#%>%
#   dplyr::full_join(tfl2_df, by = c("PlayerID", "week", "season","TFL"))

#Tackle For Loss Stats
tfl_df <-temp_tfl_df#%>%
#     dplyr::group_by(PlayerID,week,season)%>%
#   dplyr:: summarize(
#    TFL = sum(TFL)
#   )%>%
#     dplyr::ungroup()


#Safety Stats
safety_df <-data %>%
  dplyr::filter(.data$play_type %in% c("pass","run")) %>%
  dplyr::group_by(.data$safety_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    SafetyForc = sum(.data$safety)
  )%>%
  dplyr::rename(PlayerID = .data$safety_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()


#Tackle Stats
TackwAst1_df <-data %>%
  dplyr::group_by(.data$tackle_with_assist_1_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Tackle = .75*sum(.data$tackle_with_assist)
  )%>%
  dplyr::rename(PlayerID = .data$tackle_with_assist_1_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Tackle Stats
TackwAst2_df <-data %>%
  dplyr::group_by(.data$tackle_with_assist_2_player_id, .data$week, .data$season) %>%
  dplyr:: summarize(
    Tackle = .75*sum(.data$tackle_with_assist)
  )%>%
  dplyr::rename(PlayerID = .data$tackle_with_assist_2_player_id) %>%
  dplyr::ungroup() %>% 
  drop_na()

#Tackle Stats
temp_Tackle_df <- TackwAst1_df%>%
  dplyr::full_join(solo1_df, by = c("PlayerID", "week", "season","Tackle"))%>%
  dplyr::full_join(solo2_df, by = c("PlayerID", "week", "season","Tackle"))

#Tackle Stats
Tackle_df <-temp_Tackle_df%>%
  dplyr::group_by(PlayerID,week,season)%>%
  dplyr:: summarize(
    Tackle = sum(Tackle)
  )%>%
  dplyr::ungroup()











#Puts together all stats in to a single Dataframe
summary_df <-assist_df%>%
  dplyr::full_join(blocked_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(fumbled_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(fumblfrc_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(fumblrec_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(inter_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(kicker_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(kicker_ret_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(pass_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(passdef_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(penalty_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(pntret_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(punter_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(qbhit_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(rec_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(rush_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(sack_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(safety_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(Tackle_df,by = c("PlayerID", "week", "season"))%>%
  dplyr::full_join(tfl_df,by = c("PlayerID", "week", "season"))


#Prints all the Summary Stats into a file in the data folder.
write.csv(summary_df, "./data/summary_statistics.csv")


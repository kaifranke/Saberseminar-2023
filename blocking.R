library(baseballr)
library(dplyr)
library(ggplot2)
library(xgboost)
library(MLmetrics)
library(tidyverse)
library(readr)
library(caret)



#### Data Preparation ####

game_pk = unique(pitches2022$game_pk)

pbp_data = get_pbp_mlb(game_pk[1])

for (i in 2:length(game_pk)) {
  pbp_data = rbind(pbp_data, get_pbp_mlb(game_pk[i]), fill = TRUE)
}

game_pkC = unique(pbp_data$game_pk)

new = game_pk[game_pk != game_pkC]
length(new)

for (i in 1:length(new)) {
  pbp_data = rbind(pbp_data, get_pbp_mlb(new[i]), fill = TRUE)
}

df = apply(pbp_data,2,as.character)




pbp_data = read.csv(file = "pbp2022.csv")  # READING IN FILE FROM CODE ABOVE

summary(pbp_data)

pbp_data$atBatIndex = as.numeric(pbp_data$atBatIndex)

block = pbp_data %>%  # PUTTING FILES TOGETHER TO HAVE MOVEMENT DATA WITH BLOCKS
  select(game_pk, game_date, about.inning, about.halfInning, count.balls.start, count.strikes.start, 
         count.outs.start, atBatIndex, pitchNumber, pitchData.coordinates.pX, pitchData.coordinates.pZ, 
         pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ, pitchData.coordinates.x0, pitchData.coordinates.y0,
         pitchData.coordinates.z0, pitchData.startSpeed, details.awayScore, details.homeScore, 
         matchup.batSide.code, matchup.pitchHand.code, matchup.splits.menOnBase, 
         details.isStrike, details.description, details.event, details.call.description, result.description,
         result.eventType) %>%
  mutate(about.halfInning = ifelse(about.halfInning == "bottom", "Bot", "Top"),
         pitchData.coordinates.pX = round(pitchData.coordinates.pX, 2),
         pitchData.coordinates.pZ = round(pitchData.coordinates.pZ, 2),
         pitchData.coordinates.pfxX = round(pitchData.coordinates.pfxX, 2),
         pitchData.coordinates.pfxZ = round(pitchData.coordinates.pfxZ, 2))



wildPassed = block %>%
  filter(details.event %in% c("Passed Ball", "Wild Pitch")) %>%
  select(game_pk, game_date, about.inning, about.halfInning, count.balls.start, count.strikes.start, count.outs.start,
         atBatIndex)

block2 = block %>%
  filter(!is.na(pitchData.coordinates.pX),
         !is.na(pitchData.coordinates.pfxX),
         !grepl("In play", details.description),
         !grepl("Foul", details.description),
         details.description != "Pitchout",
         details.description != "Hit By Pitch")


wildPassed2 = inner_join(wildPassed, block2, by = c("game_pk", "game_date", "about.inning", "about.halfInning", "count.balls.start", "count.strikes.start", "count.outs.start",
                                     "atBatIndex")) %>%
  filter(details.description != "In play, run(s)",
         details.description != "In play, out(s)",
         details.description != "In play, no out",
         details.description != "Foul") %>%
  mutate(blocked = 0)


ggplot(wildPassed2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ)) +
  geom_point()



all = left_join(block2, wildPassed2) %>%
  mutate(blocked = ifelse(is.na(blocked), 1, blocked))

all = all %>%
  rename(pfx_x = pitchData.coordinates.pfxX,
         pfx_z = pitchData.coordinates.pfxZ,
         rel_pos_x = pitchData.coordinates.x0,
         rel_pos_y = pitchData.coordinates.y0,
         rel_pos_z = pitchData.coordinates.z0,
         release_speed = pitchData.startSpeed,
         plate_x = pitchData.coordinates.pX,
         plate_z = pitchData.coordinates.pZ) %>%
  mutate(platoon = ifelse(matchup.batSide.code == "R" & matchup.pitchHand.code == "R", "RhRp", NA),
         platoon = ifelse(matchup.batSide.code == "R" & matchup.pitchHand.code == "L", "RhLp", platoon),
         platoon = ifelse(matchup.batSide.code == "L" & matchup.pitchHand.code == "L", "LhLp", platoon),
         platoon = ifelse(matchup.batSide.code == "L" & matchup.pitchHand.code == "R", "LhRp", platoon))







all %>%
  group_by(matchup.splits.menOnBase) %>%
  summarise(n = n(),
            mBlo = mean(blocked))












modDat = all %>%
  filter(matchup.splits.menOnBase != "Empty") %>%
  select(pfx_x, pfx_z, plate_x, plate_z, release_speed, rel_pos_x, rel_pos_z, rel_pos_y, platoon, blocked, 
         game_pk, game_date, about.inning, about.halfInning, count.balls.start, count.strikes.start, count.outs.start,
         atBatIndex, pitchNumber, matchup.splits.menOnBase, result.description) 




set.seed(1)
random_sample = createDataPartition(modDat$blocked,
                                     p = 0.7, list = FALSE)

train = modDat[random_sample, ]


test = modDat[-random_sample, ]


#### EDA ####

ggplot(train, aes(plate_x, plate_z, z = blocked)) +
  stat_summary_hex()

ggplot(train, aes(pfx_x, pfx_z, z = blocked)) +
  stat_summary_hex()

train %>%
  mutate(speed = ifelse(release_speed >= 95, "95+", NA),
         speed = ifelse(release_speed < 95 & release_speed >= 92, "92-95", speed),
         speed = ifelse(release_speed < 92 & release_speed >= 88, "88-92", speed),
         speed = ifelse(release_speed < 88 & release_speed >= 84, "84-88", speed),
         speed = ifelse(release_speed < 84 & release_speed >= 80, "80-84", speed),
         speed = ifelse(release_speed < 80 & release_speed >= 75, "75-80", speed),
         speed = ifelse(release_speed < 76 & release_speed >= 70, "70-75", speed)) %>%
  filter(!is.na(speed)) %>%
  group_by(speed) %>%
  summarise(blockP = mean(blocked, na.rm = T)) %>%
  ggplot(aes(speed, blockP)) +
  geom_col()


ggplot(train, aes(rel_pos_x, rel_pos_z, z = blocked)) +
  stat_summary_hex()

train %>%
  group_by(platoon) %>%
  summarise(n = n(),
            blocks = sum(blocked),
            Bp = blocks / n)



### BASELINE MODEL ####



baselineMod = xgboost(data = data.matrix(train[,1:9]),
                      label = train$blocked,
                      eta = 0.1, 
                      nrounds = 50,
                      objective = "binary:logistic",
                      eval_metric = "logloss")

bTrPred = predict(baselineMod, newdata = data.matrix(train[,1:9]))
basePred = predict(baselineMod, newdata = data.matrix(test[,1:9]))


AUC(bTrPred, train$blocked)
AUC(basePred, test$blocked)

LogLoss(bTrPred, train$blocked)
LogLoss(basePred, test$blocked)


finBase = cbind(test, basePred)

ggplot(finBase, aes(plate_x, plate_z, z = basePred)) +
  stat_summary_hex() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = .85)


newConfus = data.frame(cbind(ifelse(basePred >= .99, 1, 0), test$blocked)) %>%
  mutate(correct = ifelse(X1 == X2, 1, 0),
         label = ifelse(X2 == 1 & correct == 1, "TP", NA),
         label = ifelse(X2 == 1 & correct == 0, "FP", label),
         label = ifelse(X2 == 0 & correct == 0, "FN", label),
         label = ifelse(X2 == 0 & correct == 1, "TN", label)) %>%
  group_by(label) %>%
  summarise(n = n())

TP = filter(newConfus, label == "TP")$n
FP = filter(newConfus, label == "FP")$n
FN = filter(newConfus, label == "FN")$n


TP / (TP + FP)
TP / (TP + FN)


mean(filter(finBase, plate_z <= 0)$blocked)
mean(filter(finBase, plate_z <= 0)$basePred)

mean(filter(finBase, plate_z >= 4)$blocked)
mean(filter(finBase, plate_z >= 4)$basePred)





importance_matrix = data.frame(xgb.importance(colnames(train[,1:9]), model = baselineMod)) %>%
  mutate(Feature = ifelse(Feature == "plate_z", "Vert Loc", Feature),
         Feature = ifelse(Feature == "plate_x", "Horz Loc", Feature),
         Feature = ifelse(Feature == "rel_pos_z", "V Rel Pt", Feature),
         Feature = ifelse(Feature == "pfx_x", "Horz Mvt", Feature),
         Feature = ifelse(Feature == "release_speed", "Velo", Feature),
         Feature = ifelse(Feature == "pfx_z", "Vert Mvt", Feature),
         Feature = ifelse(Feature == "rel_pos_x", "H Rel Pt", Feature),
         Feature = ifelse(Feature == "rel_pos_y", "Extension", Feature),
         Feature = ifelse(Feature == "platoon", "Platoon", Feature))

ggplot(importance_matrix, aes(reorder(Feature, -Gain), Gain, fill = Feature)) +
  geom_col(color = "black") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Feature", title = "XGBoost Feature Importance", subtitle = "Baseline Model",
       caption = "Jack Rogers and Kai Franke, Saberseminar 2023") +
  geom_text(aes(label = round(Gain, 3)), vjust = -.2, colour = "black") +
  ylim(0, .55) +
  theme(plot.subtitle = element_text(family = "serif", face = "bold", hjust = 0.5),
        plot.caption = element_text(family = "serif", hjust = 0.5),
        plot.title = element_text(family = "serif", face = "bold", hjust = 0.5))



#### Model w/ Grid Search ####


nrounds = seq(from = 500, to = 800, by = 100)
eta = c(0.05, 0.1)
max_depth = c(7, 8)
gamma = 0
colsample_bytree = 1
min_child_weight = 1
subsample = 1
scale_pos_weight = c(0.3, .5, 1)

results = matrix(NA, nrow = length(nrounds) * length(eta) * length(max_depth) * length(scale_pos_weight),
                 ncol = 12)
colnames(results) = c("nrounds", "eta", "max_depth", "scale_pos_weight", "mean", "min", "max", "AUC", "LL","Precision", "Recall", "lowPreds")

it = 1

for (i in 1:length(nrounds)) {
  for (j in 1:length(eta)) {
    for (k in 1:length(max_depth)) {
      for (l in 1:length(scale_pos_weight)) {
        
        print(paste0("####### Iteration ", it, " #######"))
        
        set.seed(1)
        
        modLoop = xgboost(data = data.matrix(train[,1:7]),
                          label = train$blocked,
                          nrounds = nrounds[i],
                          eta = eta[j],
                          max_depth = max_depth[k],
                          scale_pos_weight = scale_pos_weight[l],
                          gamma = gamma,
                          colsample_bytree = colsample_bytree,
                          objective = "binary:logistic")
        
        predIt = predict(modLoop, newdata = data.matrix(test[,1:7]))
        
        results[it, 1] = nrounds[i]
        results[it, 2] = eta[j]
        results[it, 3] = max_depth[k]
        results[it, 4] = scale_pos_weight[l]
        results[it, 5] = mean(predIt)
        results[it, 6] = min(predIt)
        results[it, 7] = max(predIt)
        results[it, 8] = AUC(predIt, test$blocked)
        results[it, 9] = LogLoss(predIt, test$blocked)
        
        newConfus = data.frame(cbind(ifelse(predIt >= .99, 1, 0), test$blocked)) %>%
          mutate(correct = ifelse(X1 == X2, 1, 0),
                 label = ifelse(X2 == 1 & correct == 1, "TP", NA),
                 label = ifelse(X2 == 1 & correct == 0, "FP", label),
                 label = ifelse(X2 == 0 & correct == 0, "FN", label),
                 label = ifelse(X2 == 0 & correct == 1, "TN", label)) %>%
          group_by(label) %>%
          summarise(n = n())
        
        TP = filter(newConfus, label == "TP")$n
        FP = filter(newConfus, label == "FP")$n
        FN = filter(newConfus, label == "FN")$n
        
        results[it, 10] = TP / (TP + FP)
        results[it, 11] = TP / (TP + FN)
        
        loopFin = cbind(test, predIt)
        results[it, 12] = mean(filter(loopFin, plate_z <= 0)$predIt)
        
        
        it = it + 1
        
      }
    }
  }
}



set.seed(1)
mod = xgboost(data = data.matrix(train[,1:7]),
              label = train$blocked,
              nrounds = 500,
              eta = .05,
              max_depth = 8,
              scale_pos_weight = .5,
              gamma = 0,
              colsample_bytree = 1,
              min_child_weight = 1,
              subsample = 1,
              objective = "binary:logistic")

modPred = predict(mod, newdata = data.matrix(test[,1:7]))

fin = cbind(test, modPred)


ggplot(fin, aes(plate_x, plate_z, z = blocked)) +
  stat_summary_hex() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = .75)

ggplot(fin, aes(plate_x, plate_z, z = modPred)) +
  stat_summary_hex() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = .85) +
  theme_bw()




newConfus = data.frame(cbind(ifelse(modPred >= .99, 1, 0), test$blocked)) %>%
  mutate(correct = ifelse(X1 == X2, 1, 0),
         label = ifelse(X2 == 1 & correct == 1, "TP", NA),
         label = ifelse(X2 == 1 & correct == 0, "FP", label),
         label = ifelse(X2 == 0 & correct == 0, "FN", label),
         label = ifelse(X2 == 0 & correct == 1, "TN", label)) %>%
  group_by(label) %>%
  summarise(n = n())

TP = filter(newConfus, label == "TP")$n
FP = filter(newConfus, label == "FP")$n
FN = filter(newConfus, label == "FN")$n


TP / (TP + FP)
TP / (TP + FN)


mean(filter(fin, plate_z <= 0)$blocked)
mean(filter(fin, plate_z <= 0)$modPred)

mean(filter(fin, plate_z >= 4)$blocked)
mean(filter(fin, plate_z >= 4)$modPred)

LogLoss(modPred, test$blocked)
AUC(modPred, test$blocked)








fin = fin %>%
  mutate(BAX = ifelse(blocked == 1, 1 - modPred, -modPred))


ggplot(fin, aes(plate_x, plate_z, z = BAX)) +
  stat_summary_hex()






#### 2023 Data Retrieval ####


start = as.Date("2023-03-30")
end = as.Date("2023-07-16")

theDate = start

data2023 = scrape_statcast_savant_pitcher(start_date = theDate, end_date = theDate)

theDate = theDate + 1
 
while (theDate <= end) {
    
  data2023 = rbind(data2023, scrape_statcast_savant_pitcher(start_date = theDate, end_date = theDate))
  print(paste0("Done loading date: ", theDate))
  theDate = theDate + 1
                
}




games2023 = unique(data2023$game_pk)


pbp2023 = get_pbp_mlb(games2023[1])

for (i in 2:length(games2023)) {
 
 pbp2023 = rbind(pbp2023, get_pbp_mlb(games2023[i]), fill = TRUE)
 
 print(paste0("Game #", i, " done."))
   
}

pbp2023 = apply(pbp2023,2,as.character)


pbp2023 = read.csv(file = "pbp2023.csv")






pbp2023$atBatIndex = as.numeric(pbp2023$atBatIndex)

pitch2023 = pbp2023 %>%
  arrange(game_date, game_pk, startTime) %>%
  select(game_pk, game_date, startTime, about.inning, about.halfInning, details.call.code,
         count.balls.start, count.strikes.start, 
         count.outs.start, atBatIndex, pitchNumber, pitchData.coordinates.pX, pitchData.coordinates.pZ, 
         pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ, pitchData.coordinates.x0, pitchData.coordinates.y0,
         pitchData.coordinates.z0, pitchData.startSpeed, details.awayScore, details.homeScore, 
         matchup.batSide.code, matchup.pitchHand.code, matchup.splits.menOnBase, 
         details.isStrike, details.description, details.event, details.call.description, result.description,
         result.eventType) %>%
  mutate(about.halfInning = ifelse(about.halfInning == "bottom", "Bot", "Top"),
         pitchData.coordinates.pX = round(pitchData.coordinates.pX, 2),
         pitchData.coordinates.pZ = round(pitchData.coordinates.pZ, 2),
         pitchData.coordinates.pfxX = round(pitchData.coordinates.pfxX, 2),
         pitchData.coordinates.pfxZ = round(pitchData.coordinates.pfxZ, 2))



altBlock = pitch2023 %>%
  filter(!grepl("In play", details.description),
         !grepl("Foul", details.description),
         details.description != "Pitchout",
         details.description != "Hit By Pitch",
         matchup.splits.menOnBase != "Empty") %>%
  mutate(blocked = ifelse(lead(details.event) %in% c("Passed Ball", "Wild Pitch"), 0, 1)) %>%
  arrange(game_date, game_pk, startTime) %>%
  filter(!is.na(pitchData.coordinates.pX))


ggplot(altBlock, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z = blocked)) +
  stat_summary_hex()


all2023 = altBlock %>%
  rename(pfx_x = pitchData.coordinates.pfxX,
         pfx_z = pitchData.coordinates.pfxZ,
         plate_x = pitchData.coordinates.pX,
         plate_z = pitchData.coordinates.pZ,
         release_speed = pitchData.startSpeed,
         rel_pos_x = pitchData.coordinates.x0,
         rel_pos_y = pitchData.coordinates.y0,
         rel_pos_z = pitchData.coordinates.z0) %>%
  mutate(platoon = ifelse(matchup.batSide.code == "R" & matchup.pitchHand.code == "R", "RhRp", NA),
         platoon = ifelse(matchup.batSide.code == "R" & matchup.pitchHand.code == "L", "RhLp", platoon),
         platoon = ifelse(matchup.batSide.code == "L" & matchup.pitchHand.code == "L", "LhLp", platoon),
         platoon = ifelse(matchup.batSide.code == "L" & matchup.pitchHand.code == "R", "LhRp", platoon))




modDat2023 = all2023 %>%
  filter(matchup.splits.menOnBase != "Empty") %>%
  select(pfx_x, pfx_z, plate_x, plate_z, release_speed, rel_pos_x, rel_pos_z, rel_pos_y, platoon, blocked, 
         game_pk, game_date, about.inning, about.halfInning, count.balls.start, count.strikes.start, count.outs.start,
         atBatIndex, pitchNumber, matchup.splits.menOnBase, result.description) 



pred2023 = predict(mod, newdata = data.matrix(modDat2023[,1:7]))

LogLoss(pred2023, modDat2023$blocked)
AUC(pred2023, modDat2023$blocked)


mod2023fin = cbind(modDat2023, pred2023)




ggplot(mod2023fin, aes(plate_x, plate_z, z = pred2023)) +
  stat_summary_hex(bins = 17) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = .8) +
  theme_bw() +
  xlim(-5, 5) +
  coord_fixed() +
  labs(title = "Predicted Block Probability by Location", 
       caption = "Jack Rogers and Kai Franke, Saberseminar 2023",
       subtitle = "2023 Pitch Data",  fill = "xBlock%", x = "Horizontal Location",
       y = "Vertical Location") +
  theme(plot.subtitle = element_text(family = "serif", face = "bold", hjust = 0.5),
        plot.caption = element_text(family = "serif", hjust = 0.5),
        plot.title = element_text(family = "serif", face = "bold", hjust = 0.5))





mod2023fin %>%
  group_by(matchup.splits.menOnBase) %>%
  summarise(n = n(),
            blocks = sum(blocked),
            xBlocks = sum(pred2023),
            BAX = blocks - xBlocks,
            blockedP = mean(blocked),
            xBlocked = mean(pred2023),
            BAXp = blockedP - xBlocked) %>%
  arrange(desc(BAX)) %>%
  ggplot(aes(reorder(matchup.splits.menOnBase, -BAXp), BAXp, fill = matchup.splits.menOnBase)) +
  geom_col(color = "black") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Base Runner State", y = "Blocked Above Expected", 
       title = "Blocks Above Expected by Base Runner State", subtitle = "2023 Season Until July 9th",
       caption = "Jack Rogers and Kai Franke, Saberseminar 2023") +
  theme(plot.subtitle = element_text(family = "serif", face = "bold", hjust = 0.5),
        plot.caption = element_text(family = "serif", hjust = 0.5),
        plot.title = element_text(family = "serif", face = "bold", hjust = 0.5),
        legend.position = "none")


mod2023fin %>%
  group_by(platoon) %>%
  summarise(n = n(),
            blocks = sum(blocked),
            xBlocks = sum(pred2023),
            BAX = blocks - xBlocks,
            blockedP = mean(blocked),
            xBlocked = mean(pred2023),
            BAXp = blockedP - xBlocked) %>%
  ggplot(aes(reorder(platoon, -BAXp), BAXp, fill = platoon)) +
  geom_col(color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Platoon Matchup", y = "Blocked Above Expected", 
       title = "Blocks Above Expected by Platoon Matchup",
       subtitle = "2023 Season Until July 9th",
       caption = "Jack Rogers and Kai Franke, Saberseminar 2023") +
  theme_bw() +
  theme(plot.subtitle = element_text(family = "serif", face = "bold", hjust = 0.5),
        plot.caption = element_text(family = "serif", hjust = 0.5),
        plot.title = element_text(family = "serif", face = "bold", hjust = 0.5),
        legend.position = "none")



mod2023fin2 = mod2023fin %>%
  mutate(pfx_x = round(pfx_x / 12, 2), pfx_z = round(pfx_z / 12, 2))




pitches2023 = read.csv(file = "pitches2023.csv")


catchers = pitches2023 %>%
  group_by(fielder_2) %>%
  summarize(n = n())

batters = pitches2023 %>%
  group_by(name = word(des, start = 1, end = 2)) %>%
  summarise(id = round(mean(batter)))

catchers = left_join(catchers, batters, by = c("fielder_2" = "id")) %>%
  filter(!is.na(name),
         !grepl("With", name),
         !grepl("Pitcher", name),
         name != "Bryan Baker",
         name != "Dylan Cease")

mod2023fin3 = inner_join(mod2023fin2, pitches2023, by = c("plate_x", "plate_z", "release_speed", "game_pk", "game_date")) %>%
  select(pfx_x = pfx_x.x, pfx_z.x, plate_x, plate_z, release_speed, rel_pos_x, rel_pos_z, rel_pos_y,
         platoon, blocked, pred2023, game_pk, game_date, count.balls.start, count.strikes.start,
         count.outs.start, fielder_2, on_1b, on_2b, on_3b, result.description, pitch_type)


playersDat = inner_join(mod2023fin3, catchers, by = c("fielder_2")) %>%
  mutate(BAX = ifelse(blocked == 1, 1 - pred2023, -pred2023))


catcherResults = playersDat %>%
  group_by(name) %>%
  summarize(n = n(),
            blocks = sum(blocked),
            xBlocks = sum(pred2023),
            xBLp = xBlocks / n,
            BLp = blocks / n,
            BAX = blocks - xBlocks) %>%
  arrange(desc(BAX))

playersDat$stand = ifelse(grepl("Lh", playersDat$platoon), "L", "R")


#### Player Development ####


filter(playersDat, name == "Freddy Fermin") %>%
  ggplot(aes(plate_x, plate_z, z = BAX)) +
  stat_summary_2d(bins = 5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  annotate("rect", xmin = -.7, xmax = .7, ymin = 1.6, ymax = 3.5, alpha = 0.2, color = "black") +
  annotate("rect", xmin = -.7, xmax = .7, ymin = -.1, ymax = 0, alpha = 1, color = "black", fill = "white") +
  coord_fixed() +
  labs(x = "Horizontal Location", y = "Vertical Location", fill = "BAX",
       title = "Freddy Fermin Blocks Above Expected by Location",
       subtitle = "Catcher's Point of View",
       caption = "Jack Rogers and Kai Franke, Saberseminar 2023") +
  theme_bw() +
  theme(plot.subtitle = element_text(family = "serif", face = "bold", hjust = 0.5),
        plot.caption = element_text(family = "serif", hjust = 0.5),
        plot.title = element_text(family = "serif", face = "bold", hjust = 0.5))


filter(playersDat, name == "Freddy Fermin") %>%
  ggplot(aes(plate_x, plate_z, z = pred2023)) +
  stat_summary_2d(bins = 5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = .97) +
  annotate("rect", xmin = -.7, xmax = .7, ymin = 1.6, ymax = 3.5, alpha = 0.2, color = "black") +
  annotate("rect", xmin = -.7, xmax = .7, ymin = -.1, ymax = 0, alpha = 1, color = "black", fill = "white") +
  coord_fixed() +
  labs(x = "Horizontal Location", y = "Vertical Location", fill = "xBlock%",
       title = "Freddy Fermin xBlock% by Location",
       subtitle = "Catcher's Point of View",
       caption = "Jack Rogers and Kai Franke, Saberseminar 2023") +
  theme_bw() +
  theme(plot.subtitle = element_text(family = "serif", face = "bold", hjust = 0.5),
        plot.caption = element_text(family = "serif", hjust = 0.5),
        plot.title = element_text(family = "serif", face = "bold", hjust = 0.5))

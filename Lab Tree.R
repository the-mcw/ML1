library(tidyverse)
library(arules)
library(arulesViz)

drivers = read_csv("https://raw.githubusercontent.com/the-mcw/ML1/main/data/drivers_6.csv")

rm_drivers = drivers %>%
  select(c(1,4,5,12,18,19,24,30,9)) %>%
  arrange(raceId) %>%
  group_by(driverId, year) %>%
  mutate(nextFinish = lead(positionOrder)) %>%
  ungroup() %>%
  select(-c(positionOrder,year))

rm_drivers = rm_drivers %>%
  rename(nextQualify = qualifyPosition) %>%
  mutate(points_Race = pointsSeason/round,
         conPoints_Race = ConstructorSeasonPoints/round) %>%
  select(-c(pointsSeason,ConstructorSeasonPoints)) %>%
  relocate(starts_with("next"), .after = last_col())

rm_drivers = rm_drivers %>%
  mutate(pointsBin = cut(points_Race, c(0,1,15,26),
                       labels = c("no points", "midfield", "podium"),
                       include.lowest = T, right = F),
         conBin = cut(conPoints_Race, c(0,1,15,44),
                      labels = c("no points", "midfield", "podium"),
                      include.lowest = T, right = F),
         rankBin = if_else(rank==0,"no points",
                           cut(rank, c(1,3,10,24), labels = c("podium", "midfield", "no points"),
                               include.lowest = T)),
         nextQualBin = cut(nextQualify,c(1,4,8,14,24), labels = c("1-4", "5-8", "9-14","back"),
                        include.lowest = T),
         nextFinBin = cut(nextFinish, c(1,3,10,24), labels = c("podium", "midfield", "no points"),
                          include.lowest = T))

rm_model_drivers = rm_drivers %>%
  select(ends_with("Bin")) %>%
  mutate(rankBin = factor(rankBin))


rm_model = apriori(rm_model_drivers,
                   parameter = list(support = 0.05, confidence = 0.2, minlen = 2),
                   appearance = list(rhs = c("nextFinBin=podium")))
inspect (rm_model)







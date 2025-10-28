#data qaqc

library(tidyverse)

cont = read_csv('data/nutrient_data_continuous.csv')
glimpse(cont)

disc = read_csv("data/nutrient_data_discrete.csv")
unique(disc$Units)
sort(unique(disc$Project))
sort(unique(disc$Station_Name))

stations = read_csv("data/station_metadata.csv")

unique(disc$Station_Name) %in% unique(stations$Station_Name)
unique(cont$Station_Name) %in% unique(stations$Station_Name)

ggplot(cont, aes(x = Date_Time, y = Result)) + geom_line()+
  facet_wrap(~Station_Name)

#are the times and scales right?
ggplot(disc, aes(x = Date_Time, y = Result)) + geom_point(aes(color = Project))+
  scale_color_brewer(palette = "Set3")+ theme_bw()

#are all the non-detects noted correctly?
test = filter(disc, is.na(Result))
test2 = filter(disc, !is.na(Result))
unique(test$Detection_Condition)
unique(test2$Detection_Condition)

#all the analytes there? NO extras?
unique(disc$Analyte)

#do all the projects have the analytes we think they do?
ggplot(disc, aes(x = Date_Time, y = Result)) + geom_point(aes(color = Project))+
  scale_color_brewer(palette = "Set3")+ theme_bw()+
  facet_grid(Analyte~Project, scales = "free_y")

#oh, wait, nothing is coming up for NCRO NDFS

ncro = filter(disc, Project == "NCRO NDFS Study")
ggplot(tparr, aes(x = Date_Time, y = Result)) + geom_point(aes(color = Project))+
  scale_color_brewer(palette = "Set3")+ theme_bw()+
  facet_wrap(~Analyte, scales = "free_y")

tparr = filter(disc, Project == "Tidal Parr Study")
ggplot(tparr, aes(x = Date_Time, y = Result)) + geom_point(aes(color = Project))+
  scale_color_brewer(palette = "Set3")+ theme_bw()+
  facet_wrap(~Analyte, scales = "free_y")

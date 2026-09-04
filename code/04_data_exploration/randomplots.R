#random exploratory plots

library(tidyverse)


#load the integrated dataset
nuts = read_rds("data/processed/monthly_values.rds")

names(nuts)

nuts = mutate(nuts, Yearmonth = Year + (1-Month/12))

#biovolume of each taxa group over time
ggplot(nuts, aes(x = Yearmonth, y = log(`bv_Green Algae`))) + geom_point()+ geom_line()+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/GrennAlgae_time.png", width =8, height =6)

ggplot(nuts, aes(x = Yearmonth, y = log(`bv_Pennate Diatoms`))) + geom_point()+ geom_line()+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/Pennnate_time.png", width =8, height =6)


ggplot(nuts, aes(x = Yearmonth, y = log(`bv_Cyanobacteria`))) + geom_point()+ geom_line()+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/cyanos_time.png", width =8, height =6)

ggplot(nuts, aes(x = Yearmonth, y = log(`bv_Centric Diatoms`))) + geom_point()+ geom_line()+
  facet_wrap(~Region)

ggplot(nuts, aes(x = Yearmonth, y = log(`bv_Cryptophytes`))) + geom_point()+ geom_line()+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/cryptos_time.png", width =8, height =6)

#plot of nutrients over time
ggplot(nuts, aes(x = Yearmonth, y = DissNitrateNitrite)) + geom_point()+ geom_line()+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/NN_time.png", width =8, height =6)

ggplot(nuts, aes(x = Yearmonth, y = DissAmmonia)) + geom_point()+ geom_line()+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/Ammonia_time.png", width =8, height =6)

ggplot(nuts, aes(x = Yearmonth, y = DissOrthophos)) + geom_point()+ geom_line()+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/orthophos_time.png", width =8, height =6)

ggplot(nuts, aes(x = Yearmonth, y = DON)) + geom_point()+ geom_line()+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/DON_time.png", width =8, height =6)

ggplot(nuts, aes(x = Yearmonth, y = TKN)) + geom_point()+ geom_line()+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/TKN_time.png", width =8, height =6)

#now lots of random correlations!

ggplot(nuts, aes(y = log(`bv_Green Algae`), x = DissNitrateNitrite)) + 
  geom_point()+ geom_smooth(method = "lm")+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/GreenAlgae_NN.png", width =8, height =6)

ggplot(nuts, aes(y = log(`bv_Green Algae`), x = DissAmmonia)) + 
  geom_point()+ geom_smooth(method = "lm")+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/GreenAlgae_Ammonia.png", width =8, height =6)


ggplot(nuts, aes(y = log(bv_Cyanobacteria), x = DissAmmonia)) + 
  geom_point()+ geom_smooth(method = "lm")+
  facet_wrap(~Region, scales = "free")
ggsave("plots/exploratoryplots/Cyano_ammonia.png", width =8, height =6)


ggplot(nuts, aes(y = log(bv_Cyanobacteria), x = DissNitrateNitrite)) + 
  geom_point()+ geom_smooth(method = "lm")+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/Cyano_NN.png", width =8, height =6)

gplot(nuts, aes(y = log(`bv_Centric Diatoms`), x = DissAmmonia)) + 
  geom_point()+ geom_smooth(method = "lm")+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/Centric_Ammonia.png", width =8, height =6)


ggplot(nuts, aes(y = log(`bv_Centric Diatoms`), x = DissNitrateNitrite)) + 
  geom_point()+ geom_smooth(method = "lm")+
  facet_wrap(~Region)
ggsave("plots/exploratoryplots/Centric_NN.png", width =8, height =6)

#exploring zooplankton data to see which taxa might be most important

library(tidyverse)
library(zooper)

zoops = Zoopsynther(Data_type = "Community", Sources = c("EMP", "FMWT", "STN", "20mm", "DOP"),
                    Years = c(2010:2024)) 

zoops = mutate(zoops, Month = month(Date))

#what are the most commont taxa?

zoopCommon = filter(zoops, !Undersampled) %>%
  group_by(Taxname, Lifestage, Taxlifestage, Month, SizeClass) %>%
  summarize(CPUE = sum(CPUE))

ggplot(zoopCommon, aes(x = Month, y = CPUE, fill = Taxlifestage)) + geom_area(position = "fill") +
  facet_wrap(~SizeClass)

#now get rid of the rarer stuff

zoopstot = group_by(zoopCommon, SizeClass, Month) %>%
  mutate(tot = sum(CPUE)) %>%
  ungroup() %>%
  mutate(percent = CPUE/tot,
         Taxon = case_when(percent<0.05 ~ "Other",
                           TRUE ~ Taxlifestage)) %>%
  group_by(SizeClass, Month, Taxon) %>%
  summarize(CPUE = sum(CPUE))

ggplot(zoopstot, aes(x = Month, y = CPUE, fill = Taxon)) + geom_area(position = "fill") +
  facet_wrap(~SizeClass) + scale_fill_manual(values = c("orange", "skyblue", "cyan", "darkgreen",
                                                        "purple", "pink", "black", "yellow", "red3",
                                                        "peru", "yellowgreen", "goldenrod",
                                                        "white", "grey", "deeppink", 'seagreen', "gold",
                                                      "blue", "salmon", "darkblue", "peachpuff", "slategray",
                                                      "sienna3", "cyan3", "lightgreen", "maroon"))

#OK! That's by month. 

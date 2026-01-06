# based on PESP recs

library(EDIutils)
library(glue)
library(here)
source('code/functions/ns_funcs.R')

df_dat <- get_edi_file(2209,'PESP_enumeration.csv')

# EMP
df_emp <- df_dat %>%
  filter(Survey == 'DWR-EMP',
         Lab == 'BSA',
         SampleMethod == 'centrifugal pump',
         Magnification == '800x',
         CountMethodTaxa == 'field')

# FRP
df_frp <- df_dat %>%
  filter(Survey == 'CDFW-FRP',
         Lab == 'BSA',
         SampleMethod == 'unspecified surface grab',
         Magnification == '800x',
         CountMethodTaxa == 'field')

# AWCA
df_awca <- df_dat %>%
  filter(Survey == 'DWR-AWCA',
         Lab == 'BSA',
         SampleMethod == 'unspecified surface grab',
         Magnification == '800x',
         CountMethodTaxa == 'field')

# FMWT
df_fmwt <- df_dat %>%
  filter(Survey == 'CDFW-FMWT',
         Lab == 'BSA',
         SampleMethod == 'unspecified surface grab',
         Magnification == '800x',
         CountMethodTaxa == 'field')

#YBFMP
df_ybfmp <- df_dat %>%
  filter(Survey == 'DWR-YBFMP',
         Lab == 'BSA',
         SampleMethod == 'water sampler dipper',
         Magnification == '630x',
         CountMethodTaxa == 'field')

#NDFS
df_ndfs <- df_dat %>%
  filter(Survey == 'DWR-NDFS',
         Lab == 'BSA',
         SampleMethod == 'water sampler dipper',
         Magnification == '630x',
         CountMethodTaxa == 'field')

df_all <- rbind(df_emp, df_frp, df_awca, df_fmwt, df_ybfmp, df_ndfs)

df_all %>% saveRDS(here("data/external/pesp_2013_2024.rds"))

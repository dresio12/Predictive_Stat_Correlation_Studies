library(baseballr)
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(tidyr)

pitcherstatsmain <- read.csv("https://raw.githubusercontent.com/dresio12/Predictive_Stat_Correlation_Studies/main/pitcherstats.csv", stringsAsFactors = FALSE)

batterstatsmain <- read.csv("https://raw.githubusercontent.com/dresio12/Predictive_Stat_Correlation_Studies/main/batterstats.csv", stringsAsFactors = FALSE)

batterstats <- batterstatsmain |>
  select(1, 5, 2:3, 8, 12:61, 70, 73:77, 85, 111:121, 138:147, 154:157, 281:287,
         289:291)

batterstats <- batterstats |>
  rename('name' = 'PlayerNameRoute.x',
         'BB_pct_plus' = 'BB_pct.',
         'K_pct_plus' = 'K_pct.',
         'OBP_plus' = 'OBP.',
         'SLG_plus' = 'SLG.',
         'ISO_plus' = 'ISO.',
         'BABIP_plus' = 'BABIP.',
         'LD_pct_plus' = 'LD_pct.',
         'FB_pct_plus' = 'FB_pct.',
         'GB_pct_plus' = 'GB_pct.',
         'HRFB_pct_plus' = 'HRFB_pct.'
         )

batterstats$name[c(3038)] <- "Carlos PerezOAK"


#
#

#create rowbind dfs for the adjacent seasons
#selects necessary columns
#widens df to assign season to each variable of interest
#


#2019-20

b1920 <- batterstats |>
  filter(Season == '2019' | Season == "2020") |>
  select(name, Season, 3:94, 97) |>
  pivot_wider(names_from = Season, values_from = 3:95)

#
#

#2020-21 

b2021 <- batterstats |>
  filter(Season == '2020' | Season == "2021") |>
  select(name, Season, 3:94, 97) |>
  pivot_wider(names_from = Season, values_from = 3:95)

#
#
#2021-22

b2122 <- batterstats |>
  filter(Season == '2021' | Season == "2022") |>
  select(name, Season, 3:94, 97) |> 
  pivot_wider(names_from = Season, values_from = 3:95)


#
#
#2022-23

b2223 <- batterstats |>
  filter(Season == '2022' | Season == "2023") |>
  select(name, Season, 3:94, 97) |> 
  pivot_wider(names_from = Season, values_from = 3:95)


#2023-24

b2324 <- batterstats |>
  filter(Season == '2023' | Season == "2024") |>
  select(name, Season, 3:94, 97) |> 
  pivot_wider(names_from = Season, values_from = 3:95)


# Subset data to remove non-numeric columns
#named ds for data subset, followed by corresponding tag

#2019-20
ds1920 <- b1920 %>%
  select(6:185) %>%
  na.omit()

#

#2020-21 
ds2021 <- b2021 %>%
  select(6:185) %>%
  na.omit()

#

#2021-22
ds2122 <- b2122 %>%
  select(6:185) %>%
  na.omit()

#

#2022-23
ds2223 <- b2223 %>%
  select(6:185) %>%
  na.omit()

#

#2023-24 dfs
ds2324 <- b2324 %>%
  select(6:185) %>%
  na.omit()

#

# Identify yearly columns separately
cols_2019 <- grep("_2019$", names(ds1920), value = TRUE)
cols_2020 <- grep("_2020$", names(ds1920), value = TRUE)
cols_2021 <- grep("_2021$", names(ds2122), value = TRUE)
cols_2022 <- grep("_2022$", names(ds2122), value = TRUE)
cols_2023 <- grep("_2023$", names(ds2324), value = TRUE)
cols_2024 <- grep("_2024$", names(ds2324), value = TRUE)

# Compute correlation matrix (2019 stats vs. 2020 stats)
cm1920 <- cor(ds1920[, cols_2019], 
              ds1920[, cols_2020], 
              use = "complete.obs")

#
#

# Compute correlation matrix (2020 stats vs. 2021 stats)
cm2021 <- cor(ds2021[, cols_2020], 
              ds2021[, cols_2021], 
              use = "complete.obs")

#
# Compute correlation matrix (2021 stats vs. 2022 stats)
cm2122 <- cor(ds2122[, cols_2021], 
              ds2122[, cols_2022], 
              use = "complete.obs")

#
#
# Compute correlation matrix (2022 stats vs. 2023 stats)
cm2223 <- cor(ds2223[, cols_2022], 
              ds2223[, cols_2023], 
              use = "complete.obs")

#
# Compute correlation matrix (2023 stats vs. 2024 stats)
cm2324 <- cor(ds2324[, cols_2023], 
              ds2324[, cols_2024], 
              use = "complete.obs")

#
#
#

#Pitcher Stats
pitcherstats <- pitcherstatsmain |>
  select(3, 1, 70, 4:68, 71:76) |>
  rename(Season = year) |>
  unique()



#2019-20

p1920 <- pitcherstats |>
  filter(Season == '2019' | Season == "2020") |>
  select(name, Season, 3:74) |>
  pivot_wider(names_from = Season, values_from = 3:74)

#
#

#2020-21 

p2021 <- pitcherstats |>
  filter(Season == '2020' | Season == "2021") |>
  select(name, Season, 3:74) |>
  pivot_wider(names_from = Season, values_from = 3:74)

#
#
#2021-22

p2122 <- pitcherstats |>
  filter(Season == '2021' | Season == "2022") |>
  select(name, Season, 3:74) |> 
  pivot_wider(names_from = Season, values_from = 3:74)


#
#
#2022-23

p2223 <- pitcherstats |>
  filter(Season == '2022' | Season == "2023") |>
  select(name, Season, 3:74) |> 
  pivot_wider(names_from = Season, values_from = 3:74)


#2023-24

p2324 <- pitcherstats |>
  filter(Season == '2023' | Season == "2024") |>
  select(name, Season, 3:74) |> 
  pivot_wider(names_from = Season, values_from = 3:74)


# Subset data to remove non-numeric columns
#named ds for data subset, followed by corresponding tag

#2019-20
pds1920 <- p1920 %>%
  select(4:49, 54:57, 60:71, 88:145) %>%
  na.omit()

#

#2020-21 
pds2021 <- p2021 %>%
  select(4:49, 54:57, 60:71, 88:145) %>%
  na.omit()

#

#2021-22
pds2122 <- p2122 %>%
  select(4:49, 54:57, 60:71, 88:145) %>%
  na.omit()

#

#2022-23
pds2223 <- p2223 %>%
  select(4:49, 54:57, 60:71, 88:145) %>%
  na.omit()

#

#2023-24 dfs
pds2324 <- p2324 %>%
  select(4:49, 54:57, 60:71, 88:145) %>%
  na.omit()

# Identify yearly columns separately
pcols_2019 <- grep("_2019$", names(pds1920), value = TRUE)
pcols_2020 <- grep("_2020$", names(pds1920), value = TRUE)
pcols_2021 <- grep("_2021$", names(pds2122), value = TRUE)
pcols_2022 <- grep("_2022$", names(pds2122), value = TRUE)
pcols_2023 <- grep("_2023$", names(pds2324), value = TRUE)
pcols_2024 <- grep("_2024$", names(pds2324), value = TRUE)

# Compute correlation matrix (2019 stats vs. 2020 stats)
pcm1920 <- cor(pds1920[, pcols_2019], 
               pds1920[, pcols_2020], 
               use = "complete.obs")

#
#

# Compute correlation matrix (2020 stats vs. 2021 stats)
pcm2021 <- cor(pds2021[, pcols_2020], 
               pds2021[, pcols_2021], 
               use = "complete.obs")

#
# Compute correlation matrix (2021 stats vs. 2022 stats)
pcm2122 <- cor(pds2122[, pcols_2021], 
               pds2122[, pcols_2022], 
               use = "complete.obs")

#
#
# Compute correlation matrix (2022 stats vs. 2023 stats)
pcm2223 <- cor(pds2223[, pcols_2022], 
               pds2223[, pcols_2023], 
               use = "complete.obs")

#
# Compute correlation matrix (2023 stats vs. 2024 stats)
pcm2324 <- cor(pds2324[, pcols_2023], 
               pds2324[, pcols_2024], 
               use = "complete.obs")



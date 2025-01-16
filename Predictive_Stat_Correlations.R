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

pitcherstats <- pitcherstatsmain |>
  select(1, 70, 3, 4:68, 71:76)

batterstats <- batterstats |>
  rename('name' = 'PlayerNameRoute.x')

batterstats$name[c(3038)] <- "Carlos PerezOAK"
#
#

#create rowbind dfs for the adjacent seasons
#selects necessary columns
#filters by plate appearance min
#widens df to assign season to each variable of interest
#
#2019-20: 25, 50, 75, 100, 150, 200 (naming = year combo, PA min)

b192025 <- batterstats |>
  filter(Season == '2019' | Season == "2020") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 25) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b192050 <- batterstats |>
  filter(Season == '2019' | Season == "2020") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 50) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b192075 <- batterstats |>
  filter(Season == '2019' | Season == "2020") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 75) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b1920100 <- batterstats |>
  filter(Season == '2019' | Season == "2020") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 100) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b1920150 <- batterstats |>
  filter(Season == '2019' | Season == "2020") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 150) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b1920200 <- batterstats |>
  filter(Season == '2019' | Season == "2020") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 200) |>
  pivot_wider(names_from = Season, values_from = 3:95)
#
#

#2020-21: 25, 50, 75, 100, 150, 200 

b202125 <- batterstats |>
  filter(Season == '2020' | Season == "2021") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 25) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b202150 <- batterstats |>
  filter(Season == '2020' | Season == "2021") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 50) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b202175 <- batterstats |>
  filter(Season == '2020' | Season == "2021") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 75) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2021100 <- batterstats |>
  filter(Season == '2020' | Season == "2021") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 100) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2021150 <- batterstats |>
  filter(Season == '2020' | Season == "2021") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 150) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2021200 <- batterstats |>
  filter(Season == '2020' | Season == "2021") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 200) |>
  pivot_wider(names_from = Season, values_from = 3:95)


#
#
#2021-22: 25, 50, 100, 200, 300, 400

b212225 <- batterstats |>
  filter(Season == '2021' | Season == "2022") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 25) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b212250 <- batterstats |>
  filter(Season == '2021' | Season == "2022") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 50) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2122100 <- batterstats |>
  filter(Season == '2021' | Season == "2022") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 100) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2122200 <- batterstats |>
  filter(Season == '2021' | Season == "2022") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 200) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2122300 <- batterstats |>
  filter(Season == '2021' | Season == "2022") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 300) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2122400 <- batterstats |>
  filter(Season == '2021' | Season == "2022") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 400) |>
  pivot_wider(names_from = Season, values_from = 3:95)
#
#
#2022-23: 25, 50, 100, 200, 300, 400

b222325 <- batterstats |>
  filter(Season == '2022' | Season == "2023") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 25) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b222350 <- batterstats |>
  filter(Season == '2022' | Season == "2023") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 50) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2223100 <- batterstats |>
  filter(Season == '2022' | Season == "2023") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 100) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2223200 <- batterstats |>
  filter(Season == '2022' | Season == "2023") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 200) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2223300 <- batterstats |>
  filter(Season == '2022' | Season == "2023") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 300) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2223400 <- batterstats |>
  filter(Season == '2022' | Season == "2023") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 400) |>
  pivot_wider(names_from = Season, values_from = 3:95)

#2023-24: 25, 50, 100, 200, 300, 400
b232425 <- batterstats |>
  filter(Season == '2023' | Season == "2024") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 25) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b232450 <- batterstats |>
  filter(Season == '2023' | Season == "2024") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 50) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2324100 <- batterstats |>
  filter(Season == '2023' | Season == "2024") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 100) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2324200 <- batterstats |>
  filter(Season == '2023' | Season == "2024") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 200) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2324300 <- batterstats |>
  filter(Season == '2023' | Season == "2024") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 300) |>
  pivot_wider(names_from = Season, values_from = 3:95)

b2324400 <- batterstats |>
  filter(Season == '2023' | Season == "2024") |>
  select(name, Season, 3:94, 97) |> 
  filter(PA >= 400) |>
  pivot_wider(names_from = Season, values_from = 3:95)


# Subset data to remove non-numeric columns
#named ds for data subset, followed by corresponding tag

#2019-20 dfs
ds192025 <- b192025 %>%
  select(6:185) %>%
  na.omit()

ds192050 <- b192050 %>%
  select(6:185) %>%
  na.omit()

ds192075 <- b192075 %>%
  select(6:185) %>%
  na.omit()

ds1920100 <- b1920100 %>%
  select(6:185) %>%
  na.omit()

ds1920150 <- b1920150 %>%
  select(6:185) %>%
  na.omit()

ds1920200 <- b1920200 %>%
  select(6:185) %>%
  na.omit()
#
#

#2020-21 dfs
ds202125 <- b202125 %>%
  select(6:185) %>%
  na.omit()

ds202150 <- b202150 %>%
  select(6:185) %>%
  na.omit()

ds202175 <- b202175 %>%
  select(6:185) %>%
  na.omit()

ds2021100 <- b2021100 %>%
  select(6:185) %>%
  na.omit()

ds2021150 <- b2021150 %>%
  select(6:185) %>%
  na.omit()

ds2021200 <- b2021200 %>%
  select(6:185) %>%
  na.omit()


#
#

#2021-22 dfs
ds212225 <- b212225 %>%
  select(6:185) %>%
  na.omit()

ds212250 <- b212250 %>%
  select(6:185) %>%
  na.omit()

ds2122100 <- b2122100 %>%
  select(6:185) %>%
  na.omit()

ds2122200 <- b2122200 %>%
  select(6:185) %>%
  na.omit()

ds2122300 <- b2122300 %>%
  select(6:185) %>%
  na.omit()

ds2122400 <- b2122400 %>%
  select(6:185) %>%
  na.omit()
#
#

#2022-23 dfs
ds222325 <- b222325 %>%
  select(6:185) %>%
  na.omit()

ds222350 <- b222350 %>%
  select(6:185) %>%
  na.omit()

ds2223100 <- b2223100 %>%
  select(6:185) %>%
  na.omit()

ds2223200 <- b2223200 %>%
  select(6:185) %>%
  na.omit()

ds2223300 <- b2223300 %>%
  select(6:185) %>%
  na.omit()

ds2223400 <- b2223400 %>%
  select(6:185) %>%
  na.omit()
#
#

#2023-24 dfs
ds232425 <- b232425 %>%
  select(6:185) %>%
  na.omit()

ds232450 <- b232450 %>%
  select(6:185) %>%
  na.omit()

ds2324100 <- b2324100 %>%
  select(6:185) %>%
  na.omit()

ds2324200 <- b2324200 %>%
  select(6:185) %>%
  na.omit()

ds2324300 <- b2324300 %>%
  select(6:185) %>%
  na.omit()

ds2324400 <- b2324400 %>%
  select(6:185) %>%
  na.omit()
#
#

# Identify yearly columns separately
cols_2019 <- grep("_2019$", names(ds192025), value = TRUE)
cols_2020 <- grep("_2020$", names(ds192025), value = TRUE)
cols_2021 <- grep("_2021$", names(ds212225), value = TRUE)
cols_2022 <- grep("_2022$", names(ds212225), value = TRUE)
cols_2023 <- grep("_2023$", names(ds232425), value = TRUE)
cols_2024 <- grep("_2024$", names(ds232425), value = TRUE)

# Compute correlation matrix (2019 stats vs. 2020 stats)
cm192025 <- cor(ds192025[, cols_2019], 
                ds192025[, cols_2020], 
                use = "complete.obs")

cm192050 <- cor(ds192050[, cols_2019], 
                ds192050[, cols_2020], 
                use = "complete.obs")

cm192075 <- cor(ds192075[, cols_2019], 
                ds192075[, cols_2020], 
                use = "complete.obs")

cm1920100 <- cor(ds1920100[, cols_2019], 
                ds1920100[, cols_2020], 
                use = "complete.obs")

cm1920150 <- cor(ds1920150[, cols_2019], 
                ds1920150[, cols_2020], 
                use = "complete.obs")

cm1920200 <- cor(ds1920200[, cols_2019], 
                ds1920200[, cols_2020], 
                use = "complete.obs")

#
#

# Compute correlation matrix (2020 stats vs. 2021 stats)
cm202125 <- cor(ds202125[, cols_2020], 
                ds202125[, cols_2021], 
                use = "complete.obs")

cm202150 <- cor(ds202150[, cols_2020], 
                ds202150[, cols_2021], 
                use = "complete.obs")

cm202175 <- cor(ds202175[, cols_2020], 
                ds202175[, cols_2021], 
                use = "complete.obs")

cm2021100 <- cor(ds2021100[, cols_2020], 
                 ds2021100[, cols_2021], 
                 use = "complete.obs")

cm2021150 <- cor(ds2021150[, cols_2020], 
                 ds2021150[, cols_2021], 
                 use = "complete.obs")

cm2021200 <- cor(ds2021200[, cols_2020], 
                 ds2021200[, cols_2021], 
                 use = "complete.obs")
#
#
# Compute correlation matrix (2021 stats vs. 2022 stats)
cm212225 <- cor(ds212225[, cols_2021], 
                ds212225[, cols_2022], 
                use = "complete.obs")

cm212250 <- cor(ds212250[, cols_2021], 
                ds212250[, cols_2022], 
                use = "complete.obs")

cm2122100 <- cor(ds2122100[, cols_2021], 
                ds2122100[, cols_2022], 
                use = "complete.obs")

cm2122200 <- cor(ds2122200[, cols_2021], 
                 ds2122200[, cols_2022], 
                 use = "complete.obs")

cm2122300 <- cor(ds2122300[, cols_2021], 
                 ds2122300[, cols_2022], 
                 use = "complete.obs")

cm2122400 <- cor(ds2122400[, cols_2021], 
                 ds2122400[, cols_2022], 
                 use = "complete.obs")

#
#
# Compute correlation matrix (2022 stats vs. 2023 stats)
cm222325 <- cor(ds222325[, cols_2022], 
                ds222325[, cols_2023], 
                use = "complete.obs")

cm222350 <- cor(ds222350[, cols_2022], 
                ds222350[, cols_2023], 
                use = "complete.obs")

cm2223100 <- cor(ds2223100[, cols_2022], 
                 ds2223100[, cols_2023], 
                 use = "complete.obs")

cm2223200 <- cor(ds2223200[, cols_2022], 
                 ds2223200[, cols_2023], 
                 use = "complete.obs")

cm2223300 <- cor(ds2223300[, cols_2022], 
                 ds2223300[, cols_2023], 
                 use = "complete.obs")

cm2223400 <- cor(ds2223400[, cols_2022], 
                 ds2223400[, cols_2023], 
                 use = "complete.obs")
#
#
# Compute correlation matrix (2023 stats vs. 2024 stats)
cm232425 <- cor(ds232425[, cols_2023], 
                ds232425[, cols_2024], 
                use = "complete.obs")

cm232450 <- cor(ds232450[, cols_2023], 
                ds232450[, cols_2024], 
                use = "complete.obs")

cm2324100 <- cor(ds2324100[, cols_2023], 
                 ds2324100[, cols_2024], 
                 use = "complete.obs")

cm2324200 <- cor(ds2324200[, cols_2023], 
                 ds2324200[, cols_2024], 
                 use = "complete.obs")

cm2324300 <- cor(ds2324300[, cols_2023], 
                 ds2324300[, cols_2024], 
                 use = "complete.obs")

cm2324400 <- cor(ds2324400[, cols_2023], 
                 ds2324400[, cols_2024], 
                 use = "complete.obs")


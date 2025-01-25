library(baseballr)
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(tidyr)
library(glmnet)

# Identifying Top 5 Predictors for Each Stat in Seasons 2021-2023
cor_matrices <- list(
  "2021" = cm2021,  
  "2022" = cm2122,  
  "2023" = cm2223
)

# Manually define the base stats you want to analyze
base_stat_list <- c("Age", "G", "AB", "PA", "H", "X1B", "X2B", "X3B", "HR", "R", 
                    "RBI", "BB", "IBB", "SO", "HBP", "SF", "SH", "GDP", "SB", 
                    "CS", "AVG", "GB", "FB", "LD", "IFFB", "Pitches", "Strikes", 
                    "Balls", "IFH", "BU", "BUH", "BB_pct", "K_pct", "BB_K", 
                    "OBP", "SLG", "OPS", "ISO", "BABIP", "GB_FB", "LD_pct", 
                    "GB_pct", "FB_pct", "IFFB_pct", "HR_FB", "IFH_pct", "BUH_pct",
                    "TTO_pct", "wOBA", "wRAA", "wRC", "WAR", "BaseRunning",
                    "Spd", "wRC_plus", "wBsR", "WPA", "Clutch", "O.Swing_pct",
                    "Z.Swing_pct", "Swing_pct", "O.Contact_pct", "Z.Contact_pct",
                    "Contact_pct", "Zone_pct", "F.Strike_pct", "SwStr_pct",
                    "CStr_pct", "C.SwStr_pct", "BB_pct_plus", "K_pct_plus", 
                    "OBP_plus", "SLG_plus", "ISO_plus", "BABIP_plus", "LD_pct_plus",
                    "GB_pct_plus", "FB_pct_plus", "HRFB_pct_plus", "xwOBA", 
                    "xAVG", "xSLG", "XBR", "EV", "LA", "Barrels", "Barrel_pct",
                    "maxEV", "HardHit", "HardHit_pct") 

## Function to clean column names by removing year suffix (handles multiple underscores)
clean_column_name <- function(col_name) {
  # Remove the last part if it matches a year suffix (_####)
  sub("_(\\d{4})$", "", col_name)
}

# Function to extract top predictors and their correlation values for each base stat in a given season
get_top_predictors_with_values <- function(cor_matrix, base_stat, top_n = 5) {
  predictors_with_values <- list()
  
  # Clean column names to remove year suffix
  clean_colnames <- sapply(colnames(cor_matrix), clean_column_name)
  
  for (col in colnames(cor_matrix)) {
    # Exclude the column itself to avoid self-correlation
    correlations <- cor_matrix[, col]
    correlations <- correlations[!names(correlations) %in% col]
    
    # Match the cleaned column name with the base stat
    if (clean_column_name(col) == base_stat) {
      # Sort the correlations for this predictor in descending order
      sorted_correlations <- sort(abs(correlations), decreasing = TRUE)[1:top_n]
      
      # Modify predictor names to clean them
      predictors <- sapply(names(sorted_correlations), clean_column_name)
      
      # Store the top N predictors and their corresponding correlation values
      predictors_with_values[[col]] <- data.frame(
        Predictor = predictors,
        Correlation = sorted_correlations
      )
    }
  }
  
  return(predictors_with_values)
}

# Main loop for extracting top predictors
all_predictors_values <- list()

for (base_stat in base_stat_list) {
  # Validate if the base stat exists in any cleaned column names
  clean_colnames <- sapply(colnames(cor_matrices$`2021`), clean_column_name)  # Assumes consistent column names
  if (base_stat %in% clean_colnames) {
    all_predictors_values[[base_stat]] <- list()  # Initialize for each base stat
    
    # Process each season
    for (season in names(cor_matrices)) {
      predictors_values <- get_top_predictors_with_values(cor_matrices[[season]], base_stat)
      
      # Add a Year column to each tibble for ordering
      if (length(predictors_values) > 0) {
        predictors_values <- lapply(predictors_values, function(df) {
          df$Year <- season
          return(df)
        })
        
        all_predictors_values[[base_stat]] <- c(all_predictors_values[[base_stat]], predictors_values)
      }
    }
    
    # Combine and filter results
    combined_data <- do.call(rbind, all_predictors_values[[base_stat]])
    
    # Ensure ordering by Year
    combined_data <- combined_data %>%
      group_by(Predictor, Year) %>%
      arrange(Year, desc(Correlation)) %>%
      slice_head(n = 5) %>%
      ungroup()
    
    # Save the filtered results
    all_predictors_values[[base_stat]] <- combined_data
  }
}


#Manual validation indicates the top 5 predictors in each year are accurate

#
#

#Extract Top 5 overall predictors

# Combine all predictors across base stats into a single data frame
all_predictors_combined <- bind_rows(all_predictors_values, .id = "Base_Stat")

# Create a function to calculate top 5 predictors for each stat
get_top_5_per_stat <- function(data) {
  data %>%
    group_by(Predictor) %>%                      # Group by each predictor
    summarize(
      Frequency = n(),                           # Count how many times the predictor appears
      Avg_Correlation = mean(Correlation)        # Calculate the average correlation value
    ) %>%
    arrange(desc(Frequency), desc(Avg_Correlation)) %>%  # Rank by frequency, then avg correlation
    slice_head(n = 5)                            # Take the top 5 predictors
}

# Apply the function to each stat
top_predictors_by_stat <- all_predictors_combined %>%
  group_by(Base_Stat) %>%                        # Group by each stat
  group_modify(~ get_top_5_per_stat(.x)) %>%     # Apply the function to each group
  ungroup()

# View the result
print(top_predictors_by_stat)

#
#
#

#Repeating with Standardized Data


# Identifying Top 10 Predictors for Each Stat in Seasons 2021-2023
s_cor_matrices <- list(
  "2021" = scm2021,  
  "2022" = scm2122,  
  "2023" = scm2223,
  "2024" = scm2324 
)

# Manually define the base stats you want to analyze
base_stat_list <- c("Age", "G", "AB", "PA", "H", "X1B", "X2B", "X3B", "HR", "R", 
                    "RBI", "BB", "IBB", "SO", "HBP", "SF", "SH", "GDP", "SB", 
                    "CS", "AVG", "GB", "FB", "LD", "IFFB", "Pitches", "Strikes", 
                    "Balls", "IFH", "BU", "BUH", "BB_pct", "K_pct", "BB_K", 
                    "OBP", "SLG", "OPS", "ISO", "BABIP", "GB_FB", "LD_pct", 
                    "GB_pct", "FB_pct", "IFFB_pct", "HR_FB", "IFH_pct", "BUH_pct",
                    "TTO_pct", "wOBA", "wRAA", "wRC", "WAR", "BaseRunning",
                    "Spd", "wRC_plus", "wBsR", "WPA", "Clutch", "O.Swing_pct",
                    "Z.Swing_pct", "Swing_pct", "O.Contact_pct", "Z.Contact_pct",
                    "Contact_pct", "Zone_pct", "F.Strike_pct", "SwStr_pct",
                    "CStr_pct", "C.SwStr_pct", "BB_pct_plus", "K_pct_plus", 
                    "OBP_plus", "SLG_plus", "ISO_plus", "BABIP_plus", "LD_pct_plus",
                    "GB_pct_plus", "FB_pct_plus", "HRFB_pct_plus", "xwOBA", 
                    "xAVG", "xSLG", "XBR", "EV", "LA", "Barrels", "Barrel_pct",
                    "maxEV", "HardHit", "HardHit_pct") 

## Function to clean column names by removing year suffix (handles multiple underscores)
clean_column_name <- function(col_name) {
  # Remove the last part if it matches a year suffix (_####)
  sub("_(\\d{4})$", "", col_name)
}

# Function to extract top predictors and their correlation values for each base stat in a given season
s_get_top_predictors_with_values <- function(cor_matrix, base_stat, top_n = 10) {
  predictors_with_values <- list()
  
  # Clean column names to remove year suffix
  clean_colnames <- sapply(colnames(cor_matrix), clean_column_name)
  
  for (col in colnames(cor_matrix)) {
    # Exclude the column itself to avoid self-correlation
    correlations <- cor_matrix[, col]
    correlations <- correlations[!names(correlations) %in% col]
    
    # Match the cleaned column name with the base stat
    if (clean_column_name(col) == base_stat) {
      # Sort the correlations for this predictor in descending order
      sorted_correlations <- sort(abs(correlations), decreasing = TRUE)[1:top_n]
      
      # Modify predictor names to clean them
      predictors <- sapply(names(sorted_correlations), clean_column_name)
      
      # Store the top N predictors and their corresponding correlation values
      predictors_with_values[[col]] <- data.frame(
        Predictor = predictors,
        Correlation = sorted_correlations
      )
    }
  }
  
  return(predictors_with_values)
}

# Main loop for extracting top predictors
s_all_predictors_values <- list()

for (base_stat in base_stat_list) {
  # Validate if the base stat exists in any cleaned column names
  clean_colnames <- sapply(colnames(s_cor_matrices$`2021`), clean_column_name)  # Assumes consistent column names
  if (base_stat %in% clean_colnames) {
    s_all_predictors_values[[base_stat]] <- list()  # Initialize for each base stat
    
    # Process each season
    for (season in names(s_cor_matrices)) {
      predictors_values <- s_get_top_predictors_with_values(s_cor_matrices[[season]], base_stat)
      
      # Add a Year column to each tibble for ordering
      if (length(predictors_values) > 0) {
        predictors_values <- lapply(predictors_values, function(df) {
          df$Year <- season
          return(df)
        })
        
        s_all_predictors_values[[base_stat]] <- c(s_all_predictors_values[[base_stat]], predictors_values)
      }
    }
    
    # Combine and filter results
    combined_data <- do.call(rbind, s_all_predictors_values[[base_stat]])
    
    # Ensure ordering by Year
    combined_data <- combined_data %>%
      group_by(Predictor, Year) %>%
      arrange(Year, desc(Correlation)) %>%
      slice_head(n = 5) %>%
      ungroup()
    
    # Save the filtered results
    s_all_predictors_values[[base_stat]] <- combined_data
  }
}


# Manual validation indicates the top 10 predictors in each year are accurate

#
#

# Extract Top 5 overall predictors

# Combine all predictors across base stats into a single data frame
s_all_predictors_combined <- bind_rows(s_all_predictors_values, .id = "Base_Stat")

# Create a function to calculate top 10 predictors for each stat
get_top_10_per_stat <- function(data) {
  data %>%
    group_by(Predictor) %>%                      # Group by each predictor
    summarize(
      Frequency = n(),                           # Count how many times the predictor appears
      Avg_Correlation = mean(Correlation)        # Calculate the average correlation value
    ) %>%
    arrange(desc(Frequency), desc(Avg_Correlation)) %>%  # Rank by frequency, then avg correlation
    slice_head(n = 10)                            # Take the top 5 predictors
}

# Apply the function to each stat
s_top_predictors_by_stat <- s_all_predictors_combined %>%
  group_by(Base_Stat) %>%                        # Group by each stat
  group_modify(~ get_top_10_per_stat(.x)) %>%     # Apply the function to each group
  ungroup()



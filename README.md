Creating My Own Projection Model

Current Status - Testing and Proof of Concept Phase

The R file REZ_AVG_PROJ_TESTS is my current projections for batting average. Comparing the projections to the 3-year mean batting average - the output data largely makes a lot of sense regardless of various plate appearance thresholds. This is especially true for players who have had a meaningful number of plate appearances in each of the last 3 years. 

Currently, the training data does not include NA values and the plate appearance minimum to be included was 50PA in the in the target variable year (meaning if AVG2022 is being associated with 2021, 2020, and 2019 data the player must have had 50PA in 2022 to be included). However, the data used to predict 2025AVG does include NAs and has no minimum PA requirement. So, the model is trained on clean data, but is occasionallly asked to predict on unclean data. The unclean data stems from rookies and players who missed seasons from 2024-2022 (the data used to predict), as there are NA values in any season they did not appear. 


#Future Steps
1. Examine performance when trained with unclean data and tested on unclean data

2. Remove all retired/inactive players from the predict dataset

3. Examine performance when trained with clean data, but predicting on data that includes missing season data for rookies and injured players

a. Make all missing data league averages
b. Make missing data for rookies league average and missing data for vets career averages
c. Make missing data for rookies MLB-adjusted minor league data and missing data for vets career averages
d. Make missing data for rookies MLB-adjusted minor league data and missing data for vets league averages

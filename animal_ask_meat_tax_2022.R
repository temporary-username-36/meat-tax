#Animal Ask 2022
# www.animalask.org
#This is the replication of our analysis of a meat tax campaign in the UK
#See the main analysis in our Google Spreadsheet here:
# https://docs.google.com/spreadsheets/d/1OJY2B3Xwust2QLlrZ_e5ELcW6CGGYvWxJn5zkI02V64/edit#gid=0

#################################
### Import data and variables ###
#################################

#Import data on the probability of success and counterfactual change
data_probability <- 0.05
data_counterfactual <- 1

#Import data for our pre-determined variables
df_variables <- read.csv("animal_ask_meat_tax_2022_data_variables.csv")
dfv <- data.frame(df_variables)
rownames(dfv) <- dfv[,1]
dfv[,1] <- NULL
dfv

#Import data for the consumption changes in our scenarios
df_scenarios <- read.csv("animal_ask_meat_tax_2022_data_scenarios.csv")
dfs_consumption_change <- data.frame(df_scenarios)
rownames(dfs_consumption_change) <- dfs_consumption_change[,1]
dfs_consumption_change[,1] <- NULL
dfs_consumption_change

#################################
### Intermediate calculations ###
#################################

#Consumption of individuals, annual UK
dfv$consumption_individuals <- dfv$slaughter*dfv$imports

#Wild-caught fish lifespan
#i.e., we're converting from minutes to years
dfv["fish_wild",]$lifespan <- dfv[6,]$death_duration/60/24/365.25

#Consumption of animal-years, annual UK
dfv$consumption_animalyears <- dfv$consumption_individuals*dfv$lifespan
dfv[c("cow_dairy","poultry_egg"),]$consumption_animalyears <- dfv[c(7,8),]$alive_at_a_time*dfv[c(7,8),]$imports

#############################
### Scenario calculations ###
#############################

#Change in consumption of animal-years due to tax annually
dfs_consumption_change_animalyears <- dfs_consumption_change*dfv$consumption_animalyears

#Welfare point-years gained due to tax annually
dfs_welfarepoints_change <- dfs_consumption_change_animalyears*dfv$welfare_points_uk

#Change in consumption of animal-years due to tax annually, sum
result_animalyears <- colSums(dfs_consumption_change_animalyears)

#Sum of welfare point-years gained due to tax annually
result_welfarepoints <- colSums(dfs_welfarepoints_change)

#Welfare-points gained due to tax in expectation
result_welfarepoints_expect <- result_welfarepoints*
  data_probability*data_counterfactual

##########################################
### Show the results for each scenario ###
##########################################

#The row of changes in consumption of animal years (per year)
#In tab "Main - Key Outputs" of the main analysis spreadsheet
result_animalyears

#The table of changes in consumption of animal years (per year) by species
#In tab "Main - Key Outputs" of the main analysis spreadsheet
dfs_consumption_change_animalyears

#The row of welfare points gained (per year)
#In tab "Main - Key Outputs" of the main analysis spreadsheet
result_welfarepoints

#The row of welfare points gained (in expectation)
#In tab "Main - Analysis" of the main analysis spreadsheet (bottom right)
result_welfarepoints_expect
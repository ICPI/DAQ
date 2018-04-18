##  Import SAST data on volume
##  J Davis, S Mehta
##  Purpose: Read in data from OU-level SASTs and store in rational format
##  Date: 4/17/18
##  Updated:

##  Notes
## Source: (SAST SOURCE URL HERE)


# Dependancies ------------------------------------------------------------

# install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(fs)



# Folders -----------------------------------------------------------------

import <- "C:/Users/GHFP/Documents/data/SAST"
export <- "~/Documents/Data/"
sheetname <-


# function to get OU name -------------------------------------------------

get_ou <- function(filepath){
  ou <- read_xlsx(filepath, 
                  col_names = FALSE,
                  sheet = "6_ENTER_Planned Assessments",
                  range = "F11") %>% 
    pull(X__1) }

# Import function #1 ---------------------------------------------------------

get_SAST <- function(filepath){
  df <- read_xlsx(filepath, 
                 sheet = "1_REVIEW_Facility High Volume",
                 skip = 15) %>% 
        select(-X__1)
  names(df) <- gsub(" ", "_", names(df))
  ou <- get_ou(filepath)
  df <- df %>% 
    mutate(operatingunit = ou) %>% 
    select(operatingunit, everything())
                   }
# add OU col

# create mean and std_dev -------------------------------------------------

sast_stats <- function(filepath) {

mean_df <- test %>% 
            gather(indicator, mean, HTC_TST, TX_CURR, VMMC_CIRC, KP_MAT, na.rm = TRUE) %>%
            filter(mean > 0) %>% 
            group_by(indicator) %>% 
            summarise_at(vars("mean"), ~ mean(.,na.rm = TRUE))

sd_df <- test %>% 
  gather(indicator, sd, HTC_TST, TX_CURR, VMMC_CIRC, KP_MAT, na.rm = TRUE) %>%
  filter(sd > 0) %>% 
  group_by(indicator) %>% 
  summarise_at(vars("sd"), ~ sd(.,na.rm = TRUE)) 

threshold <- bind_cols(mean_df, sd_df)
threshold <- full_join(mean_df, sd_df, by = "indicator") 

ou <- get_ou(filepath)

threshold <- threshold %>%
  mutate_at(vars(mean, sd), ~ifelse(is.na(.),0,.)) %>% 
  mutate(operatingunit = ou, 
         hv_threshold = mean + sd) %>%
  select(operatingunit, everything())  
 
}



test <- get_SAST(file.path(import, "FY18_SAST Calculator_KENYA_USAID_20170911.xlsx"))
glimpse(test)

filepath <- (file.path(import, "FY18_SAST Calculator_KENYA_USAID_20170911.xlsx"))










###########################################################
###                      MASTER SRIPT                   ###
### This code is designed to run all scripts at once.   ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

##### Paths

# 1 = Maxime
# 2 = Mathis

user_indiv <- 2

if (user_indiv == 1) {
  path_code <- "XXXXX"
  path_data <- "XXXXX"
  path_graphs <- "XXXXX"
  path_tab <- "XXXXX"
} else if (user_indiv == 2){
  path_code <- "C:\\Users\\mthsa\\Desktop\\SQD_DSSS\\ProjDSSS\\Code"
  path_data <- "C:\\Users\\mthsa\\Desktop\\SQD_DSSS\\ProjDSSS\\Data"
  path_graphs <- "C:\\Users\\mthsa\\Desktop\\SQD_DSSS\\ProjDSSS\\Figures"
  path_tab <- "C:\\Users\\mthsa\\Desktop\\SQD_DSSS\\ProjDSSS\\Tables"
}

##### Scripts

setwd(path_code)

source(paste0(path_code,"\\1_requirements.R"))
#source(paste0(path_code,"\\2_bases.R"))
#source(paste0(path_code,"\\3_stat_des_insiders.R"))
#source(paste0(path_code,"\\4_stat_des_journaux.R"))



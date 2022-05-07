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
  path_code <- "C:\\Users\\Dell\\Desktop\\DSSS propre\\Code"
  path_queries <- "C:\\Users\\Dell\\Desktop\\DSSS propre\\Queries"
  path_data <- "C:\\Users\\Dell\\Desktop\\DSSS propre\\Data"
  path_data_scraping <- "C:\\Users\\Dell\\Desktop\\DSSS propre\\Data scraping"
  path_graphs <- "XXXXX"
  path_tab <- "XXXXX"
} else if (user_indiv == 2){
  path_code <- "C:\\Users\\mthsa\\Desktop\\CovidLiterature\\Code"
  path_queries <- "C:\\Users\\mthsa\\Desktop\\CovidLiterature\\Queries"
  path_data <- "C:\\Users\\mthsa\\Desktop\\CovidLiterature\\Data"
  path_data_scraping <- "C:\\Users\\mthsa\\Desktop\\CovidLiterature\\Data scraping"
  path_graphs <- "XXXXX"
  path_tab <- "XXXXX"
}

##### Scripts

setwd(path_code)

#source(paste0(path_code,"\\1_requirements.R"))
#source(paste0(path_code,"\\2_generation_queries_corona.R"))
#source(paste0(path_code,"\\3_traitement_base_coronavirus.R"))
#source(paste0(path_code,"\\4_df_auteurs.R"))
#source(paste0(path_code,"\\5_generation_queries_auteurs.R"))
#source(paste0(path_code,"\\6_traitement_base_auteurs.R"))
#source(paste0(path_code,"\\7_df_auteurs_complete.R"))
#source(paste0(path_code,"\\8_construction_capitaux.R"))
#source(paste0(path_code,"\\9_Régressions.R"))














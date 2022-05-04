
###########################################################
###        Traitement base articles coronavirus         ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

library(plyr)

# Lire les csv téléchargés / supprimer les doublons

setwd(paste0(path_data_scraping, '\\Data auteurs'))
i = 0
deleted_lines = 0
rm('df_full_articles')
for (file in list.files()) {
  i = i+1
  print(i)
  temporary = read.csv(file)
  # Garder Scopus permet de restreindre les articles parcourus ainsi que de supprimer les lignes fautives
  print(length(rownames(temporary)))
  deleted_lines = deleted_lines + length(rownames(temporary))
  temporary = temporary[temporary$Source == 'Scopus',]
  deleted_lines = deleted_lines - length(rownames(temporary))
  if (!exists("df_full_articles")){
    df_full_articles <- temporary
  }
  if (exists("df_full_articles")){
    df_full_articles <- rbind.fill(df_full_articles, temporary)
    df_full_articles = df_full_articles[!duplicated(df_full_articles$EID), ]
  }
}

setwd(path_data)
write.csv(df_full_articles, "df_full_articles.csv")
















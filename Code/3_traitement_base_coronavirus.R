
###########################################################
###        Traitement base articles coronavirus         ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################


# Fusion des données singulières
# A ne lancer qu'une fois
setwd(paste0(path_data_scraping, "\\Data coronavirus"))
deleted_rows = 0
for (data in list.files()){
  print(data)
  temporary <-read.csv(data, header=TRUE)
  BOOL = is.null(temporary)
  if (!exists("dataset")){
    dataset <- temporary
  }
  if (exists("dataset")){
    dataset[setdiff(names(temporary), names(dataset))] <- NA
    temporary[setdiff(names(dataset), names(temporary))] <- NA
    dataset <- rbind(dataset, temporary)
  }
}

setwd(path_data)
write.csv(dataset, "full_data_coronavirus.csv")

# Dénombrement des auteurs

setwd(path_data)
dataset = read.csv("full_data_coronavirus.csv")

dataset = dataset[dataset$Source == 'Scopus',]
auteurs = c()
i = 0
for (ligne in dataset$Author.s..ID) {
  i = i+1
  print(i)
  ligne = strsplit(ligne, ';')[[1]]
  ligne = trimws(ligne)
  auteurs = append(auteurs, ligne)
}
count_authors = as.data.frame(table(auteurs))
count_authors$auteurs = as.numeric(as.character(count_authors$auteurs))
count_authors = count_authors[!is.na(count_authors$auteurs),]
count_authors = count_authors[nchar(count_authors$auteurs) > 9,]
distribution_freq = as.data.frame(table(count_authors$Freq))

# Création du squelette future dataframe auteurs

df_auteurs = count_authors
setwd(path_data)
write.csv(df_auteurs, "df_auteurs.csv")
















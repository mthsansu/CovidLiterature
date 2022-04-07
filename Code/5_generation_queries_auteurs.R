
###########################################################
###        Queries scraping par auteur                  ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################


# Construction des requêtes pour le scraping des articles par auteur

setwd(path_data)
df_auteurs = read.csv("df_auteurs_sampled.csv")

requete_liste = c()
n_auteurs = 0
requete = 'AU-ID('
auteurs_parcourus = 0
for (auteur in df_auteurs$Id_auteur) {
  print(n_auteurs)
  n_auteurs = n_auteurs+1
  if (auteurs_parcourus < 10) {
    auteurs_parcourus = auteurs_parcourus+1
    requete = paste(requete, auteur, ') OR AU-ID(', sep = '')
  } else {
    auteurs_parcourus = 0
    requete = paste(requete, ')')
    requete = gsub(' OR AU-ID\\( \\)', '', requete)
    requete_liste = append(requete_liste, requete)
    requete = 'AU-ID('
  }
}

df_requetes = as.data.frame(requete_liste)
df_requetes$repartition_requetes = 'NA'
colnames(df_requetes) = c('req_tot', 'repartition_requetes')
a_repartir = length(df_requetes$repartition_requetes)
df_requetes$repartition_requetes[0:a_repartir] = 'Maxime1'
df_requetes$repartition_requetes[0:as.integer(a_repartir*3/4)] = 'Maxime2'
df_requetes$repartition_requetes[0:as.integer(a_repartir*2/4)] = 'Mathis1'
df_requetes$repartition_requetes[0:as.integer(a_repartir*1/4)] = 'Mathis2'

df_requetes$done = 0
df_requetes$problematic = 0

setwd(path_queries)
write.csv(df_requetes, "base_requetes_auteurs.csv")

# On refait avec les problméatiques

setwd('C:\\Users\\Dell\\Desktop\\Projet DSSS\\Bases')
prob = readChar('clés_manquantes.txt', file.info('clés_manquantes.txt')$size)
prob = regmatches(prob, gregexpr("[[:digit:]]+", prob))
prob = as.numeric(unlist(numbers))
prob = unique(prob)

prob


# Lire les csv téléchargés / supprimer les doublons

setwd('C:\\Users\\Dell\\Desktop\\Projet DSSS\\Bases\\Data propre')
article_done = c()
i = 0
for (file in list.files()) {
  i = i+1
  print(i)
  read_file = read.csv(file, header = TRUE)
  BOOL = read_file$EID %in% article_done
  read_file = read_file[!BOOL,]
  article_done = c(article_done, read_file$EID)
  rm(read_file)
}


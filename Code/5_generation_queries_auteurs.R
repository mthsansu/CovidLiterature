
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
# A ne lancer qu'une fois
setwd(path_queries)
rm('df_query')
for (file_name in c('query_tracker_Maxime1', 'query_tracker_Maxime2', 'query_tracker_Mathis1', 
                    'query_tracker_Mathis2', 'query_tracker_Mathis3')) {
  temporary = read.csv(file_name)
  temporary = temporary[,c('req_tot', 'repartition_requetes', 'done', 'problematic')]
  if (!exists("df_query")){
    df_query <- temporary
  }
  if (exists("df_query")){
    df_query <- rbind(df_query, temporary)
  }
}
df_query = df_query[,c('req_tot', 'repartition_requetes', 'done', 'problematic')]
df_query = df_query[(df_query$problematic == 1),]
df_query = df_query[!duplicated(df_query), ]

# On reconstruit des requetes plus petites

library(stringr)
query_list = c()
for (query in df_query$req_tot) {
  query_list = c(query_list, as.numeric(str_extract_all(query, "[0-9]+")[[1]]))
}

auteurs = unique(query_list)
requete_liste = c()
n_auteurs = 0
requete = 'AU-ID('
auteurs_parcourus = 0
for (auteur in auteurs) {
  print(n_auteurs)
  n_auteurs = n_auteurs+1
  if (auteurs_parcourus < 5) {
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

setwd(path_queries)
write.csv(df_requetes, "base_requetes_auteurs_problematics.csv")



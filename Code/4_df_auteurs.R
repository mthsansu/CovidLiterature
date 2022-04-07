
###########################################################
### Construction base auteurs corona et échantillonage  ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################


setwd(path_data)
df_auteurs = read.csv("df_auteurs.csv")
articles_corona = read.csv("full_data_coronavirus.csv")

# Traitements nécessaires
articles_corona = articles_corona[articles_corona$Source == 'Scopus',]
articles_corona$Source.title = tolower(articles_corona$Source.title)
articles_corona$Source.title[articles_corona$Source.title == 'plos pathogens'] = 'plos one'
articles_corona$Source.title[articles_corona$Source.title %in% c('the bmj', 'bmj open')] = 'bmj'
articles_corona$Source.title[articles_corona$Source.title == 'the lancet'] = 'lancet'
articles_corona$Source.title[articles_corona$Source.title == 'the journal of general virology'] = 'journal of general virology'
articles_corona$Source.title[articles_corona$Source.title == 'the journal of infectious diseases'] = 'journal of infectious diseases'
articles_corona$Source.title[articles_corona$Source.title == 'the veterinary record'] = 'veterinary record'
articles_corona$Year = as.numeric(articles_corona$Year)
articles_corona$Cited.by = as.numeric(articles_corona$Cited.by)

# Choix 1 : On garde les auteurs avec au moins 1 article écrit dans le champ des coronavirus
colnames(df_auteurs) = c('X', 'Id_auteur', 'Nb_articles_coronavirus')
df_auteurs = df_auteurs[df_auteurs$Nb_articles_coronavirus > 1,]

# Construction de variables a partir de la base full_data_coronavirus.csv
# 
# Pour chaque journal, nombre de contributions
# Premiere annee de contribution
# Derniere annee de contribution
# Annee moyenne de contribution
# Nombre cumulé de citations coronavirus
# Somme des log des citations coronavirus
# Nombre d'articles écrits en anglais / en autre
# (et ratio)
# Nombre d'articles écrits en open access
# (et ratio)
# Nombre d'articles écrits par document type
# (et ratio)
# Nombre d'articles par publication stage
# (et ratio)
#

list_journaux = unique(articles_corona$Source.title)
articles_corona$Cited.by[is.na(articles_corona$Cited.by)] = 0
somme_log_plus1 = function(x) {
  y = sum(log(x+1))
  return(y)
}
list_types = unique(articles_corona$Document.Type)
list_pubstage = unique(articles_corona$Publication.Stage)

new_cols = c(list_journaux, 'premiere_annee_contribution', 'derniere_annee_contribution', 'annee_maximale_contribution',
             'nombre_cumulee_citations_coronavirus', 'somme_log_citations_coronavirus', 'nombre_articles_ecrits_anglais',
             'nombre_articles_ecrits_autres', 'nombre_articles_ecrits_anglais_ratio', 'nombre_articles_ecrits_autres_ratio',
             'nombre_articles_open_access', list_types, list_pubstage)
place_to_add = (length(colnames(df_auteurs))+1):(length(colnames(df_auteurs)) + length(new_cols))
df_auteurs[,place_to_add] = NA
colnames(df_auteurs)[place_to_add] = new_cols

done = c()
i = 0
for (i in c(1:length(df_auteurs$Id_auteur))) {
  i = i+1
  print(i)
  author_id = df_auteurs$Id_auteur[i]
  if (!(author_id %in% done)) {
    nb_contribs = df_auteurs$Nb_articles_coronavirus[i]
    subset_base_corona = articles_corona[grepl(as.character(author_id), articles_corona$Author.s..ID),]
    if (length(subset_base_corona$Author.s..ID) != 0) {
        
      journaux_auteurs = subset_base_corona$Source.title
      for (journal in unique(journaux_auteurs)) {
        if (journal %in% list_journaux) {
          df_auteurs[i, journal] = sum(journaux_auteurs == journal) / nb_contribs
        }
      }
      df_auteurs[i, 'premiere_annee_contribution'] = min(subset_base_corona$Year)
      df_auteurs[i, 'derniere_annee_contribution'] = max(subset_base_corona$Year)
      df_auteurs[i, 'annee_moyenne_contribution'] = mean(subset_base_corona$Year)
      df_auteurs[i, 'nombre_cumulee_citations_coronavirus'] = somme_log_plus1(subset_base_corona$Cited.by)
      df_auteurs[i, 'nombre_articles_ecrits_anglais'] = sum(subset_base_corona$Language.of.Original.Document == 'English')
      df_auteurs[i, 'nombre_articles_ecrits_autres'] = sum(subset_base_corona$Language.of.Original.Document != 'English')
      df_auteurs[i, 'nombre_articles_ecrits_anglais_ratio'] = sum(subset_base_corona$Language.of.Original.Document == 'English') / nb_contribs
      df_auteurs[i, 'nombre_articles_ecrits_autres_ratio'] = sum(subset_base_corona$Language.of.Original.Document != 'English') / nb_contribs
      doctype_auteurs = subset_base_corona$Document.Type
      for (type in unique(doctype_auteurs)) {
        if (type %in% list_types) {
          df_auteurs[i, type] = sum(doctype_auteurs == type) / nb_contribs
        }
      }
      for (pubstage in unique(doctype_auteurs)) {
        if (pubstage %in% list_pubstage) {
          df_auteurs[i, pubstage] = sum(doctype_auteurs == pubstage) / nb_contribs
        }
      }
    }
  }
}

setwd(path_data)
write.csv(df_auteurs, "df_auteurs_variables.csv")


# Stratification

data_for_stratification = df_auteurs[,c('X', 'Id_auteur')]
data_for_stratification$periode = NA
data_for_stratification$periode[df_auteurs$premiere_annee_contribution < 2003] = 1
data_for_stratification$periode[(2002 < df_auteurs$premiere_annee_contribution) & (df_auteurs$premiere_annee_contribution < 2013)] = 2
data_for_stratification$periode[(2012 < df_auteurs$premiere_annee_contribution) & (df_auteurs$premiere_annee_contribution < 2020)] = 3
data_for_stratification$periode[2019 < df_auteurs$premiere_annee_contribution] = 4
data_for_stratification$Lancet = 0
data_for_stratification$Lancet[df_auteurs$lancet > 0] = 1
data_for_stratification$PlosOne = 0
data_for_stratification$PlosOne[df_auteurs$`plos one` > 0] = 1
data_for_stratification$Nb_articles_coronavirus = 0
data_for_stratification$Nb_articles_coronavirus[df_auteurs$Nb_articles_coronavirus %in% c(3,4)] = 1
data_for_stratification$Nb_articles_coronavirus[df_auteurs$Nb_articles_coronavirus %in% c(5,6,7)] = 2
data_for_stratification$Nb_articles_coronavirus[df_auteurs$Nb_articles_coronavirus %in% c(8,9,10)] = 3
data_for_stratification$Nb_articles_coronavirus[(21 > df_auteurs$Nb_articles_coronavirus) & (df_auteurs$Nb_articles_coronavirus > 10)] = 4
data_for_stratification$Nb_articles_coronavirus[df_auteurs$Nb_articles_coronavirus > 20] = 5
data_for_stratification$Nb_citations = 0
data_for_stratification$Nb_citations[df_auteurs$nombre_cumulee_citations_coronavirus %in% c(1,2)] = 1
data_for_stratification$Nb_citations[df_auteurs$nombre_cumulee_citations_coronavirus %in% c(3,4,5)] = 2
data_for_stratification$Nb_citations[df_auteurs$nombre_cumulee_citations_coronavirus %in% c(6,7,8,9,10)] = 3
data_for_stratification$Nb_citations[(21 > df_auteurs$nombre_cumulee_citations_coronavirus) & (df_auteurs$nombre_cumulee_citations_coronavirus > 10)] = 4
data_for_stratification$Nb_citations[df_auteurs$nombre_cumulee_citations_coronavirus > 20] = 5
data_for_stratification$strat_label = ''
for (column in colnames(data_for_stratification)) {
  if (!(column %in% c('X', 'Id_auteur', 'strat_label'))) {
    data_for_stratification$strat_label = paste(data_for_stratification$strat_label, data_for_stratification[,column], sep = '_')
  }
}

i = 0
data_for_stratification = data_for_stratification[order(data_for_stratification$strat_label),]
sample_size = 10000
prob = sample_size / length(df_auteurs$Id_auteur)
sample = c()
for (group in unique(data_for_stratification$strat_label)) {
  i = i+1
  print(i)
  BOOL = data_for_stratification$strat_label == group
  sample = c(sample, sample(0:1, size = sum(BOOL), prob = c(1-prob, prob), replace = TRUE))
}
data_for_stratification$sample = sample

sampled_data = df_auteurs[df_auteurs$Id_auteur %in% data_for_stratification$Id_auteur[data_for_stratification$sample == 1],]

setwd(path_data)
write.csv(sampled_data, "df_auteurs_sampled.csv")

hist(df_auteurs$premiere_annee_contribution)
hist(sampled_data$premiere_annee_contribution)
hist(df_auteurs$Nb_articles_coronavirus)
hist(sampled_data$Nb_articles_coronavirus)
hist(df_auteurs$nombre_cumulee_citations_coronavirus)
hist(sampled_data$nombre_cumulee_citations_coronavirus)










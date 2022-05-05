
###########################################################
###           Finalisation de la base auteurs           ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)

memory.limit(size = 50000)

setwd(path_data)
df_auteurs = read.csv("df_auteurs_sampled.csv")
old_articles = fread("full_data_coronavirus.csv", select = c('Source', 'EID'), header = TRUE)
old_articles = old_articles[old_articles$Source == 'Scopus',]
list_articles = old_articles$EID
rm('old_articles')
setwd(path_data)
articles = read.csv("full_data.csv")

# Traitements nécessaires
articles = articles[articles$Source == 'Scopus',]
articles$article_corona = 0
articles$article_corona[articles$EID %in% list_articles] = 1
articles$Source.title = tolower(articles$Source.title)
articles$Source.title[articles$Source.title == 'plos pathogens'] = 'plos one'
articles$Source.title[articles$Source.title %in% c('the bmj', 'bmj open')] = 'bmj'
articles$Source.title[articles$Source.title == 'the lancet'] = 'lancet'
articles$Source.title[articles$Source.title == 'the journal of general virology'] = 'journal of general virology'
articles$Source.title[articles$Source.title == 'the journal of infectious diseases'] = 'journal of infectious diseases'
articles$Source.title[articles$Source.title == 'the veterinary record'] = 'veterinary record'
articles$Year = as.numeric(articles$Year)
articles$Cited.by = as.numeric(articles$Cited.by)
articles$Cited.by[is.na(articles$Cited.by)] = 0

# Création squelette de la base auteur année

list_journaux = unique(articles$Source.title[articles$article_corona == 1])
list_doctype = unique(articles$Document.Type[articles$article_corona == 1])
list_pubstage = unique(articles$Publication.Stage[articles$article_corona == 1])
col_names = c('nb_contribs', list_journaux, list_doctype, list_pubstage, 'nb_citations',
              'sommelog_citations', 'nombre_max_citation', 'nombre_articles_ecrits_anglais', 
              'liste_coauteurs', 'nb_coauteurs')
len_cols = length(col_names)
col_names = c('author_id', 'annee',
              unlist(lapply(col_names, paste0, '_coronavirus')),
              col_names)
somme_log_plus1 = function(x) {
  y = sum(log(x+1))
  return(y)
}

done = c()
new_df = list()
i = 0

start.time <- Sys.time()
for (annee in 2022:min(articles$Year)) {
  
  subset_annee = articles[articles$Year == annee,]
  cores=detectCores()
  cl <- makeCluster(cores[1]-2) #not to overload your computer
  registerDoParallel(cl = cl)
  computed_df = foreach(author_id = unique(df_auteurs$Id_auteur), .combine = rbind) %dopar% {
# for (annee in 1999:2022) {
#   subset_annee = articles[articles$Year == annee,]
#   for (author_id in df_auteurs$Id_auteur) {
    
    new_row = c(author_id, annee)
    
    auteur_annee = paste0(as.character(author_id), as.character(annee))
    if (!(auteur_annee %in% done)) {
      
      # Pour les articles endogènes
      
      subset_base = subset_annee[subset_annee$article_corona == 1,]
      subset_base = subset_base[grepl(paste0(';', as.character(author_id), ';'), subset_base$Author.s..ID),]
      
      if (length(subset_base$EID) != 0) {
        
        nb_contribs = length(subset_base$EID)
        new_row = c(new_row, nb_contribs)
        
        journaux_auteurs = subset_base$Source.title
        list_jour_val = c()
        for (journal in list_journaux) {
          list_jour_val = c(list_jour_val, sum(journaux_auteurs == journal) / nb_contribs)
        }
        new_row = c(new_row, list_jour_val)
        
        doctype_auteurs = subset_base$Document.Type
        list_doctype_val = c()
        for (type in list_doctype) {
          list_doctype_val = c(list_doctype_val, sum(doctype_auteurs == type) / nb_contribs)
        }
        new_row = c(new_row, list_doctype_val)
        
        doctype_auteurs = subset_base$Publication.stage
        list_pubstage_val = c()
        for (pubstage in list_pubstage) {
          list_pubstage_val = c(list_pubstage_val, sum(doctype_auteurs == pubstage) / nb_contribs)
        }
        new_row = c(new_row, list_pubstage_val)
        
        nb_citations = sum(subset_base$Cited.by)
        sommelog_citations = somme_log_plus1(subset_base$Cited.by)
        nombre_max_citation = max(subset_base$Cited.by)
        nombre_articles_ecrits_anglais = sum(subset_base$Language.of.Original.Document == 'English') / nb_contribs
        
        liste_coauteurs = sapply(strsplit(as.character(subset_base$Author.s..ID), ";"), `[`, 1)
        liste_coauteurs = unique(liste_coauteurs)
        nb_coauteurs = length(liste_coauteurs)-1
        liste_coauteurs = paste(paste0(liste_coauteurs, ';'), collapse = '')

        new_row = c(new_row, nb_citations, sommelog_citations, nombre_max_citation, nombre_articles_ecrits_anglais, 
                    liste_coauteurs, nb_coauteurs)
      }
      else {
        new_row = c(new_row, 0, rep(NA, len_cols-1))
      }
      
      # Pour les articles exogènes
      
      subset_base = subset_annee[subset_annee$article_corona == 0,]
      subset_base = subset_base[grepl(paste0(';', as.character(author_id), ';'), subset_base$Author.s..ID),]
      
      if (length(subset_base$EID) != 0) {
        
        nb_contribs = length(subset_base$EID)
        new_row = c(new_row, nb_contribs)
        
        journaux_auteurs = subset_base$Source.title
        list_jour_val = c()
        for (journal in list_journaux) {
          list_jour_val = c(list_jour_val, sum(journaux_auteurs == journal) / nb_contribs)
        }
        new_row = c(new_row, list_jour_val)
        
        doctype_auteurs = subset_base$Document.Type
        list_doctype_val = c()
        for (type in list_doctype) {
          list_doctype_val = c(list_doctype_val, sum(doctype_auteurs == type) / nb_contribs)
        }
        new_row = c(new_row, list_doctype_val)
        
        list_pubstage_val = c()
        for (pubstage in list_pubstage) {
          list_pubstage_val = c(list_pubstage_val, sum(doctype_auteurs == pubstage) / nb_contribs)
        }
        new_row = c(new_row, list_pubstage_val)
        
        nb_citations = sum(subset_base$Cited.by)
        sommelog_citations = somme_log_plus1(subset_base$Cited.by)
        nombre_max_citation = max(subset_base$Cited.by)
        nombre_articles_ecrits_anglais = sum(subset_base$Language.of.Original.Document == 'English') / nb_contribs

        liste_coauteurs = sapply(strsplit(as.character(subset_base$Author.s..ID), ";"), `[`, 1)
        liste_coauteurs = unique(liste_coauteurs)
        nb_coauteurs = length(liste_coauteurs)-1
        liste_coauteurs = paste(paste0(liste_coauteurs, ';'), collapse = '')

        new_row = c(new_row, nb_citations, sommelog_citations, nombre_max_citation, nombre_articles_ecrits_anglais, 
                    liste_coauteurs, nb_coauteurs)
      
      }
      else {
        new_row = c(new_row, 0, rep(NA, len_cols-1))
      }
      done = c(done, auteur_annee)
    }
    new_row
  }
  
  stopCluster(cl)
  
  if (length(new_df) != 0) {
    new_df = rbind(new_df, computed_df)
  } else {
    new_df = computed_df
  }
  
  print(annee)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# 55 min

new_df = as.data.frame(new_df)
colnames(new_df) = col_names
new_df = new_df[!(is.na(new_df['author_id'])),]
  
setwd(path_data)
write.csv(new_df, "df_auteurs_variables.csv")










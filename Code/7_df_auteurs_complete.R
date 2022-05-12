
###########################################################
###           Finalisation de la base auteurs           ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(readr)

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
articles$Source.title[articles$Source.title == 'bmj'] = 'the bmj'
articles$Source.title[articles$Source.title == 'lancet'] = 'the lancet'
articles$Source.title[articles$Source.title == 'the journal of general virology'] = 'journal of general virology'
articles$Source.title[articles$Source.title == 'journal of infectious diseases'] = 'the journal of infectious diseases'
articles$Source.title[articles$Source.title == 'the veterinary record'] = 'veterinary record'
articles$Year = as.numeric(articles$Year)
articles$Cited.by = as.numeric(articles$Cited.by)
articles$Cited.by[is.na(articles$Cited.by)] = 0
articles$Author.s..ID = paste0(';', articles$Author.s..ID)

# On réalise que certains articles ont un nombre de coauteurs disproportionné
len_liste = c()
count = str_count(as.character(articles$Author.s..ID), ";")
quantile(count, probs = seq(0, 1, 0.001))
articles = articles[count <= 50,]

# Traitement journaux
journaux_df = articles[articles$article_corona == 1,]
journaux_df = journaux_df[,c('Source.title', 'Cited.by')]
journaux_df = journaux_df %>% group_by(Source.title) %>% summarise(total_citations = sum(Cited.by),
                                                                   moyenne_citations = mean(Cited.by))
journaux_df$index = rownames(journaux_df)

journaux_p1 = c('Journal Of Virology',
                'Advances In Experimental Medicine And Biology',
                'Virology',
                'Journal Of General Virology',
                'Archives Of Virology',
                'Plos One',
                'Avian Diseases',
                'American Journal Of Veterinary Research',
                'Veterinary Microbiology',
                'Virus Research',
                'Veterinary Record',
                'Journal Of Immunology',
                'Veterinary Immunology And Immunopathology',
                'Journal Of Virological Methods',
                'Journal Of Biological Chemistry')
journaux_p2 = c('Journal Of Virology',
                'Virology',
                'Emerging Infectious Diseases',
                'Plos One',
                'Vaccine',
                'Journal Of Virological Method',
                'Journal Of Clinical Microbiology',
                'Journal Of General Virology',
                'Journal Of Biological Chemistry',
                'Virus Research',
                'Lancet',
                'Proceedings Of The National Academy Of Sciences Of The United States Of America',
                'Biochemical And Biophysical Research Communications',
                'Journal Of Infectious Diseases',
                'Journal Of Immunology')
journaux_p3 = c('Plos One',
                'Journal Of Virology',
                'Scientific Reports',
                'Viruses',
                'Virology',
                'Virus Research',
                'Archives Of Virology',
                'Emerging Infectious Diseases',
                'Journal Of General Virology',
                'Plos Pathogens',
                'Veterinary Microbiology',
                'Antiviral Research',
                'Vaccine',
                'Journal Of Virological Methods',
                'Virology Journal')
journaux_p4 = c('International Journal Of Environmental Research And Public Health', 
                'Plos One', 
                'Sustainability (Switzerland)', 
                'Scientific Reports', 
                'Frontiers In Psychology', 
                'BMJ', 
                'Frontiers In Immunology', 
                'Frontiers In Public Health', 
                'International Journal Of Molecular Sciences', 
                'Viruses', 
                'Journal Of Medical Virology', 
                'ACM International Conference Proceeding Series', 
                'BMJ Open', 
                'Journal Of Clinical Medicine', 
                'Vaccines')

journaux = unique(c(journaux_p1, journaux_p2, journaux_p3, journaux_p4))

journaux_df$p1 = 0
journaux_df$p1[journaux_df$Source.title %in% tolower(journaux_p1)] = 1
journaux_df$p2 = 0
journaux_df$p2[journaux_df$Source.title %in% tolower(journaux_p2)] = 1
journaux_df$p3 = 0
journaux_df$p3[journaux_df$Source.title %in% tolower(journaux_p3)] = 1
journaux_df$p4 = 0
journaux_df$p4[journaux_df$Source.title %in% tolower(journaux_p4)] = 1

for (i in 1:length(articles$Source.title)) {
  print(i)
  source = articles$Source.title[i]
  for (journal in journaux_df$Source.title) {
    journal = tolower(journal)
    if (journal == source) {
      new_source <- gsub(journal, paste0('journal_', journaux_df$index[journaux_df$Source.title == journal]), source)
      articles$Source.title[i] <- new_source
    }
  }
}

setwd(path_data)
write.csv(journaux_df, "df_journaux.csv")


# Création squelette de la base auteur année

list_journaux = paste0('journal_', journaux_df$index)
list_journaux_nb_articles = paste0(list_journaux, '_nb_articles')
list_journaux_nb_citations = paste0(list_journaux, '_nb_citations')
list_journaux_score_journal = paste0(list_journaux, '_score_journal')
list_doctype = unique(articles$Document.Type[articles$article_corona == 1])
list_pubstage = unique(articles$Publication.Stage[articles$article_corona == 1])
col_names = c('nb_contribs', 
              list_journaux_nb_articles, list_journaux_nb_citations, list_journaux_score_journal, 
              list_doctype, list_pubstage, 
              'nb_citations', 'sommelog_citations', 'nombre_max_citation', 'nombre_articles_ecrits_anglais', 
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

# for (annee in 1999:1999) {
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
        
        list_jour_nb_articles = c()
        list_jour_nb_citations = c()
        list_journaux_score_journal = c()
        for (index in journaux_df$index) {
          jour_nb_articles = sum(subset_base$Source.title == paste0('journal_', index))
          list_jour_nb_articles = c(list_jour_nb_articles, jour_nb_articles)
          jour_nb_citations = sum(subset_base$Cited.by[subset_base$Source.title == paste0('journal_', index)])
          list_jour_nb_citations = c(list_jour_nb_citations, jour_nb_citations)
          jour_score_journal = journaux_df$moyenne_citations[journaux_df$index == index]
          list_journaux_score_journal = c(list_journaux_score_journal, jour_score_journal)
        }
        new_row = c(new_row, list_jour_nb_articles, list_jour_nb_citations, list_journaux_score_journal)
        
        doctype_auteurs = subset_base$Document.Type
        list_doctype_val = c()
        for (type in list_doctype) {
          list_doctype_val = c(list_doctype_val, sum(doctype_auteurs == type))
        }
        new_row = c(new_row, list_doctype_val)
        
        doctype_auteurs = subset_base$Publication.stage
        list_pubstage_val = c()
        for (pubstage in list_pubstage) {
          list_pubstage_val = c(list_pubstage_val, sum(doctype_auteurs == pubstage))
        }
        new_row = c(new_row, list_pubstage_val)
        
        nb_citations = sum(subset_base$Cited.by)
        sommelog_citations = somme_log_plus1(subset_base$Cited.by)
        nombre_max_citation = max(subset_base$Cited.by)
        nombre_articles_ecrits_anglais = sum(subset_base$Language.of.Original.Document == 'English')
        
        liste_coauteurs = unlist(strsplit(as.character(subset_base$Author.s..ID), ";"))
        liste_coauteurs = unique(liste_coauteurs)
        liste_coauteurs = liste_coauteurs[liste_coauteurs != '']
        nb_coauteurs = length(liste_coauteurs)-1
        liste_coauteurs = paste(paste0(';', liste_coauteurs, ';'), collapse = '')

        new_row = c(new_row, nb_citations, sommelog_citations, nombre_max_citation, nombre_articles_ecrits_anglais, 
                    liste_coauteurs, nb_coauteurs)
      } else {
        new_row = c(new_row, 0, rep(NA, len_cols-1))
      }
      
      # Pour les articles exogènes
      
      subset_base = subset_annee[subset_annee$article_corona == 0,]
      subset_base = subset_base[grepl(paste0(';', as.character(author_id), ';'), subset_base$Author.s..ID),]
      
      if (length(subset_base$EID) != 0) {
        
        nb_contribs = length(subset_base$EID)
        new_row = c(new_row, nb_contribs)
        
        list_jour_nb_articles = c()
        list_jour_nb_citations = c()
        list_journaux_score_journal = c()
        for (index in journaux_df$index) {
          jour_nb_articles = sum(subset_base$Source.title == paste0('journal_', index))
          list_jour_nb_articles = c(list_jour_nb_articles, jour_nb_articles)
          jour_nb_citations = sum(subset_base$Cited.by[subset_base$Source.title == paste0('journal_', index)])
          list_jour_nb_citations = c(list_jour_nb_citations, jour_nb_citations)
          jour_score_journal = journaux_df$moyenne_citations[journaux_df$index == index]
          list_journaux_score_journal = c(list_journaux_score_journal, jour_score_journal)
        }
        new_row = c(new_row, list_jour_nb_articles, list_jour_nb_citations, list_journaux_score_journal)
        
        doctype_auteurs = subset_base$Document.Type
        list_doctype_val = c()
        for (type in list_doctype) {
          list_doctype_val = c(list_doctype_val, sum(doctype_auteurs == type))
        }
        new_row = c(new_row, list_doctype_val)
        
        list_pubstage_val = c()
        for (pubstage in list_pubstage) {
          list_pubstage_val = c(list_pubstage_val, sum(doctype_auteurs == pubstage))
        }
        new_row = c(new_row, list_pubstage_val)
        
        nb_citations = sum(subset_base$Cited.by)
        sommelog_citations = somme_log_plus1(subset_base$Cited.by)
        nombre_max_citation = max(subset_base$Cited.by)
        nombre_articles_ecrits_anglais = sum(subset_base$Language.of.Original.Document == 'English')

        liste_coauteurs = unlist(strsplit(as.character(subset_base$Author.s..ID), ";"))
        liste_coauteurs = unique(liste_coauteurs)
        liste_coauteurs = liste_coauteurs[liste_coauteurs != '']
        nb_coauteurs = length(liste_coauteurs)-1
        liste_coauteurs = paste(paste0(';', liste_coauteurs, ';'), collapse = '')

        new_row = c(new_row, nb_citations, sommelog_citations, nombre_max_citation, nombre_articles_ecrits_anglais, 
                    liste_coauteurs, nb_coauteurs)
      
      } else {
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
# 1.1h

new_df = as.data.frame(new_df)
colnames(new_df) = col_names
new_df = new_df[!(is.na(new_df['author_id'])),]
  
setwd(path_data)
write.csv(new_df, "df_auteurs_variables.csv")





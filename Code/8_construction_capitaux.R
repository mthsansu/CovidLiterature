

###########################################################
###                   Construction capitaux             ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

library(dplyr)
library(foreach)
library(doParallel)

setwd(path_data)
data_auteurs = read.csv("df_auteurs_variables.csv")
journaux_df = read.csv("df_journaux.csv")

# Traitement des données

data_auteurs = subset(data_auteurs, select = -c(X))
journaux_df = subset(journaux_df, select = -c(X))
data_auteurs = data_auteurs[!duplicated(data_auteurs[c('author_id', 'annee')]),]
data_auteurs = data_auteurs[!is.na(data_auteurs$author_id),]
data_auteurs[is.na(data_auteurs)] = 0

# Construction de la base

data_auteurs$nb_articles_cumule = NA
data_auteurs$nb_articles_cumule_coronavirus = NA
data_auteurs$nb_citations_cumule = NA
data_auteurs$nb_citations_cumule_coronavirus = NA
data_auteurs$sommelog_citations_cumule = NA
data_auteurs$sommelog_citations_cumule_coronavirus = NA
data_auteurs$premiere_annee_contribution = NA
data_auteurs$premiere_annee_contribution_coronavirus = NA
data_auteurs$annee_maximale_total_contribution = NA
data_auteurs$annee_maximale_total_contribution_coronavirus = NA
data_auteurs$annee_maximale_contribution = NA
data_auteurs$annee_maximale_contribution_coronavirus = NA
data_auteurs$derniere_annee_contribution = NA
data_auteurs$derniere_annee_contribution_coronavirus = NA
data_auteurs$nb_coauteurs_cumule = NA
data_auteurs$nb_coauteurs_cumule_coronavirus = NA
data_auteurs$nb_coauteurs_cumule_toutchamp = NA
data_auteurs$nb_max_citation_cumule = NA
data_auteurs$nb_max_citation_cumule_coronavirus = NA
data_auteurs$nb_citations_moy_cumule = NA
data_auteurs$nb_citations_moy_cumule_coronavirus = NA

data_auteurs$nb_articles_p1 = NA
data_auteurs$nb_articles_p1_coronavirus = NA
data_auteurs$nb_articles_p2 = NA
data_auteurs$nb_articles_p2_coronavirus = NA
data_auteurs$nb_articles_p3 = NA
data_auteurs$nb_articles_p3_coronavirus = NA
data_auteurs$nb_articles_p4 = NA
data_auteurs$nb_articles_p4_coronavirus = NA
data_auteurs$nb_citations_p1 = NA
data_auteurs$nb_citations_p1_coronavirus = NA
data_auteurs$nb_citations_p2 = NA
data_auteurs$nb_citations_p2_coronavirus = NA
data_auteurs$nb_citations_p3 = NA
data_auteurs$nb_citations_p3_coronavirus = NA
data_auteurs$nb_citations_p4 = NA
data_auteurs$nb_citations_p4_coronavirus = NA
data_auteurs$prestige_journal_moyenne = NA
data_auteurs$prestige_journal_moyenne_coronavirus = NA
data_auteurs$prestige_journal_somme = NA
data_auteurs$prestige_journal_somme_coronavirus = NA
data_auteurs$nb_articles_p1_cumule = NA
data_auteurs$nb_articles_p1_cumule_coronavirus = NA
data_auteurs$nb_articles_p2_cumule = NA
data_auteurs$nb_articles_p2_cumule_coronavirus = NA
data_auteurs$nb_articles_p3_cumule = NA
data_auteurs$nb_articles_p3_cumule_coronavirus = NA
data_auteurs$nb_articles_p4_cumule = NA
data_auteurs$nb_articles_p4_cumule_coronavirus = NA
data_auteurs$nb_citations_p1_cumule = NA
data_auteurs$nb_citations_p1_cumule_coronavirus = NA
data_auteurs$nb_citations_p2_cumule = NA
data_auteurs$nb_citations_p2_cumule_coronavirus = NA
data_auteurs$nb_citations_p3_cumule = NA
data_auteurs$nb_citations_p3_cumule_coronavirus = NA
data_auteurs$nb_citations_p4_cumule = NA
data_auteurs$nb_citations_p4_cumule_coronavirus = NA
data_auteurs$prestige_journal_moyenne_cumule = NA
data_auteurs$prestige_journal_moyenne_cumule_coronavirus = NA
data_auteurs$prestige_journal_somme_cumule = NA
data_auteurs$prestige_journal_somme_cumule_coronavirus = NA


full_colnames = c(colnames(data_auteurs), 
                  c(unlist(lapply(colnames(subset(data_auteurs, 
                                                  select = -c(annee, 
                                                              author_id, 
                                                              liste_coauteurs, liste_coauteurs_coronavirus))), 
                                  paste0, '_lagged'))))

i = 0
cores=detectCores()
cl <- makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(cl)
start.time <- Sys.time()
# new_df_auteurs <- foreach(auteur = unique(data_auteurs$author_id),
#                           .combine = rbind,
#                           .packages = 'dplyr') %dopar% {
for (auteur in unique(data_auteurs$author_id[1:200])) {

  # Suppression
  subset = data_auteurs[data_auteurs$author_id == auteur,]
  if (sum(subset$nb_contribs_coronavirus) != 0) {
    
    premiere_annee_contribution = min(subset$annee[(subset$nb_contribs + subset$nb_contribs_coronavirus) != 0])
    derniere_annee_contribution = max(subset$annee[(subset$nb_contribs + subset$nb_contribs_coronavirus) != 0])
    subset$premiere_annee_contribution = premiere_annee_contribution
    subset$derniere_annee_contribution = derniere_annee_contribution
    
    subset = subset[(subset$author_id == auteur) & 
                      (subset$annee >= premiere_annee_contribution) &
                      (subset$annee <= derniere_annee_contribution),]
      
    #Ajout de variables
    premiere_annee_contribution_coronavirus = min(subset$annee[subset$nb_contribs_coronavirus != 0])
    subset$premiere_annee_contribution_coronavirus = premiere_annee_contribution_coronavirus
    annee_maximale_total_contribution_coronavirus = subset$annee[subset$nb_contribs_coronavirus == max(subset$nb_contribs_coronavirus)]
    subset$annee_maximale_total_contribution_coronavirus = max(annee_maximale_total_contribution_coronavirus)
    derniere_annee_contribution_coronavirus = max(subset$annee[subset$nb_contribs_coronavirus != 0])
    subset$derniere_annee_contribution_coronavirus = derniere_annee_contribution_coronavirus
  
    subset = subset[order(subset$annee),]
    subset = within(subset, nb_articles_cumule_coronavirus <- cumsum(nb_contribs_coronavirus))
    subset = within(subset, nb_citations_cumule_coronavirus <- cumsum(nb_citations_coronavirus))
    subset = within(subset, sommelog_citations_cumule_coronavirus <- cumsum(sommelog_citations_coronavirus))
    subset = within(subset, nb_max_citation_cumule_coronavirus <- cummax(nombre_max_citation_coronavirus))
    subset = within(subset, nb_citations_moy_cumule_coronavirus <- cummean(nb_contribs_coronavirus))
    
    for (j in 1:length(subset$annee)) {
      
      liste_coauteurs_coronavirus = subset$liste_coauteurs_coronavirus[1:j]
      liste_coauteurs_coronavirus = liste_coauteurs_coronavirus[liste_coauteurs_coronavirus != '0']
      if (length(liste_coauteurs_coronavirus) != 0) {
        liste_coauteurs_coronavirus = unique(unlist(strsplit(liste_coauteurs_coronavirus, ';')))
        liste_coauteurs_coronavirus = liste_coauteurs_coronavirus[liste_coauteurs_coronavirus != '']
        nb_coauteurs_cumule_coronavirus = length(liste_coauteurs_coronavirus)
        nb_coauteurs_cumule_coronavirus = nb_coauteurs_cumule_coronavirus - 1
      } else {
        nb_coauteurs_cumule_coronavirus = 0
      }
      subset$nb_coauteurs_cumule_coronavirus[j] = nb_coauteurs_cumule_coronavirus
      
      annee_maximale_contribution_coronavirus = subset$annee[subset$nb_contribs_coronavirus == max(subset$nb_contribs_coronavirus)][1:j]
      subset$annee_maximale_contribution_coronavirus = max(annee_maximale_contribution_coronavirus)
      
      subset$nb_articles_p1_coronavirus[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p1 == 1], '_nb_articles_coronavirus')])
      subset$nb_articles_p2_coronavirus[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p2 == 1], '_nb_articles_coronavirus')])
      subset$nb_articles_p3_coronavirus[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p3 == 1], '_nb_articles_coronavirus')])
      subset$nb_articles_p4_coronavirus[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p4 == 1], '_nb_articles_coronavirus')])
      subset$nb_citations_p1_coronavirus[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p1 == 1], '_nb_citations_coronavirus')])
      subset$nb_citations_p2_coronavirus[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p2 == 1], '_nb_citations_coronavirus')])
      subset$nb_citations_p3_coronavirus[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p3 == 1], '_nb_citations_coronavirus')])
      subset$nb_citations_p4_coronavirus[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p4 == 1], '_nb_citations_coronavirus')])
      subset$prestige_journal_somme[j] = sum(as.numeric(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p1 == 1], '_score_journal_coronavirus')]))
      subset$prestige_journal_moyenne[j] = mean(as.numeric(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p1 == 1], '_score_journal_coronavirus')]))
      
      subset$nb_articles_p1_cumule_coronavirus[j] = sum(subset$nb_articles_p1_coronavirus[1:j])
      subset$nb_articles_p2_cumule_coronavirus[j] = sum(subset$nb_articles_p2_coronavirus[1:j])
      subset$nb_articles_p3_cumule_coronavirus[j] = sum(subset$nb_articles_p3_coronavirus[1:j])
      subset$nb_articles_p4_cumule_coronavirus[j] = sum(subset$nb_articles_p4_coronavirus[1:j])
      subset$nb_citations_p1_cumule_coronavirus[j] = sum(subset$nb_citations_p1_coronavirus[1:j])
      subset$nb_citations_p2_cumule_coronavirus[j] = sum(subset$nb_citations_p2_coronavirus[1:j])
      subset$nb_citations_p3_cumule_coronavirus[j] = sum(subset$nb_citations_p3_coronavirus[1:j])
      subset$nb_citations_p4_cumule_coronavirus[j] = sum(subset$nb_citations_p4_coronavirus[1:j])
      subset$prestige_journal_somme_cumule_coronavirus[j] = sum(subset$prestige_journal_moyenne[1:j])
      subset$prestige_journal_moyenne_cumule_coronavirus[j] = mean(subset$prestige_journal_somme[1:j])
      
    }
    
    if (sum(subset$nb_contribs) != 0) {
      
      annee_maximale_total_contribution = subset$annee[subset$nb_contribs == max(subset$nb_contribs)]
      subset$annee_maximale_total_contribution = max(annee_maximale_total_contribution)

      subset = within(subset, nb_articles_cumule <- cumsum(nb_contribs))
      subset = within(subset, nb_citations_cumule <- cumsum(nb_citations))
      subset = within(subset, sommelog_citations_cumule <- cumsum(sommelog_citations))
      subset = within(subset, nb_max_citation_cumule <- cummax(nombre_max_citation))
      subset = within(subset, nb_citations_moy_cumule <- cummean(nb_contribs))
      
      for (j in 1:length(subset$annee)) {
        
        liste_coauteurs = subset$liste_coauteurs[1:j]
        liste_coauteurs = liste_coauteurs[liste_coauteurs != '0']
        if (length(liste_coauteurs) != 0) {
          liste_coauteurs = unique(unlist(strsplit(liste_coauteurs, ';')))
          liste_coauteurs = liste_coauteurs[liste_coauteurs != '']
          nb_coauteurs_cumule = length(liste_coauteurs)
          nb_coauteurs_cumule = nb_coauteurs_cumule - 1
        } else {
          nb_coauteurs_cumule = 0
        }
        subset$nb_coauteurs_cumule[j] = nb_coauteurs_cumule
        
        nb_coauteurs_cumule_toutchamp = c(subset$liste_coauteurs[1:j], subset$liste_coauteurs_coronavirus[1:j])
        nb_coauteurs_cumule_toutchamp = length(unique(unlist(strsplit(nb_coauteurs_cumule_toutchamp, ';'))))
        
        if (nb_coauteurs_cumule_toutchamp != 0) {
          nb_coauteurs_cumule_toutchamp = nb_coauteurs_cumule_toutchamp - 1
        }
        subset$nb_coauteurs_cumule_toutchamp[j] = nb_coauteurs_cumule_toutchamp
        
        
        
        annee_maximale_contribution = subset$annee[subset$nb_contribs == max(subset$nb_contribs)][1:j]
        subset$annee_maximale_contribution = max(annee_maximale_contribution)
        
        subset$nb_articles_p1[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p1 == 1], '_nb_articles')])
        subset$nb_articles_p2[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p2 == 1], '_nb_articles')])
        subset$nb_articles_p3[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p3 == 1], '_nb_articles')])
        subset$nb_articles_p4[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p4 == 1], '_nb_articles')])
        subset$nb_citations_p1[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p1 == 1], '_nb_citations')])
        subset$nb_citations_p2[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p2 == 1], '_nb_citations')])
        subset$nb_citations_p3[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p3 == 1], '_nb_citations')])
        subset$nb_citations_p4[j] = sum(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p4 == 1], '_nb_citations')])
        subset$prestige_journal_somme[j] = sum(as.numeric(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index[journaux_df$p1 == 1], '_score_journal')]))
        subset$prestige_journal_moyenne[j] = mean(as.numeric(subset[j, colnames(subset) %in% paste0('journal_', journaux_df$index, '_score_journal')]))
        
        subset$nb_articles_p1_cumule[j] = sum(subset$nb_articles_p1_coronavirus[1:j])
        subset$nb_articles_p2_cumule[j] = sum(subset$nb_articles_p2_coronavirus[1:j])
        subset$nb_articles_p3_cumule[j] = sum(subset$nb_articles_p3_coronavirus[1:j])
        subset$nb_articles_p4_cumule[j] = sum(subset$nb_articles_p4_coronavirus[1:j])
        subset$nb_citations_p1_cumule[j] = sum(subset$nb_citations_p1_coronavirus[1:j])
        subset$nb_citations_p2_cumule[j] = sum(subset$nb_citations_p2_coronavirus[1:j])
        subset$nb_citations_p3_cumule[j] = sum(subset$nb_citations_p3_coronavirus[1:j])
        subset$nb_citations_p4_cumule[j] = sum(subset$nb_citations_p4_coronavirus[1:j])
        subset$prestige_journal_somme_cumule[j] = sum(subset$prestige_journal_moyenne[1:j])
        subset$prestige_journal_moyenne_cumule[j] = mean(subset$prestige_journal_somme[1:j])
      }
      
    } else {
      
      subset$premiere_annee_contribution = 0
      subset$annee_maximale_contribution = NA
      subset$derniere_annee_contribution = NA
      subset$nb_articles_cumule = 0 
      subset$nb_citations_cumule = 0
      subset$sommelog_citations_cumule = 0
      subset$nb_max_citation_cumule = 0
      subset$nb_citations_moy_cumule = 0
      subset$nb_coauteurs_cumule = 0
      subset$nb_coauteurs_cumule_toutchamp = subset$nb_coauteurs_cumule_coronavirus

    }
    
    # On cree des lags
    
    subset_lagged = dplyr::mutate_all(subset, lag)
    subset_lagged = subset(subset_lagged, select = -c(annee, author_id, liste_coauteurs, liste_coauteurs_coronavirus))
    colnames(subset_lagged) = c(unlist(lapply(colnames(subset_lagged), paste0, '_lagged')))
    subset = cbind(subset, subset_lagged)
    
    # On construit des capitaux
    
  } else {
    subset = as.data.frame(matrix(nrow = 1, ncol = length(full_colnames)))
  }
  colnames(subset) = full_colnames
  subset = subset %>% mutate(across(!liste_coauteurs & !liste_coauteurs_coronavirus, function(x) as.numeric(as.character(x))))
  subset
}
stopCluster(cl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# 36 minutes

new_df_auteurs = new_df_auteurs[!is.na(new_df_auteurs$author_id),]


data_auteurs$premiere_annee_contribution[data_auteurs$premiere_annee_contribution == 0] = data_auteurs$premiere_annee_contribution_coronavirus[data_auteurs$premiere_annee_contribution == 0]
data_auteurs$premiere_annee_contribution_lagged[!(is.na(data_auteurs$premiere_annee_contribution_lagged)) & (data_auteurs$premiere_annee_contribution_lagged == 0)] = data_auteurs$premiere_annee_contribution_coronavirus_lagged[!(is.na(data_auteurs$premiere_annee_contribution_lagged)) & (data_auteurs$premiere_annee_contribution_lagged == 0)]

setwd(path_data)
write.csv(new_df_auteurs, "df_auteurs_variables2_avec_journaux.csv")

new_df_auteurs = new_df_auteurs[, !(substr(colnames(new_df_auteurs), 1, 8) == 'journal_')]
write.csv(new_df_auteurs, "df_auteurs_variables2.csv")



















###########################################################
###                Analyses et Régressions              ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

library(ggplot2)
library(FactoMineR)
library(factoextra)


setwd(path_data)
data_auteurs = read.csv("df_auteurs_variables2.csv")

# Ajustement données

to_fill_with_0s = c('nb_contribs_coronavirus', 'sommelog_citations_coronavirus', 
                    
                    'nb_articles_cumule_coronavirus_lagged', 'nb_citations_cumule_coronavirus_lagged',
                    'sommelog_citations_cumule_coronavirus_lagged', 
                    'nb_coauteurs_cumule_coronavirus_lagged', 'premiere_annee_contribution_coronavirus', 
                    
                    'nb_articles_cumule_lagged', 'nb_citations_cumule_lagged',
                    'sommelog_citations_cumule_lagged', 'nb_coauteurs_cumule_lagged', 'premiere_annee_contribution')

for (col in to_fill_with_0s) {
  data_auteurs[is.na(data_auteurs[,col]),col] = 0
}

# Création variables journal-période


# ACP variables journaux

colnames(data_auteurs)[c(5:41, 62:98, 140:176, 196:232)] = paste0(colnames(data_auteurs)[c(5:41, 62:98, 140:176, 196:232)], '_journal')

pca_data = data_auteurs[c(3, 5:41)]
pca_data = scale(pca_data)
pca <- PCA(pca_data, scale.unit=TRUE, graph=FALSE)
barplot(pca$eig[,3], names.arg = 1:nrow(pca$eig), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
fviz_pca_var(pca, col.var="contrib",
             axes = c(3,4),
             select.var = list(name = colnames(pca_data)[!grepl('Flag', colnames(pca_data))]))
acp_coords = pca$ind$coord
pca_data = cbind(pca_data, acp_coords)


subset = data_auteurs[(data_auteurs$annee == 2005) & (data_auteurs$premiere_annee_contribution_coronavirus <= 2005),]
studied_cols = c('sommelog_citations_coronavirus', 'nb_articles_cumule_coronavirus_lagged', 'nb_citations_cumule_coronavirus_lagged',
                 'sommelog_citations_cumule_coronavirus_lagged', 
                 'nb_coauteurs_cumule_coronavirus_lagged', 'premiere_annee_contribution_coronavirus', 'nb_articles_cumule_lagged',
                 'nb_articles_cumule_lagged', 'nb_citations_cumule_lagged',
                 'sommelog_citations_cumule_lagged', 'nb_coauteurs_cumule_lagged', 'premiere_annee_contribution')
subset = subset[,studied_cols]
reg1 = lm(nb_contribs_coronavirus ~
            # Paramètre endogenes
            nb_articles_cumule_coronavirus_lagged +
            nb_citations_cumule_coronavirus_lagged +
            sommelog_citations_cumule_coronavirus_lagged + 
            nb_coauteurs_cumule_coronavirus_lagged +
            premiere_annee_contribution_coronavirus +
            # Parametres exogenes
            nb_articles_cumule_lagged +
            nb_citations_cumule_lagged +
            sommelog_citations_cumule_lagged +
            nb_coauteurs_cumule_lagged +
            premiere_annee_contribution,
          data = subset)

summary(reg1)




############### Evolution coefficients ##############

setwd(path_data)
data_auteurs = read.csv("df_auteurs_variables2.csv")

coeff_time_visualiser <- function(dep, endo, exo, ctrl, vtlog) {
  
  studied_cols = c(dep, endo, var_exogene, ctrl)
  
  for (col in studied_cols) {
    data_auteurs[is.na(data_auteurs[,col]),col] = 0
  }
  
  for (col in vtlog) {
    data_auteurs[is.na(data_auteurs[,col]),col] = log(data_auteurs[is.na(data_auteurs[,col]),col] + 1)
  }
  
  # On évalue l'évolution des coefficients au cours du temps
  
  reg_coeffs = data.frame(annee = as.numeric(0), 
                          coeff_capital_endogene = as.numeric(0), 
                          coeff_capital_exogene = as.numeric(0),
                          coeff_capital_exogene_min90 = as.numeric(0),
                          coeff_capital_exogene_max90 = as.numeric(0),
                          coeff_capital_endogene_min90 = as.numeric(0),
                          coeff_capital_endogene_max90 = as.numeric(0))
  
  summary_list = list()
  for (annee in 1995:2022) {
    
    subset = data_auteurs[(data_auteurs$annee == annee) & (data_auteurs$premiere_annee_contribution_coronavirus <= annee),]
    subset = subset[,studied_cols]
    
    for (col in studied_cols) {
      subset[,col] = scale(subset[,col])
      if (col == dep) {
        subset[,col] = subset[,col]*100
      }
    }
  
    # Première régression
    reg = lm(as.formula(paste0(dep, ' ~ .')),
             data = subset)
    
    summary_coeffs = summary(reg)$coeff
    summary_list[[annee]] = summary(reg)
    
    coeff_endogene = sum(summary_coeffs[endo, 'Estimate'])
    coeff_exogene = sum(summary_coeffs[exo, 'Estimate'])
    
    ecart_type_endogene = sqrt(sum(summary_coeffs[endo, 'Std. Error']**2))
    ecart_type_exogene = sqrt(sum(summary_coeffs[exo, 'Std. Error']**2))
    
    coeff_capital_endogene_min90 = coeff_endogene - 1.645*ecart_type_endogene
    coeff_capital_endogene_max90 = coeff_endogene + 1.645*ecart_type_endogene
    coeff_capital_exogene_min90 = coeff_exogene - 1.645*ecart_type_exogene
    coeff_capital_exogene_max90 = coeff_exogene + 1.645*ecart_type_exogene
    
    new_row = c(annee,
                coeff_endogene, coeff_exogene,
                coeff_capital_endogene_min90, coeff_capital_endogene_max90,
                coeff_capital_exogene_min90, coeff_capital_exogene_max90)
    reg_coeffs = rbind(reg_coeffs, new_row)
  }
  reg_coeffs = reg_coeffs[-1,]
  
  plot = ggplot(reg_coeffs, aes(x = annee)) + 
            geom_line(aes(y = coeff_capital_endogene), col='blue') + 
            geom_ribbon(aes(ymin = coeff_capital_endogene_min90, ymax = coeff_capital_endogene_max90), alpha = 0.1) +
            geom_line(aes(y = coeff_capital_exogene), col='red') + 
            geom_ribbon(aes(ymin = coeff_capital_exogene_min90, ymax = coeff_capital_exogene_max90), alpha = 0.1) +
            labs(title="nb_contributions",
                 x ="Année", y = "Coefficient régression")

  return(list(plot, summary_list))
}


var_endogene = c('nb_articles_cumule_coronavirus_lagged', 'nb_citations_cumule_coronavirus_lagged',
                 'sommelog_citations_cumule_coronavirus_lagged', 
                 'nb_coauteurs_cumule_coronavirus_lagged', 'premiere_annee_contribution_coronavirus')
var_exogene = c('nb_articles_cumule_lagged', 'nb_citations_cumule_lagged',
                'sommelog_citations_cumule_lagged', 'nb_coauteurs_cumule_lagged', 'premiere_annee_contribution')
var_controle = c('Article_coronavirus_lagged')
var_dependante = 'nb_contribs'
var_dependante = 'nb_contribs_coronavirus'
var_dependante = 'sommelog_citations_coronavirus'
var_dependante = 'nb_citations_coronavirus'
var_to_log = c('nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged')

results = coeff_time_visualiser(dep = var_dependante, 
                                   endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                                   vtlog = var_to_log)
coeff_plot = results[[1]]
coeff_plot
summaries = results[[2]]
summaries[[2022]]
          
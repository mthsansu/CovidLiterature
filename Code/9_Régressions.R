

###########################################################
###                Analyses et Régressions              ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################


##### Import des packages et des données #####

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(stringr)

setwd(path_data)
data_auteurs = read.csv("df_auteurs_variables2.csv")

data_auteurs$premiere_annee_contribution[data_auteurs$premiere_annee_contribution == 0] = data_auteurs$premiere_annee_contribution_coronavirus[data_auteurs$premiere_annee_contribution == 0]
data_auteurs$premiere_annee_contribution_lagged[!(is.na(data_auteurs$premiere_annee_contribution_lagged)) & (data_auteurs$premiere_annee_contribution_lagged == 0)] = data_auteurs$premiere_annee_contribution_coronavirus_lagged[!(is.na(data_auteurs$premiere_annee_contribution_lagged)) & (data_auteurs$premiere_annee_contribution_lagged == 0)]


##### Ajustement données #####

# Certains NA peuvent être comblés avec des 0
to_fill_with_0s = c('nb_contribs_coronavirus', 'sommelog_citations_coronavirus', 
                    
                    'nb_articles_cumule_coronavirus_lagged', 'nb_citations_cumule_coronavirus_lagged',
                    'sommelog_citations_cumule_coronavirus_lagged', 
                    'nb_coauteurs_cumule_coronavirus_lagged', 'premiere_annee_contribution_coronavirus', 
                    
                    'nb_articles_cumule_lagged', 'nb_citations_cumule_lagged',
                    'sommelog_citations_cumule_lagged', 'nb_coauteurs_cumule_lagged', 'premiere_annee_contribution')
for (col in to_fill_with_0s) {
  data_auteurs[is.na(data_auteurs[,col]),col] = 0
}

##### Traitement des variables journaux #####

# Statistiques descriptives

plot_data = data_auteurs
plot_data[is.na(plot_data)] = 0
plot_data = plot_data[,c(3, 4, 55, 272:length(colnames(plot_data)))] %>% group_by(annee) %>% summarize_all(sum)
plot_data$total_p1_coronavirus = (plot_data$p1_coronavirus*plot_data$nb_contribs_coronavirus)
plot_data$total_p2_coronavirus = (plot_data$p2_coronavirus*plot_data$nb_contribs_coronavirus)
plot_data$total_p3_coronavirus = (plot_data$p3_coronavirus*plot_data$nb_contribs_coronavirus)
plot_data$total_p4_coronavirus = (plot_data$p4_coronavirus*plot_data$nb_contribs_coronavirus)
ggplot(plot_data[plot_data$annee <= 2019,], aes(x = annee)) + 
  geom_line(aes(y = total_p1_coronavirus), col='blue') + 
  geom_line(aes(y = total_p2_coronavirus), col='red') + 
  geom_line(aes(y = total_p3_coronavirus), col='green') + 
  geom_line(aes(y = total_p4_coronavirus), col='black')
plot_data$total_p1_coronavirus_pond = (plot_data$p1_coronavirus_pond*plot_data$nb_contribs_coronavirus)
plot_data$total_p2_coronavirus_pond = (plot_data$p2_coronavirus_pond*plot_data$nb_contribs_coronavirus)
plot_data$total_p3_coronavirus_pond = (plot_data$p3_coronavirus_pond*plot_data$nb_contribs_coronavirus)
plot_data$total_p4_coronavirus_pond = (plot_data$p4_coronavirus_pond*plot_data$nb_contribs_coronavirus)
ggplot(plot_data[plot_data$annee <= 2019,], aes(x = annee)) + 
  geom_line(aes(y = total_p1_coronavirus_pond), col='blue') + 
  geom_line(aes(y = total_p2_coronavirus_pond), col='red') + 
  geom_line(aes(y = total_p3_coronavirus_pond), col='green') + 
  geom_line(aes(y = total_p4_coronavirus_pond), col='black')

# ACP #

# Test des différents traitements des données 
# ACP vanilla, sur les observations auteur-année
pca_data = data_auteurs[c(5:41)]
pca_data[is.na(pca_data)] = 0
pca_data = pca_data[apply(pca_data[,-1], 1, function(x) !all(x==0)),]
pca_data = pca_data[,-1]

# Aggréger années
pca_data = data_auteurs[c(3, 5:41)]
pca_data[is.na(pca_data)] = 0
pca_data = pca_data %>% group_by(annee) %>% summarize_all(mean)
pca_data = pca_data[apply(pca_data[,-1], 1, function(x) !all(x==0)),]
pca_data = pca_data[,-1]

# Aggréger auteurs
pca_data = data_auteurs[c(2, 5:41)]
pca_data[is.na(pca_data)] = 0
pca_data = pca_data %>% group_by(author_id) %>% summarize_all(mean)
pca_data = pca_data[apply(pca_data[,-1], 1, function(x) !all(x==0)),]
pca_data = pca_data[,-1]

# Aggréger nb_articles_cumules
pca_data = data_auteurs[c(119, 5:41)]
pca_data[is.na(pca_data)] = 0
pca_data = pca_data %>% mutate(cuts = cut(nb_citations_cumule_coronavirus, c(-Inf, 1, 2, 3, 5, 10, 20, Inf))) %>% group_by(cuts) %>% summarize_all(mean)
pca_data = pca_data[apply(pca_data[,-1], 1, function(x) !all(x==0)),]
pca_data = pca_data[,-c(1,2)]

# Aggréger nb_articles_cumules, annee
pca_data = data_auteurs[c(3, 118, 5:41)]
pca_data[is.na(pca_data)] = 0
pca_data = pca_data %>% 
  mutate(cuts = cut(nb_articles_cumule_coronavirus, c(-Inf, 1, 2, 3, 5, 10, 20, Inf))) %>% 
  group_by(cuts, annee) %>% 
  summarize_all(mean)
pca_data = pca_data[apply(pca_data[,-1], 1, function(x) !all(x==0)),]
pca_data = pca_data[,-1]

# Lancement de l'ACP
pca_data = scale(pca_data)
pca <- PCA(pca_data, scale.unit=FALSE, graph=FALSE)
barplot(pca$eig[,3], names.arg = 1:nrow(pca$eig), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
fviz_pca_var(pca, col.var="contrib",
             axes = c(1,2),
             select.var = list(name = colnames(pca_data)))
acp_coords = pca$ind$coord
pca_data = cbind(pca_data, acp_coords)


############### Evolution coefficients ##############

setwd(path_data)
data_auteurs = read.csv("df_auteurs_variables2.csv")

data_auteurs$premiere_annee_contribution[data_auteurs$premiere_annee_contribution == 0] = data_auteurs$premiere_annee_contribution_coronavirus[data_auteurs$premiere_annee_contribution == 0]
data_auteurs$premiere_annee_contribution_lagged[!(is.na(data_auteurs$premiere_annee_contribution_lagged)) & (data_auteurs$premiere_annee_contribution_lagged == 0)] = data_auteurs$premiere_annee_contribution_coronavirus_lagged[!(is.na(data_auteurs$premiere_annee_contribution_lagged)) & (data_auteurs$premiere_annee_contribution_lagged == 0)]

coeff_time_visualiser <- function(dep, endo, exo, ctrl, vtlog, data) {
  
  studied_cols = c(dep, endo, var_exogene, ctrl)
  
  for (col in studied_cols) {
    data[is.na(data[,col]),col] = 0
  }
  
  for (col in vtlog) {
    data[,col] = log(data[,col] + 1)
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
    
    subset = data[(data$annee == annee) & (data$premiere_annee_contribution_coronavirus <= annee),]
    subset = subset[,studied_cols]
    subset = subset[complete.cases(subset),]
    
    for (col in studied_cols) {
      subset[,col] = scale(subset[,col])
      #subset[,col] = subset[,col] / sd(subset[,col])
      if (col == dep) {
        subset[,col] = subset[,col]*100
      }
    }
  
    # Première régression
    reg = lm(as.formula(paste0(dep, ' ~ 0 + .')),
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
  
  plot1 <- ggplot(reg_coeffs, aes(x = annee)) + 
      geom_line(aes(y = coeff_capital_endogene), col='blue') + 
      geom_ribbon(aes(ymin = coeff_capital_endogene_min90, ymax = coeff_capital_endogene_max90), alpha = 0.1) +
      geom_line(aes(y = coeff_capital_exogene), col='red') + 
      geom_ribbon(aes(ymin = coeff_capital_exogene_min90, ymax = coeff_capital_exogene_max90), alpha = 0.1) +
      labs(title="nb_contributions",
           x ="Année", y = "Coefficient régression")
  plot1

  plot(reg_coeffs$coeff_capital_endogene, reg_coeffs$coeff_capital_exogene)
  abline(lm(reg_coeffs$coeff_capital_exogene ~ reg_coeffs$coeff_capital_endogene))
  plot2 <- recordPlot()

  return(list(plot1, summary_list, plot2))
}


var_endogene = c('nb_articles_cumule_coronavirus_lagged', 
                 'nb_citations_cumule_coronavirus_lagged',
                 'prestige_journal_somme_cumule_coronavirus_lagged', 
                 'nb_coauteurs_cumule_coronavirus_lagged', 
                 'premiere_annee_contribution_coronavirus')
var_exogene = c('nb_articles_cumule_lagged', 
                'nb_citations_cumule_lagged',
                'prestige_journal_somme_cumule_lagged',
                'nb_coauteurs_cumule_lagged', 
                'premiere_annee_contribution')
var_controle = c()
var_dependante = 'nb_contribs_coronavirus'
var_dependante = 'sommelog_citations_coronavirus'
var_dependante = 'nb_citations_coronavirus'
var_to_log = c()
var_to_log = c('nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged', 
               'nb_citations_cumule_coronavirus_lagged', 'nb_citations_cumule_lagged',
               'nb_articles_cumule_coronavirus_lagged', 'nb_articles_cumule_lagged',
               'prestige_journal_somme_cumule_coronavirus_lagged', 'prestige_journal_somme_cumule_lagged')

results = coeff_time_visualiser(dep = var_dependante, 
                                endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                                vtlog = var_to_log,
                                data = data_auteurs)
coeff_plot = results[[1]]
coeff_plot
summaries = results[[2]]
summaries[[2017]]
relation_plot = results[[3]]
replayPlot(relation_plot)


# Cobb Douglas et coefficients fixes

setwd(path_data)
data_auteurs = read.csv("df_auteurs_variables2.csv")

data_auteurs$premiere_annee_contribution[data_auteurs$premiere_annee_contribution == 0] = data_auteurs$premiere_annee_contribution_coronavirus[data_auteurs$premiere_annee_contribution == 0]
data_auteurs$premiere_annee_contribution_lagged[!(is.na(data_auteurs$premiere_annee_contribution_lagged)) & (data_auteurs$premiere_annee_contribution_lagged == 0)] = data_auteurs$premiere_annee_contribution_coronavirus_lagged[!(is.na(data_auteurs$premiere_annee_contribution_lagged)) & (data_auteurs$premiere_annee_contribution_lagged == 0)]

na_to_skip = c('premiere_annee_contribution_coronavirus', 'premiere_annee_contribution')

coeff_time_visualiser_CD <- function(dep, endo, exo, ctrl, vtlog) {
  
  studied_cols = c(dep, endo, var_exogene, ctrl)
  
  for (col in studied_cols) {
    if (!(col %in% na_to_skip)) {
      data_auteurs[is.na(data_auteurs[,col]),col] = 0
    }
  }
  
  for (col in vtlog) {
    data_auteurs[is.na(data_auteurs[,col]),col] = log(data_auteurs[is.na(data_auteurs[,col]),col] + 1)
  }
  
  # On évalue l'évolution des coefficients au cours du temps
  
  reg_coeffs = data.frame(annee = as.numeric(0), 
                          coeff = as.numeric(0), 
                          coeff_min90 = as.numeric(0),
                          coeff_max90 = as.numeric(0))
  
  summary_list = list()
  for (annee in 1995:2022) {
    
    subset = data_auteurs[(data_auteurs$annee == annee) & (data_auteurs$premiere_annee_contribution_coronavirus <= annee),]
    subset = subset[,studied_cols]
    
    # Première régression
    reg = lm(as.formula(paste0(dep, ' ~ .')),
             data = subset)
    summary_coeffs = summary(reg)$coeff
    
    for (col in endo) {
      subset[,col] = subset[,col]*summary_coeffs[col, 'Estimate']
    }
    for (col in exo) {
      subset[,col] = subset[,col]*summary_coeffs[col, 'Estimate']
    }
    subset$capital_endogene = rowSums(subset[,endo])
    subset$capital_exogene = rowSums(subset[,exo])
    
    subset$capital_endogene = (subset$capital_endogene - min(subset$capital_endogene)) / max(subset$capital_endogene - min(subset$capital_endogene))
    subset$capital_exogene = (subset$capital_exogene - min(subset$capital_exogene)) / max(subset$capital_exogene - min(subset$capital_exogene))
    subset$capital_endogene = log(subset$capital_endogene*100 + 1)
    subset$capital_exogene = log(subset$capital_exogene*100 + 1)
    
    subset$new_dep = log(subset[,dep] + 1) - subset$capital_endogene
    subset$new_expl = subset$capital_exogene - subset$capital_endogene
    
    # Seconde régression
    reg = lm(new_dep ~ 0 + new_expl,
             data = subset)
    
    summary_coeffs = summary(reg)$coeff
    summary_list[[annee]] = summary(reg)
    
    coeff = summary_coeffs['new_expl', 'Estimate']
    ecart_type = sqrt(sum(summary_coeffs['new_expl', 'Std. Error']**2))
    coeff_min90 = coeff - 1.645*ecart_type
    coeff_max90 = coeff + 1.645*ecart_type

    new_row = c(annee,
                coeff,
                coeff_min90, coeff_max90)
    
    reg_coeffs = rbind(reg_coeffs, new_row)
  }
  reg_coeffs = reg_coeffs[-1,]
  
  plot = ggplot(reg_coeffs, aes(x = annee)) + 
    geom_line(aes(y = coeff), col='blue') + 
    geom_ribbon(aes(ymin = coeff_min90, ymax = coeff_max90), alpha = 0.1) +
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
var_dependante = 'nb_contribs_coronavirus'
var_dependante = 'sommelog_citations_coronavirus'
var_dependante = 'nb_citations_coronavirus'
var_to_log = c()
var_to_log = c('nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged')

results = coeff_time_visualiser_CD(dep = var_dependante, 
                                   endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                                   vtlog = var_to_log)
coeff_plot = results[[1]]
coeff_plot
summaries = results[[2]]
summaries[[2022]]





<<<<<<< Updated upstream


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
library(ggthemes)
library(tidyr)

setwd(path_data)
base = read.csv("df_auteurs_variables2.csv")

base = base[(base$premiere_annee_contribution_coronavirus <= base$annee) & (base$derniere_annee_contribution_coronavirus >= base$annee),]
base$anciennete = base$annee - base$premiere_annee_contribution
base$anciennete_coronavirus = base$annee  - base$premiere_annee_contribution_coronavirus
base$anciennete[base$anciennete < 0] = 0
base$anciennete_coronavirus[base$anciennete_coronavirus < 0] = 0
base$anciennete_carre = base$anciennete**2
base$anciennete_coronavirus_carre = base$anciennete_coronavirus**2
base$nb_citations_cumule_lagged_carre = base$nb_citations_cumule_lagged**2
base$nb_citations_cumule_coronavirus_lagged_carre = base$nb_citations_cumule_coronavirus_lagged**2
base$nb_coauteurs_cumule_coronavirus_lagged_carre = data_auteurs$nb_coauteurs_cumule_coronavirus_lagged**2
base$nb_coauteurs_cumule_lagged_carre = data_auteurs$nb_coauteurs_cumule_lagged**2

### Régressions OLS ######

reg_data = base
var_endogene = c('nb_articles_cumule_coronavirus_lagged', 
                 'nb_citations_cumule_coronavirus_lagged',
                 'nb_coauteurs_cumule_coronavirus_lagged', 
                 'anciennete_coronavirus',
                 'prestige_journal_somme_cumule_coronavirus_lagged')
var_exogene = c('nb_articles_cumule_lagged', 
                'nb_citations_cumule_lagged',
                'nb_coauteurs_cumule_lagged', 
                'anciennete',
                'prestige_journal_somme_cumule_lagged')
var_controle = c()
var_dependante = 'sommelog_citations_coronavirus'
var_to_log = c('sommelog_citations_coronavirus', 
               'nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged', 
               'nb_citations_cumule_coronavirus_lagged', 'nb_citations_cumule_lagged',
               'nb_articles_cumule_coronavirus_lagged', 'nb_articles_cumule_lagged')

reg_data = reg_data[reg_data$annee == 2020,]
reg_data = reg_data[,c(var_endogene, var_exogene, var_controle, var_dependante)]
quantiles = quantile(reg_data$sommelog_citations_coronavirus, probs = seq(0,1,0.01))
quantiles
for (col in var_to_log) {
  reg_data[,col] = log(reg_data[,col] +1)
}
reg = lm(as.formula(paste0(var_dependante, ' ~ .')),
         data = reg_data)
summary(reg)

plot(reg$residuals)
qqPlot(reg$residuals)

coeff_retriever <- function(dep, endo, exo, ctrl, vtlog, data, scaling = TRUE) {
  
  studied_cols = c(dep, endo, var_exogene, ctrl)
  
  for (col in studied_cols) {
    data[is.na(data[,col]),col] = 0
  }
  
  for (col in vtlog) {
    data[,col] = log(data[,col] + 1)
  }
  
  # On évalue l'évolution des coefficients au cours du temps
  
  reg_coeffs = data.frame(annee = as.numeric(0), 
                          var = as.character(0), 
                          coeff = as.numeric(0),
                          coeff_min90 = as.numeric(0),
                          coeff_max90 = as.numeric(0))
  
  for (annee in 1995:2022) {
    
    subset = data[(data$annee == annee) & (data$premiere_annee_contribution_coronavirus <= annee),]
    subset = subset[,studied_cols]
    subset = subset[complete.cases(subset),]
    
    if (scaling) {
      for (col in studied_cols) {
        subset[,col] = subset[,col] - mean(subset[,col])
        subset[,col] = subset[,col] / sd(subset[,col])
        if (col == dep) {
          subset[,col] = subset[,col]*100
        }
      }
    }
    
    # Première régression
    reg = lm(as.formula(paste0(dep, ' ~ .')),
             data = subset)
    
    for (var in c(endo, exo, ctrl)) {
      summary_coeffs = summary(reg)$coeff
      summary_coeffs = summary_coeffs[var,]
      coeff = sum(summary_coeffs['Estimate'])
      ecart_type = sqrt(sum(summary_coeffs['Std. Error']**2))
      coeff_min90 = coeff - 1.645*ecart_type
      coeff_max90 = coeff + 1.645*ecart_type
      
      new_row = c(annee,
                  var,
                  coeff,
                  coeff_min90, coeff_max90)
      reg_coeffs = rbind(reg_coeffs, new_row)
    }
  }
  reg_coeffs = reg_coeffs[-1,]
  
  return(reg_coeffs)
}

#### Modèle standard 1 ####

data_auteurs = base

var_endogene = c('nb_articles_cumule_coronavirus_lagged', 
                 'nb_citations_cumule_coronavirus_lagged',
                 'nb_citations_cumule_coronavirus_lagged_carre',
                 'anciennete_coronavirus',
                 'prestige_journal_somme_cumule_coronavirus_lagged')
var_exogene = c('nb_articles_cumule_lagged', 
                'nb_citations_cumule_lagged_carre',
                'nb_citations_cumule_lagged',
                'anciennete',
                'prestige_journal_somme_cumule_lagged')
var_controle = c()
var_dependante = 'sommelog_citations_coronavirus'
var_to_log = c('sommelog_citations_coronavirus', 
               'nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged', 
               'nb_citations_cumule_coronavirus_lagged', 'nb_citations_cumule_lagged',
               'nb_articles_cumule_coronavirus_lagged', 'nb_articles_cumule_lagged')

results1 = coeff_retriever(dep = var_dependante, 
                          endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                          vtlog = var_to_log,
                          data = data_auteurs)

#### Modèle standard 1bis ####

data_auteurs = base

var_endogene = c('nb_articles_cumule_coronavirus_lagged', 
                 'nb_coauteurs_cumule_coronavirus_lagged',
                 'nb_coauteurs_cumule_coronavirus_lagged_carre',
                 'anciennete_coronavirus',
                 'prestige_journal_somme_cumule_coronavirus_lagged')
var_exogene = c('nb_articles_cumule_lagged', 
                'nb_coauteurs_cumule_lagged_carre',
                'nb_coauteurs_cumule_lagged',
                'anciennete',
                'prestige_journal_somme_cumule_lagged')
var_controle = c()
var_dependante = 'sommelog_citations_coronavirus'
var_to_log = c('sommelog_citations_coronavirus', 
               'nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged', 
               'nb_citations_cumule_coronavirus_lagged', 'nb_citations_cumule_lagged',
               'nb_articles_cumule_coronavirus_lagged', 'nb_articles_cumule_lagged')

results1bis = coeff_retriever(dep = var_dependante, 
                           endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                           vtlog = var_to_log,
                           data = data_auteurs)

#### Modèle avec juste nb_articles 2 #### 

data_auteurs = base

var_endogene = c('nb_articles_cumule_coronavirus_lagged')
var_exogene = c('nb_articles_cumule_lagged')
var_controle = c()
var_dependante = 'sommelog_citations_coronavirus'
var_to_log = c('sommelog_citations_coronavirus', 
               'nb_articles_cumule_coronavirus_lagged', 'nb_articles_cumule_lagged')

results2 = coeff_retriever(dep = var_dependante, 
                           endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                           vtlog = var_to_log,
                           data = data_auteurs)

#### Modèle avec juste nb_citations 3 #### 

data_auteurs = base

var_endogene = c('nb_citations_cumule_coronavirus_lagged', 'nb_citations_cumule_coronavirus_lagged_carre')
var_exogene = c('nb_citations_cumule_lagged', 'nb_citations_cumule_lagged_carre')
var_controle = c()
var_dependante = 'sommelog_citations_coronavirus'
var_to_log = c('sommelog_citations_coronavirus', 
               'nb_citations_cumule_coronavirus_lagged', 'nb_citations_cumule_lagged')

results3 = coeff_retriever(dep = var_dependante, 
                           endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                           vtlog = var_to_log,
                           data = data_auteurs)

#### Modèle avec juste nb_coauteurs 4#### 

data_auteurs = base

var_endogene = c('nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_coronavirus_lagged_carre')
var_exogene = c('nb_coauteurs_cumule_lagged', 'nb_coauteurs_cumule_lagged_carre')
var_controle = c()
var_dependante = 'sommelog_citations_coronavirus'
var_to_log = c('sommelog_citations_coronavirus', 
               'nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged')

results4 = coeff_retriever(dep = var_dependante, 
                           endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                           vtlog = var_to_log,
                           data = data_auteurs)

#### Prédiction du nombre de publications 5 #### 

data_auteurs = base

var_endogene = c('nb_articles_cumule_coronavirus_lagged', 
                 'nb_citations_cumule_coronavirus_lagged',
                 'nb_citations_cumule_coronavirus_lagged_carre',
                 'anciennete_coronavirus',
                 'prestige_journal_somme_cumule_coronavirus_lagged')
var_exogene = c('nb_articles_cumule_lagged', 
                'nb_citations_cumule_lagged_carre',
                'nb_citations_cumule_lagged',
                'anciennete',
                'prestige_journal_somme_cumule_lagged')
var_controle = c()
var_dependante = 'nb_contribs_coronavirus'
var_to_log = c('nb_contribs_coronavirus', 
               'nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged', 
               'nb_citations_cumule_coronavirus_lagged', 'nb_citations_cumule_lagged',
               'nb_articles_cumule_coronavirus_lagged', 'nb_articles_cumule_lagged',
               'prestige_journal_somme_cumule_coronavirus_lagged', 'prestige_journal_somme_cumule_lagged')

results5 = coeff_retriever(dep = var_dependante, 
                          endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                          vtlog = var_to_log,
                          data = data_auteurs)

#### Prédiction du nombre de citations 6 #### 

data_auteurs = base

var_endogene = c('nb_articles_cumule_coronavirus_lagged', 
                 'nb_citations_cumule_coronavirus_lagged',
                 'nb_citations_cumule_coronavirus_lagged_carre',
                 'anciennete_coronavirus',
                 'prestige_journal_somme_cumule_coronavirus_lagged')
var_exogene = c('nb_articles_cumule_lagged', 
                'nb_citations_cumule_lagged_carre',
                'nb_citations_cumule_lagged',
                'anciennete',
                'prestige_journal_somme_cumule_lagged')
var_controle = c()
var_dependante = 'nb_citations_coronavirus'
var_to_log = c('nb_citations_coronavirus', 
               'nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged', 
               'nb_citations_cumule_coronavirus_lagged', 'nb_citations_cumule_lagged',
               'nb_articles_cumule_coronavirus_lagged', 'nb_articles_cumule_lagged',
               'prestige_journal_somme_cumule_coronavirus_lagged', 'prestige_journal_somme_cumule_lagged')

results6 = coeff_retriever(dep = var_dependante, 
                          endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                          vtlog = var_to_log,
                          data = data_auteurs)

#### Modèle sans logarithmes 7 #### 

data_auteurs = base

var_endogene = c('nb_articles_cumule_coronavirus_lagged', 
                 'nb_citations_cumule_coronavirus_lagged',
                 'nb_citations_cumule_coronavirus_lagged_carre',
                 'anciennete_coronavirus',
                 'prestige_journal_somme_cumule_coronavirus_lagged')
var_exogene = c('nb_articles_cumule_lagged', 
                'nb_citations_cumule_lagged_carre',
                'nb_citations_cumule_lagged',
                'anciennete',
                'prestige_journal_somme_cumule_lagged')
var_controle = c()
var_dependante = 'sommelog_citations_coronavirus'
var_to_log = c()

results7 = coeff_retriever(dep = var_dependante, 
                          endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                          vtlog = var_to_log,
                          data = data_auteurs)

#### Modèle sans les outsiders (ancienneté > 0 ans) 8 #### 

data_auteurs = base

data_auteurs = data_auteurs[data_auteurs$annee > data_auteurs$premiere_annee_contribution_coronavirus,]

var_endogene = c('nb_articles_cumule_coronavirus_lagged', 
                 'nb_citations_cumule_coronavirus_lagged',
                 'nb_citations_cumule_coronavirus_lagged_carre',
                 'anciennete_coronavirus',
                 'prestige_journal_somme_cumule_coronavirus_lagged')
var_exogene = c('nb_articles_cumule_lagged', 
                'nb_citations_cumule_lagged_carre',
                'nb_citations_cumule_lagged',
                'anciennete',
                'prestige_journal_somme_cumule_lagged')
var_controle = c()
var_dependante = 'sommelog_citations_coronavirus'
var_to_log = c('sommelog_citations_coronavirus', 
               'nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged', 
               'nb_citations_cumule_coronavirus_lagged', 'nb_citations_cumule_lagged',
               'nb_articles_cumule_coronavirus_lagged', 'nb_articles_cumule_lagged',
               'prestige_journal_somme_cumule_coronavirus_lagged', 'prestige_journal_somme_cumule_lagged')

results8 = coeff_retriever(dep = var_dependante, 
                          endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                          vtlog = var_to_log,
                          data = data_auteurs)

#### Modèle sans les insiders (ancienneté < 5 ans) 9 #### 

data_auteurs = base

data_auteurs = data_auteurs[data_auteurs$annee < data_auteurs$premiere_annee_contribution_coronavirus + 5,]

var_endogene = c('nb_articles_cumule_coronavirus_lagged', 
                 'nb_citations_cumule_coronavirus_lagged',
                 'nb_citations_cumule_coronavirus_lagged_carre',
                 'anciennete_coronavirus',
                 'prestige_journal_somme_cumule_coronavirus_lagged')
var_exogene = c('nb_articles_cumule_lagged', 
                'nb_citations_cumule_lagged_carre',
                'nb_citations_cumule_lagged',
                'anciennete',
                'prestige_journal_somme_cumule_lagged')
var_controle = c()
var_dependante = 'sommelog_citations_coronavirus'
var_to_log = c('sommelog_citations_coronavirus', 
               'nb_coauteurs_cumule_coronavirus_lagged', 'nb_coauteurs_cumule_lagged', 
               'nb_citations_cumule_coronavirus_lagged', 'nb_citations_cumule_lagged',
               'nb_articles_cumule_coronavirus_lagged', 'nb_articles_cumule_lagged',
               'prestige_journal_somme_cumule_coronavirus_lagged', 'prestige_journal_somme_cumule_lagged')

results9 = coeff_retriever(dep = var_dependante, 
                          endo = var_endogene, exo = var_exogene, ctrl = var_controle, 
                          vtlog = var_to_log,
                          data = data_auteurs)


#### Spécification du résultat à observer ####
results = results4

####  Représentation graphique des sorties #### 

for (col in c('annee', 'coeff', 'coeff_min90', 'coeff_max90')) {
  results[,col] = as.numeric(results[,col])
}

plot_data = results
plot_data$corona = '0'
plot_data$corona[grepl('_coronavirus', plot_data$var)] = '1'
plot_data$var = gsub('_coronavirus', '', plot_data$var)

relabelling <- c(
  `nb_articles_cumule_lagged` = "Nombre d'articles publiés",
  `nb_citations_cumule_lagged` = "Nombre de citations total",
  `nb_coauteurs_cumule_lagged` = "Nombre de co-auteurs",
  `anciennete` = "Ancienneté",
  `prestige_journal_somme_cumule_lagged` = "Prestige de publication",
  `nb_citations_cumule_lagged_carre` = "Nombre de citations total (carré)",
  `nb_coauteurs_cumule_lagged_carre` = "Nombre de co-auteurs (carré)"
)

plot <- ggplot(plot_data) + 
  geom_point(aes(y = coeff, x = annee, color = corona)) +
  geom_ribbon(aes(y = coeff, x = annee,
                  ymin = coeff_min90, ymax = coeff_max90, fill = corona), alpha = 0.1) +
  geom_line(aes(y = coeff, x = annee, color = corona)) + 
  # geom_errorbar(aes(y = coeff, x = annee, color = corona,
  #                  ymin = coeff_min90, ymax = coeff_max90)) +
  labs(title="",
       x = "Année", 
       y = "Impact de la variable (% de SD)") +
  theme_hc() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  coord_flip() +
  theme(legend.position = "none") +
  scale_color_manual(values = c('darkblue', 'red')) +
  scale_fill_manual(values = c('darkblue', 'red')) + 
  facet_wrap(~ var, 
             nrow = length(unique(var)), 
             scales = "free_x",
             labeller = labeller(var = relabelling))
# Affichage
plot









###### Comparaison des coefficients #######
merge1 = plot_data[plot_data$corona == 1, c('coeff', 'var', 'annee')]
colnames(merge1) = c('coeff_endo', 'var', 'annee')
merge2 = plot_data[plot_data$corona == 0, c('coeff', 'var', 'annee')]
colnames(merge2) = c('coeff_exo', 'var', 'annee')
plot_data2 = merge(merge1, merge2, by = c('var', 'annee'))
plot <- ggplot(plot_data2, aes(x = coeff_endo, y = coeff_exo, color = var)) +
  geom_point() +
  geom_smooth(aes(), method = "lm") +
  theme_hc()
plot





###############






=======


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
  for (annee in 1990:2021) {
    
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




>>>>>>> Stashed changes

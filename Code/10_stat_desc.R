###########################################################
###                       STAT DESC                     ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

library(ggplot2)
library(dplyr)
library(ggthemes)
library(data.table)
library(tidyr)

setwd(path_data)
data_auteurs = read.csv("df_auteurs_variables2.csv")


data_auteurs = data_auteurs[(data_auteurs$premiere_annee_contribution_coronavirus <= data_auteurs$annee) & (data_auteurs$derniere_annee_contribution_coronavirus >= data_auteurs$annee),]
data_auteurs$anciennete = data_auteurs$annee - data_auteurs$premiere_annee_contribution
data_auteurs$anciennete_coronavirus = data_auteurs$annee  - data_auteurs$premiere_annee_contribution_coronavirus
data_auteurs$anciennete[data_auteurs$anciennete < 0] = 0
data_auteurs$anciennete_coronavirus[data_auteurs$anciennete_coronavirus < 0] = 0
data_auteurs$anciennete_carre = data_auteurs$anciennete**2
data_auteurs$anciennete_coronavirus_carre = data_auteurs$anciennete_coronavirus**2
data_auteurs$nb_citations_cumule_lagged_carre = data_auteurs$nb_citations_cumule_lagged**2
data_auteurs$nb_citations_cumule_coronavirus_lagged_carre = data_auteurs$nb_citations_cumule_coronavirus_lagged**2
data_auteurs$nb_coauteurs_cumule_coronavirus_lagged_carre = data_auteurs$nb_coauteurs_cumule_coronavirus_lagged**2
data_auteurs$nb_coauteurs_cumule_lagged_carre = data_auteurs$nb_coauteurs_cumule_lagged**2


###### Tendances globales ######

# Nombre d'auteurs et primo-auteurs par ann?e

data_plot <- data_auteurs %>% count(premiere_annee_contribution_coronavirus,author_id)
data_plot$n <- NULL
data_plot <- data_plot %>% count(premiere_annee_contribution_coronavirus)
setnames(data_plot, names(data_plot), c("annee","nb_primo"))
data_plot2 <- data_auteurs[data_auteurs$nb_contribs_coronavirus > 0,] %>% count(annee)
setnames(data_plot2, names(data_plot2), c("annee","nb_aut"))
data_plot <- data_plot %>% full_join(data_plot2)
data_plot$ratio_primo <- data_plot$nb_primo / data_plot$nb_aut

ggplot(data = data_plot[data_plot$annee < 2022,], aes(x=annee)) +
  geom_point(aes(y=nb_aut), alpha = 0.4, size = 2) +
  geom_line(aes(y=nb_aut), size = 0.8) +
  geom_point(aes(y=nb_primo), alpha = 0.4, size = 2, color = "darkblue") +
  geom_line(aes(y=nb_primo), size = 0.8, color = "darkblue") +
  geom_point(aes(y=log(nb_aut)*500), alpha = 0.4, size = 1.5) +
  geom_line(aes(y=log(nb_aut)*500), linetype = "dashed", size = 0.8) +
  geom_point(aes(y=log(nb_primo)*500), colour = "darkblue", alpha = 0.4, size = 1.5) +
  geom_line(aes(y=log(nb_primo)*500), linetype = "dashed", colour = "darkblue", size = 0.8) +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  scale_y_continuous(name = "Nombre d'auteurs", sec.axis = sec_axis(~(./500), name="?chelle logarithmique")) +
  xlab("") +
  theme_hc()


# Ratio de primo-auteurs par ann?e (corona)

ggplot(data = data_plot[data_plot$annee < 2022,], aes(x=annee)) +
  geom_point(aes(y=ratio_primo), alpha = 0.4, size = 2) +
  geom_line(aes(y=ratio_primo), size = 0.8) +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  xlab("") + ylab("Ratio de primo-auteurs") +
  theme_hc()


# Nombre moyen de contributions par auteur par ann?e

df <- data_auteurs[,c("nb_contribs_coronavirus","nb_contribs","annee")]
data_plot <- data.frame(annee = c(1990:2022), moy_cor = rep(NA,33), moy_non_cor = rep(NA,33), moy_tot = rep(NA,33))
for (i in 1990:2022) {
  df_an <- df[df$annee == i,]
  data_plot[data_plot$annee == i, "moy_cor"] <- mean(df_an$nb_contribs_coronavirus)
  data_plot[data_plot$annee == i, "moy_non_cor"] <- mean(df_an$nb_contribs)
  data_plot[data_plot$annee == i, "moy_tot"] <- data_plot[data_plot$annee == i, "moy_cor"] + data_plot[data_plot$annee == i, "moy_non_cor"]
}
ggplot(data = data_plot[data_plot$annee < 2022,], aes(x=annee)) +
  geom_point(aes(y=moy_cor), alpha = 0.4, size = 2, color ="red") +
  geom_line(aes(y=moy_cor), size = 0.8, color ="red") +
  geom_point(aes(y=moy_non_cor), alpha = 0.4, size = 2, color ="darkblue") +
  geom_line(aes(y=moy_non_cor), size = 0.8, color = "darkblue") +
  geom_point(aes(y=moy_tot), alpha = 0.4, size = 2) +
  geom_line(aes(y=moy_tot), size = 0.8) +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  xlab("") + ylab("Nombre moyen de contributions par auteur") +
  theme_hc()


# Nombre moyen de citations en 2022 par auteur par ann?e

df <- data_auteurs[,c("nb_citations_coronavirus","nb_citations","annee")]
data_plot <- data.frame(annee = c(1990:2022), moy_cor = rep(NA,33), moy_non_cor = rep(NA,33), moy_tot = rep(NA,33))
for (i in 1990:2022) {
  df_an <- df[df$annee == i,]
  data_plot[data_plot$annee == i, "moy_cor"] <- mean(df_an$nb_citations_coronavirus)
  data_plot[data_plot$annee == i, "moy_non_cor"] <- mean(df_an$nb_citations)
  data_plot[data_plot$annee == i, "moy_tot"] <- data_plot[data_plot$annee == i, "moy_cor"] + data_plot[data_plot$annee == i, "moy_non_cor"]
}
ggplot(data = data_plot[data_plot$annee < 2022,], aes(x=annee)) +
  geom_point(aes(y=moy_cor), alpha = 0.4, size = 2, color ="red") +
  geom_line(aes(y=moy_cor), size = 0.8, color ="red") +
  geom_point(aes(y=moy_non_cor), alpha = 0.4, size = 2, color ="darkblue") +
  geom_line(aes(y=moy_non_cor), size = 0.8, color = "darkblue") +
  geom_point(aes(y=moy_tot), alpha = 0.4, size = 2) +
  geom_line(aes(y=moy_tot), size = 0.8) +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  xlab("") + ylab("Nombre moyen de citations par auteur") +
  theme_hc()


# Nombre de co-auteurs par auteur par année

df <- data_auteurs[data_auteurs$nb_coauteurs < 50 & data_auteurs$nb_coauteurs_coronavirus < 50,c("nb_coauteurs_coronavirus","nb_coauteurs","annee")]
#df <- data_auteurs[,c("nb_coauteurs_coronavirus","nb_coauteurs","annee")]
data_plot <- data.frame(annee = c(1990:2022), moy_cor = rep(NA,33), moy_non_cor = rep(NA,33), moy_tot = rep(NA,33))
for (i in 1990:2022) {
  df_an <- df[df$annee == i,]
  data_plot[data_plot$annee == i, "moy_cor"] <- mean(df_an$nb_coauteurs_coronavirus)
  data_plot[data_plot$annee == i, "moy_non_cor"] <- mean(df_an$nb_coauteurs)
  data_plot[data_plot$annee == i, "moy_tot"] <- data_plot[data_plot$annee == i, "moy_cor"] + data_plot[data_plot$annee == i, "moy_non_cor"]
}
ggplot(data = data_plot[data_plot$annee < 2022,], aes(x=annee)) +
  geom_point(aes(y=moy_cor), alpha = 0.4, size = 2, color ="red") +
  geom_line(aes(y=moy_cor), size = 0.8, color ="red") +
  geom_point(aes(y=moy_non_cor), alpha = 0.4, size = 2, color ="darkblue") +
  geom_line(aes(y=moy_non_cor), size = 0.8, color = "darkblue") +
  geom_point(aes(y=moy_tot), alpha = 0.4, size = 2) +
  geom_line(aes(y=moy_tot), size = 0.8) +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  xlab("") + ylab("Nombre moyen de co-auteurs") +
  theme_hc()


# Nuage de points citations corona et citations hors corona

data_plot <- data_auteurs[, c("author_id","nb_citations_cumule", "nb_citations_cumule_coronavirus","annee",
                              "derniere_annee_contribution", "nb_articles_cumule_coronavirus","nb_articles_cumule")]
data_plot <- data_plot[data_plot$annee == data_plot$derniere_annee_contribution,]
data_plot$nb_articles_cumule_tot <- data_plot$nb_articles_cumule + data_plot$nb_articles_cumule_coronavirus
# data_plot[is.na(data_plot$nb_citations_cumule) == TRUE, "nb_citations_cumule"] <- 0
# data_plot[is.na(data_plot$nb_citations_cumule_coronavirus) == TRUE, "nb_citations_cumule_coronavirus"] <- 0
data_plot$nb_citations_cumule <- data_plot$nb_citations_cumule + 1
data_plot$nb_citations_cumule_coronavirus <- data_plot$nb_citations_cumule_coronavirus + 1

ggplot(data = data_plot, aes(x=log(nb_citations_cumule))) +
  geom_point(aes(y=log(nb_citations_cumule_coronavirus), colour = derniere_annee_contribution,
                 size = log(nb_articles_cumule_tot)), alpha = 0.2) +
  geom_smooth(aes(y=log(nb_citations_cumule_coronavirus)), method = "lm", color = "black", fill = "black", alpha = 0.15) +
  xlab("Citations cumulées hors coronavirus (log)") + ylab("Citations cumul?es coronavirus (log)") +
  labs(color = "Dernière année de contribution", size = "Nombre d'articles cumulés (log)") +
  scale_color_viridis_c(option = "magma") +
  theme_hc()




data_plot <- data_auteurs[, c("author_id","nb_citations_cumule", "nb_citations_cumule_coronavirus","annee",
                              "derniere_annee_contribution", "nb_articles_cumule_coronavirus","nb_articles_cumule", "nb_articles_cumule_coronavirus",
                              "nb_coauteurs_cumule","nb_coauteurs_cumule_coronavirus","nb_coauteurs_cumule_toutchamp")]
data_plot <- data_plot[data_plot$annee == data_plot$derniere_annee_contribution,]

data_plot$nb_articles_cumule_tot <- data_plot$nb_articles_cumule + data_plot$nb_articles_cumule_coronavirus

data_plot$nb_citations_cumule <- data_plot$nb_citations_cumule + 1
data_plot$nb_citations_cumule_coronavirus <- data_plot$nb_citations_cumule_coronavirus + 1
data_plot$nb_coauteurs_cumule <- data_plot$nb_coauteurs_cumule + 1
data_plot$nb_coauteurs_cumule_coronavirus <- data_plot$nb_coauteurs_cumule_coronavirus + 1
ggplot(data = data_plot) +
  geom_point(aes(x=log(nb_coauteurs_cumule),y=log(nb_citations_cumule),
                 size = log(nb_articles_cumule)), color = "black", fill = "darkblue", alpha = 0.1, shape = 21) +
  geom_point(aes(x=log(nb_coauteurs_cumule_coronavirus),y=log(nb_citations_cumule_coronavirus)
                 , size = log(nb_articles_cumule_coronavirus)), color = "black", fill = "red", alpha = 0.15, shape = 21) +
  geom_smooth(aes(x=log(nb_coauteurs_cumule_coronavirus),y=log(nb_citations_cumule_coronavirus)),
              method = "lm", color = "red", fill = "red") +
  geom_smooth(aes(x=log(nb_coauteurs_cumule),y=log(nb_citations_cumule)),
              method = "lm", color = "darkblue", fill = "darkblue") +
  xlab("Nombre de coauteurs (log)") + ylab("Citations cumulés (log)") +
  labs(size = "Nombre d'articles cumulés (log)") +
  theme_hc()


###### Relations entre variables ######

setwd(path_data)
data_auteurs = read.csv("df_auteurs_variables2.csv")

data_auteurs$anciennete = data_auteurs$annee - data_auteurs$premiere_annee_contribution
data_auteurs$anciennete_coronavirus = data_auteurs$annee  - data_auteurs$premiere_annee_contribution_coronavirus
data_auteurs$anciennete[data_auteurs$anciennete < 0] = 0
data_auteurs$anciennete_coronavirus[data_auteurs$anciennete_coronavirus < 0] = 0

# Correlation matrix
cor_data = data_auteurs[,c('nb_contribs_coronavirus',
                            'sommelog_citations_coronavirus',
                            'nb_citations_coronavirus',
                            'nb_articles_cumule_coronavirus_lagged',
                            'nb_citations_cumule_coronavirus_lagged',
                            'sommelog_citations_cumule_coronavirus_lagged',
                            'nb_coauteurs_cumule_coronavirus_lagged',
                            'anciennete_coronavirus',
                           'prestige_journal_somme_cumule_coronavirus_lagged',
                           'annee')]
cor_data = cor_data %>% drop_na()
cor_data = cor_data[cor_data$annee ==2010,]
cor_data$annee = NULL
cor_data$nb_citations_coronavirus = log(cor_data$nb_citations_coronavirus + 1)
cor_data = as.matrix(cor_data)
corrplot(cor(cor_data), method="color", order="FPC")

# Scatter plots

plot(data_auteurs$sommelog_citations_coronavirus,
    log(data_auteurs$nb_articles_cumule_coronavirus_lagged + 1))

plot(log(data_auteurs$sommelog_citations_coronavirus + 1),
     log(data_auteurs$nb_citations_cumule_coronavirus_lagged + 1))





















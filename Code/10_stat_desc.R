###########################################################
###                       STAT DESC                     ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

library(ggplot2)
library(dplyr)
library(ggthemes)
library(data.table)

setwd(path_data)
data_auteurs = read.csv("df_auteurs_variables2.csv")

# Nombre de primo-auteurs par année (corona)

data_plot <- data_auteurs %>% count(premiere_annee_contribution_coronavirus,author_id)
data_plot$n <- NULL
data_plot <- data_plot %>% count(premiere_annee_contribution_coronavirus)
ggplot(data = data_plot, aes(x=premiere_annee_contribution_coronavirus)) +
  geom_point(aes(y=n), alpha = 0.4, size = 2) +
  geom_line(aes(y=n), size = 0.8) +
  geom_point(aes(y=log(n)*400), colour = "darkblue", alpha = 0.4, size = 2) +
  geom_line(aes(y=log(n)*400), colour = "darkblue", size = 0.8) +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  scale_y_continuous(name = "Nombre de primo-auteurs", sec.axis = sec_axis(~(./400), name="échelle logarithmique")) +
  xlab("") +
  theme_hc()

# Ratio de primo-auteurs par année (corona)

data_plot <- data_auteurs %>% count(premiere_annee_contribution_coronavirus,author_id)
data_plot$n <- NULL
data_plot <- data_plot %>% count(premiere_annee_contribution_coronavirus)
setnames(data_plot, names(data_plot), c("annee","nb_primo"))
data_plot2 <- data_auteurs[data_auteurs$nb_contribs_coronavirus > 0,] %>% count(annee)
setnames(data_plot2, names(data_plot2), c("annee","nb_aut"))
data_plot <- data_plot %>% full_join(data_plot2)
data_plot$ratio_primo <- data_plot$nb_primo / data_plot$nb_aut

ggplot(data = data_plot, aes(x=annee)) +
  geom_point(aes(y=ratio_primo), alpha = 0.4, size = 2) +
  geom_line(aes(y=ratio_primo), size = 0.8) +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  xlab("") + ylab("Ratio de primo-auteurs") +
  theme_hc()



# df <- data_auteurs[data_auteurs$nb_contribs_coronavirus > 0,c("nb_articles_cumule_coronavirus","annee")]
# data_plot <- data.frame(annee = c(1990:2022), q0 =rep(NA, 33), q25 = rep(NA, 33), q50 = rep(NA,33), q75 = rep(NA,33),
#                         q100 = rep(NA,33), moy = rep(NA,33))
# for (i in 1990:2022) {
#   df_an <- df[df$annee == i,]
#   for (j in c(1:5)) {
#     data_plot[data_plot$annee == i, j+1] <- quantile(df_an$nb_articles_cumule_coronavirus, probs = seq(0, 1, 0.25))[j]
#   }
#   data_plot[data_plot$annee == i, "moy"] <- mean(df_an$nb_articles_cumule_coronavirus)
# }
# 
# ggplot(data = data_plot, aes(x=annee)) +
#   geom_line(aes(y=q25), size = 0.8) + geom_line(aes(y=q50), size = 0.8) +
#   geom_line(aes(y=q75), size = 0.8) + geom_line(aes(y=moy), size = 0.8, color = "darkblue") +
#   geom_vline(xintercept = 2003, linetype = "dashed", color = "red") +
#   geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
#   geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
#   xlab("") + ylab("") +
#   theme_hc()


# Nombre moyen de contributions par auteur par année

df <- data_auteurs[,c("nb_contribs_coronavirus","nb_contribs","annee")]
data_plot <- data.frame(annee = c(1990:2022), moy_cor = rep(NA,33), moy_non_cor = rep(NA,33), moy_tot = rep(NA,33))
for (i in 1990:2022) {
  df_an <- df[df$annee == i,]
  data_plot[data_plot$annee == i, "moy_cor"] <- mean(df_an$nb_contribs_coronavirus)
  data_plot[data_plot$annee == i, "moy_non_cor"] <- mean(df_an$nb_contribs)
  data_plot[data_plot$annee == i, "moy_tot"] <- data_plot[data_plot$annee == i, "moy_cor"] + data_plot[data_plot$annee == i, "moy_non_cor"]
}
ggplot(data = data_plot, aes(x=annee)) +
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



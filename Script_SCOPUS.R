library(stringr)

############ Traitement données ##########

# Lecture csv

#path_Mathis = 
path_Maxime = 'C:\\Users\\Dell\\Desktop\\Projet DSSS\\Bases'
path = path_Maxime

setwd(path)
base_journaux = read.csv("SCOPUS_Base_selection_journaux.csv", encoding = 'UTF-8')
base_pre_2019_1 = read.csv("scopus_base_2009.csv", encoding = 'UTF-8')
base_pre_2019_2 = read.csv("scopus_base_2019.csv", encoding = 'UTF-8')
base_pre_2019 = rbind(base_pre_2019_1, base_pre_2019_2)
base_pre_2019$Cited.by[is.na(base_pre_2019$Cited.by)] = 0
base_pre_2019$Author.s..ID = gsub("\\[|\\]", "", base_pre_2019$Author.s..ID)

# Constitution de la base des insiders

colnames(base_pre_2019)
insiders_list = paste(sapply(base_pre_2019$Author.s..ID, paste, collapse=";"), collapse=";")
insiders_list = strsplit(insiders_list, split = ";")
insiders_list = str_trim(insiders_list[[1]])
insiders_list = unique(insiders_list)

base_insiders = as.data.frame(insiders_list)
colnames(base_insiders) = 'Id'
base_insiders$nb_articles = 0
base_insiders$citation_max = 0
base_insiders$citation_moy = 0
base_insiders$citation_sum = 0
base_insiders$first_article_date = 0
base_insiders$coauteurs = 0
count = 0
base_insiders = as.matrix(base_insiders)
base_pre_2019 = as.matrix(base_pre_2019)
for (i in 1:length(base_insiders[,'Id'])) {
  insider = base_insiders[i,'Id']
  count = count + 1
  print(count)
  subset = base_pre_2019[grepl(insider, base_pre_2019[,"Author.s..ID"]),, drop = F]
  nb_articles = length(subset[,"Title"])
  cited.by = as.numeric(subset[,"Cited.by"])
  citation_max = max(cited.by)
  citation_moy = mean(cited.by)
  citation_sum = sum(cited.by)
  year = subset[,"Year"]
  first_article_date = min(year)
  last_article_date = max(year)
  coauteurs = paste(sapply(subset[,"Author.s..ID"], paste, collapse=";"), collapse=";")
  coauteurs = strsplit(coauteurs, split = ";")
  coauteurs = str_trim(coauteurs[[1]])
  coauteurs = unique(coauteurs)
  coauteurs = paste(coauteurs, collapse = ";")
  base_insiders[i, c("nb_articles", "citation_max", "citation_moy", "citation_sum", 
                     "first_article_date", "coauteurs")] = c(nb_articles, citation_max, citation_moy, 
                                                              citation_sum, first_article_date, coauteurs)
}
base_insiders = as.data.frame(base_insiders)
base_pre_2019 = as.data.frame(base_pre_2019)

# Sauvegarde temporaire

#write.csv(base_insiders, 'base_insiders_v1.csv', row.names = FALSE)
base_insiders = read.csv("base_insiders_v1.csv", encoding = 'UTF-8')

# Boucle pour le second ordre
base_insiders$nb_articles_coauteurs = 0
base_insiders$citations_sum_coauteurs = 0
base_insiders$citations_moy_coauteurs = 0
base_insiders$citations_max_coauteurs = 0
base_insiders$first_article_date_coauteurs_moy = 0
base_insiders$first_article_date_coauteurs_min = 0
count = 0
base_insiders = as.matrix(base_insiders)
for (i in 1:length(base_insiders[,"Id"])) {
  insider = base_insiders[i,"Id"]
  count = count + 1
  print(count)
  coauteurs = base_insiders[i,"coauteurs"]
  coauteurs = strsplit(coauteurs, split = ";")[[1]]
  subset = base_insiders[insider %in% coauteurs,,drop = F]
  nb_articles_coauteurs = length(subset[,"Id"])
  citations_sum_coauteurs = sum(as.numeric(subset[,"citation_sum"]))
  citations_moy_coauteurs = mean(as.numeric(subset[,"citation_moy"]))
  citations_max_coauteurs = max(as.numeric(subset[,"citation_max"]))
  first_article_date_coauteurs_moy = mean(as.numeric(subset[,"first_article_date"]))
  first_article_date_coauteurs_min = min(as.numeric(subset[,"first_article_date"]))
  base_insiders[i,"nb_articles_coauteurs"] = nb_articles_coauteurs
  base_insiders[i,"citations_sum_coauteurs"] = citations_sum_coauteurs
  base_insiders[i,"citations_moy_coauteurs"] = citations_moy_coauteurs
  base_insiders[i,"citations_max_coauteurs"] = citations_max_coauteurs
  base_insiders[i,"first_article_date_coauteurs_moy"] = first_article_date_coauteurs_moy
  base_insiders[i,"first_article_date_coauteurs_min"] = first_article_date_coauteurs_min
}
base_insiders = as.data.frame(base_insiders)

# Sauvegarde
#write.csv(base_insiders, 'base_insiders_v2.csv', row.names = FALSE)

# Constiution de la base outsiders

# A faire sur le meme modele que les insiders



######## Stat des ########

base_insiders = read.csv("base_insiders_v2.csv", encoding = 'UTF-8')

colnames(base_insiders)
for (colname in colnames(base_insiders)) {
  if (colname != 'coauteurs') {
    base_insiders[,colname] = as.numeric(base_insiders[,colname])
  }
}

base_insiders = base_insiders[!grepl('No author', base_insiders$Id),]
base_insiders = base_insiders[!is.na(base_insiders$Id),]

# On enlève les auteurs peu représentés
summary(base_insiders$nb_articles)
base_insiders = base_insiders[(!is.na(base_insiders$nb_articles)) & (base_insiders$nb_articles > 5),]
summary(base_insiders$nb_articles)

library(ggplot2)

ggplot(base_insiders, aes(x=log(nb_articles))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

ggplot(base_insiders, aes(x=log(nb_articles), y=log(citation_sum))) +
  geom_point()

ggplot(base_insiders, aes(x=first_article_date, y=log(nb_articles))) +
  geom_point()

ggplot(base_insiders, aes(x=log(nb_articles), y=log(nb_articles_coauteurs))) +
  geom_point()






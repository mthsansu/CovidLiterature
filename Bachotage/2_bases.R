###########################################################
###                      BASES SRIPT                    ###
### This code creates our databases.                    ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

# Lecture des bases initiales Scopus (format csv)

base_journaux = read.csv(paste0(path_data,"\\scopus_base_selection_journaux.csv"), encoding = 'UTF-8')
base_pre_2019_1 = read.csv(paste0(path_data,"\\scopus_base_2009.csv"), encoding = 'UTF-8')
base_pre_2019_2 = read.csv(paste0(path_data,"\\scopus_base_2019.csv"), encoding = 'UTF-8')
# Concaténation pour obtenir une seule base
base_pre_2019 = rbind(base_pre_2019_1, base_pre_2019_2)
base_pre_2019$Cited.by[is.na(base_pre_2019$Cited.by)] = 0
base_pre_2019$Author.s..ID = gsub("\\[|\\]", "", base_pre_2019$Author.s..ID)

# Constitution de la base des insiders

# Récupération de tous les identifiants uniques des auteurs de la base
colnames(base_pre_2019)
insiders_list = paste(sapply(base_pre_2019$Author.s..ID, paste, collapse=";"), collapse=";")
insiders_list = strsplit(insiders_list, split = ";")
insiders_list = str_trim(insiders_list[[1]])
insiders_list = unique(insiders_list)

# Création d'une base avec différentes variables
# Une ligne par auteur
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

write.csv(base_insiders, paste0(path_data,'\\base_insiders_v1.csv'), row.names = FALSE)
#base_insiders = read.csv(paste0(path_data,'\\base_insiders_v1.csv'), encoding = 'UTF-8')

# Augmentations de la base avec des variables concernant les coauteurs
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
write.csv(base_insiders, paste0(path_data,'\\base_insiders_v2.csv'), row.names = FALSE)

# Constiution de la base outsiders

# A faire sur le meme modele que les insiders






###########################################################
###                    STAT DES SRIPT                   ###
### This code does the first descriptive analyses.      ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

######## Stat desc base insiders

base_insiders = read.csv(paste0(path_data,"\\base_insiders_v2.csv"), encoding = 'UTF-8')

# Suppression quand pas d'auteur
base_insiders = base_insiders[!grepl('No author', base_insiders$Id),]
base_insiders = base_insiders[!is.na(base_insiders$Id),]
base_insiders = base_insiders[str_trim(base_insiders$Id)!= "",]


# Conversion base en numérique
colnames(base_insiders)
for (colname in colnames(base_insiders)) {
  if (colname != 'coauteurs') {
    base_insiders[,colname] = as.numeric(base_insiders[,colname])
  }
}

base_insiders$nb_coauteurs <- strsplit(as.character(base_insiders$coauteurs),split=";")
base_insiders$nb_coauteurs <- lengths(base_insiders$nb_coauteurs)

# On enlève les auteurs peu représentés
summary(base_insiders$nb_articles)
base_insiders = base_insiders[(!is.na(base_insiders$nb_articles)) & (base_insiders$nb_articles > 5),]
summary(base_insiders$nb_articles)


########## GRAPHS

# Histogramme et densité pour le nombre d'articles
ggplot(base_insiders, aes(x=nb_articles)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Nombre d'articles") + ylab("Density") +
  theme_hc()

# Histogramme et densité pour le nombre d'articles (log)
ggplot(base_insiders, aes(x=log(nb_articles))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Nombre d'articles (échelle logistique)") + ylab("Density") +
  theme_hc()

# Nombre de citations totales selon le nombre d'articles
ggplot(base_insiders, aes(x=nb_articles, y=citation_sum)) +
  geom_point() +
  xlab("Nombre d'articles") + ylab("Nombre de citations") +
  theme_hc()

# Nombre de citations totales (log) selon le nombre d'articles (log)
ggplot(base_insiders, aes(x=log(nb_articles), y=log(citation_sum))) +
  geom_point() +
  xlab("Nombre d'articles (échelle logistique)") + ylab("Nombre de citations (échelle logistique)") +
  theme_hc()

# Nombre d'articles selon la date du premier article
ggplot(base_insiders, aes(x=first_article_date, y=nb_articles)) +
  geom_point() +
  xlab("Date du premier article") + ylab("Nombre d'articles") +
  theme_hc() 

# Nombre d'articles (log) selon la date du premier article
ggplot(base_insiders, aes(x=first_article_date, y=log(nb_articles))) +
  geom_point() +
  xlab("Date du premier article") + ylab("Nombre d'articles (échelle logistique)") +
  theme_hc()  

# Nombre d'articles des coauteurs selon le nombre d'articles
ggplot(base_insiders, aes(x=nb_articles, y=nb_articles_coauteurs)) +
  geom_point() +
  xlab("Nombre d'articles") + ylab("Nombre d'articles des coauteurs") +
  theme_hc()  

# Nombre d'articles (log) des coauteurs selon le nombre d'articles (log)
ggplot(base_insiders, aes(x=log(nb_articles), y=log(nb_articles_coauteurs))) +
  geom_point() +
  xlab("Nombre d'articles (échelle logistique)") + ylab("Nombre d'articles des coauteurs (échelle logistique)") +
  theme_hc()  

# Histogramme et densité pour le nombre de coauteurs
ggplot(base_insiders, aes(x=nb_coauteurs)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Nombre de coauteurs") + ylab("Density") +
  theme_hc()

# Histogramme et densité pour le nombre de coauteurs (log)
ggplot(base_insiders, aes(x=log(nb_coauteurs))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Nombre de coauteurs (log)") + ylab("Density") +
  theme_hc()

# Evoultion du nombre d'entrée des auteurs
A <- base_insiders %>% count(first_article_date)
ggplot(A, aes(x=first_article_date, y=n)) +
  geom_point() +
  geom_line() +
  xlab("Date du premier article") + ylab("Nombre de primo-auteurs") +
  theme_hc()  

# Nombre de citations selon la date du premier article
ggplot(base_insiders, aes(x=first_article_date, y=citation_sum)) +
  geom_point() +
  xlab("Date du premier article") + ylab("Nombre de citations totales") +
  theme_hc()

# Nombre de citations (log) selon la date du premier article
ggplot(base_insiders, aes(x=first_article_date, y=log(citation_sum))) +
  geom_point() +
  xlab("Date du premier article") + ylab("Nombre de citations totales (log)") +
  theme_hc()

# Nombre moyen de citations selon la date du premier article
ggplot(base_insiders, aes(x=first_article_date, y=citation_moy)) +
  geom_point() +
  xlab("Date du premier article") + ylab("Nombre moyen de citations") +
  theme_hc()

# Nombre moyen de citations (log) selon la date du premier article
ggplot(base_insiders, aes(x=first_article_date, y=log(citation_moy))) +
  geom_point() +
  xlab("Date du premier article") + ylab("Nombre de citations totales (log)") +
  theme_hc()

# Nombre moyen de citations par année d'entrée
B <- aggregate(base_insiders$citation_sum, by=list(base_insiders$first_article_date), FUN=sum, na.rm = TRUE)
A <- A %>% full_join(B, by = c("first_article_date" = "Group.1"))
A$cit_moy_an <- A$x / A$n
ggplot(A, aes(x=first_article_date, y=cit_moy_an)) +
  geom_point() +
  geom_line() +
  xlab("Date du premier article") + ylab("Nombre moyen de citations par année d'entrée") +
  theme_hc() 

# Nombre moyen de citations selon le nombre de coauteurs
ggplot(base_insiders, aes(x=nb_coauteurs, y=citation_sum)) +
  geom_point() +
  xlab("Nombre de coauteurs") + ylab("Nombre moyen de citations") +
  theme_hc()

# Nombre moyen de citations (log) selon le nombre de coauteurs
ggplot(base_insiders, aes(x=nb_coauteurs, y=log(citation_moy))) +
  geom_point() +
  xlab("Nombre de coauteurs") + ylab("Nombre moyen de citations (log)") +
  theme_hc()

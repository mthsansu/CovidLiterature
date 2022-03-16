##### TEST MODELE


base_journaux = read.csv(paste0(path_data,"\\scopus_base_selection_journaux.csv"), encoding = 'UTF-8')

base_journaux$id_art <- rownames(base_journaux)

base_jour_aut = base_journaux
base_jour_aut = base_jour_aut[0,]

i = 0
for (article in base_journaux$id_art) {
  i = i+1
  print(i)
  ligne = base_journaux[base_journaux$id_art == article,]
  auteurs = base_journaux[base_journaux$id_art == article,]$Author.s..ID
  auteurs = strsplit(auteurs, split = ';')[[1]]
  for (auteur in auteurs) {
    auteur = str_trim(auteur)
    new_ligne = ligne
    new_ligne$Author.s..ID <- auteur
    base_jour_aut = rbind(base_jour_aut, new_ligne)
  }
}


base_an <- base_jour_aut %>% group_by(Author.s..ID) %>% summarise(Annee_entree = min(Year))
base_insider <- read.csv(paste0(path_data,"\\base_insiders_v2.csv"))
base_an <- base_an %>% left_join(base_insider, by =c("Author.s..ID" = "Id"))

base_jour_aut <- base_jour_aut %>% left_join(base_an, by =c("Author.s..ID" = "Author.s..ID"))


base_jour_aut$insider_2020 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2020, "insider_2020"] <- 1
base_jour_aut$insider_2011 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2013, "insider_2011"] <- 1
base_jour_aut$insider_2001 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2003, "insider_2001"] <- 1


base_jour_aut[is.na(base_jour_aut$Cited.by),"Cited.by"] <- 0

write.csv(base_jour_aut,paste0(path_data,"\\base_jour_aut.csv"))

##### POST-2020


A <- base_jour_aut[base_jour_aut$Year > 2019, ]
B <- aggregate(A$insider_2020, by=list(A$id_art), FUN=mean, na.rm = TRUE)
B <- B %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
B[is.na(B$Cited.by),"Cited.by"] <- 0
C <- aggregate(A$insider_2020, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0


lm1 <- lm(Cited.by ~ x + Year, data = B)
summary(lm1)

lm2 <- lm(Cited.by ~ x + I(Year-2020), data = C)
summary(lm2)

lm3 <- lm(Cited.by ~ x + I(Year-2020) + I(x*(Year-2020)), data = C)
summary(lm3)

lm4 <- lm(I(log(Cited.by + 1)) ~ x + I(Year-2020) + I(x*(Year-2020)), data = subset(C, Cited.by != 0))
summary(lm4)

lm6 <- lm(Cited.by ~ x + I(as.character(Year)) + I(x*(Year-2020)), data = C)
summary(lm6)

##### 2013-2019


A <- base_jour_aut[base_jour_aut$Year > 2012 & base_jour_aut$Year < 2019, ]
B <- aggregate(A$insider_2011, by=list(A$id_art), FUN=mean, na.rm = TRUE)
B <- B %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
B[is.na(B$Cited.by),"Cited.by"] <- 0
C <- aggregate(A$insider_2011, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0

lm1 <- lm(Cited.by ~ x + I(Year-2013), data = B)
summary(lm1)

lm2 <- lm(Cited.by ~ x + I(Year-2013), data = C)
summary(lm2)

lm3 <- lm(Cited.by ~ x + I(Year-2013) + I(x*(Year-2013)), data = C)
summary(lm3)

lm4 <- lm(I(log(Cited.by + 1)) ~ x + I(Year-2013) + I(x*(Year-2013)), data = subset(C, Cited.by != 0))
summary(lm4)

lm6 <- lm(Cited.by ~ x + I(as.character(Year)) + I(x*(Year-2013)), data = C)
summary(lm6)


table(C$x)
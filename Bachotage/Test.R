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
base_jour_aut$insider_2019 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2019, "insider_2019"] <- 1
base_jour_aut$insider_2018 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2018, "insider_2018"] <- 1
base_jour_aut$insider_2017 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2017, "insider_2017"] <- 1
base_jour_aut$insider_2016 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2016, "insider_2016"] <- 1
base_jour_aut$insider_2015 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2015, "insider_2015"] <- 1
base_jour_aut$insider_2014 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2014, "insider_2014"] <- 1
base_jour_aut$insider_2013 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2013, "insider_2013"] <- 1
base_jour_aut$insider_2012 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2012, "insider_2012"] <- 1
base_jour_aut$insider_2011 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2011, "insider_2011"] <- 1
base_jour_aut$insider_2010 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2010, "insider_2010"] <- 1
base_jour_aut$insider_2009 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2009, "insider_2009"] <- 1
base_jour_aut$insider_2008 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2008, "insider_2008"] <- 1
base_jour_aut$insider_2007 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2007, "insider_2007"] <- 1
base_jour_aut$insider_2006 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2006, "insider_2006"] <- 1
base_jour_aut$insider_2005 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2005, "insider_2005"] <- 1
base_jour_aut$insider_2004 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2004, "insider_2004"] <- 1
base_jour_aut$insider_2003 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2003, "insider_2003"] <- 1
base_jour_aut$insider_2002 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2002, "insider_2002"] <- 1
base_jour_aut$insider_2001 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2001, "insider_2001"] <- 1
base_jour_aut$insider_2000 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 2000, "insider_2000"] <- 1
base_jour_aut$insider_1999 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 1999, "insider_1999"] <- 1
base_jour_aut$insider_1998 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 1998, "insider_1998"] <- 1
base_jour_aut$insider_1997 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 1997, "insider_1997"] <- 1
base_jour_aut$insider_1996 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 1996, "insider_1996"] <- 1
base_jour_aut$insider_1995 <- 0
base_jour_aut[base_jour_aut$Annee_entree.y < 1995, "insider_1995"] <- 1

base_jour_aut[is.na(base_jour_aut$Cited.by),"Cited.by"] <- 0

write.csv(base_jour_aut,paste0(path_data,"\\base_jour_aut.csv"))



##### FIRST REGRESSIONS

base_jour_aut <- read.csv(paste0(path_data,"\\base_jour_aut.csv"))
base_journaux = read.csv(paste0(path_data,"\\scopus_base_selection_journaux.csv"), encoding = 'UTF-8')
base_journaux$id_art <- as.integer(rownames(base_journaux))

##### POST-2020

A <- base_jour_aut[base_jour_aut$Year > 2019, ]
B <- aggregate(A$insider_2020, by=list(A$id_art), FUN=mean, na.rm = TRUE)
B <- B %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
B[is.na(B$Cited.by),"Cited.by"] <- 0
C <- aggregate(A$insider_2020, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0



B1 <- lm(Cited.by ~ x + I(Year-2020), data = B)
summary(B1)

B2 <- lm(Cited.by ~ x + I(as.character(Year)) , data = B)
summary(B2)

B3 <- lm(Cited.by ~ x + I(Year-2020) + I(x*(Year-2020)), data = B)
summary(B3)

C1 <- lm(Cited.by ~ x + I(Year-2020), data = C)
summary(C1)

C2 <- lm(Cited.by ~ x + I(as.character(Year)), data = C)
summary(C2)

C3 <- lm(Cited.by ~ x + I(Year-2020) + I(x*(Year-2020)), data = C)
summary(C3)

C4 <- lm(Cited.by ~ x + I(as.character(Year)) + I(x*(Year-2020)), data = C)
summary(C4)

C5 <- lm(I(log(Cited.by + 1)) ~ x + I(Year-2020) + I(x*(Year-2020)), data = C)
summary(C5)


stargazer(C1, C2, C3, C4, C5, title="Regression Models",
          dep.var.labels=c("Number of citations"),
          align=TRUE, no.space=TRUE, out=paste0(path_tab,"\\models_2020"))


##### 2013-2019


A <- base_jour_aut[base_jour_aut$Year > 2012 & base_jour_aut$Year < 2020, ]
B <- aggregate(A$insider_2011, by=list(A$id_art), FUN=mean, na.rm = TRUE)
B <- B %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
B[is.na(B$Cited.by),"Cited.by"] <- 0
C <- aggregate(A$insider_2011, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0



B1 <- lm(Cited.by ~ x + I(Year-2013), data = B)
summary(B1)

B2 <- lm(Cited.by ~ x + I(as.character(Year)) , data = B)
summary(B2)

B3 <- lm(Cited.by ~ x + I(Year-2013) + I(x*(Year-2013)), data = B)
summary(B3)

C1 <- lm(Cited.by ~ x + I(Year-2013), data = C)
summary(C1)

C2 <- lm(Cited.by ~ x + I(as.character(Year)), data = C)
summary(C2)

C3 <- lm(Cited.by ~ x + I(Year-2013) + I(x*(Year-2013)), data = C3)
summary(C3)

C4 <- lm(Cited.by ~ x + I(as.character(Year)) + I(x*(Year-2013)), data = C)
summary(C4)

C5 <- lm(I(log(Cited.by + 1)) ~ x + I(Year-2013) + I(x*(Year-2013)), data = C)
summary(C5)


stargazer(C1, C2, C3, C4, C5, title="Regression Models",
          dep.var.labels=c("Number of citations"),
          align=TRUE, no.space=TRUE, out=paste0(path_tab,"\\models_2013"))


##### 2003-2012


A <- base_jour_aut[base_jour_aut$Year > 2002 & base_jour_aut$Year < 2012, ]
B <- aggregate(A$insider_2001, by=list(A$id_art), FUN=mean, na.rm = TRUE)
B <- B %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
B[is.na(B$Cited.by),"Cited.by"] <- 0
C <- aggregate(A$insider_2001, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0



B1 <- lm(Cited.by ~ x + I(Year-2003), data = B)
summary(B1)

B2 <- lm(Cited.by ~ x + I(as.character(Year)) , data = B)
summary(B2)

B3 <- lm(Cited.by ~ x + I(Year-2003) + I(x*(Year-2003)), data = B)
summary(B3)

C1 <- lm(Cited.by ~ x + I(Year-2003), data = C)
summary(C1)

C2 <- lm(Cited.by ~ x + I(as.character(Year)), data = C)
summary(C2)

C3 <- lm(Cited.by ~ x + I(Year-2003) + I(x*(Year-2003)), data = C3)
summary(C3)

C4 <- lm(Cited.by ~ x + I(as.character(Year)) + I(x*(Year-2003)), data = C)
summary(C4)

C5 <- lm(I(log(Cited.by + 1)) ~ x + I(Year-2003) + I(x*(Year-2003)), data = C)
summary(C5)


stargazer(C1, C2, C3, C4, C5, title="Regression Models",
          dep.var.labels=c("Number of citations"),
          align=TRUE, no.space=TRUE, out=paste0(path_tab,"\\models_2003"))

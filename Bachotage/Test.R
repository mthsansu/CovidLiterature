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


######## MAXIME - APPEND POST REU - REGRESSIONS

library(dplyr)
library(stargazer)

path_data = 'C:\\Users\\Dell\\Desktop\\Projet DSSS\\Bachotage'

# Data building

base_jour_aut <- read.csv(paste0(path_data,"\\base_jour_aut.csv"))
base_journaux <- read.csv(paste0(path_data,"\\scopus_base_selection_journaux.csv"), encoding = 'UTF-8')
base_journaux$id_art <- as.integer(rownames(base_journaux))

base_journaux$Nb_autors = NA
for (id in base_journaux$id_art) {
  base_journaux[base_journaux$id_art == id,]$Nb_autors = length(strsplit(base_journaux[base_journaux$id_art == id,]$Author.s..ID, ';')[[1]])
}


A <- base_jour_aut[base_jour_aut$Year > 2019, ]
C <- aggregate(A$insider_2020, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0
data_P4 <- C

A <- base_jour_aut[base_jour_aut$Year > 2012 & base_jour_aut$Year < 2020, ]
C <- aggregate(A$insider_2013, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0
data_P3 <- C

A <- base_jour_aut[base_jour_aut$Year > 2002 & base_jour_aut$Year < 2013, ]
C <- aggregate(A$insider_2003, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0
data_P2 <- C

A <- base_jour_aut[base_jour_aut$Year > 1995 & base_jour_aut$Year < 2003, ]
C <- aggregate(A$insider_1996, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0
data_1996_2002 <- C

A <- base_jour_aut[base_jour_aut$Year > 2007 & base_jour_aut$Year < 2013, ]
C <- aggregate(A$insider_2008, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0
data_2008_2012 <- C

A <- base_jour_aut[base_jour_aut$Year > 2015 & base_jour_aut$Year < 2020, ]
C <- aggregate(A$insider_2016, by=list(A$id_art), FUN=max, na.rm = TRUE)
C <- C %>% left_join(base_journaux, by =c("Group.1" = "id_art"))
C[is.na(C$Cited.by),"Cited.by"] <- 0
data_2016_2019 <- C

data_P4$Year = data_P4$Year-2020
reg_P4 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P4)
summary(reg_P4)
reg_P4_log <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P4)
summary(reg_P4_log)
reg_P4_x0 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P4[data_P4$Cited.by != 0,])
summary(reg_P4_x0)
reg_P4_log_x0 <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P4[data_P4$Cited.by != 0,])
summary(reg_P4_log_x0)

data_P3$Year = data_P3$Year-2013
reg_P3 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P3)
summary(reg_P3)
reg_P3_log <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P3)
summary(reg_P3_log)
reg_P3_x0 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P3[data_P3$Cited.by != 0,])
summary(reg_P3_x0)
reg_P3_log_x0 <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P3[data_P3$Cited.by != 0,])
summary(reg_P3_log_x0)

data_P2$Year = data_P2$Year-2003
reg_P2 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P3)
summary(reg_P2)
reg_P2_log <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P3)
summary(reg_P2_log)
reg_P2_x0 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P2[data_P2$Cited.by != 0,])
summary(reg_P2_x0)
reg_P2_log_x0 <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_P2[data_P2$Cited.by != 0,])
summary(reg_P2_log_x0)

data_2016_2019$Year = data_2016_2019$Year-2016
reg_2016 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_2016_2019)
summary(reg_2016)
reg_2016_log <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_2016_2019)
summary(reg_2016_log)
reg_2016_x0 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_2016_2019[data_2016_2019$Cited.by != 0,])
summary(reg_2016_x0)
reg_2016_log_x0 <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_2016_2019[data_2016_2019$Cited.by != 0,])
summary(reg_2016_log_x0)

data_2008_2012$Year = data_2008_2012$Year-2008
reg_2008 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_2008_2012)
summary(reg_2008)
reg_2008_log <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_2008_2012)
summary(reg_2008_log)
reg_2008_x0 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_2008_2012[data_2008_2012$Cited.by != 0,])
summary(reg_2008_x0)
reg_2008_log_x0 <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_2008_2012[data_2008_2012$Cited.by != 0,])
summary(reg_2008_log_x0)

data_1996_2002$Year = data_1996_2002$Year-1996
reg_1996 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_1996_2002)
summary(reg_1996)
reg_1996_log <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_1996_2002)
summary(reg_1996_log)
reg_1996_x0 <- lm(Cited.by ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_1996_2002[data_1996_2002$Cited.by != 0,])
summary(reg_1996_x0)
reg_1996_log_x0 <- lm(log(Cited.by + 1) ~ x + Year + I(x*Year) + Nb_autors + Document.Type, data = data_1996_2002[data_1996_2002$Cited.by != 0,])
summary(reg_1996_log_x0)

stargazer(reg_P4, reg_P3, reg_P2, reg_2016, reg_2008, reg_1996, 
          title="Regression Models",
          dep.var.labels=c("Number of citations"),
          align=TRUE, 
          no.space=TRUE, 
          omit.stat = c("f", "ser"),
          column.sep.width = "-15pt",
          out=paste0(path_data,"\\models"))


stargazer(reg_P4_log, reg_P3_log, reg_P2_log, reg_2016_log, reg_2008_log, reg_1996_log, 
          title="Regression Models",
          dep.var.labels=c("Number of citations log+1"),
          align=TRUE, 
          no.space=TRUE, 
          omit.stat = c("f", "ser"),
          column.sep.width = "-15pt",
          out=paste0(path_data,"\\models_log"))

stargazer(reg_P4_x0, reg_P3_x0, reg_P2_x0, reg_2016_x0, reg_2008_x0, reg_1996_x0, 
          title="Regression Models",
          dep.var.labels=c("Number of citations without 0s"),
          align=TRUE, 
          no.space=TRUE, 
          omit.stat = c("f", "ser"),
          column.sep.width = "-15pt",
          out=paste0(path_data,"\\models_x0"))


stargazer(reg_P4_log_x0, reg_P3_log_x0, reg_P2_log_x0, reg_2016_log_x0, reg_2008_log_x0, reg_1996_log_x0, 
          title="Regression Models",
          dep.var.labels=c("Number of citations log+1 without 0s"),
          align=TRUE, 
          no.space=TRUE, 
          omit.stat = c("f", "ser"),
          column.sep.width = "-15pt",
          out=paste0(path_data,"\\models_log_x0"))


plot(reg_P4_log_x0$residuals, log(data_P4$Cited.by[data_P4$Cited.by != 0] +1))
plot(reg_P4_log$residuals, log(data_P4$Cited.by +1))

mean(reg_P4_log$residuals[data_P4$Cited.by == 0])
mean(reg_P4_log$residuals[data_P4$Cited.by == 1])
mean(reg_P4_log_x0$residuals[data_P4$Cited.by[data_P4$Cited.by!=0] == 1])


qqnorm(reg_P4_log_x0$residuals, pch = 1, frame = FALSE)
qqline(reg_P4_log_x0$residuals, col = "steelblue", lwd = 2)

qqnorm(reg_P4_log$residuals, pch = 1, frame = FALSE)
qqline(reg_P4_log$residuals, col = "steelblue", lwd = 2)



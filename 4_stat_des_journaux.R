###########################################################
###                    STAT DES SRIPT                   ###
### This code does the first descriptive analyses.      ###
###             CHABRIEL / SANSU - 30/12/2021           ###
###########################################################

######## Stat desc base journaux

base_journaux = read.csv(paste0(path_data,"\\scopus_base_selection_journaux.csv"), encoding = 'UTF-8')

base_journaux$Source.title <- tolower(base_journaux$Source.title)
base_journaux[base_journaux$Source.title == "lancet","Source.title"] <- "the lancet"
base_journaux[base_journaux$Source.title == "the journal of general virology","Source.title"] <- "journal of general virology"

A <- base_journaux %>% count(Source.title, Year)

ggplot(subset(A, Year < 2020), aes(x=Year, y=n, color=Source.title)) +
  geom_point() +
  geom_line() +
  xlab("Années") + ylab("Nombre d'articles") +
  theme_hc() +
  theme(legend.text = element_text(size=7), legend.title = element_text(size=8)) +
  scale_colour_discrete("Journal")

ggplot(A, aes(x=Year, y=n, color=Source.title)) +
  geom_point() +
  geom_line() +
  xlab("Années") + ylab("Nombre d'articles") +
  theme_hc()   +
  theme(legend.text = element_text(size=7), legend.title = element_text(size=8)) +
  scale_colour_discrete("Journal")




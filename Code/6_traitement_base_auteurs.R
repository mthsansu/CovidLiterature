
###########################################################
###        Traitement base articles g?n?raux            ###
###             CHABRIEL / SANSU - 29/12/2021           ###
###########################################################

memory.limit(size=10000)

library(data.table)
library(dplyr)

# Fusion des données singulières
# A ne lancer qu'une fois
setwd(paste0(path_data_scraping, "\\Data auteurs"))
col_selected = c('EID', 'Source', 'Source title', 'Year', 'Cited by', 'Document Type', 
                 'Publication Stage', 'Author(s) ID', 'Language of Original Document')
articles_vus = c()
i = 0
rm(dataset)
done = c()
list_data = list()
for (data in list.files()) {
  if (!(data %in% done)) {
    print(i)
    temporary = fread(data, select = col_selected)
    temporary = as.matrix(temporary)
    temporary = temporary[(temporary[,'Source'] == 'Scopus'),]
    temporary = temporary[!(temporary[,'EID'] %in% articles_vus), ]
    if (length(temporary[,'EID']) != 0) {
      i = i+1
      list_data[[i]] = temporary
      articles_vus = c(articles_vus, list(temporary[,'EID']))
    }
    done = c(done, data)
  }
}
dataset = bind_rows(lapply(list_data,  as.data.frame))
dataset = dataset %>% distinct(EID, .keep_all= TRUE)

setwd(path_data)
write.csv(dataset, "full_data.csv")












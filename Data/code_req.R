journaux <- "Journal Of Virology, Advances In Experimental Medicine And Biology, Virology, Journal Of General Virology, Archives Of Virology, Plos One, Avian Diseases, American Journal Of Veterinary Research, Veterinary Microbiology, Virus Research, Veterinary Record, Journal Of Immunology, Veterinary Immunology And Immunopathology, Journal Of Virological Methods, Journal Of Biological Chemistry, Journal Of Virology, Virology, Emerging Infectious Diseases, Plos One, Vaccine, Journal Of Virological Method, Journal Of Clinical Microbiology, Journal Of General Virology, Journal Of Biological Chemistry, Virus Research, Lancet, Proceedings Of The National Academy Of Sciences Of The United States Of America, Biochemical And Biophysical Research Communications, Journal Of Infectious Diseases, Journal Of Immunology, Plos One, Journal Of Virology, Scientific Reports, Viruses, Virology, Virus Research, Archives Of Virology, Emerging Infectious Diseases, Journal Of General Virology, Plos Pathogens, Veterinary Microbiology, Antiviral Research, Vaccine, Journal Of Virological Methods, Virology Journal, International Journal Of Environmental Research And Public Health, Plos One, Sustainability Switzerland, Scientific Reports, Frontiers In Psychology, BMJ, Frontiers In Immunology, Frontiers In Public Health, International Journal Of Molecular Sciences, Viruses, Journal Of Medical Virology, ACM International Conference Proceeding Series, BMJ Open, Journal Of Clinical Medicine, Vaccines"
list_journaux <- as.list(strsplit(journaux, ", ")[[1]])
list_journaux <- as.character(unique(list_journaux))
req_cov <- '"COVID-19" OR "Coronavirus" OR "Corona virus" OR "2019-nCoV" OR "SARS-CoV" OR "MERS-CoV" OR "Severe Acute Respiratory Syndrome" OR "Middle East Respiratory Syndrome" AND '
req_p1 <- '( EXCLUDE ( PUBYEAR,2003) OR EXCLUDE ( PUBYEAR,2004) OR EXCLUDE ( PUBYEAR,2005) OR EXCLUDE ( PUBYEAR,2006) OR EXCLUDE ( PUBYEAR,2007) OR EXCLUDE ( PUBYEAR,2008) OR EXCLUDE ( PUBYEAR,2009) OR EXCLUDE ( PUBYEAR,2010) OR EXCLUDE ( PUBYEAR,2011) OR EXCLUDE ( PUBYEAR,2012) OR EXCLUDE ( PUBYEAR,2013) OR EXCLUDE ( PUBYEAR,2014) OR EXCLUDE ( PUBYEAR,2015) OR EXCLUDE ( PUBYEAR,2016) OR EXCLUDE ( PUBYEAR,2017) OR EXCLUDE ( PUBYEAR,2018) OR EXCLUDE ( PUBYEAR,2019) OR EXCLUDE ( PUBYEAR,2020) OR EXCLUDE ( PUBYEAR,2021) OR EXCLUDE ( PUBYEAR,2022) OR EXCLUDE ( PUBYEAR,2023) OR EXCLUDE ( PUBYEAR,2024) )'
req_p2 <- '( LIMIT-TO ( PUBYEAR,2003) OR LIMIT-TO ( PUBYEAR,2004) OR LIMIT-TO ( PUBYEAR,2005) OR LIMIT-TO ( PUBYEAR,2006) OR LIMIT-TO ( PUBYEAR,2007) OR LIMIT-TO ( PUBYEAR,2008) OR LIMIT-TO ( PUBYEAR,2009) OR LIMIT-TO ( PUBYEAR,2010) OR LIMIT-TO ( PUBYEAR,2011)  OR LIMIT-TO ( PUBYEAR,2012) )'
req_p3 <- '( LIMIT-TO ( PUBYEAR,2013) OR LIMIT-TO ( PUBYEAR,2014) OR LIMIT-TO ( PUBYEAR,2015) OR LIMIT-TO ( PUBYEAR,2016) OR LIMIT-TO ( PUBYEAR,2017) OR LIMIT-TO ( PUBYEAR,2018) OR LIMIT-TO ( PUBYEAR,2019) )'
req_p4 <- '( LIMIT-TO ( PUBYEAR,2020) OR LIMIT-TO ( PUBYEAR,2021) OR LIMIT-TO ( PUBYEAR,2022) )'
req_jour1 <- ' AND ( LIMIT-TO ( EXACTSRCTITLE,"'
req_jour2 <- '" )'
df <- data.frame (journal  = rep(list_journaux,4), req_journal = rep(list_journaux,4), periode = rep(1:4, each = 40),
                  req_periode = rep(c(req_p1,req_p2,req_p3,req_p4),each = 40),req_tot = rep(req_cov,160))
df$req_journal <- paste0(req_jour1, df$req_journal,req_jour2)
df$req_tot <- paste0(df$req_tot, df$req_periode,df$req_journal)

write.csv(df,"C:\\Users\\mthsa\\Desktop\\SQD_DSSS\\ProjDSSS\\base_req.csv")


jour_red <- "Plos One, Scientific Reports, International Journal Of Environmental Research And Public Health, Sustainability Switzerland, Frontiers In Psychology"
list_journaux_red <- as.list(strsplit(jour_red, ", ")[[1]])
list_journaux_red <- as.character(unique(list_journaux_red))
req_cov <- '"COVID-19" OR "Coronavirus" OR "Corona virus" OR "2019-nCoV" OR "SARS-CoV" OR "MERS-CoV" OR "Severe Acute Respiratory Syndrome" OR "Middle East Respiratory Syndrome" AND '
req_y1 <- '( LIMIT-TO ( PUBYEAR,2020) )'
req_y2 <- '( LIMIT-TO ( PUBYEAR,2021) )'
req_y3 <- '( LIMIT-TO ( PUBYEAR,2022) )'
req_jour1 <- ' AND ( LIMIT-TO ( EXACTSRCTITLE,"'
req_jour2 <- '" )'
df <- data.frame (journal  = rep(list_journaux_red,3), req_journal = rep(list_journaux_red,3),
                  req_year = rep(c(req_y1,req_y2,req_y3),each = 5), req_tot = rep(req_cov,15))
df$req_journal <- paste0(req_jour1, df$req_journal,req_jour2)
df$req_tot <- paste0(df$req_tot, df$req_year,df$req_journal)

write.csv(df,"C:\\Users\\mthsa\\Desktop\\SQD_DSSS\\ProjDSSS\\base_req_red.csv")






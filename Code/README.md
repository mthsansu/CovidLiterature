# Dossier Code

Le dossier contient les scripts pour répliquer les analyses (extraction des données, retraitement des données, traitements statistiques).

- Le fichier ```Scraping scopus.ipynb``` est le script (Python) qui permet de réaliser l'aspiration automatique des données sur Scopus. Nous déconseillons d'essayer de l'utiliser sans précaution : cela peut tourner durant plusieurs heures et connaître des bugs selon l'interface utilisateur.
Pour l'utiliser, il faut spécifier les identifiant et mot de passe d'un compte utilisateur sur Scopus.

- Le fichier ```0_master.R``` permet de spécifier les chemins de l'utilisateur pour tous les traitements conduits sous le logiciel R.

- Le fichier ```2_generation_queries_corona.R``` génère automatiquement les requêtes à réaliser sous Scopus pour extraire nos données initiales, i.e. en suivant la méthodologie CORD-19 sur les journaux les plus importants en termes de contributions aux champ des coronavirus (à l'aide du script ```Scraping Scopus.ipynb```).

- Le fichier ```3_traitement_base_coronavirus.R``` formate les données initiales extraites pour enlever les erreurs et construire une base des articles extraits.

- Le fichier ```4_df_auteurs.R``` permet de reconstituer une base des auteurs (restriction aux auteurs ayant 2 publications sur les coronavirus au moins). De plus, il y est opéré l'échantillonage stratifié pour récupérer un nombre d'auteurs plus faible (environ 10 000) pour lesquels nous allons extraire toute leur production scientifique (dans le domaine des coronavirus ou non).

- Le fichier ```5_generation_queries_auteurs.R``` génère automatiquement les requêtes à réaliser sous Scopus pour extraire les données scientométriques des auteurs échantillonés (il faut là encore recourir au script ```Scraping scopus.ipynb```).

- Le fichier ```6_traitement_base_auteurs.R``` conduit la mise en forme de toutes les données extraites par l'étape d'aspiration automatique précédente (niveau articles).

- Le fichier ```7_df_auteurs_complete.R``` permet de remonter nos données sous forme d'observations au niveau auteur * année.

- Le fichier ```8_construction_capitaux.R``` correpsond à la constitution de variables à partir des données brutes.

- Le fichier ```9_Régressions.R``` mène les analyses empiriques.

- Le fichier ```10_stat_desc.R``` construit des visualisations des statistiques descriptives sur nos données finales.

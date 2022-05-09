# Dossier Code

Le dossier contient les scripts pour répliquer les analyses (extraction des données, retraitement des données, traitements statistiques).

Le fichier ```Scraping Scopus.ipynb``` est le script (Python) qui permet de réaliser l'aspiration automatique des données sur Scopus. Nous déconseillons d'essayer de l'utiliser sans précaution : cela peut tourner durant plusieurs heures et connaître des bugs selon l'interface utilisateur.
Pour l'utiliser, il faut spécifier les identifiant et mot de passe d'un compte utilisateur sur Scopus.

Le fichier ```0_master.R``` permet de spécifier les chemins de l'utilisateur pour tous les traitements conduits sous le logiciel R.

Le fichier ```2_generation_queries_corona.R``` génère automatiquement les requêtes à réaliser sous Scopus pour extraire nos données initiales, i.e. en suivant la méthodologie CORD-19 sur les journaux les plus importants en termes de contributions aux champ des coronavirus (à l'aide du script ```Scraping Scopus.ipynb```).



### Axes de recherche envisagés

* Propagation de la recherche du covid dans les milieux scientifiques
* Qui sont les spécialistes reconnus ajd ? 
* Pq le sont-ils ?
* Les spécialistes de coronavirus avant le covid le sont-ils encore ajd ?
* Interdisciplinarité de la recherche du covid 
* Quelles sont les disciplines en interaction ?
* Quels sont les papiers intercités ?
* Quel est l’impact réel des papiers ?
* Popularité dans les médias
* Utilisation par les pouvoirs publics
* Corrélation nb de papiers / citations, avec sortie des vaccins
* Quels sont les incitations à la publication (le nombre de cas, les répercussions économiques, un effet de mode, la sortie des vaccins, annonces de la WHO, les poussées politiques de rendre les vaccins gratuits…) ?
* Quels sont les mécanismes selon les différents types de papiers (preprints, open access, close access, peer-reviewed, etc.)
* Lier ça aux fake news ?
* Corrélation papiers scientifiques, fake news (on peut faire des études de cas sur une fake news particulière). L’hydroxychloroquine par exemple. 
* Là on a un débat scientifique sérieux. On peut s’interroger sur qui a raison et comment, non pas par le contenu des discours, mais par le capital scientifique des individus. 
* Le Covid contiendrait des fragments adn du sida
* Comparer les articles du covid avec les articles d’autres épidémies (sars, ebola, grippe aviaire, zika…)


### Données exploitables

* SSRN : https://papers.ssrn.com/sol3/DisplayJournalBrowse.cfm
* Jstor : https://www.jstor.org/
* ScienceDirect & SCOPUS : https://www.sciencedirect.com/ ; https://service.elsevier.com/app/answers/detail/a_id/15534/supporthub/scopus/#tips
* LitCovid : https://www.ncbi.nlm.nih.gov/research/coronavirus/docsum?filters=topics.General%20Info
* COVID-19 in Higher Education Literature Database
* Dimensions : https://www.dimensions.ai/
* Covid Economics : https://cepr.org/content/covid-economics-vetted-and-real-time-papers-0
* Retraction watch : https://retractionwatch.com/
* Web of Science
* PubMed, PubMed Central
* NCBI (ils ont une API)


### Méthodes d'acquisition :
* Demander à l’ENSAE ?
* Envoyer un mail aux sites internet
* Contacter les chercheurs qui ont bossé sur le sujet
* Scrapper
* Scrapper par mot-clés les articles
* Rescraper par auteur une fois la base constituée

### Données recherchées

Pour chaque papier :
Le titre
L’abstract
Les auteurs
Date de publication 
Nombre de citations / de téléchargement
Académie / think-tank / labo … de l’auteur
Langue de publication 
Nature de l’article (recherche, presse, condensé de littérature...)

Pour chaque auteur :
Nombre de publications
Nombre de publication par thème / discipline
Parcours académique de l’auteur
En particulier université / labo / think tank… de résidence
Date de naissance / âge
Nationalité

Variables de contrôle (distinguer effet de volume / de substitution…) ?
Volume d’articles pour d’autres maladies
La grippe / le SIDA ?
Evolution du nombre d’articles publiés par quelques auteurs sélectionnés au hasard ?

### Problèmes rencontrables

* Biais liés aux articles rétractés (des articles par la suite invalidés vont disparaitre de la base).
* Propositions de résolution : Voir Retraction Watch (https://retractionwatch.com/). Apparament, ça ne concerne que 1% des articles, mais checker quand même.

* Distinction preprints / peer reviewed articles : tous les preprints n’ont pas de doi


### Propositions de résolution :
* Bien définir l’objet d’étude
* Voir en fonction des données qu’on aura. 

### Problèmes de données 
* Biais de sélection
* Doublons lié au croisement des bases de recherche :
* Faire du regex
* Utiliser le doi, autres ID 
* Différencier date de publication, date de mise en ligne
* Faire attention au problème de la troncature temporelle : Comme il y a un lag entre la date de publication et de mise en ligne, il faudra faire attention à la fin de notre période d’étude (si on l’arrête à la semaine dernière, on aura des papiers en sous-effectifs parce que ils n’ont pas encore été mis en ligne)
* De même pour les retracted articles (il y a un délai entre la date de publication et la date de rétraction)


### Comment mesurer l’effort de recherche ?
Le nombre d’articles n’est pas forcément une bonne mesure puisque un chercheur peut soit décider de publier plein de petits articles ou un très gros, pour un même volume de travail. 
Propositions de résolution :
Contrôler au niveau de l’université. L’université d’accueil publie combien d’articles par an en moyenne dans les autres disciplines / thèmes de recherche non covid, puis comparer. 
Contrôler au niveau de l’auteur. Écrit-il plus / moins qu’avant ?
Créer une variable taille des articles. Elle n’est pas non plus une bonne mesure mais on peut créer un indicateur synthétique entre le nombre et la taille des articles pour compenser les éventuels biais. 
Le plus grand biais possible est qu’un chercheur écrive plusieurs articles pour un même travail de recherche, un pour la communauté scientifique, un autre pour un magazine, un autre pour un chapitre de livre… Si on cherche le nombre d’articles publiés dans un seul secteur, on devrait limiter ce nombre de biais. 

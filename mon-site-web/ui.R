#library(readr)
#table_cours <- read_csv("liste des cours.csv")
#   ____________________________________________________________________________
#   UI                                                                      ####

library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)
library(shinyWidgets)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#C10250 purple
#03BCC0 green
#D2D945 yellow/green
#FCB040 orange
#FF5850 red
#436983 hipster blue

shinyUI(navbarPage(title = "J.A Datatech Consulting",
                   theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home",
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                              tags$link(rel = "icon", 
                                        type = "image/jpg", 
                                        href = "images/logo_icon.jpg")
                            )
                   ),
                   
                   # ----------------------------------
                   # tab panel 2 - Neighborhood Browser
                   tabPanel("Free Courses",
                            #includeHTML("scrollToTop.html"),
                            tags$img(src="images/free_courses1.png", width=1500,height=600),
                            br(),
                            br(),
                            fluidRow(column(width=2),
                                     column(
                                       
                                       h1(p("Free Data Science courses for Beginners",style="color:black;text-align:center")),
                                       width=8,style="background-color:lavender;border-radius: 10px"
                                     )
                            ),
                            br(),
                            #br(),
                            #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/W0RVXyVETcw" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                            #tags$img(src="images/free_courses1.png", width=1500,height=600),
                            #br(),
                            br(),
                            fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                                     column(
                                       strong("Start your learning journey in data science by taking these free courses. 
                                         You'll learn how to to import, clean, manipulate, analyze and  visualize data 
                                         by using programming languages such as Python, R, SQL and other tools. 
                                         Through project-based courses, you'll get hands-on with some of the most 
                                         popular Python libraries, including pandas, NumPy, Matplotlib and some of 
                                         the most popular R packages including ggplot2 and tidyverse packages like 
                                         dplyr and readr. You'll then work with real-world datasets to learn 
                                         fundamentals statistical and machine learning techniques.",style="color:black;text-align:justify"),
                                       
                                       width = 8,style="background-color:lavender;border-radius: 10px")),
                            br(),
                            fluidRow(column(width=2),
                                     column(
                                       tags$a("All you need to know about Data Science field here", href="https://medium.com/@afouda.josue/tout-ce-que-vous-devez-savoir-sur-la-data-science-23bd3e746a4c"),
                                       
                                       width = 8,style="background-color:papayawhip; border-radius: 10px"
                                     )),
                            hr(),
                            tags$style(HTML("
                                        
                                          .tabbable > .nav > li[class=active]    > a {background-color: #BFF7BB; color:black}
                                        
                                        ")),
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Statistique pour la Data Science avec Python - Partie 1")),
                                     h4(p("Ce cours est basé sur mon livre intitulé :"), 
                                        p(a("Statistique et Simulation avec Python : Cours et Exercices corrigés", 
                                            href = "https://www.amazon.fr/gp/product/B08SCYRX3H/ref=dbs_a_def_rwt_hsch_vamf_tkin_p1_i4"))),
                                     h4(p("Dans ce cours, vous apprendrez à : 
                                     - calculer des mesures de tendance centrale et des mesures de dispersion pour décrire les données ;
                                     - créer et interpréter des graphiques de visualisation des données en utilisant des 
                                     librairies comme Matplotlib et Seaborn ; - effectuer des analyses multivariées afin d’examiner les 
                                     relations existant entre plusieurs variables ; - penser de manière probabiliste afin de réaliser des 
                                     inférences statistiques pour tirer des conclusions à partir d’échantillons de données ; - simuler des 
                                     données afin d’estimer la probabilité d’un événement ; - rééchantilloner les données à l’aide de 
                                     techniques comme le Bootstrapping et visualiser les échantillons bootstrap ; - calculer les 
                                     intervalles de confiance des statistiques récapitulatives d’une population ; - effectuer 
                                     et interpréter les tests d’hypothèse appropriés à utiliser pour les ensembles de données ; 
                                     - appliquer des techniques de modélisation statistique aux données 
                                     (régression linéaire et régression logistique) ; - interpréter les résultats d’une modélisation 
                                          statistique et évaluer la qualité des modèles.")),
                                     #br(),
                                     h4(p(a("Téléchargez les ressources de ce cours", href = "https://drive.google.com/file/d/16Jt-KlqgZdGuc8jyzt1egpAdF6ce34w-/view?usp=sharing"), ".")),
                                     br()
                                     
                              ),
                              column(6,
                                     h4(p("Videos du cours")),
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euTXMPRdHjSoOlRmTbLz4mAK" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Initiation à la Statistique descriptive et inférentielle avec R")),
                                     h4(p("Ce cours est destiné à toutes personnes débutantes en programmation avec R."), 
                                        p(a("Cliquez ici pour pourvoir installer R dans votre ordinateur", 
                                            href = "https://rpubs.com/Josue90/installation_R_RStudio"))),
                                     h4(p("Dans ce cours, vous apprendrez à : 
                                     - calculer des statistiques descriptives et à créer des graphiques de visualisation ; 
                                     - cacluler et visualiser des corrélation esntre variables ; 
                                     - réaliser des tests de Student indépendants et appariés et à visualiser vos résultats ; 
                                     - réaliser une analyse unidirectionnelle de la variance (ANOVA) ; 
                                     - créer des tableaux croisés avec l'analyse du chi2. Voici le lien des données utilisées :
                                          https://raw.githubusercontent.com/JosueAfouda/Statistics-with-Python/main/nhanes_2015_2016.csv")),
                                     #br(),
                                     h4(p(a("Téléchargez les ressources de ce cours", href = "https://drive.google.com/file/d/17FosWDt00JPSjYUTfn_KMVT1EbROoPtD/view?usp=sharing"), ".")),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
            
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euQdW49lojDKQknyu9G4xUWZ" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            br(),
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Maîtriser Python pour la Data Science")),
                                     h4(p("Ce cours est basé sur mon livre intitulé :"), 
                                        p(a("Savoir programmer avec le langage Python appliqué à l'analyse des données: Cours, Exercices corrigés et Projets réels", 
                                            href = "https://www.amazon.fr/gp/product/B08CWM7L1Y/ref=dbs_a_def_rwt_hsch_vamf_tkin_p1_i3"))),
                                     h4(p("A travers ce cours, vous apprendrez à : ✅ Configurer votre environnement Python pour 
                                     un travail efficace ; ✅ Écrire du code propre et concis avec Python 3 incluant des structures c
                                     onditionnelles, des boucles et des fonctions ; ✅ Écrire des programmes Python qui interagissent a
                                     vec les utilisateurs et traite leurs entrées afin de générer les sorties souhaitées ; 
                                     ✅ Automatiser des tâches en écrivant des fonctions  ;✅ Utiliser certains outils modernes comme les
                                     fonctions lambda, les listes de compréhension, les fonctions map() et filter() ; ✅ Stocker l
                                     ’information dans des structures de données et générer des statistiques ; ✅ Effectuer une analyse d
                                     es données avec des fonctionnalités de Numpy et de Pandas ; Etc.")),
                                     #br(),
                                     h4(p(a("Téléchargez les ressources de ce cours", 
                                            href = "https://drive.google.com/file/d/1eBe_csn6H1a5S8BAYcJbBNrDKpAsxiSG/view?usp=sharing"), ".")),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euS1IIrTnbNzUvrC_wWPkfwI" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("SQL pour les Business Analysts, Data Analysts et Data Scientists (Partie 1)")),
                                     h4(p("Dans ce cours, vous apprendrez le langage SQL standard qui est commun à tous les SGBDR. 
                                     Ce qui veut dire que vous allez pouvoir appliquer vos connaissances dans MySQL, PostgreSQL, etc.
                                     Au lieu de passer votre temps à comprendre les détails d’installation de tel ou tel autre SGBDR, 
                                     vous devriez passer du temps à apprendre réellement la programmation en SQL afin d’examiner vos données.
                                     Databricks est l’un des outils les plus importants actuellement sur le marché qui vous donne 
                                     cette possibilité. Le cours est subdivisé en 5 grandes sections : 1) Généralités et Installation 
                                     de l’environnement de travail, 2) Création d’une base de données et d’une table et Insertion des 
                                     données dans une table, 3) Ecriture des requêtes basiques pour interroger une table, 
                                     4) Jointures de tables et écriture de sous-requêtes, 5) Transformation des données")),
                                     #br(),
                                     h4(p(a("Téléchargez les ressources de ce cours", href = "https://drive.google.com/file/d/1EpJlQi1b2rYAvYl0AtTpmWJj-PjXAiyl/view?usp=sharing"), ".")),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euS0H-WL6caV7K7SgY8R9FYj" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Maîtriser la Visualisation avancée des données dans R")),
                                     h4(p("La visualisation des données permet de les
                                     explorer et fournit des informations utiles. De bonnes visualisations vous aident 
                                     également à communiquer vos données à d'autres personnes et sont utiles aux analystes de 
                                     données et aux autres consommateurs de données. Il existe plusieurs packages de R permettant de 
                                     créer des graphiques dont ggplot2 qui est certainement le plus utilisé. Dans ce cours, 
                                     vous apprendrez de manière pratique à créer des graphiques courants tels que les nuages de points, 
                                     les histogrammes, les boîtes à moustache, les diagrammes à barres et d'autres graphiques plus complexes. De plus, vous apprendrez à 
                                     personnaliser vos graphiques afin d'en faciliter la lecture à vos interlocuteurs ce qui vous 
                                     permettra de mieux communiquer les résultats de vos analyses. Ce cours se décompose en 8 sections :
                                     1- Installation de l'environnement de travail ; 2- Apprendre à créer des graphiques de corrélation ; 
                                     3- Apprendre à créer des graphiques de déviation ; 4- Apprendre à créer des graphiques de classement ; 
                                     5- Apprendre à créer des graphiques de distribution ; 6- Apprendre à créer des graphiques de composition ;
                                     7- Apprendre à créer des graphiques de visualisation de séries temporelles ; 8- Apprendre à créer des 
                                          graphiques de clustering.")),
                                     #br(),
                                     h4(p(a("Téléchargez les ressources de ce cours", href = "https://drive.google.com/file/d/1OPDiJhSf1VpLN-lFKgVMKx0LvGMpz3GQ/view?usp=sharing"), ".")),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euQrgm_zLBW3SZgGVJa4ArMh" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Visualisation interactive des données avec Plotly Express dans Python")),
                                     h4(p("Dans cette formation sous forme de projet guidé, vous apprendrez à créer des visuels 
                                          interactifs avec le module Plotly Express afin d'effectuer une analyse graphique 
                                          d'un jeu de données. Vous pourrez ainsi appliquer les connaissances acquises dans 
                                          cette formation pour créer de jolis graphiques interactifs avec vos propres données 
                                          et donner ainsi vie à vos analyses. Le projet est subdivisé en 7 tâches. 
                                          - Tâche 1 : Importation des librairies et des données, 
                                          - Tâche 2 : Diagramme à barres, - Tâche 3 : Nuage de points, - Tâche 4 : Graphiques interactifs à bulles (Bubble Charts), 
                                          - Tâche 5 : Création d'animations interactives, - Tâche 6 : Représentation des données géographiques, 
                                          - Tâche 7 : Graphiques interactifs linéaires et surfaciques")),
                                     #br(),
                                     h4(p(a("Téléchargez les ressources de ce cours", href = "https://drive.google.com/file/d/1d7BTqjQYuyMTZoTysVKwgrjnHw_HE3-N/view?usp=sharing"), ".")),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euTGGxeI8aURCOWQO-otywaH" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            br(),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Réalisez efficacement une analyse exploratoire des données dans R")),
                                     h4(p("Les données brutes que vous avez collecté ne sont pas encore des informations (insights) ni 
                                     des réponses. Pour passer des données aux réponses, vous devez effectuer une analyse exploratoire 
                                     de ces données. L'analyse exploratoire des données est un processus permettant d'explorer des 
                                     ensembles de données, de répondre à des questions et de visualiser les résultats.
                                     Les Data Scientists passent environ 80% du temps voir plus à explorer les données et à les 
                                     préparer pour la modélisation. Dans ce cours, vous découvrirez plusieurs solutions simples 
                                     et efficaces pour réaliser une analyse exploratoire des données avec R.")),
                                     #br(),
                                     h4(p(a("Téléchargez les ressources de ce cours", href = "https://drive.google.com/file/d/1Za8l8-npFZfEmgc5vkCR4fGl6CamCghY/view?usp=sharing"), ".")),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euT5dT61NpDf1vSsfb_eLql-" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            br(),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Initiation à la création et au déploiement de modèles de Machine Learning avec Python")),
                                     h4(p("Selon Wikipédia, L'apprentissage automatique (Machine Learning en anglais) est un champ 
                                     d'étude de l'Intelligence artificielle qui se fonde sur des approches mathématiques et statistiques 
                                     pour donner aux ordinateurs la capacité d' apprendre à partir de données, 
                                     c'est-à-dire d'améliorer leurs performances à résoudre des tâches sans être explicitement programmés 
                                     pour chacune de ces tâches. Plus largement, il concerne la conception, l'analyse, l'optimisation, 
                                     le développement et l'implémentation de telles méthodes. En clair, le système apprend à partir 
                                     des exemples qu'on lui montre. Ces exemples sont des données qui sont rentrées dans le système 
                                          par des hommes. Dans ce cours, vous serez initié à la formation, l'évaluation et au déploiement d'un
                                          modèle de Machine Learning")),
                                     #br(),
                                     h4(p(a("Téléchargez les ressources de ce cours", 
                                            href = "https://drive.google.com/file/d/1DDkBHEwS2ZOoHuJAUmLo27Y2qB0AH_WE/view?usp=sharing"))),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/7-WsA_arUpg" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            
                            br(),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Comment créer de A à Z un modèle Machine Learning avec Scikit-Learn ?")),
                                     h4(p("La construction d'un modèle de Machine Learning est tout un art. Dans cette vidéo, je vous montre les étapes rigoureusement à suivre lorsqu'on construit un modèle de machine learning. A travers ce tutoriel, vous apprendrez entres autres à : 
- nettoyer vos données (gestion des valeurs manquantes et gestion des valeurs aberrantes) via des techniques simples et efficaces ;
- diviser votre ensemble de données en données d'entraînement, de validation et de test ;
- résoudre le problème de déséquilibre de classe rencontré dans les tâches de classification automatique à partir des méthodes de sur-échantillonnage et de sous-échantillonnage ;
- sélectionner les meilleurs prédicteurs pour vos algorithmes ;
- construire différents modèles à partir de divers algorithmes de machine learning (régression logistique, forêt aléatoire, SVM, etc.
- évaluer  les performances des modèles construits ;
- choisir le meilleur modèle sur la base d'une métrique bien définie ;
- évaluer la performance du modèle choisi sur les données de test.")),
                                     #br(),
                                     h4(p(a("Téléchargez les ressources de ce cours", 
                                            href = "https://github.com/JosueAfouda/TUTORIALS/blob/main/Tuto_Machine_Learning.ipynb"))),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/idPDzWybHMw" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            #br(),
                            #hr(),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Machine Learning pour la Prédiction du désabonnement des clients d'une entreprise")),
                                     h4(p("Dans cette formation, vous apprendrez à entraîner plusieurs algorithmes de Machine Learning 
                                     dans le but de prédire les clients d’une entreprise de Télécom qui sont susceptibles de 
                                     se désabonner des services de cette entreprise. La prédiction du taux de désabonnement est 
                                     cruciale pour les entreprises, car le coût de fidélisation d’un client existant est bien 
                                     inférieur à celui d’acquisition d’un nouveau client. Je vous guiderai pas à pas vers l’atteinte de 
                                     cet objectif à travers ces étapes : - Compréhension de la problématique business ; 
                                     - Importation des outils nécessaires ; - Importation des données ; - Analyse exploratoire des données ;
                                     - Prétraitement des données ; - Modélisation (Machine Learning et Réseau de neurones artificiel). 
                                     A travers cette formation, vous développerez vos compétences principalement en programmation 
                                     avec le langage Python  et en construction de modèles de Machine Learning avec la librairie Scikit-Learn.
                                          BONNE FORMATION 🙂")),
                                     #br(),
                                     h4(p(a("Téléchargez le notebook complet (codes, résultats et commentaires) de ce cours", 
                                            href = "https://drive.google.com/file/d/1FhgdrC9hXEYIOrY_VyuaSW68Bs5x9uhE/view?usp=sharing"))),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euQOdkzeAyICws1aYJXTD4dI" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            br(),
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Modélisation statistique du risque de crédit (Credit Scoring) dans R")),
                                     h4(p("Lorsqu'une banque prête de l'argent à une personne, elle prend le risque que cette 
                                     dernière ne rembourse pas cet argent dans le délai convenu. Ce risque est appelé Risque de Crédit. 
                                     Alors avant d'octroyer un crédit, les banques vérifient si le client (ou la cliente) qui 
                                     demandent un prêt sera capable ou pas de le rembourser. Grâce à des modèles de Machine Learning, 
                                     les banques peuvent modéliser la probabilité de défaut de paiement et ainsi attribuer un score à 
                                     chaque nouveau demandeur de crédit : Credit Scoring. Dans cette formation, vous apprendrez à 
                                     construire et évaluer un modèle de Machine Learning pour prédire si un demandeur de crédit 
                                     sera en défaut de paiement ou non. Il s'agit d'une tâche de classification. A travers cette formation, 
                                     vous développerez vos compétences en : ✅ programmation avec le langage R (
                                     création de fonctions, boucle for, structure if,  ggplot2, etc.) ; ✅ analyse statistique et v
                                     isualisation des données en utilisant la célèbre librairie Tidyverse ; 
                                          ✅ Machine Learning (Modèles GLM, Stepwie Regression Model, librairie pROC, etc.).")),
                                     #br(),
                                     h4(p(a("Téléchargez le notebook complet (codes, résultats et commentaires) de ce cours", 
                                            href = "https://drive.google.com/file/d/1FdgIpRzo3W9Zrd05ksiDwy5d9FfkM3Zz/view?usp=sharing"))),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euTlCGNR9OQgUzvXmB3aCc3G" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            br(),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Comment intégrer un modèle Machine Learning dans un dashboard R Shiny ?")),
                                     h4(p("Après avoir construit votre modèle Machine Learning, vous pouvez le déployer en tant que 
                                          service prédictif en temps réel pour permettre à des application de l'utiliser dans le 
                                          but d'effectuer des prédictions sur de nouvelles données non étiquetées. 
                                          Il existe plusieurs manières de déployer un modèle. Dans cette vidéo, 
                                          je vous montre comment intégrer un modèle prédictif de risque de crédit dans une application 
                                          web créée avec R Shiny et shinydasboard. Cette application peut être utilisée par une banque 
                                          pour déterminer en temps réel si une personne est éligible ou pas à un crédit.")),
                                     h4(p("✅  Lien des données Kaggle sur le risque de crédit : h
                                          ttps://www.kaggle.com/laotse/credit-risk-dataset", " Pour savoir créer des applications web aevc 
                                          R Shiny, vous pouvez acheter ce livre 100% pratique :")),
                                     h4(p(a("Développement Web en Data Science avec R Shiny sans HTML, CSS, PHP ni JavaScript", 
                                            href = "https://www.amazon.fr/gp/product/B0966HGP6T/ref=dbs_a_def_rwt_hsch_vapi_tkin_p1_i3"))),
                                     h4(p(a("Téléchargez les ressources de ce cours", 
                                            href = "https://drive.google.com/file/d/1KOrmCT6SWCvFRsGJjXtq5L8vM_vzb-Gb/view?usp=sharing"))),
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/uU4kjctCFDw" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     br()
                              )
                            ),
                            br(),
                            
                            ),
                   
                   # ----------------------------------
                   # tab panel 3 - Location Comparison
                   tabPanel("Premium Courses",
                            #includeHTML("scrollToTop.html"),
                            tags$img(src="images/premiumcourses.png", width=1500,height=600),
                            br(),
                            br(),
                            fluidRow(column(width=2),
                                     column(
                                       
                                       h1(p("Premium AI courses for Data Scientists",style="color:black;text-align:center")),
                                       width=8,style="background-color:lavender;border-radius: 10px"
                                     )
                            ),
                            br(),
                            #br(),
                            #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/W0RVXyVETcw" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                            #tags$img(src="images/free_courses1.png", width=1500,height=600),
                            #br(),
                            br(),
                            fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                                     column(
                                       strong("You have already learned the basic skills in Data Science. Now, it's time for you to get started in Artificial Intelligence field (Machine Learning, Deep Learning, Natural Language Processing, Image Processing, ...)",style="color:black;text-align:justify"),
                                       
                                       width = 8,style="background-color:lavender;border-radius: 10px")),
                            br(),
                            fluidRow(column(width=2),
                                     column(
                                       tags$a("All you need to know about Artificial Intelligence field here", href="https://medium.com/@afouda.josue/intelligence-artificielle-bref-aper%C3%A7u-aa639009629c"),
                                       
                                       width = 8,style="background-color:papayawhip; border-radius: 10px"
                                     )),
                            hr(),
                            tags$style(HTML("
                                        
                                          .tabbable > .nav > li[class=active]    > a {background-color: #BFF7BB; color:black}
                                        
                                        ")),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Développeur d'applications web R Shiny (Formation complète et 100% pratique)")),
                                     h4(p("Cette formation est 100% pratique et basée sur 05 grands projets :

✅ Application web R Shiny pour la création des nuages de mots (Word Cloud)

✅ Application web R Shiny de Segmentation et de Réduction de dimensionnalité par des algorithmes de Machine Learning

✅ Application web R Shiny pour l'analyse et l'optimisation de portefeuille financier

✅ Application web R Shiny pour l'analyse technique des marchés boursiers

✅ Déploiement d'un modèle Machine Learning de Credit Scoring dans un tableau de bord Shiny.

Grâce à des explications claires et précises, vous serez capables d'utiliser R Shiny pour créer des applications Web interactives et performantes même si vous n’aviez aucune expérience en développement Web. Que vous soyez Data Scientist, Data Analyst, Statisticien, Chercheur, Etudiant sans connaissances en HTML, CSS, PHP, ou JavaScript, vous pouvez créer des applications web dynamiques en utilisant uniquement du code R.

A travers cette formation, vous apprendrez à :

✅ traduire des besoins métiers en un projet de création d’application web ;

✅ implémenter dans une application web interactive un processus de prise de décision basée sur les données ;

✅ développer une mise en page de base de l’interface utilisateur (UI) d’une application web Shiny ;

✅ développer un interface utilisateur dynamique en donnant la possibilité aux utilisateurs contrôler les affichages de l’application ;

✅ écrire les codes dans le serveur pour générer les sorties (outputs) tout en utilisant la Réactivité ;

✅ contrôler le comportement réactif de votre application web pour une utilisation optimale ;

✅ réaliser et déployer des applications web performantes à la demande et sur-mesure afin de constituer votre portfolio de projets et ainsi montrer votre talent au monde 🙂")),
                                     
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euStJ32KHOThQzJ7hub-JXA0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     h4(p("En bonus, vous aurez gratuitement la version PDF de mon livre :", a("Développement Web en Data Science avec R Shiny sans HTML, CSS, PHP ni JavaScript", 
                                                                                                               href = "https://www.amazon.fr/gp/product/B0966HGP6T/ref=dbs_a_def_rwt_hsch_vapi_tkin_p1_i3"))),
                                     h4(p(a("Téléchargez les ressources (codes, notebooks, PDFs, explications, etc.) de ce cours", 
                                            href = "https://buy.stripe.com/7sI4ki6Fgfjk6Ws002")))
                                                                   
                              )
                            ),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Création de tableaux de bords avec Dash + Plotly")),
                                     h4(p("- Contexte et Problématique Business")),
                                     h4(p("- Petit rappel sur Plotly : Diagramme à barres du total des ventes par pays")),
                                     h4(p("- Structure générale d'une application Dash")),
                                     h4(p("- Ajout et positionnement de plusieurs composants dans une application Dash")),
                                     h4(p("- Autres personnalisations avec HTML et CSS")),
                                     h4(p("- Fonctions Callbacks")),
                                     h4(p("- Réutilisation des composants de Dash")),
                                     h4(p("- Entrées utilisateur dans les composants Dash")),
                                     h4(p("- Tableaux Dash interactifs")),
                                     h4(p("- Tableau de bord pour l'analyse exploratoire des données de la Banque mondiale sur la pauvreté et l'équité dans le monde")),
                                     
                                     br()
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLmJWMf9F8euTPoOtzpxXjsSqs4vd9VcJY" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     
                                     h4(p(a("Téléchargez les ressources (codes, notebooks, explications, etc.) de ce cours", 
                                            href = "https://buy.stripe.com/8wMg30e7I1sudkQeUX")))
                                     
                              )
                            ),
                            
                            br(),
                            hr(),
                            
                   ),
                   
                   # ----------------------------------
                   # tab panel 4 - Consulting
                   tabPanel("Consulting"
                            # idée design de la page consulting https://rasa.com/solutions/customer-service-automation/
                   ),
                   
                   # ----------------------------------
                   # tab panel 5 - Ebooks & Notebooks
                   tabPanel("Ebooks & Notebooks",
                            
                            tags$img(src="images/cover_3d_livre_stats_python.png", width=1500,height=600),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Ebook Statistique et Simulation avec Python : Cours et Exercices corrigés")),
                                     br(),
                                     h4(p("Dans ce livre 100% pratique, vous trouverez : ")),
                                     h4(p("- Programmation avec Python ;")),
                                     h4(p("- Statistiques descriptives ;")),
                                     h4(p("- Probabilités ;")),
                                     h4(p("- Statistique inférentielle ;")),
                                     h4(("- Modélisation statistique ;")),
                                     h4(p("- Machine Learning, etc.")),
                                     h4(p(a("Cliquez ici pour acheter et télécharger le livre", 
                                            href = "https://buy.stripe.com/eVaaIGgfQefgbcIfZ3")))
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/-hbNZst9aLY" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     
                                     
                                     
                              )
                            ),
                            
                            br(),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Ebook APPRENDRE A PROGRAMMER AVEC R ET RSTUDIO")),
                                     br(),
                                     
                                     h4(p("Ce livre est conçu pour guider pas à pas les débutants en programmation avec le langage R. 
                                          Que vous soyez Statisticien, Etudiant / Doctorant en Finance ou Econométrie ou Sciences sociales, 
                                          Data Scientist enthousiaste ou professionnel, Data Analyst, Analyste quantitatif, ..., 
                                          vous devez maîtriser R pour gagner en efficacité dans vos différentes tâches relatives 
                                          à l'analyse et la modélisation des données.")),
                                     h4(p("A la fin de ce livre, vous allez maîtriser les concepts fondamentaux de la programmation 
                                          informatique. Vous serez aussi capables de créer vos propres programmes incluant des 
                                          structures if, for, while ainsi que vos propres fonctions. De plus, vous serez capables 
                                          d'importer des données à partir de différentes sources telles que les fichiers Excel, CSV, 
                                          les API et même des données du Web avec le Web Scraping. Vous allez maîtriser 
                                          l'utilisation de la célèbre bibliothèque Tidyverse  ainsi que ces principaux packages 
                                          dont GGPLOT2 pour la visualisation des données, DPLYR, FORCATS, etc.")),
                                     
                                     h4(p(a("Cliquez ici pour acheter et télécharger le livre", 
                                            href = "https://buy.stripe.com/aEU7wu1kW4EG94A6ov")))
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/-hbNZst9aLY" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     tags$img(src="images/1640525017.png", width=560,height=315)
                                     
                                     
                              )
                            ),
                            
                            br(),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Analyse des données avec Python et Pandas pour débutants")),
                                     br(),
                                     h4(p("La maîtrise de Pandas est absolument essentielle. Ce produit 
                                          (notebooks + codes + PDF + Données + HTMLs) est destiné aux débutants en Data Science souhaitant 
                                          s'approprier rapidement et de manière efficace l'analyse des données avec Python et Pandas.")),
                                     br(),
                                     h4(p(a("Cliquez ici pour acheter et télécharger le produit", 
                                            href = "https://buy.stripe.com/8wMaIGfbM0oqeoU8wC")))
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     tags$img(src="images/conseils.png", width=560,height=315)
                                     #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/-hbNZst9aLY" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     
                                     
                                     
                              )
                            ),
                            
                            br(),
                            
                            fluidRow(
                              column(6,
                                     #br(),
                                     h3(strong("Ethique dans la Data Science et l'Intelligence Artificielle (ebook)")),
                                     br(),
                                     h4(p("Ce livre s'adresse particulièrement aux Etudiants et Professionnels en Data Management, 
                                          Intelligence Artificielle (IA) et Big Data. Il contient des cas pratiques d'étude de 
                                          l'éthique en DATA et IA en milieu professionnel ainsi que des exercices/Quizz")),
                                     br(),
                                     h4(p(a("Cliquez ici pour acheter et télécharger le livre", 
                                            href = "https://buy.stripe.com/fZe0429Rsdbc3Kg148")))
                                     
                                     #hr(),
                                     
                              ),
                              column(6,
                                     
                                     br(),
                                     tags$img(src="images/file-20210416-17-1s42eoj.jpg", width=560,height=315)
                                     #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/-hbNZst9aLY" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                     
                                     
                                     
                              )
                            ),
                            
                            hr(),
                            
                   ),
                   
                   # ----------------------------------
                   # tab panel 6 - Blog
                   tabPanel("Blog",
                            tags$img(src="images/blog.png", width=1500,height=600),
                            br(),
                            br(),
                            
                            fluidRow(
                              
                              column(6,
                                     
                                     br(),
                                     tags$img(src="images/img_article_cloud.png", width=560,height=315)
                                    
                              ),
                              
                              column(6,
                                     #br(),
                                     h3(strong("Cloud Computing : Définitions, Avantages et Inconvénients de son utilisation")),
                                     br(),
                                     h4(p("L’une des plus grandes révolutions technologiques de ces dernières décennies est le Cloud. 
                                          D’après un article du magazine Forbes, le marché du Cloud devrait atteindre 331 milliards de 
                                          dollars d’ici 2022 (Source : Public Cloud Soaring To $331B By 2022 According To Gartner). 
                                          Les entreprises adoptent de plus en plus les solutions du Cloud précisément du Cloud Computing 
                                          afin de toujours proposer des services innovants et de meilleure qualité à leurs clients. 
                                          Mais qu’est-ce que réellement le Cloud et le Cloud Computing ? 
                                          Cet article est une introduction au Cloud Computing, ses services ainsi que les avantages et 
                                          inconvénients liés à son utilisation.")),
                                     
                                     h4(p(a("Lire tout l'article...", 
                                            href = "https://medium.com/@afouda.josue/cloud-computing-d%C3%A9finitions-avantages-et-inconv%C3%A9nients-de-son-utilisation-74893ca15372")))
                                     
                                     #hr(),
                                     
                              )
                              
                            ),
                            
                            br(),
                            
                            fluidRow(
                              
                              column(6,
                                     
                                     br(),
                                     tags$img(src="images/miniature_article_ethics1.png", width=560,height=315)
                                     
                              ),
                              
                              column(6,
                                     #br(),
                                     h3(strong("L’Ethique dans la Data Science et l’Intelligence Artificielle (Partie 1)")),
                                     br(),
                                     h4(p("Dans cette première partie de ma série d’articles sur l’éthique dans la Data Science et 
                                          l’Intelligence Artificielle, je vous parle des Théories et Concepts impliqués dans le domaine de l’éthique."
                                          )),
                                     h4(p("Chaque personne a sa conception du bien et du mal. Ceci provient souvent de son éducation, 
                                          de l’influence de son entourage, de sa religion, etc. Si l’ensemble de la Société humaine est 
                                          d’accord sur le caractère bien ou mal de certaines choses, d’un point de vue éthique la 
                                          distinction entre le bien et le mal n’est pas simple. ")),
                                     
                                     h4(p(a("Lire tout l'article...", 
                                            href = "https://medium.com/@afouda.josue/lethique-dans-la-data-science-et-l-intelligence-artificielle-partie-1-36c243641202")))
                                     
                                     #hr(),
                                     
                              )
                              
                            ),
                            
                            br(),
                            
                            fluidRow(
                              
                              column(6,
                                     
                                     br(),
                                     tags$img(src="https://miro.medium.com/max/1400/1*xTJtSQEGM4gkapqbeMRhAA.jpeg", width=560,height=315)
                                     
                              ),
                              
                              column(6,
                                     #br(),
                                     h3(strong("10 CONSEILS PRATIQUES POUR DÉCROCHER UN JOB EN DATA SCIENCE MÊME EN ÉTANT DÉBUTANT")),
                                     #br(),
                                     h4(p("C’est le meilleur moment pour devenir Data Scientist. La matière première en Data Science, 
                                          c’est-à-dire les données, est partout. Les entreprises recherchent de plus en plus des Data 
                                          Scientists qualifiés qui pourront les aider à tirer profit de cette grande masse de données 
                                          disponible pour améliorer leur compétitivité.")),
                                     
                                     h4(p("La moisson est bonne mais il y a peu d’ouvriers. En effet, l’offre en professionnels 
                                          DATA est faible tandis que la demande s’accroît de manière exponentielle. Il est alors 
                                          temps que vous saisissiez les plus belles opportunités qu’offre ce secteur. Il est temps 
                                          que vous décrochiez l’emploi de vos rêves en Data Science.")),
                                     
                                     h4(p("Dans cet article, je vous donne 10 conseils pratiques pour décrocher le job de vos rêves en Data Science même en 
                                          étant débutant. Lisez cet article jusqu’à la fin car le dixième conseil est très précieux mais souvent négligé 
                                          par les chercheurs d’emploi.")),
                                     
                                     h4(p(a("Lire tout l'article...", 
                                            href = "https://medium.com/@afouda.josue/10-conseils-pratiques-pour-d%C3%A9crocher-un-job-en-data-science-m%C3%AAme-en-%C3%A9tant-d%C3%A9butant-e70e6efdafca")))
                                     
                                     #hr(),
                                     
                              )
                              
                            ),
                            
                            br(),
                            
                            fluidRow(
                              
                              column(6,
                                     
                                     br(),
                                     tags$img(src="https://miro.medium.com/max/1400/1*e8FOCYrTCfCFRRHc1IeN5g.jpeg", width=560,height=315)
                                     
                              ),
                              
                              column(6,
                                     #br(),
                                     h3(strong("03 APPLICATIONS BUSINESS DU MACHINE LEARNING")),
                                     br(),
                                     h4(p("Vous connaissez sans doute cette célèbre définition du Machine Learning de l’Informaticien 
                                          Arthur Samuel qui est l’un des pionniers du domaine : “L’apprentissage automatique est un 
                                          domaine d’études qui donne aux ordinateurs la possibilité d’apprendre sans être 
                                          explicitement programmé”. Dans mon livre intitulé Machine Learning par la pratique avec 
                                          Python, j’ai pris le soin d’expliquer dès les premières pages l’origine de ce concept de 
                                          Machine Learning et comment il faut le comprendre. Je suis convaincu qu’après avoir lu les 
                                          pages 1 à 6 de mon livre, vous comprendrez mieux ce qu’est le Machine Learning, son 
                                          origine et ses différentes techniques.")),
                                     
                                     h4(p("Dans cet article, je vais vous plonger au cœur de certaines applications métiers du Machine 
                                          Learning. Après avoir lu entièrement cet article, vous comprendrez cinq importants et célèbres 
                                          cas d’applications du Machine Learning (j’aime beaucoup le terme anglais) dans le monde du 
                                          Business.")),
                                     
                                     h4(p(a("Lire tout l'article...", 
                                            href = "https://medium.com/@afouda.josue/03-applications-business-du-machine-learning-39405c93dc99")))
                                     
                                     #hr(),
                                     
                              )
                              
                            ),
                            
                        
                   ),
                   
                   # ----------------------------------
                   # tab panel 7 - About
                   tabPanel("About",
                            includeHTML("about.html"),
                            shinyjs::useShinyjs(),
                            tags$head(
                                tags$link(rel = "stylesheet", 
                                          type = "text/css", 
                                          href = "plugins/carousel.css"),
                                tags$script(src = "plugins/holder.js")
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            )
                   )
                   
))
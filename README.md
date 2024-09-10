# Module-R-Projet-3

Lien des standard des différents documents d'urbanisme :
https://cnig.gouv.fr/ressources-dematerialisation-documents-d-urbanisme-a2732.html

Lien code alphanumérique SUP : 
https://www.geoinformations.developpement-durable.gouv.fr/fichier/pdf/tableau_sup_codes_alpha-numerique_maj_20_06_24_2_cle5b12c4.pdf?arg=177836384&cle=d45a3272d7cdbd4f794a17e20e04a63590fc6a8b&file=pdf%2Ftableau_sup_codes_alpha-numerique_maj_20_06_24_2_cle5b12c4.pdf

Lien code SUP
https://www.geoinformations.developpement-durable.gouv.fr/fichier/pdf/tableau_alpha-numerique_des_supet_base_legale_maj_20_06_24_2_cle7dbc5e.pdf?arg=177836385&cle=0ef0d333b5d4e35418b5f3b71d2d3849a002c26f&file=pdf%2Ftableau_alpha-numerique_des_supet_base_legale_maj_20_06_24_2_cle7dbc5e.pdf

# Brouillon 

Document urbanisme :
- Perimètre forestier
- Site archéologique
- Emprise ou localisation des immeubles batis ou non batis classés ou inscrits au titre de monuments historiques

  Prescriptions (traitement environnemental et paysagé) :
  - Espace boisées classés
  - élément...particulier protégé
  - arbre remarquable
  - 

Quand RNU : penser au SUP et au SCOT !

# Repartition des tâches vendredi 6/09 après-midi 
- Adèle : écriture du code indiquant pour une zone les documents d'urbanisme disponibles
- Sylvain : écriture du code pour les forêts classées
- Cyril : écriture du code pour les monuments historiques
- Louise : écriture du code pour les sites archéologiques
reste : arbres remarquables, recherche sur les autres données utiles pour l'écriture de documents d'aménagement, en dehors des enjeux patrimoiniaux


--------------------------------------------------------------------------------
NINON-----------------------------------------------------------------------------
code_prescription <- c("01","07","18","19","25","31", "34", "35", "43")
libelle_prescription <- c("Espace boisé",
             "Patrimoine bâti, paysager ou éléments de paysages à protéger",
             "Périmètre comportant des orientations d’aménagement et deprogrammation (OAP)",
             "Secteur protégé en raison de la richesse du sol et du sous-sol",
             "Eléments de continuité écologique et trame verte et bleue",
             "Espaces remarquables du littoral"
             "Espaces, paysage et milieux caractéristiques du patrimoine naturel et
culturel montagnard à préserver"
             "Terres nécessaires au maintien et au développement des activités
agricoles, pastorales et forestières à préserver"
             "Réalisation d’espaces libres, plantations, aires de jeux et de loisir"
             )

code_info <- c("03", "08", "16", "21", "22", "37", "40")
libelle_info <- c("Zone de préemption dans un espace naturel et sensible",
                  "Périmètre forestier : interdiction ou réglementation des plantations
(code rural et de la pêche maritime), plantations à réaliser et semis
d'essence forestière",
                  "Site archéologique",
                  "Projet de plan de prévention des risques",
                  "Protection des rives des plans d'eau en zone de montagne",
                  "Bois ou forêts relevant du régime forestier",
                  "Périmètre d’un bien inscrit au patrimoine mondial ou Zone tampon d’un bien inscrit au patrimoine mondial"
                  )
                  
             )

LOUISE -------------------------------------------------------------------------------------------------------

insee <- c("87186", "87120", "87051", "19036", "87147", "87193", "87194", "19095", "19209", "19262", "19269", "19131")
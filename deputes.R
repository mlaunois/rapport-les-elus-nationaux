library(dplyr)
library(readr)
library(tricky)
library(reshape2)
library(ggplot2)
library(forcats)
library(lubridate)
library(stringr)

# Ouvrir le fichier 8-rne-deputes.txt
table_deputes <- read_tsv(
    file = "8-rne-deputes.txt",
    skip = 1, # ne pas lire la 1ère ligne
    locale = locale(
      date_format = "%d/%m/%Y", # détection de date automatique
      encoding = "ISO-8859-1" # encodage du fichier
    )
  ) %>%
  set_standard_names() # conversion des noms de colonnes en noms utilisables en R

# Compter le nombre de députés
table_deputes_nombre_deputes <- table_deputes %>%
  count(code_sexe, sort = TRUE)
# résultat :
#   code_sexe n
# 1 M         348
# 2 F         229

nombre_deputes = table_deputes_nombre_deputes %>%
  summarise(
    value = sum(n) # le résultat sera un tibble de taille 1 x 1
  )
# résultat :
# sum(n) = sum(c(348, 229)) = 348 + 229 = 577

print(str_c(
  'Il y a',
  nombre_deputes,
  'députés.',
  sep = ' '
))
# affiche :
# Il y a 577 députés.

# Compter le nombre d'hommes et de femmes
nombre_deputes_hommes = table_deputes_nombre_deputes %>%
  filter(code_sexe == 'M') %>% # filtre les lignes selon la valeur de la colonne "code_sexe"
  summarise(
    value = n
  )
# résultat :
# 348

print(str_c(
  nombre_deputes_hommes,
  'députés sont des hommes.',
  sep = ' '
))
# affiche :
# 348 députés sont des hommes.

nombre_deputes_femmes = table_deputes_nombre_deputes %>%
  filter(code_sexe == 'F') %>%
  summarise(
    value = n
  )
print(str_c(
  nombre_deputes_femmes,
  'députés sont des femmes.',
  sep = ' '
))
# affiche :
# 229 députés sont des hommes

# Faire le graphique
table_deputes_nombre_deputes %>%
  ggplot() + # crée un objet ggplot à partir de la table
  geom_col( # ce graphique sera à barres
    mapping = aes(
      x = code_sexe, # l'abscisse représente le code sexe (homme ou femme)
      y = n, # l'ordonnée est le nombre de députés
      fill = c('red', 'blue') # couleurs d'arrière-plan des barres
    ),
    show.legend = FALSE # empêche une légende "c('red', 'blue')" de s'afficher à côté
                        # du graphique
  ) + 
  scale_x_discrete(
    name = "Sexe", # nom de l'axe X
    labels = c("F" = "Femmes", "M" = "Hommes") # étiquettes correspondant aux codes sexes
  ) +
  scale_y_continuous(
    name = "Nombre de députés" # nom de l'axe Y
  )

# Quelles sont les professions les plus fréquentes (20 professions)
table_deputes_profession <- table_deputes %>%
  count(libelle_de_la_profession, sort = TRUE) %>%
  slice(1:20) # prend les professions 1 à 20
# résultat :
#     libelle_de_la_profession          n
# 1   Cadres supérieurs (secteur privé) 48
# 2   Autres cadres (secteur privé)     46
# 3   Fonctionnaires de catégorie A     45
# 4   Industriels-Chefs entreprise      42
# 5   Autres professions                33
# ...

table_deputes_profession_seulement <- table_deputes_profession %>%
  group_by(profession = libelle_de_la_profession) %>%
  summarise() # supprime toutes les colonnes sauf celle de la profession
print('Les vingt premières professions exercées par les députés sont :')
print(table_deputes_profession_seulement)
# affiche :
#     profession
# 1   Cadres supérieurs (secteur privé)
# 2   Autres cadres (secteur privé)
# 3   Fonctionnaires de catégorie A
# 4   Industriels-Chefs entreprise
# 5   Autres professions
# ...

table_deputes_profession %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = fct_reorder(
        .f = libelle_de_la_profession,
        .x = n,
        desc = FALSE
      ), # classe les professions de la plus à la moins pratiquée chez les députés
      y = n
    )
  ) +
  coord_flip() + # l'axe X va en ordonnée et l'axe Y en abscisse
  scale_x_discrete(name = "Profession") +
  scale_y_continuous(name = "Nombre de députés") +
  labs(
    title = "Professions les plus exercées par les députés", 
    caption = "20 premières professions"
  )

# Quels sont les prénoms les plus fréquents hommes et femmes
table_deputes_prenoms <- table_deputes %>%
  count(prenom_de_l_elu, code_sexe, sort = TRUE)
table_deputes_prenom_seulement <- table_deputes_prenoms %>%
  slice(1:20) %>%
  group_by(prenom = prenom_de_l_elu) %>% # cette fonction ordonne également les prénoms
                                         # par ordre alphabétique !
  summarise() # supprime toutes les colonnes sauf celui du nom de l'élu

print('Les vingt prénoms les plus fréquents chez les députés sont :')
print(table_deputes_prenom_seulement)

table_deputes_prenoms %>%
  slice(1:20) %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = fct_reorder(
        .f = prenom_de_l_elu,
        .x = n,
        desc = FALSE
      ),
      y = n
    )
  ) +
  coord_flip() +
  scale_x_discrete(name = "Prénom") +
  scale_y_continuous(
    name = "Nombre de députés",
    breaks = c(0, 2, 4, 6, 8, 10, 12) # définit les arrêts affichés dans le graphique
  ) +
  labs(
    title = "Prénoms les plus fréquents chez les députés", # titre du graphique
    caption = "20 premiers prénoms" # sous-titre du graphique
  )

table_deputes_prenom_homme_seulement <- table_deputes_prenoms %>%
  filter(code_sexe == 'M') %>% # utilise les prénoms des hommes
  slice(1:10) %>% # récupère les dix premiers prénoms
  group_by(
    prenom = prenom_de_l_elu
  ) %>%
  summarise() # supprime toutes les colonnes sauf celui du prénom

print('Les dix prénoms les plus fréquents chez les députés hommes sont :')
print(table_deputes_prenom_homme_seulement)
table_deputes_prenoms %>%
  filter(code_sexe == 'M') %>%
  slice(1:10) %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = fct_reorder(
        .f = prenom_de_l_elu,
        .x = n,
        desc = FALSE
      ),
      y = n,
      fill = 'blue'
    ),
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_x_discrete(name = "Prénom") +
  scale_y_continuous(
    name = "Nombre de députés",
    breaks = c(0, 2, 4, 6, 8, 10, 12)
  ) +
  labs(
    title = "Prénoms les plus fréquents chez les députés hommes", 
    caption = "10 premiers prénoms"
  )

table_deputes_prenom_femme_seulement <- table_deputes_prenoms %>%
  filter(code_sexe == 'F') %>%
  slice(1:10) %>%
  group_by(
    prenom = prenom_de_l_elu
  ) %>%
  summarise()
print('Les dix prénoms les plus fréquents chez les députées femmes sont :')
print(table_deputes_prenom_femme_seulement)
table_deputes_prenoms %>%
  filter(code_sexe == 'F') %>%
  slice(1:10) %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = fct_reorder(
        .f = prenom_de_l_elu,
        .x = n,
        desc = FALSE
      ),
      y = n,
      fill = 'red'
    ),
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_x_discrete(name = "Prénom") +
  scale_y_continuous(
    name = "Nombre de députés",
    breaks = c(0, 2, 4, 6, 8, 10, 12)
  ) +
  labs(
    title = "Prénoms les plus fréquents chez les députées femmes", 
    caption = "10 premiers prénoms"
  )

# Calculer l'âge des députés
table_deputes_dates_naissances <- table_deputes %>%
  count(date_de_naissance)
table_deputes_date_naissance_seulement <- table_deputes_dates_naissances %>%
  group_by(naissance = date_de_naissance) %>%
  summarise()
print('Les dates de naissance des députés sont :')
print(table_deputes_date_naissance_seulement)

table_deputes_avec_ages <- table_deputes %>%
  mutate(
    age = floor( # utilisons des nombres entiers et ne nous confusons pas avec
                 # 49.29320 ans
      interval(start = date_de_naissance, end = today()) %>% # intervalle entre la date de
                                                             # naissance et aujourd'hui
        time_length(unit = "years") # l'unité de l'intervalle est l'année
    )
  ) %>%
  select( # sélection des colonnes qui nous intéressent.
    nom_de_l_elu,
    prenom_de_l_elu,
    code_sexe,
    libelle_du_departement,
    date_de_naissance,
    age
  )

table_deputes_age_seulement <- table_deputes_avec_ages %>%
  group_by(age) %>%
  summarise()
print('Les âges sont :')
print(table_deputes_age_seulement)

# Quel est l'âge maximal, l'âge moyen, l'âge minimal pour les hommes et les femmes
table_deputes_statistiques_age_seulement <- table_deputes_avec_ages %>%
  group_by(code_sexe) %>%
  summarise(
    age_moyen = mean(age), # moyenne de tous les âges du tableau
    age_max = max(age), # le plus grand âge du tableau
    age_min = min(age) # le plus petit âge du tableau
  )
print(str_c(
  'L\'âge moyen des députés est égal à',
  round(table_deputes_avec_ages %>%
    summarise(
      age_moyen = mean(age)
    ), digits = 2),
  'ans',
  sep = ' '
))
print(str_c(
  'L\'âge moyen des députées femmes est égal à',
  round(table_deputes_statistiques_age_seulement %>%
    filter(code_sexe == 'F') %>%
    summarise(age_moyen), digits = 2
  ),
  'ans',
  sep = ' '
))
print(str_c(
  'L\'âge moyen des députés hommes est égal à',
  round(table_deputes_statistiques_age_seulement %>%
    filter(code_sexe == 'M') %>%
    summarise(age_moyen), digits = 2
  ),
  'ans',
  sep = ' '
))
print(str_c(
  'L\'âge minimum d\'une députée femme est égal à',
  round(table_deputes_statistiques_age_seulement %>%
    filter(code_sexe == 'F') %>%
    summarise(age_min), digits = 2
  ),
  'ans',
  sep = ' '
))
print(str_c(
  'L\'âge minimum d\'un député homme est égal à',
  round(table_deputes_statistiques_age_seulement %>%
    filter(code_sexe == 'M') %>%
    summarise(age_min), digits = 2
  ),
  'ans',
  sep = ' '
))
print(str_c(
  'L\'âge maximum atteint par une députée femme est égal à',
  round(table_deputes_statistiques_age_seulement %>%
    filter(code_sexe == 'F') %>%
    summarise(age_max), digits = 2
  ),
  'ans',
  sep = ' '
))
print(str_c(
  'L\'âge maximum atteint par un député homme est égal à',
  round(table_deputes_statistiques_age_seulement %>%
    filter(code_sexe == 'M') %>%
    summarise(age_max), digits = 2
  ),
  'ans',
  sep = ' '
))

table_deputes_statistiques_age_format_ggplot <- melt(
  table_deputes_statistiques_age_seulement
)
table_deputes_statistiques_age_format_ggplot %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = variable,
      y = value,
      fill = c('blue', 'red', 'blue', 'red', 'blue', 'red')
    ),
    show.legend = FALSE,
    stat = 'identity'
  ) + 
  scale_x_discrete(
    name = "",
    labels = c(
      "age_moyen" = "Âge moyen",
      "age_min" = "Âge minimal", 
      "age_max" = "Âge maximal"
    )
  ) +
  scale_y_continuous(
    name = "Âge"
  ) +
  labs(
    title = "Âge moyen, minimum et maximum des députés",
    subtitle = "Classement par sexe"
  ) +
  facet_grid(
    cols = vars(code_sexe),
    labeller = as_labeller(
      c('F' = 'Femme', 'M' = 'Homme')
    ),
    scale = "free"
  )

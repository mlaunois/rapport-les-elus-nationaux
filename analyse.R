library(dplyr)
library(readr)
library(tricky)
library(reshape2)
library(ggplot2)
library(forcats)
library(lubridate)

table_maires <- read_tsv(
  file = "9-rne-maires.txt", 
  skip = 1, 
  locale = locale(encoding = "ISO-8859-1")
  ) %>%
  set_standard_names()

table_maires %>% names()
table_maires %>% count()
table_maires %>% count(code_du_departement_maire_)

table_maires_nombre_maires_departement <-
  table_maires %>% count(libelle_de_departement_maires_, sort = TRUE)
table_maires_nombre_maires_profession <-
  table_maires %>% count(libelle_de_la_profession, sort = TRUE)
table_maires_nombre_maires_sexe <-
  table_maires %>% count(code_sexe, sort = TRUE)
table_maires_nombre_maires_prenom <-
  table_maires %>% count(prenom_de_l_elu, sort = TRUE)
table_maires_nombre_maires_nom <-
  table_maires %>% count(nom_de_l_elu, sort = TRUE)
table_maires_nombre_maires_date_debut_mandat <-
  table_maires %>% count(date_de_debut_du_mandat, sort = TRUE)

table_maires %>% count(libelle_de_departement_maires_, sort = TRUE) %>%
  View('Ordre croissant du nombre de maires par libellé de département')

table_maires_nombre_maires_sexe %>%
  ggplot() +
  geom_col(
    mapping = aes(x = code_sexe, y = n)
  ) + 
  scale_x_discrete(
    name = "Sexe",
    labels = c("F" = "Femmes", "M" = "Hommes")
  )

table_maires_nombre_maires_profession %>%
  ggplot() +
  geom_col(
    mapping = aes(x = libelle_de_la_profession, y = n)
  ) + 
  coord_flip()

table_maires_nombre_maires_profession %>% 
  slice(1:20)

table_maires_nombre_maires_profession %>% 
  filter(n >= 500)

table_maires_nombre_maires_profession %>%
  filter(n >= 500) %>%
  ggplot() +
  geom_col(
    mapping = aes(x = fct_reorder(.f = libelle_de_la_profession, .x = n, .desc = FALSE), y = n)
  ) + 
  coord_flip() +
  scale_x_discrete(name = "Profession") + 
  scale_y_continuous(name = "Nombre de maires") + 
  labs(
    title = "Professions les plus représentées", 
    caption = "Professions avec au moins 500 personnes"
    )

table_maires_nombre_maires_prenom %>%
  slice(1:20) %>%
  ggplot() +
  geom_col(
    mapping = aes(x = fct_reorder(.f = prenom_de_l_elu, .x = n, .desc = FALSE), y = n)
  ) + 
  coord_flip() +
  scale_x_discrete(name = "Prénom") + 
  scale_y_continuous(name = "Nombre de maires") + 
  labs(
    title = "Prénoms de maires les plus répandus", 
    caption = "Les 20 prénoms de maires les plus répandus"
  )

table_maires %>% 
  count(prenom_de_l_elu, code_sexe) %>%
  group_by(code_sexe) %>%
  arrange(desc(n)) %>% 
  slice(1:20)  %>%
  ggplot() +
  geom_col(
    mapping = aes(x = fct_reorder(.f = prenom_de_l_elu, .x = n, .desc = FALSE), y = n)
  ) + 
  coord_flip() +
  scale_x_discrete(name = "Prénom") + 
  scale_y_continuous(name = "Nombre de maires") + 
  labs(
    title = "Prénoms de maires les plus répandus", 
    caption = "Les 20 prénoms de maires les plus répandus"
  ) + 
  facet_wrap(vars(code_sexe), scales = "free")

table_maires_nombre_maires_nom %>%
  slice(1:20) %>%
  ggplot() +
  geom_col(
    mapping = aes(x = fct_reorder(.f = nom_de_l_elu, .x = n, .desc = FALSE), y = n)
  ) + 
  coord_flip() +
  scale_x_discrete(name = "Nom") + 
  scale_y_continuous(name = "Nombre de maires") + 
  labs(
    title = "Noms de maires les plus répandus", 
    caption = "Les 20 noms de maires les plus répandus"
  )

# Quel est le départmenet où il y a le plus de maires ? Count par département
# Quelles sont les professions les plus fréquentes ? Count par profession
# Quel est le nombre d'hommes ? de femmes ? Count par masculin féminin
# Quels sont les prénoms les plus fréquents ? Count par prénom

# Age de maxime
interval(start = dmy("17/9/2004"), end = today()) %>% time_length(unit = "years")
interval(start = dmy("17/9/2004"), end = today()) %>% time_length(unit = "months")
interval(start = dmy("17/9/2004"), end = today()) %>% time_length(unit = "days")
interval(start = dmy("17/9/2004"), end = today()) %>% time_length(unit = "seconds")

table_maires %>% glimpse()

table_maires2 <- table_maires %>%
  mutate(
    date_de_naissance = dmy(date_de_naissance), 
    annee_de_naissance = year(date_de_naissance),
    jour_de_naissance = wday(date_de_naissance, label = TRUE, abbr = FALSE), 
    age = floor(
      interval(start = date_de_naissance, end = today()) %>% time_length(unit = "years")
    )
  ) %>%
  select(nom_de_l_elu, prenom_de_l_elu, code_sexe, libelle_de_departement_maires_, libelle_de_la_commune, date_de_naissance, age) 

table_maires2 %>% View()

table_maires2 %>%
  summarise(
    age_moyen = mean(age), 
    age_max = max(age),
    age_min = min(age)
  )

# Calculer l'âge minimum
table_maires3 <- table_maires2 %>%
  group_by(code_sexe) %>%
  summarise(
    age_moyen = mean(age),
    age_max = max(age),
    age_min = min(age)
  )
# Calculer l'âge moyen l'âge minimum et l'âge maximum par code sexe

table_maires <- melt(table_maires3)
table_maires4 %>%
  ggplot(aes(x = variable, y = value)) +
  geom_bar(stat = 'identity') + 
  scale_x_discrete(
    name = "",
    labels = c("age_moyen" = "Âge moyen", "age_min" = "Âge minimal", "age_max" = "Âge maximal")
  ) +
  scale_y_continuous(
    name = "Âge"
  ) +
  labs(
    title = "Âge moyen, minimum et maximum des maires",
    subtitle = "Classement par sexe"
  ) +
  facet_grid(code_sexe ~ ., labeller = as_labeller(
    c('F' = 'Femme', 'M' = 'Homme')), scale = "free")

# Faire le graphique

table_maires5 <- table_maires2 %>%
  group_by(libelle_de_departement_maires_) %>%
  summarise(
    age_moyen = mean(age),
    age_max = max(age),
    age_min = min(age)
  ) %>% melt()
# Calculer l'âge moyen l'âge minimum et l'âge max par libelle_de_departement_maires

table_maires5 %>%
  ggplot(aes(x = variable, y = value)) +
  geom_bar(stat = 'identity') + 
  scale_y_continuous(
    name = "Âge"
  ) +
  labs(
    title = "Âge moyen, minimum et maximum des maires",
    subtitle = "Classement par département"
  ) +
  facet_grid(libelle_de_departement_maires_ ~ ., labeller = as_labeller(
    c('F' = 'Femme', 'M' = 'Homme')), scale = "free")
# Faire le graphique


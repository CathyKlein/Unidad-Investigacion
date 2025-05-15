# library loading
library(dplyr)
library(arrow)
library(purrr)
library(stringr)
library(janitor)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)

# Age distribution
data_2019_pacientes %>%  names()
data_2019_pacientes %>%  glimpse()

data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  count(diagnostico_cancer) %>%
  mutate(pct = round(100 * n / sum(n), 2))

data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  group_by(diagnostico_cancer) %>%
  summarise(mean_age = round(mean(edad, na.rm = TRUE), 2))

t.test(
  edad ~ diagnostico_cancer,
  data = data_2019_pacientes %>% filter(edad >= 15, UPC == TRUE)
)

# Distribution by age groups
data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  count(diagnostico_cancer, grupo_edad) %>%
  group_by(diagnostico_cancer) %>%
  mutate(
    pct = round(100 * n / sum(n), 2)
  ) %>%
  ungroup()

# Bivariate analysis age groups and cancer diagnosis
tabla_chi <- data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  count(grupo_edad, diagnostico_cancer) %>%
  tidyr::pivot_wider(
    names_from = diagnostico_cancer,
    values_from = n,
    values_fill = 0
  ) %>%
  column_to_rownames("grupo_edad") %>%
  as.matrix()

chisq.test(tabla_chi)

# Distribution UPC cancer_diagnosis by sex
data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  count(diagnostico_cancer, sexo) %>%
  group_by(diagnostico_cancer) %>%
  mutate(
    pct = round(100 * n / sum(n), 2)
  ) %>%
  ungroup()

tabla_sexo <- data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  count(sexo, diagnostico_cancer) %>%
  pivot_wider(
    names_from = diagnostico_cancer,
    values_from = n,
    values_fill = 0
  ) %>%
  column_to_rownames("sexo") %>%
  as.matrix()

chisq.test(tabla_sexo)

# Distribution of nationality
data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  count(diagnostico_cancer, nacionalidad_dico) %>%
  group_by(diagnostico_cancer) %>%
  mutate(
    pct = round(100 * n / sum(n), 2)
  ) %>%
  ungroup()

tabla_nacionalidad <- data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  count(nacionalidad_dico, diagnostico_cancer) %>%
  pivot_wider(
    names_from = diagnostico_cancer,
    values_from = n,
    values_fill = 0
  ) %>%
  column_to_rownames("nacionalidad_dico") %>%
  as.matrix()

chisq.test(tabla_nacionalidad)

# Distribution by region

data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  count(region, diagnostico_cancer) %>%
  group_by(region) %>%
  mutate(
    pct = round(100 * n / sum(n), 2)
  ) %>%
  ungroup() %>%
  print(n = Inf)  # to see all rows

# Distribution by macrozona

data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  count(macrozona, diagnostico_cancer) %>%
  group_by(macrozona) %>%
  mutate(
    pct = round(100 * n / sum(n), 2)
  ) %>%
  ungroup() %>%
  print(n = Inf)  # to see all rows

# Calculating population for macrozona
pob_region <- data.frame(
  region = c(
    "Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama",
    "Valparaíso", "Coquimbo", "Metropolitana de Santiago",
    "Libertador General Bernardo O'Higgins", "Maule", "Ñuble",
    "Biobío", "Los Lagos", "Magallanes y de la Antártica Chilena",
    "La Araucanía", "Los Ríos", "Aysén del General Carlos Ibáñez del Campo"
  ),
  poblacion = c(
    261779, 410903, 722120, 320864,
    2040166, 889273, 8471244,
    1033238, 1181074, 523876,
    1690616, 916721, 184216,
    1035918, 414293, 108744
  )
)

data_2019_pacientes <- data_2019_pacientes %>%
  left_join(pob_region, by = "region")

# Calculate rates for CPU for cancer and non cancer patients
upc_cancer_macrozona <- data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE, !is.na(macrozona)) %>%
  count(macrozona, diagnostico_cancer)

macrozona_poblacion <- data.frame(
  macrozona = c(
    "macrozona norte", "macrozona centro", "macrozona metropolitana",
    "macrozona centro sur", "macrozona sur", "macrozona austral"
  ),
  poblacion = c(
    261779 + 410903 + 722120 + 320864,  # norte
    889273 + 2040166,                   # centro
    8471244,                            # metropolitana
    1033238 + 1181074 + 523876 + 1690616, # centro sur
    1035918 + 414293 + 916721,          # sur
    108744 + 184216                     # austral
  )
)

tasa_cancer_macrozona <- upc_cancer_macrozona %>%
  left_join(macrozona_poblacion, by = "macrozona") %>%
  mutate(rate_100k = round((n / poblacion) * 100000, 2))

# Create a proper graph
# Define and reverse the order
orden_macrozona <- c(
  "macrozona norte",
  "macrozona centro",
  "macrozona metropolitana",
  "macrozona centro sur",
  "macrozona sur",
  "macrozona austral"
)

# Apply reversed order to display from top to bottom
tasa_cancer_macrozona_orden <- tasa_cancer_macrozona %>%
  filter(macrozona %in% orden_macrozona) %>%
  mutate(macrozona = factor(macrozona, levels = rev(orden_macrozona)))  # <--- REVERSE

# Plot again with flipped axes
ggplot(tasa_cancer_macrozona_orden, aes(x = macrozona, y = rate_100k, fill = diagnostico_cancer)) +
  geom_col(position = "dodge", color = "black", width = 0.6) +
  coord_flip() +
  geom_text(
    aes(label = round(rate_100k, 1)),
    position = position_dodge(width = 0.6),
    hjust = -0.2,
    size = 4,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("no_oncologico" = "#66B2FF", "oncologico" = "#66CC66"),
    labels = c("No Oncológico", "Oncológico")
  ) +
  labs(
    title = "Distribución de uso UPC según diagnóstico \nde cáncer tasa por 100.000 habitantes",
    x = "Macrozona",
    y = "Tasa por 100.000",
    fill = "Diagnóstico"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  ) +
  expand_limits(y = max(tasa_cancer_macrozona_orden$rate_100k) * 1.1)

ggsave(
  filename = "fig_UPC_macrozona_cancer.png",
  path = "/Users/catherineklein/Desktop/DOCTORADO ICIM/UNIDAD DE INVESTIGACION/GRD_2019_LIMPIO/05_figures",
  width = 10,
  height = 6,
  dpi = 300
)

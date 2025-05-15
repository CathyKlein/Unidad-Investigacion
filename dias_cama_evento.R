# dias cama a nivel de eventos

library(tidyverse)

# Use of df data_2019_eventos
eventos <- read_rds("02_outputs/data_2019_eventos.rds")

eventos

eventos %>%  glimpse()

eventos %>%  distinct(servicioingreso)

eventos %>%  distinct(serviciotraslado1)
eventos %>%  distinct(serviciotraslado2)

eventos %>%  
  filter(!is.na(serviciotraslado1)) %>%  
  slice(100)

eventos %>%  
  select(starts_with("fecha")) 

eventos %>%  
  slice(6) %>%  
  glimpse()


# ids
eventos2 <- eventos %>% 
  mutate(id = 1:n()) %>%  
  relocate(id, .before = 1) %>% 
  # calculate age filter (>=15 years old)
  mutate(edad = fechaalta - fecha_nacimiento) %>%  
  mutate(edad = as.numeric(edad)/ 365.25) %>% 
  relocate(edad, fechaalta, .after = fecha_nacimiento) %>%  
  filter(edad >= 15)

upc_units <- c(
  # Critical Care Units (UCI)
  "UNIDAD DE CUIDADOS INTENSIVOS (UCI) (INDIFERENCIADO)",
  "UNIDAD DE CUIDADOS INTENSIVOS ADULTO",
  "UNIDAD DE CUIDADOS INTENSIVOS CARDIOLOGÍA",
  "UNIDAD DE CUIDADOS INTENSIVOS CARDIVASCULAR",
  # "UNIDAD DE CUIDADOS INTENSIVOS PEDIATRÍA",
  "UNIDAD DE CUIDADOS INTENSIVOS QUEMADOS",
  
  # Intermediate Care Units (UTI)
  "UNIDAD DE TRATAMIENTO INTERMEDIO (UTI) (INDIFERENCIADO) ADULTO",
  "UNIDAD DE TRATAMIENTO INTERMEDIO CORONARIO",
  "UNIDAD DE TRATAMIENTO INTERMEDIO MEDICINA ADULTO",
  "UNIDAD DE TRATAMIENTO INTERMEDIO CARDIOVASCULAR",
  "UNIDAD DE TRATAMIENTO INTERMEDIO CIRUGÍA ADULTO",
  "UNIDAD DE TRATAMIENTO INTERMEDIO NEUROQUIRÚRGICO",
  "UNIDAD DE TRATAMIENTO INTERMEDIO QUEMADOS",
  "UNIDAD DE TRATAMIENTO INTERMEDIO NEUROLOGIA"
)

# from ids evento, calculate how many used critical care unit either in entry or traslado.
eventos_upc <- eventos2 %>%  
  select(id, servicioingreso, contains("serviciotraslado")) %>%  
  pivot_longer(cols = starts_with("servicio")) %>%  
  filter(value %in% upc_units)
  # filter(str_detect(value, "UNIDAD DE CUIDADOS INTENSIVOS|UNIDAD DE TRATAMIENTO INTERMEDIO"))

# A pesar que filtre por mayores de 15 años, hay adultos con registro de unidad de UPC == neonatologia (error)
# excluir pediatria, neonatologia, etc.
eventos_upc_incorrectos <- eventos2 %>%  
  select(id, servicioingreso, contains("serviciotraslado")) %>%  
  pivot_longer(cols = starts_with("servicio")) %>%  
  filter(value %in% c("UNIDAD DE CUIDADOS INTENSIVOS NEONATOLOGÍA",
                      "UNIDAD DE CUIDADOS INTENSIVOS PEDIATRÍA",
                      "UNIDAD DE TRATAMIENTO INTERMEDIO NEONATOLOGÍA",
                      "UNIDAD DE TRATAMIENTO INTERMEDIO PEDIATRÍA"))

eventos_upc %>%  
  distinct(value)

eventos2 %>%  
  filter(id %in% eventos_upc$id)


# eventos3 <- eventos2 |>
#   # crear variable de eventos que pasaron en algun traslado por upc
#   mutate(pasa_por_upc = id %in% eventos_upc$id) |>
#   relocate(pasa_por_upc, .after = id) |>
#   # remover eventos que en en algun momento pasaron por neonatología, pediatría, etc.
#   filter(!id %in% eventos_upc_incorrectos$id)
# 
# eventos3 |>
#   select(id, pasa_por_upc, servicioingreso, starts_with("serviciotraslado")) |>
#   print(n=30)
# 
# eventos2
# 
# eventos3
# 
# 
# # revisar neonatologia
# ids_neonato <- eventos_upc |>
#   filter(value == "UNIDAD DE CUIDADOS INTENSIVOS NEONATOLOGÍA") |>
#   pull(id)
# 
# ids_neonato
# 
# eventos2 |>
#   filter(id %in% ids_neonato)

eventos3 <- eventos2 %>% 
  # # crear variable de eventos que pasaron en algun traslado por upc
  # mutate(pasa_por_upc = id %in% eventos_upc$id) |>
  # relocate(pasa_por_upc, .after = id) |>
  # remover eventos que en en algun momento pasaron por neonatología, pediatría, etc.
  filter(!id %in% eventos_upc_incorrectos$id)

# Cancer distribution among critical care events
eventos_cancer <- eventos3 %>%  
  select(id, contains("diagnostico")) %>%  
  pivot_longer(cols = contains("diagnostico"))

cancer_icd10_codes <- c(
  "C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14",
  "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C34", "C25", "C26",
  "C30", "C31", "C32", "C33", "C34", "C35", "C36", "C37", "C38", "C39",
  "C40", "C41",
  "C43", "C44",
  "C45", "C46", "C47", "C48", "C49",
  "C50",
  "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58",
  "C60", "C61", "C62", "C63",
  "C64", "C65", "C66", "C67", "C68",
  "C69", "C70", "C71", "C72",
  "C73", "C74", "C75",
  "C76", "C77", "C78", "C79", "C80",
  "C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", "C90", "C91", "C92", "C93", "C94", "C95", "C96",
  "C97",
  "D00", "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09",
  "D37", "D38", "D39", "D40", "D41", "D42", "D43", "D44", "D45", "D46", "D47", "D48"
)

cancer_icd10_codes_str <- cancer_icd10_codes %>%  paste(collapse = "|")

eventos_cancer2 <- eventos_cancer %>%  
  filter(str_detect(value, cancer_icd10_codes_str))

eventos_cancer2$id


# Calculate hospitalization days —----

eventos_long_traslado <- eventos3 %>%  
  select(id, servicioingreso, contains("serviciotraslado")) %>%  
  pivot_longer(cols = starts_with("servicio"),
               values_to = "unidad") %>%  
  mutate(movimiento = str_extract(name, "\\d+"),
         movimiento = replace_na(movimiento, "0")) %>%  
  select(-name)


eventos_long_fechas <- eventos3 %>%  
  select(id, fecha_ingreso, contains("fechatraslado")) %>%  
  pivot_longer(cols = starts_with("fecha"),
               values_to = "fecha") %>%  
  mutate(movimiento = str_extract(name, "\\d+"),
         movimiento = replace_na(movimiento, "0")) %>%  
  select(-name)


eventos_long_traslado
eventos_long_fechas


eventos4 <- left_join(eventos_long_traslado,
                      eventos_long_fechas,
                      by = c("id", "movimiento")) %>%  
  relocate(movimiento, .after = id)

# Add upc event
eventos5 <- eventos4 %>%  
  # mutate(upc = str_detect(unidad, "UNIDAD DE CUIDADOS INTENSIVOS|UNIDAD DE TRATAMIENTO INTERMEDIO"))
  mutate(upc = id %in% eventos_upc$id)

# calculate hospitalization days
eventos6 <- eventos5 |> 
  # solamente que hayan pasado por upc
  filter(upc) |> 
  mutate(dias = fecha - lead(fecha))


# add fecha alta to calculate hospitalization days when upc unit was the last unit.
eventos7 <- eventos6 %>%  
  left_join(eventos3 %>%  select(id, fechaalta),
            by = "id") %>% 
  # only if days == NA then use fecha alta to calculate
  mutate(dias = if_else(!is.na(fecha) & is.na(dias), 
                       fecha - fechaalta,
                       dias)) %>%  
  mutate(dias = as.numeric(dias) %>%  abs())
  
# agregar si tiene diagnóstico de cáncer
eventos8 <- eventos7 %>%  
  mutate(cancer = ifelse(id %in% eventos_cancer2$id, "sí", "no"))


percentil_90 <- eventos8 %>%  summarise(quantile(dias, .99, na.rm = T)) %>%  pull()

# removing outliers (p90)
eventos8b <- eventos8 %>%  filter(is.na(dias) | dias <= percentil_90)

library(ggplot2)

dias_cama_cancer <- eventos8b %>%  
  ggplot() +
  aes(dias, fill = cancer) +
  geom_histogram() +
  facet_wrap(~cancer, ncol = 1, scales = "free_y")

ggsave("dias_cama_cancer.png", dias_cama_cancer, width = 8, height = 6, dpi = 300)

eventos8b |> 
  ggplot() +
  aes(dias) +
  geom_histogram() +
  facet_wrap(~cancer, ncol = 1) +
  scale_y_log10()


# results ----

# solamente el movimiento por upc
eventos8b %>%  
  filter(str_detect(unidad, "UNIDAD DE CUIDADOS INTENSIVOS|UNIDAD DE TRATAMIENTO INTERMEDIO"))

# por unidad
eventos8b %>%  
  filter(str_detect(unidad, "UNIDAD DE CUIDADOS INTENSIVOS|UNIDAD DE TRATAMIENTO INTERMEDIO")) %>%  
  group_by(unidad) %>%  
  summarize(promedio = mean(dias, na.rm = TRUE),
            minima = min(dias, na.rm = TRUE),
            mediana = median(dias, na.rm = TRUE),
            maxima = max(dias, na.rm = TRUE),
            iqr = IQR(dias, na.rm = TRUE))

# por unidad y por oncologico
eventos8b %>%  
  filter(str_detect(unidad, "UNIDAD DE CUIDADOS INTENSIVOS|UNIDAD DE TRATAMIENTO INTERMEDIO")) %>%  
  group_by(unidad, cancer) %>%  
  summarize(#promedio = mean(dias, na.rm = TRUE),
            # minima = min(dias, na.rm = TRUE),
            mediana = median(dias, na.rm = TRUE),
            # maxima = max(dias, na.rm = TRUE),
            iqr = IQR(dias, na.rm = TRUE)) %>%  
  pivot_wider(names_from = cancer, values_from = c(mediana, iqr))

# ALL types of critical care units grouped
eventos8b %>%  
  filter(str_detect(unidad, "UNIDAD DE CUIDADOS INTENSIVOS|UNIDAD DE TRATAMIENTO INTERMEDIO")) %>%  
  summarize(promedio = mean(dias, na.rm = TRUE),
            minima = min(dias, na.rm = TRUE),
            mediana = median(dias, na.rm = TRUE),
            maxima = max(dias, na.rm = TRUE),
            iqr = IQR(dias, na.rm = TRUE))

# de todos
eventos8b %>%  
  filter(str_detect(unidad, "UNIDAD DE CUIDADOS INTENSIVOS|UNIDAD DE TRATAMIENTO INTERMEDIO")) %>%  
  group_by(cancer) %>%  
  summarize(promedio = mean(dias, na.rm = TRUE),
            minima = min(dias, na.rm = TRUE),
            mediana = median(dias, na.rm = TRUE),
            maxima = max(dias, na.rm = TRUE),
            iqr = IQR(dias, na.rm = TRUE))

wilcox.test(dias ~ cancer,
            data = eventos8b %>%
              filter(str_detect(unidad, "UNIDAD DE CUIDADOS INTENSIVOS|UNIDAD DE TRATAMIENTO INTERMEDIO")))

eventos8b %>%
  filter(str_detect(unidad, "UNIDAD DE CUIDADOS INTENSIVOS|UNIDAD DE TRATAMIENTO INTERMEDIO")) %>%
  ggplot(aes(x = dias, fill = cancer)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Distribución de días hospitalizados en UCI por grupo",
       x = "Días", y = "Densidad")

ggsave("distribucion_dias_uci.png", width = 8, height = 5, dpi = 300)

saveRDS(eventos8, file = "02_outputs/eventos8_hospitalization_days.rds")
saveRDS(eventos8b, file = "02_outputs/eventos8_hospitalization_days_no_outliers.rds")


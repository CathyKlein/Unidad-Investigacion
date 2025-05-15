# Script: 01_clean_data.R
# Purpose: Explore, create, and clean the GRD_2019 data set for ICU and further cancer study.
# Author: Catherine Klein
# Date: 2025-04-27
# Notes: This script loads raw data and prepares a clean version for further analysis using patient (CIP_ENCRIPTADO) as unit of analysis

# library loading
library(dplyr)
library(arrow)
library(purrr)
library(stringr)
library(janitor)
library(tidyr)
library(tibble)

# Data loading, removing variables that will not be used and exploring.
data_2019 <- arrow::read_parquet("00_data/GRD_PUBLICO_2019.parquet")

# Explore variable names
data_2019 %>%  names()
data_2019 %>%  glimpse()


# Remove the variables that will not be used in this data set
variables_to_clean <- c(
  "TIPO_PROCEDENCIA",
  "CONDICIONDEALTANEONATO1", "PESORN1", "SEXORN1", "RN1ESTADO",
  "CONDICIONDEALTANEONATO2", "PESORN2", "SEXORN2", "RN2ESTADO",
  "CONDICIONDEALTANEONATO3", "PESORN3", "SEXORN3", "RN3ESTADO",
  "CONDICIONDEALTANEONATO4", "PESORN4", "SEXORN4", "RN4ESTADO",
  "MEDICOALTA_ENCRIPTADO", "USOSPABELLON", "TIPO_INGRESO",
  "FECHAPROCEDIMIENTO1", "FECHAINTERV1", "MEDICOINTERV1_ENCRIPTADO",
  "ESPECIALIDADINTERVENCION", "HOSPPROCEDENCIA", "TIPO_ACTIVIDAD",
  "ESPECIALIDAD_MEDICA"
)

data_2019_clean <- data_2019[, !(names(data_2019) %in% variables_to_clean)]

# View new df
data_2019_clean |> names()

# Standardizing variable names
data_2019_clean <- data_2019_clean %>%
  clean_names() 

# Checking variable types
str(data_2019_clean)
data_2019_clean <- data_2019_clean %>%
  mutate(
    fecha_nacimiento = as.Date(fecha_nacimiento),
    fechaalta = as.Date(fechaalta),
    fecha_ingreso = as.Date(fecha_ingreso),
    fechatraslado1 = as.Date(fechatraslado1),
    fechatraslado2 = as.Date(fechatraslado2),
    fechatraslado3 = as.Date(fechatraslado3),
    fechatraslado4 = as.Date(fechatraslado4),
    fechatraslado5 = as.Date(fechatraslado5),
    fechatraslado6 = as.Date(fechatraslado6),
    fechatraslado7 = as.Date(fechatraslado7),
    fechatraslado8 = as.Date(fechatraslado8),
    fechatraslado9 = as.Date(fechatraslado9)
  )

data_2019_clean <- data_2019_clean %>%
  mutate(across(starts_with("procedimiento"), as.character))

# Creating new variables for analysis

# How many patients had multiple hospitalizations
data_2019 %>%
  count(CIP_ENCRIPTADO) %>%
  filter(n > 1) %>%
  summarise(
    paciente_multiple_hosp= n(), #165.146 pacientes con multiples hospitalizaciones
  )

# 1. Base full con hospitalizaciones para analizar posteriormente dias cama y otros (U.Analisis evento)
data_2019_eventos <- data_2019_clean 

# 2. Calculo de variable edad usando la hospitalizacion mas reciente por paciente
data_2019_pacientes <- data_2019_clean %>%
  group_by(cip_encriptado) %>%
  slice_max(fecha_ingreso, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    edad = as.numeric((fecha_ingreso - fecha_nacimiento) / 365.25)
  )

data_2019_pacientes <- data_2019_pacientes %>%
  mutate(
    grupo_edad = case_when(
      edad < 15              ~ "≤14 años",
      edad >= 15 & edad < 45 ~ "15-44 años",
      edad >= 45 & edad < 65 ~ "45-64 años",
      edad >= 65             ~ "≥65 años"
    )
  )

# Variable nacionalidad
data_2019_pacientes <- data_2019_pacientes %>%
  mutate(nacionalidad_dico = case_when(
    nacionalidad == "CHILE" ~ "CHILE",
    TRUE ~ "OTROS"
  ))

# Variable tipo de alta
data_2019_pacientes <- data_2019_pacientes %>%
  mutate(tipoalta_grupo = case_when(
    tipoalta == "FALLECIDO" ~ "FALLECIDO",
    tipoalta %in% c("DESCONOCIDO", "NO IDENTIFICADA") ~ "DESCONOCIDO",
    TRUE ~ "OTROS"
  ))

data_2019_pacientes %>%
  group_by(tipoalta_grupo) %>%
  summarise(
    Frecuencia = n(),
    Porcentaje = round(n() / nrow(data_2019_clean) * 100, 2)
  )

# Save clean patients dataset
saveRDS(data_2019_pacientes, file = "02_outputs/data_2019_pacientes.rds")
saveRDS(data_2019_pacientes_diag_cancer, file = "02_outputs/data_2019_pacientes_diag_cancer.rds")
saveRDS(data_2019_eventos, file = "02_outputs/data_2019_eventos.rds")

# Later or after restart
data_2019_pacientes <- readRDS("02_outputs/data_2019_pacientes.rds")

# Variable PREVISION 
# Grouping insurance types into 4 categories: FONASA, PRIVADO, FF.AA, DESCONOCIDO
data_2019_pacientes <- data_2019_pacientes %>%
  mutate(
    prevision_grupo = case_when(
      prevision %in% c(
        "FONASA INSTITUCIONAL - (MAI) A",
        "FONASA INSTITUCIONAL - (MAI) B",
        "FONASA INSTITUCIONAL - (MAI) C",
        "FONASA INSTITUCIONAL - (MAI) D",
        "FONASA LIBRE ELECCIÓN (FMLE_B)",
        "FONASA LIBRE ELECCIÓN (FMLE_C)",
        "FONASA LIBRE ELECCIÓN (FMLE_D)"
      ) ~ "FONASA",
      
      prevision %in% c(
        "ISAPRE",
        "PARTICULAR"
      ) ~ "PRIVADO",
      
      prevision %in% c(
        "DIPRECA",
        "CAPREDENA",
        "CAJA DE PREVISIÓN FFAA (SISA)"
      ) ~ "FF.AA",
      
      prevision %in% c(
        "NO IDENTIFICADA",
        "NO CONSIGNADO",
        "DESCONOCIDO"
      ) ~ "DESCONOCIDO",
      
      TRUE ~ NA_character_
    )
  ) 


# Create variable REGION and MACROZONA 
provincia_region <- tibble::tibble(
  provincia = c(
    "AISEN", "ANTÁRTICA CHILENA", "ANTOFAGASTA", "ARAUCO", "ARICA", "BIO-BIO", 
    "CACHAPOAL", "CAPITAN PRAT", "CARDENAL CARO", "CAUQUENES", "CAUTIN", "CHACABUCO", 
    "CHAÑARAL", "CHILOE", "CHOAPA", "COIHAIQUE", "COLCHAGUA", "CONCEPCION", "COPIAPO", 
    "CORDILLERA", "CURICO", "DESCONOCIDO", "DIGUILLÍN", "EL LOA", "ELQUI", 
    "GENERAL CARRERA", "HUASCO", "IQUIQUE", "ISLA DE PASCUA", "ITATA", "LIMARI", 
    "LINARES", "LLANQUIHUE", "LOS ANDES", "MAGALLANES", "MAIPO", "MALLECO", 
    "MARGA MARGA", "MELIPILLA", "ÑUBLE", "OSORNO", "PALENA", "PARINACOTA", 
    "PETORCA", "PUNILLA", "QUILLOTA", "RANCO", "SAN ANTONIO", "SAN FELIPE", 
    "SANTIAGO", "TALAGANTE", "TALCA", "TAMARUGAL", "TIERRA DEL FUEGO", "TOCOPILLA", 
    "ULTIMA ESPERANZA", "VALDIVIA", "VALPARAISO"
  ),
  region = c(
    "Aysén del General Carlos Ibáñez del Campo", "Magallanes y de la Antártica Chilena", "Antofagasta", 
    "Biobío", "Arica y Parinacota", "Biobío", "Libertador General Bernardo O'Higgins", 
    "Aysén del General Carlos Ibáñez del Campo", "Libertador General Bernardo O'Higgins", 
    "Maule", "La Araucanía", "Metropolitana de Santiago", "Atacama", "Los Lagos", 
    "Coquimbo", "Aysén del General Carlos Ibáñez del Campo", "Libertador General Bernardo O'Higgins", 
    "Biobío", "Atacama", "Metropolitana de Santiago", "Maule", "DESCONOCIDO", 
    "Ñuble", "Antofagasta", "Coquimbo", "Aysén del General Carlos Ibáñez del Campo", 
    "Atacama", "Tarapacá", "Valparaíso", "Metropolitana de Santiago", "Coquimbo", 
    "Maule", "Los Lagos", "Valparaíso", "Magallanes y de la Antártica Chilena", 
    "Metropolitana de Santiago", "La Araucanía", "Valparaíso", "Metropolitana de Santiago", 
    "Ñuble", "Los Lagos", "Los Lagos", "Arica y Parinacota", "Valparaíso", 
    "Ñuble", "Valparaíso", "Los Ríos", "Valparaíso", "Valparaíso", 
    "Metropolitana de Santiago", "Metropolitana de Santiago", "Maule", "Tarapacá", 
    "Magallanes y de la Antártica Chilena", "Antofagasta", "Magallanes y de la Antártica Chilena", 
    "Los Ríos", "Valparaíso"
  )
)

data_2019_pacientes <- data_2019_pacientes %>%
  left_join(provincia_region, by = "provincia")

data_2019_pacientes %>%
  filter(is.na(region)) %>%
  count(provincia) # no mismatch

# data_2019_pacientes <- data_2019_pacientes %>% 
#  mutate(
#    macrozona = case_when(
#      region %in% c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama") ~ "macrozona norte",
#      region %in% c("Coquimbo", "Valparaíso") ~ "macrozona centro",
#      region == "Metropolitana de Santiago" ~ "macrozona metropolitana",
#      region %in% c("Libertador General Bernardo O'Higgins", "Maule", "Ñuble", "Biobío") ~ "macrozona centro sur",
#      region %in% c("La Araucanía", "Los Ríos", "Los Lagos") ~ "macrozona sur",
#      region %in% c("Aysén del General Carlos Ibáñez del Campo", "Magallanes y de la Antártica Chilena") ~ "macrozona austral",
#      TRUE ~ "desconocido"
#    )
#  )

# data_2019_pacientes %>%
#  count(macrozona)

# Create variable of people of entered at least one any UPC unit vs. patients that did not.
upc_units <- c(
  # Critical Care Units (UCI)
  "UNIDAD DE CUIDADOS INTENSIVOS (UCI) (INDIFERENCIADO)",
  "UNIDAD DE CUIDADOS INTENSIVOS ADULTO",
  "UNIDAD DE CUIDADOS INTENSIVOS CARDIOLOGÍA",
  "UNIDAD DE CUIDADOS INTENSIVOS CARDIVASCULAR",
  "UNIDAD DE CUIDADOS INTENSIVOS PEDIATRÍA",
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

# check if a patient either entered the hospital through a UPC unit or was anytime admitted during traslados1:9

data_2019_pacientes <- data_2019_pacientes %>%
  mutate(
    upc = if_else(
      servicioingreso %in% upc_units |
        serviciotraslado1 %in% upc_units |
        serviciotraslado2 %in% upc_units |
        serviciotraslado3 %in% upc_units |
        serviciotraslado4 %in% upc_units |
        serviciotraslado5 %in% upc_units |
        serviciotraslado6 %in% upc_units |
        serviciotraslado7 %in% upc_units |
        serviciotraslado8 %in% upc_units |
        serviciotraslado9 %in% upc_units,
      TRUE,
      FALSE
    )
  )

data_2019_pacientes %>%
  count(UPC)

# Cancer diagnosis. Based on ICD-10 codes, patients are filtered in cancer/no cancer if diagnostico1:35 matches any ICD-10 diagnosis of cancer.

cancer_icd10_codes<- c(
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


data_2019_pacientes <- data_2019_pacientes %>%
  mutate(
    diagnostico_cancer = case_when(
      if_any(starts_with("diagnostico"), ~ str_sub(., 1, 3) %in% cancer_icd10_codes) ~ "oncologico",
      TRUE ~ "no_oncologico"
    )
  )

data_2019_pacientes <- data_2019_pacientes %>%
  select(-cancer_status)

# Distribution based on type of cancer
cancer_icd10_groups <- tibble::tibble(
  codigo = c(
    # Lip, oral cavity and pharynx
    "C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14",
    
    # Digestive organs
    "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26",
    
    # Respiratory and intrathoracic organs
    "C30", "C31", "C32", "C33", "C34", "C35", "C36", "C37", "C38", "C39",
    
    # Bone and cartilage
    "C40", "C41",
    
    # Skin
    "C43", "C44",
    
    # Mesothelial and soft tissues
    "C45", "C46", "C47", "C48", "C49",
    
    # Breast
    "C50",
    
    # Female genital organs
    "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58",
    
    # Male genital organs
    "C60", "C61", "C62", "C63",
    
    # Urinary tract
    "C64", "C65", "C66", "C67", "C68",
    
    # Eye, brain and CNS
    "C69", "C70", "C71", "C72",
    
    # Endocrine glands
    "C73", "C74", "C75",
    
    # Ill-defined, secondary, and unspecified sites
    "C76", "C77", "C78", "C79", "C80",
    
    # Lymphatic and hematopoietic tissue
    "C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", "C90", "C91", "C92", "C93", "C94", "C95", "C96",
    
    # Multiple independent sites
    "C97",
    
    # Tumors in situ
    "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09",
    
    # Tumors of uncertain or unknown behavior
    "D37", "D38", "D39", "D40", "D41", "D42", "D43", "D44", "D45", "D46", "D47", "D48"
  ),
  cancer_group = c(
    rep("Lip, oral cavity and pharynx", 15),
    rep("Digestive organs", 12),
    rep("Respiratory and intrathoracic organs", 10),
    rep("Bone and cartilage", 2),
    rep("Skin", 2),
    rep("Mesothelial and soft tissues", 5),
    rep("Breast", 1),
    rep("Female genital organs", 8),
    rep("Male genital organs", 4),
    rep("Urinary tract", 5),
    rep("Eye, brain and CNS", 4),
    rep("Endocrine glands", 3),
    rep("Ill-defined/secondary/unspecified", 5),
    rep("Lymphatic and hematopoietic", 16),
    rep("Multiple primary tumors", 1),
    rep("Tumors in situ", 9),
    rep("Tumors uncertain behavior", 12)
  )
)


# Hospital mortality outcomes
data_2019_pacientes %>%
  count(tipoalta_grupo) %>%
  mutate(pct = round(100 * n / sum(n), 1))

data_2019_pacientes %>%  glimpse()
unique(data_2019_pacientes$tipoalta_grupo)

data_2019_pacientes %>%
  filter(tipoalta_grupo == "FALLECIDO") %>%
  count(diagnostico_cancer) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# Build a 2x2 table of cancer status vs mortality
tab_mortalidad_hosp <- data_2019_pacientes %>%
  filter(!is.na(diagnostico_cancer), tipoalta_grupo %in% c("FALLECIDO", "OTROS")) %>%
  mutate(mortalidad = if_else(tipoalta_grupo == "FALLECIDO", "FALLECIDO", "VIVO")) %>%
  count(diagnostico_cancer, mortalidad) %>%
  pivot_wider(names_from = mortalidad, values_from = n, values_fill = 0) %>%
  column_to_rownames("diagnostico_cancer") %>%
  as.matrix()

# Step 2: Run chi-square test
chisq.test(tab_mortalidad_hosp)

# General upc mortality
data_2019_pacientes %>%
  count(mortalidad_upc) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# sort(sapply(ls(), function(x) object.size(get(x))), decreasing = TRUE) problema de memoria

#Given that we are working now with regions, remove variable macrozona
data_2019_pacientes_diag_cancer <- data_2019_pacientes_diag_cancer %>%
  select(-macrozona)


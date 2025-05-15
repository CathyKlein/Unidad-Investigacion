library(dplyr)
data_2019_pacientes %>%  glimpse()

data_2019_pacientes <- data_2019_pacientes %>%
  select(-UPC, -UPC_ingreso_2019) # remuevo la variable duplicada.


# MORTALITY in servicio ingreso == UPC unit
mortalidad_upc_ingreso <- data_2019_pacientes |> 
  mutate(mortalidad_upc = servicioingreso %in% upc_units & tipoalta_grupo == "FALLECIDO") |> 
  filter(mortalidad_upc)

mortalidad_upc_ingreso |> count(mortalidad_upc) # 5,999 patients

# MORTALITY based on last SERVICIOTRASLADO == UPC unit and died
mortalidad_upc_traslado <- data_2019_pacientes |> 
  filter(!cip_encriptado %in% mortalidad_upc_ingreso$cip_encriptado) |> 
  select(cip_encriptado, fecha_ingreso, tipoalta, tipoalta_grupo, starts_with("serviciotraslado")) |> 
  pivot_longer(cols = starts_with("serviciotraslado")) |>
  mutate(orden = str_extract(name, "\\d+") |> as.numeric()) |> 
  filter(!is.na(value)) |> 

  # filtrar ultimo traslado
  group_by(cip_encriptado) |> 
  slice_max(orden) |> 
  filter(tipoalta == "FALLECIDO") |> 
  filter(value %in% upc_units)

nrow(mortalidad_upc_traslado)


data_2019_pacientes <- data_2019_pacientes |> 
  mutate(mortalidad_upc = cip_encriptado %in% c(mortalidad_upc_traslado$cip_encriptado, mortalidad_upc_ingreso$cip_encriptado),
         mortalidad_upc_traslado = cip_encriptado %in% mortalidad_upc_traslado$cip_encriptado,
         mortalidad_upc_ingreso = cip_encriptado %in% mortalidad_upc_ingreso$cip_encriptado)

# tabla de contingencia
data_2019_pacientes |> 
  count(upc,
        mortalidad_upc,
        mortalidad_upc_traslado,
        mortalidad_upc_ingreso)

# mortality in UPC divided by cancer and no cancer patients. 
data_2019_pacientes |> 
count(mortalidad_upc, diagnostico_cancer) |> 
  group_by(mortalidad_upc) |> 
  mutate(p = n/sum(n)*100)

# Check differences base on bivariate analysis

df_mortalitad_upc <- data_2019_pacientes %>%
  filter(mortalidad_upc == TRUE)

tabla_mortalidad_cancer <- table(df_mortalitad_upc$diagnostico_cancer)

chisq.test(tabla_mortalidad_cancer)

# Distribution of gender in patients who died IN critical care units
data_2019_pacientes %>%
  count(mortalidad_upc, diagnostico_cancer, sexo) %>%
  group_by(mortalidad_upc, sexo) %>%
  mutate(p = round(n / sum(n) * 100, 2)) %>%
  ungroup()

# Analysis if there are any statistical differences in mortality between genders
# Create contingency table: Sexo vs Diagnóstico de cáncer
sexo_cancer_mortalidad_tabla <- table(df_mortalitad_upc$sexo, df_mortalitad_upc$diagnostico_cancer)

# Apply Chi-squared test
chisq.test(sexo_cancer_mortalidad_tabla)
fisher.test(sexo_cancer_mortalidad_tabla)

# Analysis on mortality for ALL patients that used UPC unit (not necessarily died in UPC unit)
data_2019_pacientes %>%
  filter(upc == TRUE, tipoalta_grupo == "FALLECIDO") %>%
  count(diagnostico_cancer) %>%
  mutate(pct = round(100 * n / sum(n), 2))

df_fallecido_uso_upc <- data_2019_pacientes %>%
  filter(upc == TRUE, tipoalta_grupo == "FALLECIDO")

tabla_fallecido_alta <- table(df_fallecido_uso_upc$diagnostico_cancer)

chisq.test(tabla_fallecido_alta)

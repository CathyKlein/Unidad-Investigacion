
# murieron en upc al ingreso
mortalidad_upc_ingreso <- data_2019_pacientes |> 
  mutate(mortalidad_upc = servicioingreso %in% upc_units & tipoalta_grupo == "FALLECIDO") |> 
  filter(mortalidad_upc)

mortalidad_upc_ingreso |> count(mortalidad_upc)

# pacientes que en su ultimo traslado fue a UPC y fallecieron
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

# mortalidad por oncológicos
data_2019_pacientes |> 
count(mortalidad_upc, diagnostico_cancer) |> 
  group_by(mortalidad_upc) |> 
  mutate(p = n/sum(n)*100)

# library loading
library(dplyr)
library(arrow)
library(purrr)
library(stringr)
library(janitor)
library(tidyr)
library(tibble)
library(ggplot2)

data_2019_pacientes_diag_cancer %>%  glimpse()
unique(data_2019_pacientes$region)

str(pob_region)

anti_join(data_2019_pacientes_diag_cancer, pob_region, by = "region") %>%
  count(region) %>%
  arrange(region)

data_2019_pacientes_diag_cancer %>%
  left_join(pob_region, by = "region") %>%
  glimpse()

names(data_2019_pacientes_diag_cancer)
unique(data_2019_pacientes_diag_cancer$poblacion)

data_2019_pacientes_diag_cancer <- data_2019_pacientes_diag_cancer %>%
  rename(pob_region = poblacion)

data_2019_pacientes_diag_cancer %>%
  select(region, pob_region) %>%
  glimpse()

# UPC unit use by region
# Calculate UPC rate per region
data_2019_pacientes_diag_cancer %>%
  filter(upc == TRUE, !is.na(region)) %>%
  count(region, sort = TRUE)

tasa_upc_df <- data_2019_pacientes_diag_cancer %>%
  filter(upc == TRUE, !is.na(pob_region)) %>%
  count(region) %>%
  left_join(
    data_2019_pacientes_diag_cancer %>%
      select(region, pob_region) %>%
      distinct(),
    by = "region"
  ) %>%
  mutate(tasa_UPC_region = round((n / pob_region) * 100000, 2)) %>%
  select(region, tasa_UPC_region)


ggplot(tasa_upc_df, aes(x = reorder(region, tasa_UPC_region), y = tasa_UPC_region)) +
  geom_col(fill = "#4DBBD5", color = "black") +
  coord_flip() +
  labs(
    title = "Tasa de hospitalización en UPC por región\n(por 100.000 habitantes)",
    x = "Región",
    y = "Tasa UPC por 100.000"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)  # Add extra space around
  )

ggsave("tasa_upc_region.png", width = 10, height = 6, dpi = 300)

# Step 2: Add it to your main dataframe (NA for regions not in tasa)
data_2019_pacientes_diag_cancer <- data_2019_pacientes_diag_cancer %>%
  left_join(tasa_upc_df, by = "region")

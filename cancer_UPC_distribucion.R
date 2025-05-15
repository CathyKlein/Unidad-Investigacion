# library loading
library(dplyr)
library(arrow)
library(purrr)
library(stringr)
library(janitor)
library(tidyr)
library(dplyr)
library(ggplot2)

# Age distribution
data_2019_pacientes %>%  names()
data_2019_pacientes %>%  glimpse()

sum(is.na(data_2019_pacientes$edad))
sum(is.na(data_2019_pacientes$grupo_edad))
data_2019_pacientes %>%
  filter(!is.na(edad) & is.na(grupo_edad)) %>%
  select(edad) %>%
  distinct()

table(data_2019_pacientes$grupo_edad) # 13 NA values

# Distribution based on UPC for patients older or equal than 15 years old
table(data_2019_pacientes$UPC[data_2019_pacientes$edad >= 15])
prop.table(table(data_2019_pacientes$UPC[data_2019_pacientes$edad >= 15])) * 100

# Diagnosis of cancer in people older or equal than 15.
older_equal_edad_data <- data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  mutate(UPC_label = "uso UPC")

older_equal_edad_data %>%
  count(UPC, diagnostico_cancer)

upc_cancer_dist <- older_equal_edad_data %>%
  count(UPC_label, diagnostico_cancer) %>%
  group_by(UPC_label) %>%
  mutate(
    pct = round(100 * n / sum(n), 1),
    label = paste0(n, " (", pct, "%)")
  )

# Define total UPC patients aged ≥15
total_upc_patients <- 52960 

# Now count and calculate correct % over total UPC
upc_cancer_plot_dist <- data_2019_pacientes %>%
  filter(edad >= 15, UPC == TRUE) %>%
  mutate(UPC_label = "uso UPC") %>%
  count(UPC_label, diagnostico_cancer) %>%
  mutate(
    pct = round(100 * n / total_upc_patients, 2),
    label = paste0(n, " (", pct, "%)")
  )



ggplot(upc_cancer_plot_dist, aes(x = UPC_label, y = n, fill = diagnostico_cancer)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.9),
            vjust = -0.4, color = "black", size = 5) +
  scale_fill_manual(values = c("no_oncologico" = "#66B2FF", "oncologico" = "#66CC66")) +
  labs(
    title = "Cancer Diagnosis Distribution\nin Patients with CPU Use",
    x = NULL,
    y = "Number of Patients",
    fill = "Cancer Diagnosis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", lineheight = 1.2),
    legend.position = "top"
  ) +
  expand_limits(y = max(upc_cancer_plot_dist$n) * 1.15)


ggsave(
  filename = "fig_cancer_diagnosis_UPC.png",
  path = "/Users/catherineklein/Desktop/DOCTORADO ICIM/UNIDAD DE INVESTIGACION/GRD_2019_LIMPIO/05_figures",
  width = 10,
  height = 6,
  dpi = 300
)

# Distribution of CPU cancer patients based on type of cancer

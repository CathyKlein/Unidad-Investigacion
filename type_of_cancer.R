# Load required packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


# ICD10 code groups (from earlier)
codigo_no_solido <- c("C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", "C90", "C91", "C92", "C93", "C94", "C95", "C96")
codigo_inespecifico <- c("C76", "C77", "C78", "C79", "C80", "D37", "D38", "D39", "D40", "D41", "D42", "D43", "D44", "D45", "D46", "D47", "D48")

data_2019_pacientes %>%  glimpse()

# Pivot first 15 diagnosis columns
diagnosticos_largos <- data_2019_pacientes %>%
  select(cip_encriptado, diagnostico1:diagnostico15) %>%
  pivot_longer(
    cols = starts_with("diagnostico"),
    names_to = "diagnostico_n",
    values_to = "codigo_icd10"
  ) %>%
  filter(!is.na(codigo_icd10)) %>%
  mutate(codigo_prefix = str_sub(codigo_icd10, 1, 3))

# Join to classify cancer type
diagnosticos_cancer <- diagnosticos_largos %>%
  left_join(cancer_icd10_groups, by = c("codigo_prefix" = "codigo")) %>%
  filter(!is.na(cancer_group))

primer_cancer_por_paciente <- diagnosticos_cancer %>%
  arrange(cip_encriptado, diagnostico_n) %>%
  group_by(cip_encriptado) %>%
  slice(1) %>%
  ungroup()

primer_cancer_por_paciente <- primer_cancer_por_paciente %>%
  mutate(
    tumor_dico = case_when(
      codigo_prefix %in% codigo_no_solido ~ "no_solido",
      codigo_prefix %in% codigo_inespecifico ~ NA_character_,
      TRUE ~ "solido"
    )
  )

data_2019_pacientes_diag_cancer <- data_2019_pacientes %>%
  left_join(
    primer_cancer_por_paciente %>%
      select(cip_encriptado, cancer_group, tumor_dico),
    by = "cip_encriptado"
  ) %>%
  mutate(
    diagnostico_cancer = if_else(!is.na(cancer_group), "oncologico", "no_oncologico")
  )

table(data_2019_pacientes_diag_cancer$diagnostico_cancer, useNA = "ifany")
data_2019_pacientes_diag_cancer %>%  glimpse()

data_2019_pacientes_diag_cancer %>%
  filter(!is.na(cancer_group)) %>%
  group_by(cancer_group, upc) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cancer_group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(cancer_group, desc(upc)) %>% 
  print(n = Inf)


# Filter and summarize
grafico_tumores_upc <- data_2019_pacientes_diag_cancer %>%
  filter(upc == TRUE, !is.na(tumor_dico)) %>%
  count(tumor_dico) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# Plot
ggplot(grafico_tumores_upc, aes(x = tumor_dico, y = n, fill = tumor_dico)) +
  geom_col(color = "black", width = 0.6) +
  geom_text(aes(label = paste0(n, " (", pct, "%)")),
            vjust = -0.4, size = 5) +  # Move label slightly higher
  labs(
    title = "Distribution of Tumor Type in CPU Patients",
    subtitle = "Only patients with confirmed cancer diagnosis",
    x = "Tumor Type",
    y = "Number of Patients",
    fill = "Tumor Type"
  ) +
  scale_fill_manual(values = c("solid" = "#4DBBD5", "no_solido" = "#E64B35")) +
  expand_limits(y = max(grafico_tumores_upc$n) * 1.10) +  # Add 10% headroom
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

ggsave("grafico_tumores_upc.png", width = 8, height = 6, dpi = 300)

# Plot top 10 types of cancer
top10_cancer_group <- data_2019_pacientes_diag_cancer %>%
  filter(!is.na(cancer_group)) %>%
  count(cancer_group, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  pull(cancer_group)

# Summarize for plotting
cancer_top10_pct <- data_2019_pacientes_diag_cancer %>%
  filter(cancer_group %in% top10_cancer_group) %>%
  count(cancer_group, upc) %>%
  group_by(cancer_group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()


ggplot(cancer_top10_pct, aes(x = reorder(cancer_group, -pct), y = pct, fill = upc)) +
  geom_col(position = "stack", color = "black", width = 0.7) +
  geom_text(
    data = subset(cancer_top10_pct, upc == TRUE),
    aes(label = paste0(pct, "%")),
    position = position_stack(vjust = 1.2),  # place just above green bar
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#00BA38", "FALSE" = "#3B9AB2"),  # green for UPC, blue for non-UPC
    labels = c("TRUE" = "UPC", "FALSE" = "Non-UPC")
  ) +
  labs(
    title = "Proportion of CPU vs Non-CPU Admissions by Cancer Type",
    x = "Cancer Type",
    y = "Percentage within Cancer Type",
    fill = "Admission"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold", size = 16)
  )

ggsave("cancer_top10_pct.png", width = 8, height = 6, dpi = 300)

cancer_top10_upc <- cancer_top10_upc %>%
  group_by(cancer_group) %>%
  mutate(total_n = sum(n)) %>%      # total bar height
  ungroup()

ggplot(cancer_top10_upc,
       aes(x = reorder(cancer_group, -total_n),  # keep most prevalent first
           y = n,
           fill = upc)) +                       # upc is TRUE/FALSE
  
  geom_col(width = 0.75, colour = "black") +    # stacked bars with outline
  
  scale_fill_manual(
    values = c("FALSE" = "#3B9AB2",   # blue  = No UPC
               "TRUE"  = "#1B9E77"),  # green = UPC
    labels = c("FALSE" = "No UPC",
               "TRUE"  = "UPC")
  ) +
  
  labs(
    title = "Top 10 Cancer Types in CPU vs. NO/CPU Patients",
    x     = "Cancer Type",
    y     = "Number of Patients",
    fill  = "Admission"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x        = element_text(angle = 30, hjust = 1),
    plot.title         = element_text(face = "bold", size = 16, hjust = .5),
    panel.grid.major.x = element_blank()
  )


ggsave("cancer_top10_upc.png", width = 8, height = 6, dpi = 300)


# Distribution of cancer type by sex in CPU patients

# Step 1: Identify the top 10 cancer types among UPC patients
top10_cancer_group_upc <- data_2019_pacientes_diag_cancer %>%
  filter(upc == TRUE, !is.na(cancer_group)) %>%
  count(cancer_group, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  pull(cancer_group)

# Step 2: Filter and compute sex-specific percentages for those top 10 types
cancer_upc_sexo <- data_2019_pacientes_diag_cancer %>%
  filter(upc == TRUE, cancer_group %in% top10_cancer_group_upc, !is.na(sexo)) %>%
  count(cancer_group, sexo) %>%
  group_by(sexo) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()


ggplot(cancer_upc_sexo,
       aes(x = reorder(cancer_group, -n),
           y = n,
           fill = sexo)) +
  
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6,
           color = "black") +
  
  scale_fill_manual(
    values = c("MUJER" = "#D73027",   # red for women
               "HOMBRE" = "#4575B4"), # blue for men
    labels = c("MUJER" = "Mujer",
               "HOMBRE" = "Hombre")
  ) +
  
  labs(
    title = "Top 10 Cancer Types in ICU Patients (UPC)\nby Sex",
    x     = "Cancer Type",
    y     = "Number of Patients",
    fill  = "Sex"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x        = element_text(angle = 30, hjust = 1),
    plot.title         = element_text(face = "bold", size = 16, hjust = .5),
    panel.grid.major.x = element_blank()
  )

ggsave("cancer_upc_sexo.png", width = 8, height = 6, dpi = 300)

# Mortality based on type of cancer
data_2019_pacientes_diag_cancer %>%  glimpse()

# Step 1: Identify top 5 cancer types in critical care (UPC) patients
top5_cancer_upc <- data_2019_pacientes_diag_cancer %>%
  filter(upc == TRUE, !is.na(cancer_group)) %>%
  count(cancer_group, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  pull(cancer_group)

# Step 2: Calculate N and % mortality within each of those cancer types
data_2019_pacientes_diag_cancer %>%
  filter(upc == TRUE,
         cancer_group %in% top5_cancer_upc,
         !is.na(mortalidad_upc)) %>%
  count(cancer_group, mortalidad_upc) %>%
  group_by(cancer_group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

# Step 2: Build the contingency table (cancer group × mortality)
tab_mortalidad_cancer_upc <- data_2019_pacientes_diag_cancer %>%
  filter(upc == TRUE,
         cancer_group %in% top5_cancer_upc,
         !is.na(mortalidad_upc)) %>%
  count(cancer_group, mortalidad_upc) %>%
  tidyr::pivot_wider(names_from = mortalidad_upc, values_from = n, values_fill = 0) %>%
  column_to_rownames("cancer_group") %>%
  as.matrix()

# Step 3: Run chi-square test
chisq.test(tab_mortalidad_cancer_upc)

library(readxl)
library(dplyr)
library(purrr)
library(lme4)
library(lmerTest)
library(FactoMineR)
library(factoextra)
library(ggplot2)

# =========================
# 1 Leer datos + filtrar
# =========================
data <- read_excel("ACP.xlsx")




data <- read_excel("ACP.xlsx") %>%
  filter(SITE == "NONOGASTA",
         SEASON == "2021 - 2022") %>%
  mutate(
    tratamiento = as.factor(`PRUNING TREATMENT`)
  )

# =========================
# 2 Seleccionar variables del PCA
# =========================
X_all <- data %>%
  select(
    `Number of bunches`,
    `Average bunch weight`,
    `Yield  per vine`
  ) %>%
  mutate(across(everything(), as.numeric))

# =========================
# 3 PCA
# =========================
res.pca <- PCA(X_all, scale.unit = TRUE, graph = FALSE)

# =========================
# 4 Calcular centroides por tratamiento
# =========================
scores <- as.data.frame(res.pca$ind$coord[, 1:2])
scores$tratamiento <- data$tratamiento

centroids <- scores %>%
  group_by(tratamiento) %>%
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2),
    .groups = "drop"
  )

# =========================
# 5 Plot
# =========================
fviz_pca_biplot(
  res.pca,
  geom.ind = "point",   # o "blank"
  label = "var",
  habillage = data$tratamiento,
  addEllipses = TRUE,
  ellipse.type = "confidence",
  repel = TRUE,
  arrowsize = 0.6,
  col.var = "black"
) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_blank(),
    
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )
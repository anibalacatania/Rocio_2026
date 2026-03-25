library(readxl)
library(dplyr)
library(purrr)
library(lme4)
library(lmerTest)
library(FactoMineR)
library(factoextra)
library(ggplot2)

# =========================
# 1 Leer datos
# =========================
data <- read_excel("cienaga_raleo/datasep.xlsx") %>%
  mutate(
    panelista = as.factor(panelista),
    producto  = as.factor(producto)
  )

# =========================
# 2 Variables sensoriales
# =========================
vars <- names(data %>% select(-producto, -panelista))

# =========================
# 3 Modelo mixto
# =========================
pvals <- map_dfr(vars, function(v){
  
  fml <- as.formula(paste0("`", v, "` ~ producto + (1|panelista)"))
  mod <- lmer(fml, data = data)
  
  p <- anova(mod)["producto", "Pr(>F)"]
  
  tibble(
    variable = v,
    pvalue   = as.numeric(p)
  )
})

vars_sig <- pvals %>%
  filter(!is.na(pvalue), pvalue < 0.05) %>%
  pull(variable)

print(vars_sig)

if(length(vars_sig) < 2){
  stop("Hay menos de 2 variables significativas. No se puede hacer el PCA.")
}

# =========================
# 4 PCA solo con variables significativas
# =========================
X_sig <- data %>%
  select(all_of(vars_sig)) %>%
  mutate(across(everything(), as.numeric))

res.pca <- PCA(X_sig, scale.unit = TRUE, graph = FALSE)

# =========================
# 5 Recortar flechas
# =========================
arrow_scale <- 0.00001
res.pca$var$coord[, 1:2] <- res.pca$var$coord[, 1:2] * arrow_scale

# =========================
# 6 Calcular centroides de tratamientos
# =========================
scores <- as.data.frame(res.pca$ind$coord[, 1:2])
scores$producto <- data$producto

centroids <- scores %>%
  group_by(producto) %>%
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2),
    .groups = "drop"
  )

# =========================
# 7 Plot
# =========================
fviz_pca_biplot(
  res.pca,
  geom.ind = "blank",
  label = "var",
  habillage = data$producto,
  addEllipses = TRUE,
  ellipse.type = "confidence",
  repel = TRUE,
  arrowsize = 0.8
) +
  geom_text(
    data = centroids,
    aes(x = Dim.1, y = Dim.2, label = producto, color=producto),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_blank()
  )
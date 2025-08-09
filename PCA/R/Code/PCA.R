#install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "openxlsx", 
#"caret", "scales", "factoextra", "nnet"))

library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(openxlsx)
library(scales)
library(factoextra)
library(nnet)

# ----------------------
# 1. Load and Inspect
# ----------------------
data(iris)
df <- iris

# ----------------------
# 2. Min-Max Normalization
# ----------------------
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

df_norm <- df %>%
  mutate(across(where(is.numeric), normalize))

# ----------------------
# 3. Standardization (Z-score)
# ----------------------
df_scaled <- df_norm %>%
  mutate(across(where(is.numeric), scale))

# ----------------------
# 4. PCA (all components)
# ----------------------
pca_model <- prcomp(df_scaled[, 1:4], center = FALSE, scale. = FALSE)
summary(pca_model)
cumulative_variance <- summary(pca_model)$importance[3, ]
print(cumulative_variance)
fviz_eig(pca_model, addlabels = TRUE, ylim = c(0, 100))


# Get PC scores
pca_scores <- as.data.frame(pca_model$x)
pca_scores$Species <- df$Species

# ----------------------
# 5. Export to Excel
# ----------------------
output <- cbind(df, df_norm[, 1:4], df_scaled[, 1:4], pca_scores)
write.xlsx(output, "pca_output_R.xlsx", rowNames = FALSE)
cat("✅ Exported to pca_output_R.xlsx\n")
# Explained variance (importance matrix)
importance_matrix <- summary(pca_model)$importance

# Save variance ratio and cumulative variance
explained_variance_ratio <- importance_matrix[2, ]
cumulative_variance <- importance_matrix[3, ]

# Display in console
cat("\nExplained Variance Ratio:\n")
print(round(explained_variance_ratio, 4))

cat("\nCumulative Variance:\n")
print(round(cumulative_variance, 4))

# ----------------------
# 6. Dot Plot: Standardized Features
# ----------------------
df_scaled$Species <- df$Species
df_scaled$sample <- 1:nrow(df_scaled)

df_scaled_long <- pivot_longer(df_scaled, cols = 1:4, names_to = "Feature", values_to = "Value")

ggplot(df_scaled_long, aes(x = Feature, y = Value, color = Species)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.6) +
  theme_minimal() +
  ggtitle("Dot Plot: Standardized Features by Species")

df_pca_long <- pca_scores %>%
  mutate(sample = 1:nrow(.)) %>%
  pivot_longer(cols = starts_with("PC"), 
               names_to = "Component", 
               values_to = "Score")
pca_scores <- as.data.frame(pca_model$x)
pca_scores$Species <- df$Species  # <-- this line must be included



# ----------------------
# 7. Dot Plot: PCA Components
# ----------------------
df_pca_long <- pca_scores %>%
  mutate(sample = 1:nrow(.)) %>%
  pivot_longer(cols = starts_with("PC"), names_to = "Component", values_to = "Score")

ggplot(df_pca_long, aes(x = Component, y = Score, color = Species)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.6) +
  theme_minimal() +
  ggtitle("Dot Plot: PCA Scores by Species")

table(pca_scores$Species)
pca_scores$Species <- df$Species


# ----------------------
# 8. Classification + Confusion Matrix (Logistic Regression)
# ----------------------
set.seed(123)
train_index <- createDataPartition(pca_scores$Species, p = 0.8, list = FALSE)
train_data <- pca_scores[train_index, ]
test_data <- pca_scores[-train_index, ]

# Multinomial logistic regression
model <- multinom(Species ~ PC1 + PC2 + PC3 + PC4, data = train_data)

# Prediction
pred <- predict(model, newdata = test_data)
conf_mat <- confusionMatrix(pred, test_data$Species)

print(conf_mat)
# 2D Scatter Plot: PC1 vs PC2
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Scatter Plot: PC1 vs PC2", x = "PC1", y = "PC2") +
  scale_color_brewer(palette = "Dark2")
# Dot Plot of PCA Scores (PC1–PC4)

ggplot(df_pca_long, aes(x = Component, y = Score, color = Species)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.6) +
  theme_minimal() +
  labs(title = "Dot Plot: PCA Components by Species", y = "PCA Score") +
  scale_color_brewer(palette = "Set2")

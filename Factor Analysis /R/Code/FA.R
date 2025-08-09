# Load data
data("USArrests")
head(USArrests)

# -------------------
# Step 1. Preprocessing
# -------------------
# Standardize variables
usarrests_scaled <- scale(USArrests)

# -------------------
# Step 2. Suitability Tests
# -------------------
install.packages("psych")
install.packages("nFactors")
library(psych)
library(nFactors)

# Correlation matrix
cor_mat <- cor(usarrests_scaled)
print(cor_mat)

# Kaiser-Meyer-Olkin (KMO) measure
KMO(cor_mat)

# Bartlett’s test of sphericity
cortest.bartlett(cor_mat, n = nrow(usarrests_scaled))

# Scree plot (eigenvalues)
ev <- eigen(cor_mat)$values
plot(ev, type="b", main="Scree Plot", xlab="Factor", ylab="Eigenvalue")

# -------------------
# Step 3. Factor Analysis
# -------------------
# Suppose we extract 2 factors with varimax rotation
fa_res <- fa(usarrests_scaled, nfactors=2, rotate="varimax", fm="ml")
print(fa_res)

# -------------------
# Step 4. Visualization
# -------------------
fa.diagram(fa_res)


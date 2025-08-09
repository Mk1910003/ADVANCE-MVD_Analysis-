data("USArrests")

# Discretize continuous variables into categories
usarrests_cat <- as.data.frame(lapply(USArrests, function(x) cut(x, 3, labels=c("Low","Medium","High"))))

# Build contingency table
tab <- table(usarrests_cat$Murder, usarrests_cat$Assault)

# Run Correspondence Analysis
library(ca)
ca_res <- ca(tab)
summary(ca_res)
plot(ca_res)

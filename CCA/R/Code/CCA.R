library(tidyverse) 
library(GGally) 
library(CCP) 
library(CCA) 
library(ggplot2)
FreshmenData <- read.csv(file.choose(), header = TRUE)
head(FreshmenData)
colnames(FreshmenData) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", "Science", "Sex")

summary(FreshmenData) 
xtabs(~Sex, data = FreshmenData) 
psychological<- FreshmenData[, 1:3] 
academic <- FreshmenData[, 4:8] 
ggpairs(psychological) 
ggpairs(academic) 
matcor(psychological, academic) 
canoncor1<- cc(psychological,academic) 
canoncor1$cor 
canoncor1[3:4] 
canoncor2 <- comput(psychological, academic, canoncor1) 
canoncor2[3:6] 
rho <- canoncor1$cor 
## Define number of of observations 
n<-dim(psychological)[1] 
## Number of variables in the first set 
p <- length(psychological) 
## Number of variables in the second set 
q <- length(academic) 
p.asym(rho,n,p,q,tstat ="Wilks") 
p.asym(rho,n,p,q, tstat = "Hotelling") 
p.asym(rho,n,p,q,tstat = "Pillai") 
p.asym(rho,n,p,q,tstat = "Roy") 
s1 <- diag(sqrt(diag(cov(psychological)))) 
s1 %*% 
  canoncor1$xcoef 
s2<- diag(sqrt(diag(cov(academic)))) 
s2 %*% 
  canoncor1$ycoef


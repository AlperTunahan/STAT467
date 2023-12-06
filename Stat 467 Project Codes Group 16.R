
# data has been read
dt <- read.csv("data.csv")

summary(dt)

# artist, id, name, release data columns has been removed from the original data set
dt <- dt[,-c(4, 9, 15, 17)]
dt$explicit <- as.factor(dt$explicit)
dt$mode <- as.factor(dt$mode)
# explicit and mode taken as factors
dt1 <- dt[sample(nrow(dt), 3000), ]
# sample of 3000 from 170653 observatins were taken

dim(dt1)
# dimensions of our dataset is 3000 observations and 15 columns
str(dt1)
summary(dt1)

dt_num <- dt1[, -c(2, 4, 6, 8, 9, 11, 14, 15, 17)]
str(dt1)

#Asummptionslar oluuuumm
# load MVN package
library(MVN)
result <- mvn(data = dt_num, mvnTest = "mardia")
result$multivariateNormality

mvn(dt_num,multivariatePlot= "qq")

#Multivariate outliers
# Mahalanobis distance
result <- mvn(data = dt_num, mvnTest = "hz", multivariateOutlierMethod = "quan")

# Adjusted Mahalanobis distance
result <- mvn(data = dt_num, mvnTest = "hz", multivariateOutlierMethod = "adj")

library(tidyverse)
library("ICSNP")

#Inference about mean vector (Hotelling's T^2)
#One Sample
Y12 <- data.frame(dt1$energy, dt1$danceability)
names(Y12) <- c("Y1","Y2")
attach(Y12)
cor(dt_num)
mean(Y1); sd(Y1)
mean(Y2); sd(Y2)

# Install and load R psych package
# Graph dependent variable means using error.bars() function
library(psych)
error.bars(dt_num,ylab="Group Means",xlab="Dependent Variables", col = nrow(dt_num))
corrplot::corrplot(cor(dt_num), method = "number")

# Conduct a Hotelling T2 test of null hypothesis that dependent means are different than zero
# muH0 assigns population means in matrix vector to equal zero
muH0 <- c(rep(0,10))
HotellingsT2(dt_num, mu=muH0)

# Visualize dataset,

library(gridExtra)
p1 <- ggplot(dt1, aes(x = explicit, y = popularity, fill = explicit)) + 
  geom_boxplot(outlier.shape = NA) + theme(legend.position="top")
p2 <- ggplot(dt1, aes(x = explicit, y = energy, fill = explicit)) + 
  geom_boxplot(outlier.shape = NA) + theme(legend.position="top")
grid.arrange(p1, p2, ncol=2)

# for popularity explicicity is better as well as for the energyyy, but popularity is more affected by explicity

# perform one-way MANOVAPermalink
dep_vars <- cbind(dt1$popularity, dt1$energy)
fit <- manova(dep_vars ~ explicit, data = dt1)
library(xtable)
summary(fit)

# get effect size
library(effectsize)
effectsize::eta_squared(fit)

# The Pillai's Trace test statistics is statistically significant 
#[Pillai's Trace = 0.041, F(2, 2997) = 64.65, p < 0.001] and indicates that explicity
#has a statistically significant association with both combined popularity and energy.

library(rstatix)
dt1 %>% group_by(explicit) %>%  shapiro_test(popularity, energy)

library(mvnormalTest)
mardia(dt1[, c("popularity", "energy")])$mv.test

library(heplots)
boxM(Y = dt1[, c("popularity", "energy")], group = dt1$explicit)

#Multivariate outliers
library(rstatix)
# get distance
sum(mahalanobis_distance(data = dt1[, c("popularity", "energy")])$is.outlier)


#Linearity assumption

library(gridExtra)
p1 <- dt1  %>% group_by(explicit) %>% filter(explicit == 0) %>% 
  ggplot(aes(x = popularity, y = energy)) + geom_point() + ggtitle("Non-explicit")
p2 <- dt1  %>% group_by(explicit) %>% filter(explicit == 1) %>% 
  ggplot(aes(x = popularity, y = energy)) + geom_point() + ggtitle("Explicit") 
grid.arrange(p1, p2, ncol=2)

cor.test(x = dt1$popularity, y = dt1$energy, method = "pearson")$estimate



#calculate principal components
dt_pca <- dt[, -c(4,8,9,14,15,17)]
results <- prcomp(dt_pca, scale = TRUE)

#reverse the signs
results$rotation <- -1*results$rotation

#display principal components
xtable(results$rotation)

results$x <- -1*results$x

head(results$x)
summary(results)

var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:13), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") + theme_minimal()

library(pls)
#fit PCR model
library(xtable)
library(knitr)
model <- pcr(popularity~., data=dt_pca, scale=TRUE, validation="CV")
xtable(summary(model))
a <- summary(model)
validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")
#in each plot model fit improves by adding more components
#explained variance increases with each added component


#define training and testing sets
train <- dt_pca[1:130000,]
y_test <- dt_pca[130000:nrow(dt_num), c("popularity")]
test <- dt_pca[130000:nrow(dt_num), -11]

#use model to make predictions on a test set
model <- pcr(popularity~year+valence+acousticness+danceability+duration_ms+energy+instrumentalness+key+liveness+loudness+speechiness+tempo,
             data=train, scale=TRUE, validation="CV")

pcr_pred <- predict(model, test, ncomp=8)

#calculate RMSE
sqrt(mean((pcr_pred - y_test)^2))
#We can see that the test RMSE turns out to be 13.79935. This is the average deviation 
#between the predicted value for popularity and the observed value for popularity 
#for the observations in the testing set.

# Factor Analysis
library(corrplot)
cm <- cor(dt_pca, method="pearson")
corrplot::corrplot(cm, method= "number", order = "hclust")
# we can see some correlations between variables

library(psych)
KMO(r=cm)
#   Since MSA > 0.5, we can run Factor Analysis on this data. 
#Besides, Bartletts test of sphericity should be significant.

print(cortest.bartlett(cm,nrow(dt_pca)))

parallel <- fa.parallel(dt_pca, fm = "minres", fa = "fa")
parallel
# Parallel analysis suggests that the number of factors =  6 

factanal(dt_pca, factors = 6, method ="mle")$PVAL
factanal(dt_pca, factors = 6, method ="mle")

scores<-factanal(dt_pca, factors = 6, method ="mle",scores="regression")$scores
head(scores)


cm1 <- cor(scores, method="pearson")
corrplot::corrplot(cm1, method= "number", order = "hclust")
# As you see, they are almost uncorrelated which guarantees that no 
#multicollinearity problem in linear regression.


# Canonical correlation
require(GGally)
require(CCA)    #facilitates canonical correlation analysis
require(CCP)

chars <- dt[, c(1, 3, 5, 7, 10, 12, 18)]
other <- dt[, c(2, 6, 11, 13, 16, 19)]

ggpairs(chars[1:1000,])
ggpairs(other[1:1000,])
# here we can look at the correlations between each other

cor_psych_acad <- matcor(chars, other)
cor_psych_acad
img.matcor(cor_psych_acad, type = 2)
# correlation between each group and between groups

cc1 <- cancor(chars, other)  ### function from standard R instalation
cc2 <- cc(chars, other)      ### function for the R package 'CCA'
cc1$cor
cc2$cor
#The canonical correlations are the same for both approaches.

par(mfrow = c(1,2))
barplot(cc1$cor, main = "Canonical correlations for cancor()", col = "blue")
barplot(cc2$cor, main = "Canonical correlations for cc()", col = "green")
par(mfrow = c(1,1))
cc2[3:4]$ycoef
#For the variable read, a one unit increase in energy leads to a 3.123 increase 
#in the first canonical variate of set 1 when all of the other variables are held constant.

# compute canonical loadings
cc3 <- comput(chars, other, cc2)
names(cc3)
cc3[3:6]



# tests of canonical dimensions
rho <- cc2$cor ; rho
n <- dim(chars)[1] ; n
p <- length(chars) ; p
q <- length(other)  ; q

a <- as.table(p.asym(rho, n, p, q, tstat = "Wilks"))
xtable(a)
b <- p.asym(rho, n, p, q, tstat = "Hotelling")
p.asym(rho, n, p, q, tstat = "Pillai")
#Therefore dimensions 1 to 5 must each be significant while dimension 6 is not, according to p-values.



# standardized chars canonical coefficients diagonal matrix of psych sd's
s1 <- diag(sqrt(diag(cov(chars))))
s1 %*% cc2$xcoef

# standardized other canonical coefficients diagonal matrix of acad sd's
s2 <- diag(sqrt(diag(cov(other))))
s2 %*% cc2$ycoef

#############################################3
#Classification 
library(caret)
library(mlbench)
dt_glm <- dt[, -c(4,9,15,17)]
# data was sampled with 3500 obs
dt_glm$popularity <- as.factor(ifelse(dt_glm$popularity < 60, "not-popular", "popular"))

# Split the data into training and test set
set.seed(123)
index_train = sample(nrow(dt_glm), round(0.8*nrow(dt_glm))) #produce index number for train data

train.data  <- dt_glm[index_train, ]
dim(train.data)
test.data <- dt_glm[-index_train, ]
dim(test.data)
model_glm <- glm(popularity~., data = train.data, family = binomial)
summary(model_glm)
# All the main effects were significant at 5% significance, except acousticness, key and tempo. Acousticness was significant at 10% significance.

pred.probabilities <- model_glm %>% predict(test.data, type = "response")
head(pred.probabilities)
contrasts(test.data$popularity)
# Three different cutoffs were made to asses the classification results which are 0.2, 0.5 and 0.8.
#for 0.2
predicted.classes_0.2 <- ifelse(pred.probabilities > 0.2, "popular", "not-popular")
#Creating confusion matrix
measeures_02 <- confusionMatrix(data=factor(predicted.classes_0.2), 
                                reference = factor(test.data$popularity),positive = "popular")
#Display results 
measeures_02
#For the cutoff 0.2, positive predictive value is too low, so it cannot classify popular songs correctly, but it is better in predicting non-popular songs.
# It does not seem to perform well.


capture.output(measeures_02, file = "cutoff02.txt")
#for 0.5
predicted.classes_0.5 <- ifelse(pred.probabilities > 0.5, "popular", "not-popular")
#Creating confusion matrix
measeures_05 <- confusionMatrix(data=factor(predicted.classes_0.5), 
                                reference = factor(test.data$popularity),positive = "popular")
#Display results 
measeures_05
#For cutoff 0.5, although it classifies most of the songs correctly, its sensitivity is too low. This cutoff does not 
# perform well either.
capture.output(measeures_05, file = "cutoff05.txt")
#for 0.8
predicted.classes_0.8 <- ifelse(pred.probabilities > 0.8, "popular", "not-popular")
#Creating confusion matrix
measeures_08 <- confusionMatrix(data=factor(predicted.classes_0.8), 
                                reference = factor(test.data$popularity),positive = "popular")
#Display results 
measeures_08

capture.output(measeures_08, file = "cutoff08.txt")

#Clusteeeer
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


dt_clust <- dt[dt$popularity>70,]
dt_sample <- dt_clust[, -c(4,8,9,14,15,17)]
dt_sample <- scale(dt_sample)

k2 <- kmeans(dt_sample, centers = 2, nstart = 25)
str(k2)

k2
fviz_cluster(k2, data = dt_sample)

k3 <- kmeans(dt_sample, centers = 3, nstart = 25)
k4 <- kmeans(dt_sample, centers = 4, nstart = 25)
k5 <- kmeans(dt_sample, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = dt_sample) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = dt_sample) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = dt_sample) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = dt_sample) + ggtitle("k = 5")

library(gridExtra)

grid.arrange(p1, p2, p3, p4, nrow = 2)

set.seed(123)

fviz_nbclust(dt_sample, kmeans, method = "wss")

fviz_nbclust(dt_sample, kmeans, method = "silhouette")

# compute gap statistic
set.seed(1)
gap_stat <- clusGap(dt_sample[1:1000,], FUN = kmeans, nstart = 25,
                    K.max = 15, B = 50, iter.max = 1000)


# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

# Compute k-means clustering with k = 4
set.seed(2)
final <- kmeans(dt_sample, 11, nstart = 25, iter.max = 100)
print(final)

fviz_cluster(final, data = dt_sample)

dt_clust$cluster <- final$cluster
head(dt_clust[dt_clust$cluster == 2,])


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms


dt_sample <- scale(dt_sample)
head(dt_sample)

# Dissimilarity matrix
d <- dist(dt_sample, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Compute with agnes
hc2 <- agnes(dt_sample, method = "complete")

# Agglomerative coefficient
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(dt_sample, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(dt_sample, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

# compute divisive hierarchical clustering
hc4 <- diana(dt_sample)

# Divise coefficient; amount of clustering structure found
hc4$dc

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 2 groups
sub_grp <- cutree(hc5, k = 2)

# Number of members in each cluster
table(sub_grp)
library(dplyr)

dt_sample %>% mutate(cluster = sub_grp) %>% head

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 2, border = 2:5)


#Decision Tree 
set.seed(123)

dt2 <- dt[1:10000, -c(4,9,15,17)]

dt2$popularity <- as.factor(ifelse(dt2$popularity < 50, "not-popular", "popular"))

set.seed(1)
shuffle_index <- sample(1:nrow(dt2))
head(shuffle_index)

dt2 <- dt2[shuffle_index, ]
head(dt2)

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size*n_row
  train_sample <- 1:total_row
  if(train == TRUE){
    return(data[train_sample, ])
  }else{
    return(data[-train_sample, ])
  }
}

set.seed(12)
data_train <- create_train_test(dt2, 0.8, train = TRUE)
data_test <- create_train_test(dt2, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

prop.table(table(data_train$popularity))
prop.table(table(data_test$popularity))

library("rpart.plot")
library("rpart")
library("rattle")
#rpart(formula, data=, method='')
#arguments:         
#- formula: The function to predict
#- data: Specifies the data frame- method:          
#- "class" for a classification tree            
#- "anova" for a regression tree   

fit <- rpart(popularity ~ .-year, data = data_train, method = 'class')
rpart.plot(fit, type = 0)
rpart.plot(fit, type = 1)
rpart.plot(fit, type = 2)
rpart.plot(fit, type = 3)
rpart.plot(fit, type = 4)
rpart.plot(fit, type = 5)
fancyRpartPlot(fit)

predict_unseen <-predict(fit, data_train, type = 'class')

table_mat <- table(data_test$popularity, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

library(caret)
test_con_mat = confusionMatrix(table_mat, positive = "popular")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])

# Random Forest

# Loading package
library(caTools) # For sampling the dataset
library(randomForest) # For implementing random forest algorithm

# Splitting data in train and test data
set.seed(222)
ind <- sample(2, nrow(dt2), replace = TRUE, prob = c(0.7, 0.3))
train <- dt2[ind==1,]
test <- dt2[ind==2,]

dim(train); dim(test)

set.seed(123) # Setting seed
classifier_RF = randomForest(popularity~., data=train[sample(nrow(train), 2000),], proximity=TRUE) 
xtable(classifier_RF$confusion)
plot(classifier_RF)
# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[,-13])
xtable(importance(classifier_RF))
varImpPlot(classifier_RF)
# Confusion Matrix
confusion_mtx = table(test[, 13], y_pred)
confusion_mtx
confusionMatrix(y_pred, test$popularity)

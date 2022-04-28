library(ggplot2)        # for generating visualizations
library(dplyr)
library(forcats)
library(grid)
library(gridExtra)
library(readxl)
library(readxl)
library(corpcor)
library(GPArotation)
library(psych)
library(ggplot2)
library(MASS)
library(MVN)
library(psy)
library(grid)
library(REdaS)

set.seed(12345)

######################2021##################################
data_2021 <- read.csv("semester-3 R project/2021-aps-employee-census-5-point-dataset.csv", header=TRUE, na.strings=c(" "), stringsAsFactors=T)

View(data_2021)

data_selection_2021 <- data_2021 %>% dplyr::select('ï..AS','q1','q2.','q5.'
                                                   ,'q19a','q19b','q19c','q19d','q19e','q19f','q19g','q19h','q19i','q48b'
                                                   ,'q48a','q17b','q17g','q17h','q17i','q17a','q33c','q44c','q34e')
View(data_selection_2021)
dim(data_selection_2021)

#converting questions to numerical using mutate_at
data_2021_b <- data_selection_2021 %>% mutate_at(c('q19a','q19b','q19c','q19d','q19e','q19f','q19g','q19h','q19i','q48b'
                                                   ,'q48a','q17b','q17g','q17h','q17i','q17a','q44c','q34e'),funs(recode(.,'Strongly agree'=5,'Agree'=4,'Neither agree nor disagree'=3,'Disagree'=2,'Strongly disagree'=1)))
data_2021_b  <- data_2021_b  %>% mutate_at(c('q33c'),funs(recode(.,'Always'=5,'Often'=4,'Sometimes'=3,'Rarely'=2,'Never'=1)))

View(data_2021_b)


#checking summary before
library(psych)
df2021.1 <- psych::describe(data_2021_b)
df2021.1 <- data.frame(df2021.1)
View(df2021.1)

#missing value of 2021 each column
missing_2021 <- sapply(data_2021_b,function(x)sum(is.na(x)))
data_missing <- data.frame(missing_2021)
View(data_missing)
sum(data_missing)
dim(data_2021_b)


#checking null value row wise
sum(!complete.cases(data_2021_b[c('ï..AS','q1','q2.','q5.'
                                  ,'q19a','q19b','q19c','q19d','q19e','q19f','q19g','q19h','q19i','q48b'
                                  ,'q48a','q17b','q17g','q17h','q17i')]))


#cleaning the null values
d2021 <- na.omit(data_2021_b)
View(d2021)

library(psych)
df2021.2 <- psych::describe(d2021)
df2021.2 <- data.frame(df2021.2)
View(df2021.2)

#checking missing values again
missing_2021 <- sapply(d2021,function(x)sum(is.na(x)))
data_missing <- data.frame(missing_2021)
View(data_missing)

library(naniar)
vis_miss(d2021,warn_large_data=F,show_perc_col = F,show_perc = TRUE)
gg_miss_var(d2021)

View(d2021)

#inserting year
d2021$year <- 2021

View(d2021)

d2021 <- d2021 %>% dplyr::select('year','ï..AS','q1','q2.','q5.','q19a','q19b','q19c','q19d','q19e','q19f','q19g','q19h','q19i','q48b'
                                 ,'q48a','q17b','q17g','q17h','q17i','q17a','q33c','q44c','q34e')
View(d2021)
#column names of 2021 dataset
colnames(d2021) <- c('Year','Agency','Gender','Age','Job_Level','q19a','q19b','q19c','q19d','q19e','q19f','q19g','q19h','q19i','q48b'
                     ,'q48a','q17b','q17g','q17h','q17i','q17a','q33c','q44c','q34e')

View(d2021)
dim(d2021)

#Anova Test
#Anova Test
options(scipen = 999)
d2021$Job_Level <- ordered(d2021$Job_Level,
                           levels = c("Trainee/Graduate/APS", "EL", "SES"))
res.aov <- aov(q19a ~ Job_Level, data=d2021)
summary(res.aov)

TukeyHSD(res.aov)

#install.packages("broom")
library(broom)

for(i in 6:ncol(d2021))
{
  
  column <- names(d2021[i])
  #tidy will summarise and return neat format
  avz <- broom::tidy(aov(d2021[,i] ~ Job_Level, data = d2021))
  
  # Add this condition if you only want aov with P < 0.05 printed
  if(avz$p.value[1] < 0.05) {
    
    print(column)
    print(avz)
  }
}




pc1 <- d2021[,c(6:24)]
View(pc1)

#A. Correlation Matrix
corr <- cor(pc1)
round(corr,2)
library(ggcorrplot)
corr <- round(cor(pc1,method = "pearson"), 2)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_col = "black",
           outline.color = "white",
           colors = c("skyblue",  "white","red"),
           lab_size = 3, 
           method="square", 
           show.legend = TRUE, legend.title = "correlation", 
           title="Correlarion Plot", 
           ggtheme=theme_bw)

#B. Anti-image correlation matrix diagonals

X <- cor(pc1) 
iX <- ginv(X) 
S2 <- diag(diag((iX^-1)))
AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
IS <- X+AIS-2*S2                         # image covariance matrix
Dai <- sqrt(diag(diag(AIS)))
IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix

print(diag(AIR), row.names = FALSE)


#C. Measure of Sampling Accuracy (MSA) using KMO Test
KMO(corr)

#D. Bartletts Test of Sphericity

cortest.bartlett(corr,nrow(pc1))

#EFA round-1
pc1 <- principal(pc1,nfactors = length(pc1), rotate = "none")
pc1

cat("Eigen values\n")
print(pc1$values)

plot(pc1$values, type = "b",xlab = "Factors", ylab = "Eigen values", main = "SCREE PLOT")

#using FA parallel analysis to see number of factors 
pc2 <- d2021[,c(6:24)]
View(pc2)
library(GPArotation)

nofactors <- fa.parallel(pc2, fm="pa", fa="fa") #parallel analysis say factors is 6,. but, scree plot shows only 2.
sum(nofactors$fa.values > 0.7) #kaiser criterion shows only 2

solution <- fa(pc2, nfactors=2, rotate="varimax", fm="pa") #varimax rotation lead to more variables loading to the same factors
fa.diagram(solution)
print(solution$loadings, digits=3, cutoff=0.5)

#EFA ROUND_2 ON SELECTED LEADERSHIP QUESTIONS

round_2<- d2021 %>%
  dplyr::select("q19b",   "q19c"  , "q19d" ,  "q19e" ,  "q19f" ,  "q19g"  ,
                "q19h" ,  "q19i",   "q48b")

pc <- princomp(round_2)
summary(pc)
print(pc$loadings, digits=3, cutoff=0.5)

fit <- principal(round_2, nfactors=3, rotate="varimax")
library(factoextra)
fviz_eig(pc)

nofactors <- fa.parallel(round_2, fm="pa", fa="fa") #parallel analysis say factors is 6,. but, scree plot shows only 2.
sum(nofactors$fa.values > 0.7)

fa1 <- fa(round_2, nfactors=3, rotate="varimax", fm="pa")
fa1
fa.diagram(fa1)
print(fa1)
print(fa1$loadings, digits=3, cutoff=0.5)

library(corrplot)
corr <- cor(d2021[,c("q19b",   "q19c"  , "q19d" ,  "q19e" ,  "q19f" ,  "q19g"  ,
                     "q19h" ,  "q19i",   "q48b")], method="pearson")
corrplot.mixed(corr, lower="circle", upper="number")

###kmo and barlett
KMO(corr)

cortest.bartlett(corr, n=nrow(round_2))

cronbach(round_2)

#on selected Job level
library(psych)
t = d2021  %>% filter(Job_Level == 'Trainee/Graduate/APS') %>% dplyr::select("q19b",   "q19c"  , "q19d" ,  "q19e" ,  "q19f" ,  "q19g"  ,
                                                                         "q19h" ,  "q19i",   "q48b") 
View(t)

pc <- princomp(t)
summary(pc)
print(pc$loadings, digits=3, cutoff=0.5)

fit <- principal(t, nfactors=3, rotate="varimax")
library(factoextra)
fviz_eig(pc)

fa1 <- fa(t, nfactors=3, rotate="varimax", fm="pa")
fa1
fa.diagram(fa1)
print(fa1)
print(fa1$loadings, digits=3, cutoff=0.5)

# Further inferential
View(d2021)
library(psych)
t = d2021  %>% dplyr::select("q19b",   "q19c"  , "q19d" ,  "q19e" ,  "q19f" ,  "q19g"  ,
                             "q19h" ,  "q19i",   "q48b") 
View(t)
pc <- princomp(t)
summary(pc)

fa1 <- fa.parallel(t, fm="pa", fa="fa")  #suggest number of factors 5
sum(fa1$fa.values > 1.0)
sum(fa1$fa.values > 0.7)

fa3 <- fa(t, nfactors=3, rotate="varimax", fm="pa")
print(fa3$loadings, digits=3, cutoff=0.5)
fa.diagram(fa3)

scores <- fa3$scores
colnames(scores) <- c('EL','TL','IL')

fa.scores <- cbind(d2021,scores)
View(fa.scores)

EL <- c('q19b','q19d','q19c')
TL <- c('q19e','q19f','q48b')
IL <- c('q19g','q19i','q19h')

psych::alpha(d2021[,EL])
psych::alpha(d2021[,TL])
psych::alpha(d2021[,IL])

#install.packages("caret")
library(caret)
set.seed(100)
a <- createDataPartition(fa.scores$q48a ,p=0.8,list=F)
TrainSet <- fa.scores[a,]
TestSet <- fa.scores[-a,]
summary(TrainSet)

library(MASS)
str(TrainSet)
TrainSet$q48a <- as.ordered(TrainSet$q48a)
TestSet$q48a <- as.ordered(TestSet$q48a)

#ordinal logistics
model <- polr(q48a ~ EL+TL+IL+Age+Gender, data= TrainSet[TrainSet$Job_Level=='Trainee/Graduate/APS',],Hess = T)
summary(model)

summary_table <- coef(summary(model))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

predictinovation = predict(model,TestSet)
table(predictinovation,TestSet$q48a)
mean(as.character(TestSet$q48a) != as.character(predictinovation))

confusionMatrix(predictinovation,TestSet$q48a)


library("effects")
plot(Effect(focal.predictors = c("EL", "Gender"),model))
plot(Effect(focal.predictors = c("TL", "Gender"),model))
plot(Effect(focal.predictors = c("IL", "Gender"),model))
plot(Effect(focal.predictors = c("EL", "Age"),model))
plot(Effect(focal.predictors = c("TL", "Age"),model))
plot(Effect(focal.predictors = c("IL", "Age"),model))


# Random Forest

TrainSet$q19a <- as.factor(TrainSet$q19a)
TrainSet$q19b <- as.factor(TrainSet$q19b)
TrainSet$q19c <- as.factor(TrainSet$q19c)
TrainSet$q19d <- as.factor(TrainSet$q19d)
TrainSet$q19e <- as.factor(TrainSet$q19e)
TrainSet$q19f <- as.factor(TrainSet$q19f)
TrainSet$q19g <- as.factor(TrainSet$q19g)
TrainSet$q19h <- as.factor(TrainSet$q19h)
TrainSet$q19i <- as.factor(TrainSet$q19i)
TrainSet$q48b <- as.factor(TrainSet$q48b)
TrainSet$q48a <- as.factor(TrainSet$q48a)
TrainSet$q17b <- as.factor(TrainSet$q17b)
TrainSet$q17g <- as.factor(TrainSet$q17g)
TrainSet$q17h <- as.factor(TrainSet$q17h)
TrainSet$q17i <- as.factor(TrainSet$q17i)
TrainSet$q17a <- as.factor(TrainSet$q17a)
TrainSet$q33c <- as.factor(TrainSet$q33c)
TrainSet$q44c <- as.factor(TrainSet$q44c)
TrainSet$q34e <- as.factor(TrainSet$q34e)

summary(TestSet)
str(TestSet)
str(TrainSet)
TestSet$q19a <- as.factor(TestSet$q19a)
TestSet$q19b <- as.factor(TestSet$q19b)
TestSet$q19c <- as.factor(TestSet$q19c)
TestSet$q19d <- as.factor(TestSet$q19d)
TestSet$q19e <- as.factor(TestSet$q19e)
TestSet$q19f <- as.factor(TestSet$q19f)
TestSet$q19g <- as.factor(TestSet$q19g)
TestSet$q19h <- as.factor(TestSet$q19h)
TestSet$q19i <- as.factor(TestSet$q19i)
TestSet$q48b <- as.factor(TestSet$q48b)
TestSet$q48a <- as.factor(TestSet$q48a)
TestSet$q17b <- as.factor(TestSet$q17b)
TestSet$q17g <- as.factor(TestSet$q17g)
TestSet$q17h <- as.factor(TestSet$q17h)
TestSet$q17i <- as.factor(TestSet$q17i)
TestSet$q17a <- as.factor(TestSet$q17a)
TestSet$q33c <- as.factor(TestSet$q33c)
TestSet$q44c <- as.factor(TestSet$q44c)
TestSet$q34e <- as.factor(TestSet$q34e)



library(randomForest)
table(fa.scores$q48a)
#majority of data points refer to strongly agree case
set.seed(123)
rf <- randomForest(q48a ~ EL+TL+IL+Age+Gender, data= TrainSet[TrainSet$Job_Level=='Trainee/Graduate/APS',])
print(rf)
#q48a is innovation we are going to predict from leadership quesction Tl and cl
#q48 is ordinal so used classification method of random forest instead of regression
#default no. of tress are 500, and no of variables split is square root of features which is p
#out of box estimate error is  35.8% which shows about 64% of accuracy
#For first class and second class disagree and strongly disagree the  classification error is very high for predicting disagree and strongly disagree

summary(rf)


#error rate
plot(rf)
#oob error are growing and in some cases decreasing and becoming constant not able to improve errors after 100 trees

#tune mtry
View(TrainSet)
t <- tuneRF(TrainSet[,-15], TrainSet[,15], stepFactor = 0.5,plot=T, ntreeTry = 100,trace=T,improve = 0.05)
t
#oob error is hight for mtry =2 then it fells down reaches about 20. so give idea to choose mtry which is 20



rf1 <- randomForest(q48a ~ EL+TL+IL+Age+Gender, data= TrainSet,ntree=100, mtry=20,importane=T)
print(rf1)
#classification error is very good for agree and strongly agree but still high for disagree and strongly disagree
library(caret)

p2 <- predict(rf1,TestSet)
confusionMatrix(p2,TestSet$q48a)
#accuracy comes down to 62%
hist(treesize(rf1), main='No. of Nodes for the Trees',
     col="green")

#variable importance
varImpPlot(rf1)
rf1
importance(rf1)
varUsed(rf1) 




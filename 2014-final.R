library(ggplot2)        
library(dplyr)
library(tidyr)
library(forcats)
library(grid)
library(gridExtra)
library(tidyverse)
library(MASS)

set.seed(12345)

######################2014##################################
data_2014 <- read.csv("C:/00_Langara/3-Sem/01_DANA4830/Project/2014-aps-employee-census-5-point-dataset.csv", header=TRUE, na.strings=c(" "), stringsAsFactors=T)

View(data_2014)

######selection of columns desired
data_selection_2014 <- dplyr::select(data_2014,c('ï..AS','q1','q2.','q6.'
                                          ,'q20a', 'q20b', 'q20c', 'q20d' ,'q20e', 'q20f', 'q20g'
                                          ,'q55a','q55b','q55c','q55d','q55e','q55f','q55g','q55h','q55i','q55j'
                                          ,'q62a', 'q18d', 'q18h','q62f'
                                          ))

######converting questions to numerical using mutate_at
data_2014_a <- data_selection_2014 %>% mutate_at(c('q20a','q20b','q20c','q20d','q20e','q20f','q20g',
                                                   'q62a', 'q18d', 'q18h','q62f')
                                                 ,funs(recode(.,'Strongly agree'=5,'Agree'=4,'Neither agree nor disagree'=3,'Disagree'=2,'Strongly disagree'=1)))

data_2014_a  <- data_2014_a  %>% mutate_at(c('q55a','q55b','q55c','q55d','q55e','q55f','q55g','q55h','q55i','q55j')
                                           ,funs(recode(.,'Very satisfied'=5,'Satisfied'=4,'Neither satisfied nor dissatisfied'=3,'Dissatisfied'=2,'Very dissatisfied'=1)))

######checking summary before
library(psych)
df2014.1 <- psych::describe(data_2014_a)
df2014.1 <- data.frame(df2014.1)
View(df2014.1)

#missing value of 2014 each column
missing_2014 <- sapply(data_2014_a,function(x)sum(is.na(x)))
data_missing <- data.frame(missing_2014)
View(data_missing)
sum(data_missing)

pctmiss <- function(x){sum(is.na(x)/length(x)*100)}
miss_2014 <- apply(data_2014_a, 1,pctmiss)
table(miss_2014)

#missing value visualization
library(naniar)
vis_miss(data_2014_a,warn_large_data=F,show_perc_col = TRUE,show_perc = TRUE)
gg_miss_var(data_2014_a)

#checking null value row wise
miss_rowwise <- sum(!complete.cases(data_2014_a)) / nrow(data_2014_a)
miss_rowwise
compl.data <- sum(complete.cases(data_2014_a))
compl.data

###missing at random test
library(naniar)
mcar_test(data_2014_a[5:21])

#cleaning the null values
d2014 <- na.omit(data_2014_a)
dim(d2014)
View(d2014)

#summary after cleaning dataset
library(psych)
df2014.2 <- psych::describe(d2014)
df2014.2 <- data.frame(df2014.2)
View(df2014.2)

#checking missing values again
missing_2014 <- sapply(d2014,function(x)sum(is.na(x)))
data_missing <- data.frame(missing_2014)
View(data_missing)
View(d2014)

#dataset without missing values
d2014$year <- 2014
d2014 <- d2014 %>% rename(agency='ï..AS',gender='q1',age='q2.',job_level='q6.')
View(d2014)

write.csv(d2014, file="Cleaned-2014.csv")

#################outliers checking 
mahal_2014 <- mahalanobis(d2014[,5:21], colMeans(d2014[,5:21], na.rm=FALSE), cov(d2014[,5:21], use="pairwise.complete.obs"))
cutoff <- qchisq(1-.001, ncol(d2014))
cutoff
summary(mahal_2014 < cutoff)

d2014_noout <- d2014[mahal_2014<cutoff,5:21]
noout <- data.frame(psych::describe(d2014_noout))
reg <- data.frame(psych::describe(d2014[,5:21]))
sort(noout$mean - reg$mean)

#################correlation check
library(ggcorrplot)
corr <- round(cor(d2014[,5:25],method = "pearson"), 2)
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

#################job level response difference
options(scipen = 999)
d2014$job_level <- ordered(d2014$job_level,
                         levels = c("Trainee/Graduate/APS", "EL", "SES"))
res.aov <- aov(q20a ~ job_level, data=d2014)
summary(res.aov)

TukeyHSD(res.aov)

for(i in 6:ncol(d2014)-1)
{
  column <- names(d2014[i])
  #tidy will summarise and return neat format
  avz <- broom::tidy(aov(d2014[,i] ~ job_level, data = d2014))
  
  # Add this condition if you only want aov with P < 0.05 printed
  if(avz$p.value[1] < 0.05) {
    print(column)
    print(avz)
  }
}

################Exploratory Factor Analysis
###correlation adequacy
library(psych)
library(GPArotation)
corr <- cor(d2014[,5:21])
cortest.bartlett(corr, n=nrow(d2014[,5:21]))
KMO(corr)
KMO(d2014[,5:21])

#########round1 EFA
#using PCA to see number of factors with max variance
library(GPArotation)
pca.data <- d2014[,5:21]
pca.res <- princomp(pca.data)
summary(pca.res)

#using FA parallel analysis to see number of factors 
library(psych)
nofactors <- fa.parallel(pca.data, fm="pa", fa="fa") 
sum(nofactors$fa.values > 0.7) #kaiser criterion shows only 2

fa1 <- fa(pca.data, nfactors=2, rotate="oblimin", fm="pa")
fa1 <- fa(pca.data, nfactors=2, rotate="varimax", fm="pa") #varimax rotation lead to more variables loading to the same factors
fa.diagram(fa1)
print(fa1$loadings, digits=3, cutoff=0.5) 


##round2 
fa.data <- d2014[,c('q55a','q55b','q55c','q55d','q55e','q55f','q55g','q55h','q55i','q55j')]
factor2 <- fa.parallel(fa.data, fm="pa", fa="fa")
sum(factor2$fa.values > 1.0)
sum(factor2$fa.values > 0.7)

fa2 <- fa(fa.data, nfactors=2, rotate="varimax", fm="ml")
print(fa2$loadings, digits=3, cutoff=0.5)
fa.diagram(fa2)


##round3
fa.data3 <- d2014[,c('q20a', 'q20b', 'q20c', 'q20d' ,'q20e', 'q20f', 'q20g')]
factor3 <- fa.parallel(fa.data3, fm="ml", fa="fa")
sum(factor3$fa.values > 1.0)
sum(factor3$fa.values > 0.7)

fa3 <- fa(fa.data3, nfactors=3, rotate="varimax", fm="ml")
print(fa3$loadings, digits=3, cutoff=0.5)
fa.diagram(fa3)

fa3.1 <- fa(fa.data3, nfactors=2, rotate="varimax", fm="ml")
print(fa3.1$loadings, digits=3, cutoff=0.3)
fa.diagram(fa3.1)


##round4 
library(psych)
d2014[d2014$job_level=='Trainee/Graduate/APS' ,
      c( 'q20c', 'q20d' ,'q20e', 'q20f','q20g','q55b','q55c', 'q55d', 'q55h','q62f')]
fa.data4 <- d2014[d2014$job_level=='SES' ,
                  c('q20c', 'q20d' ,'q20e', 'q20f','q55a','q55b','q55c', 'q55d'
                    , 'q55h','q55i','q62f')]
fa.data4 <- d2014[d2014$job_level=='EL' ,
                  c('q20c', 'q20d' ,'q20e', 'q20f','q55a','q55b','q55c', 'q55d'
                    , 'q55h','q55i','q62f')]
fa.data4 <- d2014[d2014$job_level=='Trainee/Graduate/APS' ,
                  c('q20c', 'q20d' ,'q20e', 'q20f','q55a','q55b','q55c', 'q55d'
                    , 'q55h','q55i','q62f')]
res4 <- princomp(fa.data4)
summary(res4)
fviz_eig(res4)

factor4 <- fa.parallel(fa.data4, fm="pa", fa="fa")
sum(factor4$fa.values > 1.0)
sum(factor4$fa.values > 0.7)

fa4 <- fa(fa.data4, nfactors=2, rotate="none", fm="pa")
fa.diagram(fa4, main="Factor Analysis without rotation")

fa4 <- fa(fa.data4, nfactors=2, rotate="oblimin", fm="pa")
fa.diagram(fa4, main="Factor Analysis with oblimin rotation")

fa4 <- fa(fa.data4, nfactors=2, rotate="varimax", fm="pa")
fa.diagram(fa4, main="Factor Analysis with varimax rotation")

print(fa4$loadings, digits=3, cutoff=0.5)


plot(fa4, cut=0.5, labels=colnames(d2014[,c('q20c', 'q20d' ,'q20e', 'q20f','q55a','q55b','q55c', 'q55d'
                                              , 'q55h','q55i','q62f')]))
lr <- dia.ellipse(9,3,"lower right",xlim=xlim,ylim=ylim)

##cronbach alpha reliability
library(psych)
fa.scores <- fa4$scores
colnames(fa.scores) <- c('TL','CL')

TL <- c('q55a','q55b','q55c', 'q55d', 'q55h','q55i','q62f')
CL <- c('q20c', 'q20d' ,'q20e', 'q20f')

psych::alpha(d2014[,TL])
psych::alpha(d2014[,CL])

################Predictive Modelling
library("FactoMineR")
fa.scores <- fa4$scores
colnames(fa.scores) <- c('TL','CL')

fa.scores <- data.frame(cbind(d2014[d2014$job_level=="Trainee/Graduate/APS",], fa.scores))
View(fa.scores)
head(fa.scores)

#innovation
fa.scores$q62a <- factor(fa.scores$q62a, levels=c(1,2,3,4,5), ordered=TRUE)

library(caret)
t.s. <- createDataPartition(fa.scores$q62a, p=0.8, list=FALSE)
train.data <- fa.scores[t.s.,]
test.data <- fa.scores[-t.s.,]

library(MASS)
model1 <- polr(q62a ~  TL + CL + gender + age ,
               data=train.data, Hess=TRUE)
summary(model1)

ctable <- coef(summary(model1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = round(p,4))
print(ctable, digits=3)

library(effects)
plot(Effect(focal.predictors=c('TL','gender'),  model1))
plot(Effect(focal.predictors=c('TL','age'),  model1))
plot(Effect(focal.predictors=c('CL','gender'),  model1))
plot(Effect(focal.predictors=c('CL','age'),  model1))


preds <- predict(model1, newdata=test.data,type="class")
b <- table(preds, test.data$q62a)
sum(diag(b)) / sum(b)

confusionMatrix(b)


library(randomForest)
rf <- randomForest(q62a ~ TL + CL + gender + age , 
                   data=train.data, ntree=100, mtry=2, importance=TRUE)
rf

varImpPlot(rf)

p2 <- predict(rf,test.data)
confusionMatrix(p2,test.data$q62a)

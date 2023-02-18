setwd("D:/Cours MOOC/Introduction à la statistique avec R/Applications") #pour le répertoire
heart <- read.csv("heart.csv")
str(heart)
table(heart$target)
heart$sex <- factor(heart$sex, level=c(0,1), labels=c("F","M"))
table(heart$sex)
barplot(table(heart$sex))
barplot(table(heart$target))
hist(heart$chol, main = "Histogramme de l'age", col="yellow", xlab = "age", ylab="Fréquence")
hist(heart$chol, main = "Histogramme du cholesterol", col="green", xlab = "Cholesterol", ylab="Fréquence")
hist(heart$trestbps, main = "Histogramme du rythme cardiaque", col="blue", xlab = "btpms", ylab="Fréquence")
boxplot(heart$age, xlab="age")
boxplot(heart$age~heart$target)
boxplot(heart$chol~heart$target)
boxplot(heart$trestbps~heart$target)
plot(jitter(heart$age), jitter(heart$trestbps))

cor(heart$age, heart$target, use="complete.obs")
cor(heart$chol, heart$target, use="complete.obs")
cor(heart$trestbps, heart$target, use="complete.obs")
cor(heart$age, heart$trestbps, use="complete.obs")
cor(heart$age, heart$chol, use="complete.obs")

hsub <- subset(heart, heart$age>60, c(age,chol,trestbps))
summary(hsub)
summary(heart)

chisq.test(heart$cp, heart$target, correct=FALSE)
chisq.test(heart$sex, heart$target, correct=FALSE)
fisher.test(heart$sex, heart$target)
by(heart$age, heart$target, mean, na.rm=TRUE)
by(heart$chol, heart$target, mean, na.rm=TRUE)
by(heart$trestbps, heart$target, mean, na.rm=TRUE)
by(heart$age, heart$target, sd, na.rm = TRUE)
t.test(heart$age~heart$target,var.equal=TRUE)
by(heart$chol, heart$target, sd, na.rm = TRUE)
t.test(heart$chol~heart$target,var.equal=TRUE)
by(heart$trestbps, heart$target, sd, na.rm = TRUE)
t.test(heart$trestbps~heart$target,var.equal=TRUE)

wilcox.test(heart$age, heart$target)
wilcox.test(heart$trestbps, heart$target)
wilcox.test(heart$chol,heart$target)

cor.test(heart$age, heart$target)
cor.test(heart$chol, heart$target)
cor.test(heart$trestbps, heart$target)
cor.test(heart$age, heart$chol)
cor.test(heart$age, heart$trestbps)
cor.test(heart$trestbps, heart$chol)

modl <- lm(chol~age, data=heart)
summary(modl)
mod1 <- lm(trestbps~age, data=heart)
summary(mod1)
mod2 <- lm(trestbps~age+chol, data=heart)
summary(mod2)
hist(resid(mod2))
mod3 <- lm(chol~age+sex, data=heart)
summary(mod3)
drop1(mod3,.~.,test="F")

mod4 <- glm(target~age, data = heart, family="binomial")
summary(mod4)

mod5 <- glm(target~age+chol+trestbps+slope+sex, data = heart, family="binomial")
summary(mod5)


library(corrplot)
corrplot(cor(heart[,c("age","chol","trestbps","target","cp")], use="complete.obs"), method = "circle")

#acp
library(psy)
mdspca(heart)
sphpca(smp.l[,var], v=55)
expliquer <- "target"
explicatives <-  c("age", "chol","sex", "slope", "trestbps")
fpca(data = heart, y=expliquer, x=explicatives, partial="No")

cah <- hclust(dist(t(scale(heart))), method="ward.D") #le "t" pour classer les variables et non pas les individus
plot(cah,main="classification hiérarchique")

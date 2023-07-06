                               ###### BUSINESS DATA ANALYTICS ######
              ############################## SECTION 1 ##############################

                 
#Loading the dataset with only ordinal variables...
df <- read.csv("exploratory.csv", header = T)

#Sorting data frames...
tempDF <- df

#Rename the columns using name function...
names(tempDF) <- 1:28

#Check variables as per the values assigned...
names(df[1])
names(df[2])
names(df[28])

#Put the original data frame...
dfFN <- df

#Final data frame...
df <- tempDF

#Nullifying tempDF....
tempDF <- NULL


#Using screeplot to check the number of components to be retained.
fit <- prcomp(df)
screeplot(fit, type = "line")
#According to the screeplot (elbow method) => 2 components!


#Factor analysis for component retention.
#Loading the MASS and paran packages...
library(MASS)
library(paran)
paran(df)
#So, Horn's Parallel Analysis showed 2 components!


#Loading the package psych...
library(psych)

#Reducing the dimensionality of the dataset using Principal Component Analysis to 2 factors...
fit <- pca(df, nfactors = 2)
fit
#The analysis shows that (Rotated Component 1) RC1 is associated with instr!
#As per the analysis, (Rotated Component 2) RC2 is associated with the course!


#Getting the component scores...
RC1 <- fit$scores[,1]
RC2 <- fit$scores[,2]


#Sticking the components RC1 & RC2 to the original data frame with all the categorical
#and ordinal variables!

#Loading the original data frame...
df <- read.csv("Data1.csv", header = T)

#Putting RC1 & RC2 into the original data frame...
df$RC1 <- RC1
df$RC2 <- RC2
#Components are normalized and can be used as dependent variables.


#Converting the independent variables into factors...
df$instr <- as.factor(df$instr) 
fact1 <- factor(df$instr)
fact1                           #Factor with 3 levels
table(df$instr)

df$class <- as.factor(df$class)
fact2 <- factor(df$class)
fact2                           #Factor with 13 levels
table(df$class)

df$nb.repeat <- as.factor(df$nb.repeat)
fact3 <- factor(df$nb.repeat)
fact3                           #Factor with 3 levels
table(df$nb.repeat)

df$attendance <- as.factor(df$attendance)
fact4 <- factor(df$attendance)
fact4                           #Factor with 5 levels
table(df$attendance)

df$difficulty <- as.factor(df$difficulty)
fact5 <- factor(df$difficulty)
fact5                           #Factor with 5 levels
table(df$difficulty)


#Checking the normality of the rotated components using the Q-Q Plot graphical technique...
qqnorm(df$RC1, main ="RC1 - Normal Q-Q Plot")
qqline(df$RC1, col = "red")

qqnorm(df$RC2, main ="RC2 - Normal Q-Q Plot")
qqline(df$RC2, col = "blue")
#As per the Q-Q Plot, the components showed fairly normal distribution.


######## Is there an effect of independent variable "instr" on dependent variable "RC1"? ########

#H0: There is no effect of instr on RC1
#H1: There is an effect of instr on RC1

#Loading the car package...
library(car)

#Checking the homogeneity of variance using Levene's Test...
leveneTest(df$RC1 ~ df$instr)
#[F(2,5817 = 7.4841, p < 0.001)]
#The test shows that the parametric assumptions are violated since the p-value is less than 0.05.


#Graphical representation to identify the presence of outliers using Boxplot...
boxplot(df$RC1 ~ df$instr)
#Box representing 50% of the central data, with a line inside that represents median.


#Since parametric assumptions were violated, non-parametric alternative of ANOVA,
#Kruskal-Wallis Test will be used...

#Loading the package reshape2...
library(reshape2)

#Kruskal-Wallis Test to see if there's a significant difference...
kruskal.test(df$RC1 ~ df$instr)
#Kruskal-Wallis Test showed significant result [chi-squared = 44.44, p-value < 0.05], 
#indicating a significant difference between two or more of the group means.


#Performing post-hoc Dunn Test, to recognise where the difference lies...

#Loading the FSA package...
library(FSA)

#Following the Dunn Test...
dunnTest(RC1 ~ instr, data = df)

#Calculating the mean
mean(df[df$instr == "1",]$RC1)   # -0.03311975
mean(df[df$instr == "2",]$RC1)   #  0.1499601
mean(df[df$instr == "3",]$RC1)   # -0.05300598

#The results indicate that there is a significant difference between the average satisfaction of
#instr [2] (Mean = 0.1499601) and instr [3] (Mean = -0.05300598) (Z = 6.666141, p < 0.05). 
#Also, there is a significant difference between the average satisfaction outcome of
#instr [1] (Mean = -0.03311975) and instr [2] (Mean = 0.1499601) (Z = -3.374740, p < 0.05).

#Hence, we can eliminate the null hypothesis, 
#since there is an effect of instructor satisfaction on RC1


##### Is there an effect of independent variable "nb.repeat" on dependent variable "RC2"? #####


#H0: There is no effect of nb.repeat on RC2
#H1: There is an effect of nb.repeat on RC2


#Checking the homogeneity of variance using Levene's Test...
leveneTest(df$RC2 ~ df$nb.repeat)
#[F(2,5817 = 0.3571, p > 0.05)]
#The results imply that the parametric assumptions are not violated.

#Can't run the t.test, since the grouping factor must have exactly 2 levels.

#(The OMNIBUS test)
#Moving ahead with ANOVA, to know if there is a significant difference in the means...
fit2 <- aov(RC2 ~ nb.repeat, data = df)
summary(fit2)
summary.lm(fit2)

#Load the package lsr...
library(lsr)

#Compute etaSquared...
etaSquared(fit2)   #0.0002130822
#[F(2,5817 = 0.62, p > 0.05, multiple R-squared = 0.0002)]
#The results imply that there is no significant effect of "nb.repeat" on "RC2" since p > 0.05.

#Since the results of ANOVA showed no significant effect, 
#there is no need to run the post-hoc test i.e., TukeyHSD


#Plotting graph with confidence intervals...
#sjPlot
library(sjPlot)
fit2_plot <- lm(RC2 ~ nb.repeat, data = df)
plot_model(fit2_plot, type = "pred")
#Plot representing no significant differences in the means.


#Hence, we can accept the null hypothesis that there is no effect of number of repeats on RC2.


##### Effect of instr, class, nb.repeat, attendance and difficulty on RC1 & RC2 using MANOVA #####


#MANOVA (Multivariate analysis of variance)...
fit3 <- manova(cbind(df$RC1, df$RC2) ~ (df$instr + df$class + df$nb.repeat + df$attendance + df$difficulty))
#Results for Omnibus:
summary(fit3)
#Results for each dependent variable:
summary.aov(fit3)
#The results showed significant main effect of instr [F(2, 5796) = 40.91, p < 0.001, V = 0.028], 
#class [F(11, 5796) = 11.97, p < 0.001, V = 0.04], attendance [F(4, 5796) = 32.31, p < 0.001, V = 0.04] 
#and difficulty[F(4, 5796) = 18.96, p < 0.001, V = 0.026]


                 ######################### Interactions #########################

                                         ##### RC1 #####


#Run ANOVA to check RC1 ~ instr*class...
fit4 <- aov(RC1 ~ instr*class, data = df)
summary.aov(fit4)
etaSquared(fit4)
#The omnibus test indicated that “instr” had a significant effect on “RC1”,
#[F(2, 5806) = 22.44, p < 0.001, etaSquared = 0.006]. 
#Also, the “class” had a significant effect on “RC1”,
#[F(11, 5806) = 14.65, p < 0.001, etaSquared = 0.027].

#Post-hoc Test for two way ANOVA...
TukeyHSD(fit4)
#The results showed significant differences!

#Load the library ggplot2 & ggpubr...
library(ggplot2)
library(ggpubr)

#ggline plot...
ggline(df, main = "Instructor as per the Class", x = "class", y = "RC1", color = "instr", add = c("mean_ci")) 
#The graph showed significant effect!


#Run ANOVA to check RC1 ~ instr*nb.repeat...
fit5 <- aov(RC1 ~ instr*nb.repeat, data = df)
summary.aov(fit5)
etaSquared(fit5)
#The results showed no significant interaction.
ggline(df, main = "Instructor as per number of repeats", x = "nb.repeat", y = "RC1", color = "instr", add = c("mean_ci")) 



#Run ANOVA to check RC1 ~ instr*attendance...
fit6 <- aov(RC1 ~ instr*attendance, data = df)
summary.aov(fit6)
etaSquared(fit6)
#The results showed no significant interaction.
ggline(df, main = "Instructor as per the Attendance", x = "attendance", y = "RC1", color = "instr", add = c("mean_ci")) 



#Run ANOVA to check RC1 ~ instr*difficulty...
fit7 <- aov(RC1 ~ instr*difficulty, data = df)
summary.aov(fit7)
etaSquared(fit7)
#The results showed no significant interaction.
ggline(df, main = "Instructor as per difficulty", x = "difficulty", y = "RC1", color = "instr", add = c("mean_ci")) 



                                         ##### RC2 #####


#Run ANOVA to check RC2 ~ nb.repeat*difficulty...
fit8 <- aov(RC2 ~ nb.repeat*difficulty, data = df)
summary.aov(fit8)
etaSquared(fit8)
#The results showed no significant interaction.
ggline(df, main = "Number of repeats as per difficulty", x = "difficulty", y = "RC2", color = "nb.repeat", add = c("mean_ci")) 



#Run ANOVA to check RC2 ~ attendance*difficulty...
fit9 <- aov(RC2 ~ attendance*difficulty, data = df)
summary.aov(fit9)
etaSquared(fit9)
#The results showed no significant interaction.
ggline(df, main = "Attendance as per difficulty", x = "difficulty", y = "RC2", color = "attendance", add = c("mean_ci")) 



#Run ANOVA to check RC2 ~ nb.repeat*instr...
fit10 <- aov(RC2 ~ nb.repeat*instr, data = df)
summary.aov(fit10)
etaSquared(fit10)
#The results showed no significant interaction.
ggline(df, main = "Number of repeats as per Instructor", x = "instr", y = "RC2", color = "nb.repeat", add = c("mean_ci")) 






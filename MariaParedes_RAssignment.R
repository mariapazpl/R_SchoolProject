---
title: "R_Assignment_5"
author: "Maria Paz Paredes Lagos"
date: "`r Sys.Date()`"
output: html_document
---

Question 1:
This code takes a sample of probabilities of a coin (Head versus Tail) randomly 50 times, with replacement in every run. We called the function "Coin.Throw" and "Tab" to assigned the table of this one. Barplot will help us to visualize the probability of the coin and the colors will differentiate between tail and head.

```{r}
Coin.Throw<-sample(c('H','T'),50 ,replace=TRUE)
Coin.Throw
tab<-table(Coin.Throw)
barplot(tab,
        xlab= "Head versus Tail",
        ylab= "Quantity of throws",
        main= "Thrown of a Coin",
        col=c("#DF536B","#990000"))
```

Question 2:
Similar to the problem 1, this code will show the probability of a coin with a sample size of 100. The relative frequency will measure the likelihood of the event occurring compared to the total of trails. We can calculated by dividing the number of favorable outcomes (cumulative sum of elements) by the total number of flips.

```{r}
CoinThrow2<-sample(c(0, 1), 100, replace=TRUE)
table(CoinThrow2)
cumsum(CoinThrow2)
Relfreq<-cumsum(CoinThrow2)/length(CoinThrow2)
num<-1:100
plot(num, Relfreq,
     pch ="*",
     ylab = "Frequency",
     xlab = "Sample Size",
     main = "Relative frequency of a Coin",
     col = "#009999")
```

Question 3:
Two players gambling on a coin game, Amy wins 1 dollar if head comes up or loss 1 dollar otherwise. This parameter will help us to write our code, our sample will between (1, -1) from winning or losing, with a sample space of 100 and replacement in every run. 
As well as in the last question, we need to know the relative frequency and plot it.

```{r}
Gambling <-sample(c(1, -1),100, replace = TRUE)
table(Gambling)
cumsum(Gambling)
Relfreq <- cumsum(Gambling)/length(Gambling)
num<-1:100
plot(num, Relfreq,
     pch = "*",
     ylab = "Frequency",
     xlab = "Sample Size",
     main = "Relative frequency of Gambling",
     col = "#9933FF")
```

Question 4:
We were giving data from the Titanic and our first step is to load our data into the program, we did this by storing the exact location of the data in the variable "mydata1" with the mode of reading 
The head function will give us part of the data, and the function names will give us the names of the variables for the two-way tables.


```{r}
mydata1 <-read.csv("C:\\Users\\maria\\Downloads\\TitanicDataA5.csv", header = TRUE)

head(mydata1)
names(mydata1)
```
(C) We will store every variable with their respective names from the giving data. This will help us to create easy and clear table with the data. The tables will help us with the probabilities of the variables.
```{r}
Fate<-mydata1$Fate
Age<- mydata1$Age
Sex<-mydata1$Sex
Pclass<-mydata1$Pclass

TableFA<-table(Fate, Age)
TableFS<-table(Fate, Sex)
TableFP<-table(Fate, Pclass)
```
(D) Installing a colors palette to plot graphs from the tables that we already did. The colors will help us to differentiate between the outputs. Each graph have their respective title and axis names.
```{r}
library("RColorBrewer")
plot(TableFA,
     main = "Table Fate and Age",
     col = brewer.pal(n= 4, name = "RdBu"))
plot(TableFP,
     main = "Table Fate and Pclass",
     col = brewer.pal(n= 3, name = "RdBu"))
plot(TableFS,
     main = "Table Fate and Sex",
     col = brewer.pal(n= 3, name = "RdBu"))
```
(E) To calculate relevant probabilities, I worked with conditional probabilities based on the variables. We will use margin in our probabilities. The "margin" parameter determines whether we want to calculate probabilities along rows (margin == 1) or columns (margin ==2)
```{r}
prob_survival_given_class<-prop.table(TableFP, margin = 1)

TableSA<-table(mydata1$Sex, mydata1$Age)
prob_sex_given_age<-prop.table(TableSA, margin = 2)

Prob_fate_given_class<-prop.table(TableFP, margin = 2)

cat("Probability of survival given passenger class:")
prob_survival_given_class

cat("Probability of being male or female given age group:")
prob_sex_given_age

cat("Probability of different fates given passanger class:")
Prob_fate_given_class
```
(F) This code uses the "itself" function to create a new variable "Survived" with values 1 for "Survived" and 0 for "Died". The Overall survival rate is calculated by dividing the number of passenger who survived by the total of passengers
```{r}
mydata1$Survived<-ifelse(mydata1$Fate == "Survived", 1, 0)

total_passengers<-nrow(mydata1)
total_passengers
total_survived<-sum(mydata1$Survived == 1)
total_survived
overall_survival_rate<-total_survived / total_passengers

print("Overall Survival Rate:")
round(overall_survival_rate, 2)
```
After finding the overall survival rate, we will do the same with every category (females, males, kids, young, adults, 1st class, 2nd class, 3rd class) To find each category we will sum the survival data with each category data.
```{r}
survived_females <-sum(mydata1$Survived == 1 & mydata1$Sex == "Female")
female_survival_rate <-survived_females/total_survived
print("Overall Female Survival Rate:")
round(female_survival_rate, 2)

survived_males <-sum(mydata1$Survived == 1 & mydata1$Sex == "Male")
male_survival_rate<-survived_males/total_survived
print("Overall Male Survival Rate:")
round(male_survival_rate, 2)

kids_survival<-sum(mydata1$Survived == 1 & mydata1$Age == "17 & Below")
kids_rate<-kids_survival/total_survived
print("Overall Kids Survival Rate:")
round(kids_rate, 2)

young_survival<-sum(mydata1$Survived == 1 & mydata1$Age == "18-30")
young_rate<-young_survival/total_survived
print("Overall Young Survival Rate:")
round(young_rate, 2)

adults_survival<-sum(mydata1$Survived == 1 & mydata1$Age == "31-60")
adults_rate<-adults_survival/total_survived
print("Overall Adults Survival Rate:")
round(adults_rate, 2)

elderly_survival<-sum(mydata1$Survived == 1 & mydata1$Age == "61 Above")
elderly_rate<-elderly_survival/total_survived
print("Overall Elderly Survival Rate:")
round(elderly_rate, 2)

Fclass_survival<-sum(mydata1$Survived == 1 & mydata1$Pclass == "1st Class")
Fclass_rate<-Fclass_survival/total_survived
print("Overall First Class Survival Rate:")
round(Fclass_rate, 2)

Sclass_survival<-sum(mydata1$Survived == 1 & mydata1$Pclass == "2nd Class")
Sclass_rate<-Sclass_survival/total_survived
print("Overall Second Class Survival Rate:")
round(Sclass_rate, 2)

Tclass_survival<-sum(mydata1$Survived == 1 & mydata1$Pclass == "3rd Class")
Tclass_rate<-Tclass_survival/total_survived
print("Overall Third Class Survival Rate:")
round(Tclass_rate, 2)
```
After we got every survival rate, we can determinate that females, adults, and first class have a significantly higher likelihood of survival.



Question 5:
(A) To solve this question we will have a similar approach from the last question to load our data into the program. As well as, determinate each variable to create two ways tables.

```{r}
mydata2 <-read.csv("C:\\Users\\maria\\Downloads\\ChildDataB52.csv", header = TRUE)
attach(mydata2)
names(mydata2)

Sex<-mydata2$Sex
Domhand<-mydata2$Domhand
SchWorkPressure<-mydata2$SchWorkPressure
Preferred2be<-mydata2$Preferred2be

table_SD<-table(Sex, Domhand)
table_SW<-table(Sex, SchWorkPressure)
table_SP<-table(Sex, Preferred2be)
```
(D) After we have every table, we will have a clean code to plot each graph. Every graph will have their respective names and we will use the function "legend.text" to differentiate the stack data.
```{r}
barplot(table_SW,
        ylab = "Quantity of Students",
        main = "Work Pressure of Students",
        col = c("#B2182B", "#4393C3"),
        legend.text = rownames(table_SD),
        beside = FALSE)

barplot(table_SP,
        ylab = "Quantity of Students",
        main = "What Students wants to be",
        col = c("#B2182B", "#4393C3"),
        legend.text = rownames(table_SD),
        beside = FALSE)

barplot(table_SD,
        ylab = "Quantity of Students",
        main = "Dominant Hand of Students",
        col = c("#B2182B", "#4393C3"),
        legend.text = rownames(table_SD),
        beside = FALSE)
```

(E) After installing the package "psych" we will load it, the function "describe" is used to obtain descriptive statistics that includes columns 2 to 4.
```{r}
library("psych")
describe(mydata2[,2:4])

```

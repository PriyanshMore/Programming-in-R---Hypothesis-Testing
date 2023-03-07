printname <- ("FINAL PROJECT")
prefixname <- paste("MORE", printname, sep = " ")
prefixname

#install.packages(c("FSA", "FSAdata", "magrittr", "dplyr", "tidyr", "plyr", 
#"tidyverse", "psych", "janitor", "plotrix", "ggplot2", "lubridate", "cleaner", "epidisplay"))
lapply(c("plyr", "magrittr", "dplyr", "plotrix", "ggplot2", "cleaner", "grid", "gridExtra", "MASS", "Hmisc", "fastDummies",
         "moments", "janitor", "tidyverse", "lubridate", "psych", "reshape", "reshape2", "corrplot", "stargazer",
         "epiDisplay", "FSA", "FSAdata", "ggthemes", "crosstable","knitr","bruceR", "shiny", "ggiraphExtra"), 
       require, character.only = TRUE)

options(max.print = 100000)
Car_new <- read.csv("car_adv.csv")

#Cleaning_dataset (Replacing NA/Blanks with 0 and character)
Car_new$engV[is.na(Car_new$engV)] <- 0

Car_new$drive[Car_new$drive == ""] <- "awd"

#Using_gusb_function_to_replace_full_with_awd(all_wheel_drive)
Car_new$drive <- gsub("full", "awd", Car_new$drive)

#Rename column in dataset 
colnames(Car_new)[colnames(Car_new) == "engV"] = "engine"

#Drop column from dataset
Car_new <- Car_new[-7]

#Take the complete cases into a new dataset called Car_new
Car_new <- Car_new[complete.cases(Car_new),]

#Removing unrealistic data 0 price, mileage, engine volumne larger than 10 L and equal to 0 
Car_new <- Car_new[!Car_new$price == 0,]
Car_new <- Car_new[!Car_new$mileage == 0,]
Car_new <- Car_new[!Car_new$mileage > 400,]
Car_new <- Car_new[!Car_new$engine > 8,]
Car_new <- Car_new[!Car_new$engine <= 0.99,]


#Descriptive Statistics of Dataset
class(Car_new)
str(Car_new)
summary(Car_new)
describe(Car_new)

#Filtering and Sorting Data by Fuel Type OR CREATING SUB DATASET
Gas <- filter(Car_new, engType == "Gas")
#Gas
Petrol <- filter(Car_new, engType == "Petrol")
#Petrol
Diesel <- filter(Car_new, engType == "Diesel")
#Diesel
Other <- filter(Car_new, engType == "Other")
#Other
Front <- filter(Car_new, drive == "front")
#Front
Rear <- filter(Car_new, drive == "rear")
#Rear
AWD <- filter(Car_new, drive == "awd")
#AWD

#ScatterPlot
pchs <- c("+", "x")
cols <- c("red", "grey49")
plot(Car_new$mileage, Car_new$engine, main = "Cars Mileage vs Engine Size", cex.main = 1,
     xlim = c(0,400), ylim = c(1,8), xlab = "Mileage(Miles Per Gallon)",
     ylab = "Types Engine Size", col.main = "blue", col.lab = "blue",
     col = cols, pch = pchs)

#BOXPLOT
boxplot(Diesel$mileage, Petrol$mileage, Gas$mileage, Other$mileage,
        main = "BOXPLOT", ylim = c(0,500), ylab = "MILEAGE PER GALLON",
        col.main = "darkblue", col.axis = "Brown", col.lab = "darkblue",
        xlab = "ENGINE TYPE", names = c("Diesel", "Petrol", "Gas", "Other"),
        las = 2, cex.axis = 0.7, cex.main = 1)


#JITTER Function
attach(Car_new)
def_par <- par(mar=c(6,4.5,3,3))
par(mfcol = c(1,2))
plot(year, mileage, main = "WITHOUT JITTER MILEAGE OF CAR", xlab = "YEAR", 
     ylab = "NUMBER OF CARS SOLD", ylim = c(0,400), cex = 1, col.main = "darkblue", 
     cex.axis = 0.8, col.axis = "blue", col.lab = "darkblue", cex.main = 0.8)
Year_jitter <- jitter(year)
plot(Year_jitter, mileage, main = "JITTER MILEAGE OF CAR", xlab = "YEAR", 
     ylab = "NUMBER OF CARS SOLD", ylim = c(0,400), cex = 1, col.main = "darkblue", 
     cex.axis = 0.8, col.axis = "blue", col.lab = "darkblue", cex.main = 0.8)    

sample <- filter(Car_new, drive == "rear")
#sample

#P-Value Hypothesis Testing and T-Test
mu = mean(Car_new$price)
sd = sd(sample$price)
n = 758
sm = mean(sample$price)

t = (sm - mu)/(sd/sqrt(n))
t

#P value for Two-tail test
2*pt(-abs(t),df = n-1)

#P value One-tail test
pt(-abs(t),df = n-1)

#T-Test
t.test(sample$price, mu = mu )

#TEST 1 Linear Regression
relation <- lm(Other$engine~Other$mileage)
print(summary(relation))
stargazer(relation,type = 'text')
plot(Other$engine, Other$mileage, col="royalblue", main="Other Cars Engine Size and Mileage Regression",
     abline(lm(Other$mileage~Other$engine)),cex=1.3, pch=16, xlab="", ylab="Mileage")


#TEST 2 DUMMY VARIABLE
car_adv2 <- subset(Car_new, drive == 'awd' | drive == 'rear', select = price : drive)

drive <- dummy_cols(car_adv2, select_columns = "drive")
#drive

#Model 1
model <- lm(price ~ mileage + drive_awd, data = drive)
summary(model)
ggPredict(model, se = TRUE, interactive = TRUE)

#Model 2
model2 <- lm(price ~ mileage + drive_rear, data = drive)
summary(model2)
ggPredict(model2, se = TRUE, interactive = TRUE)


#TEST 3
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)
AnovaM <- lm(Car_new$price ~ mileage + engine, data=Car_new)
library(car)
anova(AnovaM)
summary(AnovaM)
M <- Car_new[,c(4,7)]
C <- cor(M)
corrplot(C, method="pie")

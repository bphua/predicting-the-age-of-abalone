#Cleaning
#install.packages("reshape")
library(reshape)
new_names <- c("Sex", "Length", "Diameter", "Height", "Whole weight", "Shucked weight", "Viscera weight", "Shell weight", "Rings")

colnames(data) <- new_names

summary(data)

#Length, Diameter, Height in mm
#Weight in g

#Outliers
melt(data) %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(outlier.size=2, outlier.colour='blue') + facet_wrap(~ variable, scales = 
  "free") 

#Remove Height = 0.515mm and 1.13mm as it is greater than Q3 + (1.5*IQR) = 0.24
new_data <- subset(data, data$Height < 0.5)
ggplot(new_data, aes(y = Height)) +
geom_boxplot(outlier.size=3, outlier.colour='blue')

#Remove measurements that equals to 0 as well as weight s.t. `Shucked weight` + `Viscera weight` + `Shell weight` > `Whole weight`
new_data %>% filter(Length, Diameter, Height, `Whole weight`, `Shucked weight`, `Viscera weight`, `Shell weight` == 0) %>% filter(`Shucked weight` + `Viscera weight` + `Shell weight` > `Whole weight`)
summary(new_data)

#Multivariate Normality
#install.packages("energy")
#install.packages("mvtnorm")
library(energy)
library(mvtnorm)
data_less_sex <- new_data %>% select(-Sex)
mvnorm.e(data_less_sex)

#install.packages("MVN")
library(MVN)
result <- mvn(data = data_less_sex, mvnTest = "dh")
result$multivariateNormality
result <- mvn(data = data_less_sex, mvnTest = "dh", univariatePlot = "histogram")

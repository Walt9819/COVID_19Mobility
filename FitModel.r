library(tibble)
library(dplyr)
library(magrittr)
library(ggplot2)

path = "D:\\Documentos\\MT\\Mobility\\MobilityGitHub\\Data\\"

data = read.csv(paste0(path, "DataMexico", ".csv"))
data$Date = as.Date(data$Date)

pdata = data %>% filter(Region == "NACIONAL")
lm.fit <- lm(Cases ~ Workplaces, data = pdata)
summary(lm.fit)
plot(pdata)

ggplot(pdata, aes(x=Date)) +
  geom_point(aes(y = Cases), color = "darkred") +
  geom_point(aes(y = Residential), color="steelblue")

plot(pdata$Workplaces, pdata$Cases)
abline(lm.fit, col="blue")

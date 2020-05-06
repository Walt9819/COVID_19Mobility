library(tibble)
library(dplyr)
library(magrittr)
library(ggplot2)
library(R0)

path = "D:\\Documentos\\MT\\Mobility\\MobilityGitHub\\Data\\"

data = read.csv(paste0(path, "DataMexico", ".csv"))
data$Date = as.Date(data$Date)

pdata = data %>% filter(Region == "NACIONAL" & Date >= "2020-02-27")
selectedD <- pdata %>% transmute(Cases) %>% unlist() %>% as.character() %>% as.numeric()
names(selectedD) <- pdata$Date

GT.covid<-generation.time("gamma", c(5.07, 2.21))
estR0.TD <- est.R0.TD(selectedD, GT.covid, t = names(selectedD), time.step=1, nsim=500)
#fix(estR0.TD)
#plot(estR0.TD)
pdata$R0 = unname(estR0.TD$R)

lm.fit <- lm(R0 ~ Residential, data = pdata)
summary(lm.fit)
# 1. Open jpeg file
jpeg(paste0(path, "rplot.jpg"), width = 350, height = 350)
# 2. Create the plot
plot(pdata$Residential, pdata$R0, xlab="Residential %", ylab="R0")
abline(lm.fit, col="blue")
# 3. Close the file
dev.off()

ggplot(pdata, aes(x=Date)) +
  geom_point(aes(y = Cases), color = "darkred") +
  geom_point(aes(y = Residential), color="steelblue")

#estimateR0 <- estimate.R(newNCD[54:length(newNCD)], GT.covid, begin=1, end=60, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=1e+06, nsim=100)

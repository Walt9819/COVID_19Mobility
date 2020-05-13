library(dplyr)
library(magrittr)
library(ggplot2)
library(EpiEstim)

path = "D:\\Documentos\\MT\\Mobility\\MobilityGitHub\\Data\\"
setwd(path)

data = read.csv(paste0(path, "DataMexico", ".csv"))
data$Date = as.Date(data$Date)

pdata = data %>% filter(Region == "NACIONAL" & Date >= "2020-02-28")

d <- data.frame("dates" = pdata$Date, "I" = pdata$Cases)
##########################################
#-------------------------Control-------------------------------------------
#Considerando la ultima ventana de 7 dias
t_start <- c(2, 25, nrow(d)-7)
t_end <- c(24, nrow(d)-8, nrow(d))


#Parametros de distribucion Gamma a priori
#------Parametros de Nishiura
mean_nish<-4.7
sd_nish<-2.9
#------Parametros de Du
mean_du<-3.96
sd_du<-4.75


#--------------------------Control---------------------------------------
#Funcion para estimar R_t con ventanas disjuntas
fun.estim_rts_control<-function(d,t_st,t_e,mean_gamma,sd_gamma)
{
  rts_control <- estimate_R(d,
                         method="parametric_si",
                         config = make_config(list(
                           t_start = t_st,
                           t_end = t_e,
                           mean_si = mean_gamma,
                           std_si = sd_gamma))
  )
  return(rts_control)
}

pdf("NewResults.pdf")
rts_control_nish=fun.estim_rts_control(d, t_start, t_end, mean_nish, sd_nish)
plot(rts_control_nish, "R")+
  geom_hline(aes(yintercept = 1), color = "red", lty = 2)

rts_control_du=fun.estim_rts_control(d, t_start, t_end, mean_du, sd_du)
plot(rts_control_du, "R")+
  geom_hline(aes(yintercept = 1), color = "red", lty = 2)

#--------------------------Uncertain---------------------------------------
config_nish <- make_config(list(mean_si = 4.7, std_mean_si = 1,
                                min_mean_si = 3.1, max_mean_si = 6.3,
                                std_si = 2.9, std_std_si = 0.5,
                                min_std_si = 1.4, max_std_si = 4.4))

config_du <- make_config(list(mean_si = 3.96, std_mean_si = 1,
                              min_mean_si = 2.36, max_mean_si = 5.56,
                              std_si = 4.75, std_std_si = 0.5,
                              min_std_si = 3.25, max_std_si = 6.25))

#Funcion para estimar R_t con ventanas disjuntas
fun.estim_rts_uncertain<-function(data,config_gamma)
{ rts_uncertain <- estimate_R(data,
                              data, method = "uncertain_si",
                              config = config_gamma)
  return(rts_uncertain)
}

rts_uncertain_nish=fun.estim_rts_uncertain(d,config_nish)
plot(rts_uncertain_nish, legend = TRUE)

rts_uncertain_du=fun.estim_rts_uncertain(d,config_du)
plot(rts_uncertain_du, legend = TRUE)

barplot(d$I, names.arg = d$dates)
dev.off()

#### R0 approach ####
#library(R0)
#selectedD <- pdata %>% transmute(Cases) %>% unlist() %>% as.character() %>% as.numeric()
#names(selectedD) <- pdata$Date

#GT.covid<-generation.time("gamma", c(5.07, 2.21))
#estR0.TD <- est.R0.TD(selectedD, GT.covid, t = names(selectedD), time.step=1, nsim=500)
#fix(estR0.TD)
#plot(estR0.TD)
#pdata$R0 = unname(estR0.TD$R)

#lm.fit <- lm(R0 ~ Residential, data = pdata)
#summary(lm.fit)
# 1. Open jpeg file
#jpeg(paste0(path, "rplot.jpg"), width = 350, height = 350)
# 2. Create the plot
#plot(pdata$Residential, pdata$R0, xlab="Residential %", ylab="R(t)")
#abline(lm.fit, col="blue")
# 3. Close the file
#dev.off()

#ggplot(pdata, aes(x=Date)) +
#  geom_point(aes(y = Cases), color = "darkred") +
#  geom_point(aes(y = Residential), color="steelblue")

#estimateR0 <- estimate.R(newNCD[54:length(newNCD)], GT.covid, begin=1, end=60, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=1e+06, nsim=100)

library(dplyr)
library(magrittr)
library(ggplot2)
library(EpiEstim)
library(incidence)

path = "D:\\Documentos\\MT\\Mobility\\MobilityGitHub\\Data\\"
setwd(path)
set.seed(1)

mob <- read.csv(paste0(path, "GlobalMobilityReport.csv"))
mob <- mob %>% filter(country_region_code == "MX")
states <- unique(mob$sub_region_1)
estados <- c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
                                              "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Ciudad de México",
                                              "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                              "Sinaloa", "Sonora", "México", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")

for (state in seq(length(states)))
{
  mob$sub_region_1[mob$sub_region_1 == states[state]] <- estados[state]
}
names(mob) <- c("Country Code", "Country", "State", "SubState", "Date", "RetailRecreation", "GroceryPharmacy", "Parks", "TransitStations", "Workplaces", "Residential")
mob <- mob %>% select(Date, State, RetailRecreation, GroceryPharmacy, Parks, TransitStations, Workplaces, Residential)
Dates <- c(min(mob$Date), max(mob$Date))

file.entidades <- read.csv(paste0(path, "entidades.csv"))
totdata <- read.csv(paste0(path, "TotalMX.csv"))
totdata$FECHA_ACTUALIZACION = as.Date(totdata$FECHA_ACTUALIZACION)
totdata$FECHA_INGRESO = as.Date(totdata$FECHA_INGRESO)
totdata$FECHA_SINTOMAS = as.Date(totdata$FECHA_SINTOMAS)
ndat <- totdata %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES) %>% filter(RESULTADO == 1 & FECHA_SINTOMAS >= Dates[1] & FECHA_SINTOMAS <= Dates[2]) %>% summarise("I" = n()) %>% select(FECHA_SINTOMAS, ENTIDAD_RES, I)
ndat <- ndat %>% filter(ENTIDAD_RES <= 32)

days <- seq(as.Date(Dates[1]), as.Date(Dates[2]), 1)
for (enti in seq(length(unique(ndat$ENTIDAD_RES))))
{
  days0 <- days[!(days %in% (ndat$FECHA_SINTOMAS[ndat$ENTIDAD_RES == file.entidades[enti, 1]]))]
  tmp.ndat <- data.frame(FECHA_SINTOMAS = days0, ENTIDAD_RES = file.entidades[enti, 1], I = 0)
  ndat <- rbind(ndat, tmp.ndat)
  rm(tmp.ndat, days0)
}

for (enti in seq(length(unique(ndat$ENTIDAD_RES))))
  ndat$ENTIDAD_RES[ndat$ENTIDAD_RES == file.entidades[enti, 1]] <- file.entidades[enti, 2]

ndat <- as.data.frame(rbind(ndat, data.frame(ndat %>% group_by(FECHA_SINTOMAS) %>% summarise("I" = sum(I)), ENTIDAD_RES = "Estados Unidos Mexicanos")))
names(ndat) <- c("Date", "State", "I")

###############################################################
mobData <- merge(ndat, mob, by = c("Date", "State"))
###############################################################

slideMean <- function(data, window){
  total <- length(data)
  remain <- mod(total, window) - 1
  spots <- seq(from=1, to=total-window, by=window)
  result <- vector(length = total)
  for(i in spots){
    result[i:(i+window-1)] <- mean(data[i:(i+window-1)])
  }
  if (remain >= 0)
    result[(total-remain):total] <- mean(data[(total-remain):total])
  return(result)
}

## Select State ##
mobData %>% select("State") %>% unique()
state <- "Ciudad de México"
d <- data.frame("dates" = mobData %>% filter(State == state) %>% select(Date), "I" = mobData %>% filter(State == state) %>% select(I))

plot(as.incidence(d$I))


##########################################
#### R(t) inference from data ############
##########################################
#-------------------------Control-------------------------------------------
#Considerando la ultima ventana de 7 dias
sinceQuarintine <- as.numeric(as.Date("2020-03-23") - as.Date(Dates[1]))
t_start <- c(2, sinceQuarintine, nrow(d)-7)
t_end <- c(sinceQuarintine - 1, nrow(d)-8, nrow(d))

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

rts_control_nish=fun.estim_rts_control(d, t_start, t_end, mean_nish, sd_nish)
plot(rts_control_nish, "R")+
  geom_hline(aes(yintercept = 1), color = "red", lty = 2)

rts_control_du=fun.estim_rts_control(d, t_start, t_end, mean_du, sd_du)
plot(rts_control_du, "R")+
  geom_hline(aes(yintercept = 1), color = "red", lty = 2)


#--------------------------Uncertain---------------------------------------
config_nish <- make_config(list(t_start = t_start, t_end = t_end,
                                mean_si = 4.7, std_mean_si = 1,
                                min_mean_si = 3.1, max_mean_si = 6.3,
                                std_si = 2.9, std_std_si = 0.5,
                                min_std_si = 1.4, max_std_si = 4.4))

config_du <- make_config(list(t_start = t_start, t_end = t_end,
                              mean_si = 3.96, std_mean_si = 1,
                              min_mean_si = 2.36, max_mean_si = 5.56,
                              std_si = 4.75, std_std_si = 0.5,
                              min_std_si = 3.25, max_std_si = 6.25))

#Funcion para estimar R_t con ventanas disjuntas
fun.estim_rts_uncertain<-function(data,config_gamma)
{ rts_uncertain <- estimate_R(data,
                              method = "uncertain_si",
                              config = config_gamma)
  return(rts_uncertain)
}

rts_uncertain_nish=fun.estim_rts_uncertain(d,config_nish)
plot(rts_uncertain_nish, legend = TRUE)

rts_uncertain_du=fun.estim_rts_uncertain(d,config_du)
plot(rts_uncertain_du, legend = TRUE)

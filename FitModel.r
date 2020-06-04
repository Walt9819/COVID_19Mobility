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

mobA <- read.csv(paste0(path, "GlobalMobilityApple.csv"))
mobA <- mobA %>% filter(region == "Mexico" | (country == "Mexico")) %>% filter(transportation_type == "driving")
mobAMX <- mobA %>% filter(geo_type %in% c("sub-region", "country/region") | region == "Mexico City")
mobAMX <- mobAMX %>% select(-c(geo_type, transportation_type, alternative_name, sub.region, country))
mobAMX$region <- c("Estados Unidos Mexicanos", "Ciudad de México", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
                    "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
                    "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                    "Sinaloa", "Sonora", "México", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")

names(mobAMX)[2:length(names(mobAMX))] <- sub("X", "", names(mobAMX)[2:length(names(mobAMX))])
mobDriv <- data.frame()
for (state in seq(nrow(mobAMX)))
{
  tmp.mobA <- data.frame(State = mobAMX$region[state], Date = names(mobAMX)[2:length(names(mobAMX))], Driving = as.numeric(mobAMX[state, 2:length(mobAMX)]))
  tmp.mobA$Driving[tmp.mobA$Date %in% c("11.05.2020", "12.05.2020")] <- mean(tmp.mobA$Driving[tmp.mobA$Date %in% c("09.05.2020", "10.05.2020", "13.05.2020", "14.05.2020")])
  mobDriv <- rbind(mobDriv, tmp.mobA)
  rm(tmp.mobA)
}
mobDriv$Date <- as.Date(mobDriv$Date, format="%Y.%m.%d")
mobDriv$Driving <- mobDriv$Driving - 100

###############################################################
mobData <- merge(mobData, mobDriv, by = c("Date", "State"))
###############################################################

################################################################################
########### NOW All DATA IS IN ONE DATAFRAME (mobData) #########################
################################################################################

## Select State ##
mobData %>% select("State") %>% unique() %>% as.character() ##all states list
state <- "Querétaro"

d <- data.frame("dates" = mobData %>% filter(State == state) %>% select(Date), "I" = mobData %>% filter(State == state) %>% select(I))
plot(as.incidence(d$I))

##########################################
############ PCA Analysis ################
##########################################

#################### METHOD 1 (Eigen method) ###############
# libreria
library(matlib)
# datos
d <- mobData %>% filter(State == state) %>% select(-c(Date, I, State))
d[is.na(d)] <- 0

# covarianza
Σ = cov(d)
Eigen = eigen(Σ)
Eigen$values/sum(Eigen$values)
Eigen$vectors[1, ] ###Principal componet

#################### METHOD 2 (DataCamp) ###############
params.pca <- prcomp(d, center = TRUE,scale. = TRUE)
summary(params.pca) ###Same results as before


## Plot mobility data before and after PCA ##
png(paste0(path, "Data.jpg"))
plot(d)
dev.off()
png(paste0(path, "DataPCA.jpg"))
plot(d * Eigen$vectors[1,])
dev.off()


##########################################
#### R(t) inference from data ############
##########################################
#-------------------------Funciones-------------------------------------------
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

#Funcion para estimar R_t con ventanas disjuntas
fun.estim_rts_uncertain<-function(data,config_gamma)
{ rts_uncertain <- estimate_R(data,
                              method = "uncertain_si",
                              config = config_gamma)
  return(rts_uncertain)
}

#-------------------------Parámetros-------------------------------------------
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

#--------------------------Evaluar R(t)---------------------------------------
d <- data.frame("dates" = mobData %>% filter(State == state) %>% select(Date), "I" = mobData %>% filter(State == state) %>% select(I))

rts_control_nish=fun.estim_rts_control(d, mean_nish, sd_nish)
plot(rts_control_nish, "R")+
  geom_hline(aes(yintercept = 1), color = "red", lty = 2)

rts_control_du=fun.estim_rts_control(d, t_start, t_end, mean_du, sd_du)
plot(rts_control_du, "R")+
  geom_hline(aes(yintercept = 1), color = "red", lty = 2)


rts_uncertain_nish=fun.estim_rts_uncertain(d,config_nish)
plot(rts_uncertain_nish, legend = TRUE)

rts_uncertain_du=fun.estim_rts_uncertain(d,config_du)
plot(rts_uncertain_du, legend = TRUE)

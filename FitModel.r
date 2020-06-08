library(dplyr)
library(magrittr)
library(ggplot2)
library(EpiEstim)
library(incidence)
library(plotly)

path = "Data/"

set.seed(1)

mob <- read.csv(paste0(path, "GlobalMobilityReport.csv"))
mob <- mob %>% filter(country_region_code == "MX")
states <- unique(mob$sub_region_1)
estados <- c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
                                              "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Ciudad de México",
                                              "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                              "Sinaloa", "Sonora", "México", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")
mob$sub_region_1 <- as.character(mob$sub_region_1)
mob$sub_region_1 <- enc2utf8(mob$sub_region_1)
mob$date <- as.Date(mob$date, format="%Y-%m-%d")

for (state in seq(length(states)))
{
  mob$sub_region_1[mob$sub_region_1 == states[state]] <- estados[state]
}
names(mob) <- c("Country Code", "Country", "State", "SubState", "Date", "RetailRecreation", "GroceryPharmacy", "Parks", "TransitStations", "Workplaces", "Residential")
mob <- mob %>% select(Date, State, RetailRecreation, GroceryPharmacy, Parks, TransitStations, Workplaces, Residential)
Dates <- c(min(mob$Date), max(mob$Date))

file.entidades <- read.csv(paste0(path, "entidades.csv"))
totdata <- read.csv(paste0(path, "TotalMX.csv"))
totdata$FECHA_ACTUALIZACION = as.Date(totdata$FECHA_ACTUALIZACION, format="%Y-%m-%d")
totdata$FECHA_INGRESO = as.Date(totdata$FECHA_INGRESO, format="%Y-%m-%d")
totdata$FECHA_SINTOMAS = as.Date(totdata$FECHA_SINTOMAS, format="%Y-%m-%d")
ndat <- totdata %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES) %>% filter(RESULTADO == 1 & FECHA_SINTOMAS >= Dates[1] & FECHA_SINTOMAS <= Dates[2]) %>% summarise("I" = n()) %>% select(FECHA_SINTOMAS, ENTIDAD_RES, I)
ndat <- ndat %>% filter(ENTIDAD_RES <= 32)
days <- seq(as.Date(Dates[1]), as.Date(Dates[2]), 1)

file.entidades$entidad <- as.character(file.entidades$entidad)

for (enti in seq(length(unique(ndat$ENTIDAD_RES))))
{
  days0 <- days[!(days %in% (ndat$FECHA_SINTOMAS[ndat$ENTIDAD_RES == file.entidades[enti, 1]]))]
  tmp.ndat <- data.frame(FECHA_SINTOMAS = as.Date(days0), ENTIDAD_RES = file.entidades[enti, 1], I = as.integer(0))
  tmp.ndat <- as.data.frame(tmp.ndat)
  ndat <- rbind(as.data.frame(ndat), tmp.ndat)
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
  tmp.mobA$Driving[tmp.mobA$Date %in% c("2020.05.11", "2020.05.12")] <- mean(tmp.mobA$Driving[tmp.mobA$Date %in% c("2020.05.09", "2020.05.10", "2020.05.13", "2020.05.14")])
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

##########################################
############ PCA Analysis ################
##########################################

#################### PCA por estaddo ###############

#Se genera el data frame con tres columnas, la varianza, la entidad, y el vector
PoV.states <- data.frame(PoV = c(0), entidad = estados)
PoV.states$Vector <- list(c(1,2,3,4,5,6,7))
PoV.states$entidad <- as.character(PoV.states$entidad)
####################### Loop para obtener los valores #########################################################

for(i in seq(length(PoV.states$entidad))){                                            #
  var1 <- mobData %>% filter(State == PoV.states[i, 2]) %>% select(-c(Date,I,State))  #
  ## var1[is.na(d)] <- 0 ¿¿¿ Para qué es esto ??? Chequen línea 92                    #
  pca.var1 = prcomp(var1, center = TRUE, scale. = TRUE)
  PoV.states$Vector[[i]] = as.numeric(pca.var1$rotation[,1] )                   #
  PoV  <- pca.var1$sdev^2/sum(pca.var1$sdev^2) ##tiene dimension N = 7 (todos los PCA) #
  PoV.states[i,1] = PoV[1] ##agarramos únicamente el primero                          #
}
#Cálculo de la proyección de los valores de movilidad de cada día sobre la componente principal
#Se guarda sobre mobData (data frame que jusnta todos los datos)
norm_vec <- function(x) sqrt(sum(x^2))
mobData$PPCA <- rep(0,dim(mobData)[1])

for (i in 1:dim(mobData)[1]) {
  PC <- PoV.states[which(PoV.states$entidad==mobData[i,	"State"]),"Vector"]
  PC <- PC[[1]]
  mobData$PPCA[i] = as.numeric(mobData[i,4:(dim(mobData)[2]-1)])%*%PC/norm_vec(PC)
}
#Agrego una columna para indicar el periodo temporal en el que se encuantra dado día para el calculo de R
mobData$Period = rep(0,dim(mobData)[1])

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
#Además se calcula por estado
RS <- data.frame(estado = estados)
RS$RMedia <- list(c(1,2,3))
RS$estado <- as.character(RS$estado)

for (i in estados) {
  d <- data.frame("dates" = mobData %>% filter(State == i) %>% select(Date), "I" = mobData %>% filter(State == i) %>% select(I))
  sinceQuarintine <- as.numeric(as.Date("2020-03-23") - as.Date(Dates[1]))
  t_start <- c(2, sinceQuarintine, nrow(d)-7)
  t_end <- c(sinceQuarintine - 1, nrow(d)-8, nrow(d))
  for (j in 1:3) {
    mobData$Period[mobData$State==i & mobData$Date %in% d$Date[t_start[j]:t_end[j]]] = j
  }

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
  #calcular r

  rts_control_du= tryCatch(
    fun.estim_rts_control(d, t_start, t_end, mean_du, sd_du),warning=function(w) {
      print(i)
      tmp_r <- fun.estim_rts_control(d, t_start, t_end, mean_du, sd_du)
      print(plot(tmp_r, "R")+ geom_hline(aes(yintercept = 1), color = "red", lty = 2))
      return(tmp_r)
    }
  )
  RS$RMedia[which(RS$estado==i)][[1]] = as.numeric(rts_control_du$R[,"Mean(R)"])

}
#Finalmente gráficar




fd <- mobData[which(mobData$State=="Querétaro"&mobData$Period!=0),]
mfd <- fd %>% group_by(Period) %>% summarise(MPPCA = mean(PPCA))


g <- ggplot(fd ,aes(Period, PPCA)) + geom_point()

g <-g  + geom_hline(yintercept=mean(fd[which(fd$Period == 1),]$PPCA),color='red')
g <-g  + geom_hline(yintercept=mean(fd[which(fd$Period == 2),]$PPCA),color='green')
g <-g  + geom_hline(yintercept=mean(fd[which(fd$Period == 3),]$PPCA),color='blue')

g

f <- list(
  family = "Roboto Slab",
  size = 18,
  color = "#000"
)
ft <- list(
  family = "Roboto Slab",
  size = 20,
  color = "#000"
)


x <- list(
  title = "R",
  titlefont = f
)
y <- list(
  title = "Proy 1°CP",
  titlefont = f
)
t <- list(
  title = "Querétaro",
  titlefont = ft
)
fig <-  plot_ly()
fig <- fig %>%  add_trace(data = fd, x = ~RS[which(RS$estado=="Querétaro"),"RMedia"][[1]][Period], y = ~PPCA, mode = 'markers',name="Datos")
fig <- fig %>% add_trace(x = RS[which(RS$estado=="Querétaro"),"RMedia"][[1]][mfd$Period], y = mfd$MPPCA, mode = "line",name="Medias")
fig <- fig %>% layout(title = "Querétaro",xaxis = x, yaxis = y,font=ft)
fig












#--------------------------Evaluar R(t)---------------------------------------

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

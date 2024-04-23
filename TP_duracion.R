#Carga de los paquetes
library(survival)
library(tidyverse)
library(survminer)
library(ggplot2)
library(ggmosaic)

# Carga de los datos
data <- rotterdam %>%
  filter(year > 1991) %>% select(-c(nodes, rtime, recur))


# Cantidad de mujeres para cada tamaño

data %>% group_by(size) %>% summarise(nombre = sum(death < 2))


# Cantidad de mujeres para cada tratamiento

data %>% group_by(hormon) %>% summarise(nombre = sum(death < 2))


# Hazard promedio

data %>% group_by(size) %>% summarise(suma = 100*365*sum(death)/sum(dtime))

data %>% group_by(hormon) %>% summarise(suma = 100*365*sum(death)/sum(dtime))




# Media truncada



media_estimada(data$dtime, data$death, data$size)

a1 <- survfit(Surv(data$dtime[data$size == "<=20"], data$death[data$size == "<=20"]) ~ 1)
media1 <- 1 * (a1$time[1] - 0)
for (i in 2:length(a1$time)) {
  media <- media + a1$surv[i-1] * (a1$time[i] - a1$time[i-1])
}

a2 <- survfit(Surv(data$dtime[data$size == "20-50"], data$death[data$size == "20-50"]) ~ 1)
media2 <- 1 * (a2$time[1] - 0)
for (i in 2:length(a2$time)) {
  media <- media + a2$surv[i-1] * (a2$time[i] - a2$time[i-1])
}

a3 <- survfit(Surv(data$dtime[data$size == ">50"], data$death[data$size == ">50"]) ~ 1)
media3 <- 1 * (a3$time[1] - 0)
for (i in 2:length(a3$time)) {
  media <- media + a3$surv[i-1] * (a3$time[i] - a3$time[i-1])
}

# Curva de supervivencia 

fh = survfit(Surv(dtime/365, death) ~ 1, 
                  type="fleming-harrington", 
                  data=data)

ggsurvplot(fit = fh, data = data,
           conf.int = F,
           censor.shape = 20, 
           xlab = "Años", 
           ylab = "Prob. de supervivencia estimada",
           break.x.by = 1,
           legend.title="",
           palette = c("purple"),
           legend.labs = "Todas las personas",
           title = "Supervivencia estimada según tamaño del tumor"
)


# Curva de supervivencia segun tamaño

data = data %>%  mutate(size_num = case_when(data$size == "<=20" ~ 1, data$size == "20-50" ~ 2, data$size == ">50" ~ 3)) 


fh_size = survfit(Surv(dtime, death) ~ size_num, 
               type="fleming-harrington", 
               data=data)

ggsurvplot(fit = fh_size, data = data,
           conf.int = F, pval = T,
           censor.shape = 20, 
           xlab = "Años", 
           ylab = "Prob. de supervivencia estimada",
           break.x.by = 1,
           test.for.trend = T,
           legend.title="",
           palette = c("purple", "lightgreen", "skyblue"),
           legend.labs = c("Menor o igual a 20mm", "Entre 20 y 50mm", "Mayor a 50mm"),
           title = "Supervivencia estimada según tamaño del tumor"
           )


# Curva de supervivencia segun tratamiento hormonal

fh_horm = survfit(Surv(dtime, death) ~ hormon, 
                  type="fleming-harrington", 
                  data=data)

ggsurvplot(fit = fh_horm, data = data,
           conf.int = F, 
           pval = T,
           censor.shape = 20, 
           xlab = "Años", 
           ylab = "Prob. de supervivencia estimada",
           break.x.by = 1,
           legend.title="",
           palette = c("purple", "lightgreen"),
           legend.labs = c("Sin tratamiento hormonal", "Con tratamiento hormonal"),
           title = "Supervivencia estimada según si recibio tratamiento hormonal" 
) 



# Curva de supervivencia segun tamaño para las que recibieron tratamiento hormonal

data_hormon_si = data %>% filter(hormon == 1)

fh_1 = survfit(Surv(dtime, death) ~ size, 
               type="fleming-harrington", 
               data=data_hormon_si)

ggsurvplot(fit = fh_1, data = data_hormon_si,
           conf.int = F,
           censor.shape = 20, 
           xlab = "Años", 
           ylab = "Prob. de supervivencia estimada",
           break.x.by = 1,
           legend.title="Tamaño del tumor",
           palette = c("purple", "lightgreen", "skyblue"),
           legend.labs = c("Menor o igual a 20mm", "Entre 20 y 50mm", "Mayor a 50mm")
           ) 

# Curva de supervivencia segun tamaño para las que no recibieron tratamiento hormonal

data_hormon_no = data %>% filter(hormon == 0)


fh_2 = survfit(Surv(dtime, death) ~ size, 
               type="fleming-harrington", 
               data=data_hormon_no)

ggsurvplot(fit = fh_2, data = data_hormon_no,
           conf.int = F, 
           censor.shape = 20, 
           xlab = "Años", 
           ylab = "Prob. de supervivencia estimada",
           break.x.by = 1,
           legend.title="Tamaño del tumor",
           palette = c("purple", "lightgreen", "skyblue"),
           legend.labs = c("Menor o igual a 20mm", "Entre 20 y 50mm", "Mayor a 50mm")) 






# Tree map flashero


media_trunc <- data %>% 
  group_by(size) %>% 
  summarise(media_truncada = sum(dtime)/365) %>% ungroup()

hazard_prom <- data %>% 
  group_by(size) %>% 
  summarise(hazard = 100*365*sum(death)/sum(dtime)) %>% 
  ungroup()
cant <- data %>% 
  group_by(size) %>% summarise(cantidad = sum(size == size))



data$hormon_factor = as.factor(data$hormon)






ggplot(data = data) +
  geom_mosaic(aes(x = product(size, hormon_factor), fill=size)) +
  geom_mosaic_text(aes(x = product(size, hormon_factor), label = after_stat(.wt))) +
  scale_x_productlist_yo(name = "Tratamiento hormonal", labels = c("Sin", "Con"), position = "top", 
                         sec.axis = dup_axis(labels = c("316","128"), name = "")) +
  scale_y_productlist_yo(name =  "Tamaño del tumor", labels = c("Menor o igual a 20mm", "Entre 20 y 50mm", "Mayor a 50mm"), 
                         sec.axis = sec_axis(transform = ~.*1, breaks = c(0.25,0.6,.95), labels = c("64","152","228"), name = "")) +
  theme(panel.background = element_blank(), legend.position = "none", 
          axis.ticks = element_blank(), axis.text.y.right = element_text(vjust = 2,size = 11),axis.text.x.bottom = element_text(size = 11)) + 
  ggtitle("Mosaicos del tamaño del tumor vs tratamiento hormonal") 




#Faltan razon de hazards

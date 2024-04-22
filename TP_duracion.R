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



# Curva de supervivencia segun tamaño

data = data %>%  mutate(size_num = case_when(data$size == "<=20" ~ 1, data$size == "20-50" ~ 2, data$size == ">50" ~ 3)) 


fh_size = survfit(Surv(dtime, death) ~ size_num, 
               type="fleming-harrington", 
               data=data)

ggsurvplot(fit = fh_size, data = data,
           conf.int = F, pval = T,
           censor.shape = 20, 
           xlab = "Días", 
           test.for.trend = T,
           ylab = "Prob. de supervivencia estimada",
           legend.title="Tamaño del tumor",
           palette = c("purple", "lightgreen", "skyblue"),
           legend.labs = c("Menor o igual a 20mm", "Entre 20 y 50mm", "Mayor a 50mm")
           )


# Curva de supervivencia segun tratamiento hormonal

fh_horm = survfit(Surv(dtime, death) ~ hormon, 
                  type="fleming-harrington", 
                  data=data)

ggsurvplot(fit = fh_horm, data = data,
           conf.int = F, 
           pval = T,
           censor.shape = 20, 
           xlab = "Días", 
           ylab = "Prob. de supervivencia estimada",
           legend.title="",
           palette = c("purple", "lightgreen"),
           legend.labs = c("Sin tratamiento hormonal", "Con tratamiento hormonal") 
) 





# Curva de supervivencia segun tamaño para las que recibieron tratamiento hormonal

data_hormon_si = data %>% filter(hormon == 1)

fh_1 = survfit(Surv(dtime, death) ~ size, 
               type="fleming-harrington", 
               data=data_hormon_si)

ggsurvplot(fit = fh_1, data = data_hormon_si,
           conf.int = F,
           censor.shape = 20, 
           xlab = "Dìas", 
           ylab = "Prob. de supervivencia estimada",
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
           xlab = "Dìas", 
           ylab = "Prob. de supervivencia estimada",
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
datos_treemap <- cant %>% mutate(hazard_prom[,2]) %>% mutate(media_trunc[,2])



ggplot(data = datos_treemap) +
  aes(area = cantidad, fill = hazard, label = paste0(size, "\n Hazard promedio " , round(hazard, 2),"\n", round(media_truncada, 2))) +
  geom_treemap() + 
  geom_treemap_text() +
  scale_fill_gradient(low ="green3", high = "red")
library(treemapify)


#


data$hormon = as.factor(data$hormon)

ggplot(data = data) +
  geom_mosaic(aes(x = product(size, hormon), fill=size)) +
  geom_mosaic_text(aes(x = product(size, hormon), label = after_stat(.wt))) +
  scale_x_productlist("Tratamiento hormonal", labels = c("Sin", "Con"), position = "top") +
  scale_y_productlist("Tamaño del tumor", labels = c("Menor o igual a 20mm", "Entre 20 y 50mm", "Mayor a 50mm")) +
  theme(panel.background = element_blank(), legend.position = "none")



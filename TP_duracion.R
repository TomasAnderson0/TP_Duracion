#Carga de los paquetes
library(survival)
library(tidyverse)
library(survminer)
library(ggplot2)
library(ggmosaic)
library(plotly)
source("Funciones.R")

# Carga de los datos
data <- rotterdam %>%
  filter(year > 1991) %>% select(-c(nodes, rtime, recur))



# Cantidad de mujeres para cada tamaño

data %>% group_by(size) %>% summarise(nombre = sum(death < 2), media_tiempo = mean(dtime))


# Cantidad de mujeres para cada tratamiento

data %>% group_by(hormon) %>% summarise(nombre = sum(death < 2), media_tiempo = mean(dtime))


# Hazard promedio

data %>% summarise(suma = 100*365*sum(death)/sum(dtime))

data %>% group_by(size) %>% summarise(suma = 100*365*sum(death)/sum(dtime))

data %>% group_by(hormon) %>% summarise(suma = 100*365*sum(death)/sum(dtime))

data %>% group_by(er2) %>% summarise(suma = 100*365*sum(death)/sum(dtime))

data %>% group_by(pgr2) %>% summarise(suma = 100*365*sum(death)/sum(dtime))





# Media truncada



media_estimada(data$dtime, data$death, data$size)


# Curva de supervivencia para todas las mujeres

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
           ggtheme = theme_bw(),
           legend.labs = "Todas las personas",
           title = "Supervivencia estimada para todas las mujeres"
)



# Curva de supervivencia segun pgr

summary(data$pgr)

data = data %>% mutate(pgr2 = case_when(pgr == 0 ~ "Igual a 0", pgr > 0 & pgr <=50 ~ "Entre 0 y 50",
                                        pgr > 50 ~ "Mayor a 50"))


fh_pgr = survfit(Surv(dtime/365, death) ~ pgr2, 
                  type="fleming-harrington", 
                  data=data)

ggsurvplot(fit = fh_pgr, data = data,
           conf.int = F, 
           pval = T,
           censor.shape = 20, 
           xlab = "Años", 
           ylab = "Prob. de supervivencia estimada",
           test.for.trend = T,
           break.x.by = 1,
           legend.title="",
                      ggtheme = theme_bw(),
           palette = c("purple", "lightgreen", "red", "blue"),
           title = "Supervivencia estimada según si recibio tratamiento hormonal" 
) 







# Curva de supervivencia segun er

summary(data$er)

data = data %>% mutate(er2 = case_when(er == 0 ~ "Igual a 0", er > 0 & er <=50 ~ "Entre 0 y 50",
                                        er > 50 ~ "Mayor a 50"))

fh_er = survfit(Surv(dtime/365, death) ~ er2, 
                  type="fleming-harrington", 
                  data=data)

ggsurvplot(fit = fh_er, data = data,
           conf.int = F, 
           pval = T,
           censor.shape = 20, 
           xlab = "Años", 
           ylab = "Prob. de supervivencia estimada",
           test.for.trend = T,
           break.x.by = 1,
           legend.title="",
           ggtheme = theme_bw(),
           palette = c("purple", "lightgreen", "red", "blue"),
           title = "Supervivencia estimada según si recibio tratamiento hormonal")
 

data = data %>% mutate(hormon2 = as.factor(hormon))

survdiff(Surv(dtime/365, death) ~ er2 + strata(hormon), data = data)




# Curva de supervivencia segun tamaño

data = data %>%  mutate(size_num = case_when(data$size == "<=20" ~ 1, data$size == "20-50" ~ 2, data$size == ">50" ~ 3)) 


fh_size = survfit(Surv(dtime/365, death) ~ size_num, 
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
           title = "Supervivencia estimada para las mujeres según el tamaño del tumor"
           )





data$hormon_factor = as.factor(data$hormon)




ggplot(data = data) +
  geom_mosaic(aes(x = product(er2, hormon_factor), fill=er2)) +
  geom_mosaic_text(aes(x = product(er2, hormon_factor), label = after_stat(.wt))) +
  scale_x_productlist_yo(name = "Tratamiento hormonal", labels = c("Sin", "Con"), position = "top", 
                         sec.axis = dup_axis(labels = c("316","128"), name = "")) +
  scale_y_productlist_yo(name =  " Receptores de estrógeno (en fmol/l)", labels = c("Igual a 0", "Entre 0 y 50", "Mayor a 50"), 
                         sec.axis = sec_axis(transform = ~.*1, breaks = c(0.25,0.6,.95), labels = c("149","207","88"), name = "")) +
  theme(panel.background = element_blank(), legend.position = "none", 
          axis.ticks = element_blank(), axis.text.y.right = element_text(vjust = 2,size = 11),axis.text.x.bottom = element_text(size = 11)) + 
  ggtitle("Tabla de contingencia de los receptores de estrógeno vs tratamiento hormonal") 




#Curvas de supervivencia para hormonas x er

data_hormon_si = data %>% filter(hormon == 1)


fit_si = survfit(Surv(dtime/365, death) ~ er2, 
                 type="fleming-harrington", 
                 data=data_hormon_si)

dat_si = data.frame(surv = fit_si$surv, tiempo = fit_si$time, trat = as.factor(c(rep("Igual a 0",fit_si$strata[1]), rep("Entre 0 y 50",fit_si$strata[2]), rep("Mayor a 50",fit_si$strata[3]))), hormon = "Con")



data_hormon_no = data %>% filter(hormon == 0)


fit_no = survfit(Surv(dtime/365, death) ~ er2, 
                 type="fleming-harrington", 
                 data=data_hormon_no)

dat_no <- data.frame(surv = fit_no$surv, tiempo = fit_no$time, trat = as.factor(c(rep("Igual a 0",fit_no$strata[1]), rep("Entre 0 y 50",fit_no$strata[2]), rep("Mayor a 50",fit_no$strata[3]))),hormon = "Sin")

dat = rbind(dat_si,dat_no)

dat$trat = factor(dat$trat, levels = c("Igual a 0", "Entre 0 y 50", "Mayor a 50"))
dat$hormon = factor(dat$hormon, levels = c("Sin", "Con"))

ggplotly(
  ggplot() +
    geom_step(data = dat, aes(x = tiempo, y = surv, color = trat, linetype = hormon)) +
    labs(x = "Años", y = "Prob. de supervivencia estimada") +
    scale_y_continuous(limits = c(0,1))+
    scale_linetype_discrete(name = "")+
    scale_color_discrete(name = "Tratamiento hormonal\ny receptores de estrogeno", )+
    theme_bw()
)

# ------------------------------------------------------------------------------
# --------------------------- Modelos de Cox -----------------------------------
# ------------------------------------------------------------------------------
View(data)
coxph(Surv(dtime/365, death) ~ age, data = data, ties = "breslow")
0.4482
coxph(Surv(dtime/365, death) ~ meno, data = data, ties = "breslow")
0.4983
coxph(Surv(dtime/365, death) ~ size, data = data, ties = "breslow")
0
coxph(Surv(dtime/365, death) ~ grade, data = data, ties = "breslow")
0.002
coxph(Surv(dtime/365, death) ~ pgr, data = data, ties = "breslow")
0
coxph(Surv(dtime/365, death) ~ er, data = data, ties = "breslow")
0.0052
coxph(Surv(dtime/365, death) ~ hormon, data = data, ties = "breslow")
0.0071
coxph(Surv(dtime/365, death) ~ chemo, data = data, ties = "breslow")
0.0787

modelo <- coxph(Surv(dtime/365, death) ~ size + pgr + grade + er + hormon, data = data, ties = "breslow")
modelosh <- coxph(Surv(dtime/365, death) ~ size + pgr + grade + er, data = data, ties = "breslow")
anova(modelo, modelosh)
# Es 0

modelo <- modelosh
modeloser <- coxph(Surv(dtime/365, death) ~ size + pgr + grade, data = data, ties = "breslow")
anova(modelo, modeloser)
# Es 0

modelo <- modeloser
modelosgr <- coxph(Surv(dtime/365, death) ~ size + pgr, data = data, ties = "breslow")
anova(modelo, modelosgr)
# 0? cachi 0

modeloint <- coxph(Surv(dtime/365, death) ~ size*pgr + grade, data = data, ties = "breslow")
anova(modelo, modeloint)
# Nada che

modeloint <- coxph(Surv(dtime/365, death) ~ size + pgr*grade, data = data, ties = "breslow")
anova(modelo, modeloint)
# Menos todavia

modeloint <- coxph(Surv(dtime/365, death) ~ size*grade + pgr, data = data, ties = "breslow")
anova(modelo, modeloint)
# Nada por acá

# Nos quedamos con "modelo"

# Probamos linearidad de pgr

datos_pgr <- data %>% mutate(pgr_dum = cut(pgr, breaks = c(-1, quantile(data$pgr)[2], quantile(data$pgr)[3],quantile(data$pgr)[4], Inf)))
table(datos_pgr$pgr_dum)

modelodum <- coxph(Surv(dtime/365, death) ~ size + pgr_dum + grade, data = datos_pgr, ties = "breslow")
anova(modelo, modelodum)
# No rechazo Ho, entonces es lineal, me quedo con "modelo"

# Interpretación de modelo
summary(modelo)
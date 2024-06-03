#Jose Ignacio Valdivia Aguero
#DOE: T2

#Integrator
###Instalando paquetes
if(!require(dplyr)){install.packages("dplyr")}
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(lattice)){install.packages("lattice")}
if(!require(stringr)){install.packages("stringr")}
#if(!require(phia)){install.packages("phia")}

###Llamando librerias
library(dplyr)
library(psych)
library(FSA)
library(ggplot2)
library(car)
library(multcompView)
library(lsmeans)
library(rcompanion)
library(multcomp)
library(lattice)
library(stringr)
#library(phia)

###Ingresando resultados del experimento

#########################################
############      Integrator      ############
#########################################

data<-read.csv("./DatosFinales.csv", sep=";", header=TRUE)

data$Integrator <- as.factor(data$Integrator) 
data$Scene <- as.factor(data$Scene) 
data$Accel <- as.factor(data$Accel) 
data$OS <- as.factor(data$OS) 
data$Numerator <- as.factor(data$Numerator) 




###Verificando
headTail(data)
str(data)
summary(data)
#Aqui se puede observar la cantidad de repeticiones realizadas.

###Plots
### Headers list
###             OS	Numerador	Scene	Accel	Integrator	Seconds	tiempo

#Scene
#Plot simple: Algo OS
interaction.plot(x.factor = data$Scene,
                 trace.factor = data$OS,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )


#Plot simple: Algo accel
interaction.plot(x.factor = data$Scene,
                 trace.factor = data$Accel,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )

#Plot simple: Algo Integrator
interaction.plot(x.factor = data$Scene,
                 trace.factor = data$Integrator,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )







#OS

#Plot simple: OS
interaction.plot(x.factor = data$OS,
                 trace.factor = data$Scene,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )


#Plot simple: OS Accel
interaction.plot(x.factor = data$OS,
                 trace.factor = data$Accel,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )

#Plot simple: OS Integrator
interaction.plot(x.factor = data$OS,
                 trace.factor = data$Integrator,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )



#Accel

interaction.plot(x.factor = data$Accel,
                 trace.factor = data$OS,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )


#Plot simple: Resolucion
interaction.plot(x.factor = data$Accel,
                 trace.factor = data$Scene,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )

#Plot simple: Resolucion
interaction.plot(x.factor = data$Accel,
                 trace.factor = data$Integrator,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )




#Integrator

interaction.plot(x.factor = data$Integrator,
                 trace.factor = data$OS,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )


#Plot simple: Resolucion
interaction.plot(x.factor = data$Integrator,
                 trace.factor = data$Scene,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )

#Plot simple: Resolucion
interaction.plot(x.factor = data$Integrator,
                 trace.factor = data$Accel,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )


#Numerator
interaction.plot(x.factor = data$Numerator,
                 trace.factor = data$OS,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )


#Plot simple: Resolucion
interaction.plot(x.factor = data$Numerator,
                 trace.factor = data$Scene,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )

#Plot simple: Resolucion
interaction.plot(x.factor = data$Numerator,
                 trace.factor = data$Accel,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )


#Plot simple: Resolucion
interaction.plot(x.factor = data$Numerator,
                 trace.factor = data$Integrator,
                 response= data$Seconds,
                 fun = mean,
                 type="b",
                 col=c ("black" ,"red" ,"green"),
                 pch=c(19, 17, 15),
                 #fixed-TRUE,
                 leg.bty = "o" )





model = lm(Seconds ~ Integrator*Scene*Accel*OS, data=data)




x = residuals(model)
plotNormalHistogram(x)

plot(fitted(model), residuals(model))


#Anova
Anova(model, type="II")

#Plot model
plot(model)

#Test para supuesta varianza constante
  leveneTest(Seconds ~ Integrator*Scene*OS, data=data)








###Modelo de Raiz Cuadrada
T_sqrt = sqrt(data$Seconds)
model = lm(T_sqrt ~ Integrator*Scene*Accel*OS, data=data)


x = residuals(model)
plotNormalHistogram(x)


#Varianza constante (Homocedasticidad)
plot(fitted(model), residuals(model))

#Test para supuesta varianza constante
  leveneTest(T_sqrt ~ Integrator*Scene*OS, data=data)
#Anova
Anova(model, type="II")

#Plot model
plot(model)






#Transformacion de datos 2 - raiz cubica
T_cub = sign(data$Seconds) * abs(data$Seconds) ^(1/3)
model = lm(T_cub ~ Integrator*Scene*Accel*OS, data=data)


#Histograma de residuos
x = residuals(model)
plotNormalHistogram(x)

plot(fitted(model), residuals(model))
  leveneTest(T_cub ~ Integrator*Scene*OS, data=data)
#Anova
Anova(model, type="II")

#Plot model
plot(model)






###Transformacion de datos 3 - log
##T_log = log(data$Seconds)
##model = lm(T_log ~ Integrator*Scene*Accel*OS, data=data)
##
##
###Histograma de residuos
##x = residuals(model)
##plotNormalHistogram(x)
##
##
##plot(fitted(model), residuals(model))
##leveneTest(T_log ~ Integrator*Scene*Accel*OS, data=data)
##
###Anova
##Anova(model, type="II")
##
###Plot model
##plot(model)




pairwise.t.test(T_cub, data$Scene:data$Integrator, p.adjust.method = "BH")
pairwise.t.test(T_cub, data$Scene:data$Accel, p.adjust.method = "BH")
pairwise.t.test(T_cub, data$Scene:data$OS, p.adjust.method = "BH")

pairwise.t.test(T_cub, data$Accel:data$Integrator, p.adjust.method = "BH")
pairwise.t.test(T_cub, data$Accel:data$OS, p.adjust.method = "BH")

pairwise.t.test(T_cub, data$Integrator:data$OS, p.adjust.method = "BH")










































Sum = Summarize(T_cub ~ Integrator+OS, data = data, digits = 3)

Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

Sum$OS = factor(Sum$OS, levels = unique(Sum$OS))
pd = position_dodge(.2)

#Plot de Integrator y OS
ggplot(Sum, aes(x = OS, y = mean, color = Integrator)) + 
  geom_errorbar(aes(ymin=mean - se,  ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  scale_colour_manual(values=c("black", "red", "green", "yellow")) +
  ylab("Cubic root of t+Integrator")



###Integrator y Accel
#Sum = promedios + se
Sum = Summarize(T_cub ~ Integrator+Accel, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$Accel = factor(Sum$Accel, levels = unique(Sum$Accel))
pd = position_dodge(.2)

#Plot de Integrator y Accel
ggplot(Sum, aes(x = Accel, y = mean, color = Integrator)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  scale_colour_manual(values=c("black", "red", "green", "yellow")) +
  ylab("Cubic root of t+Integrator")



###Integrator y Accel
#Sum = promedios + se
Sum = Summarize(T_cub ~ Integrator+Accel, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$Accel = factor(Sum$Accel, levels = unique(Sum$Accel))
pd = position_dodge(.2)

#Plot de Integrator y Accel
ggplot(Sum, aes(x = Accel, y = mean, color = Integrator)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  scale_colour_manual(values=c("black", "red", "green", "yellow")) +
  ylab("Cubic root of t+Integrator")

Summarize(T_cub ~ Integrator+Accel, data = data, digits = 3)



Summarize(T_cub ~ OS+Scene, data = data, digits = 3)





###Integrator y Scene
#Sum = promedios + se
Sum = Summarize(T_cub ~ Integrator+Scene, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$Scene = factor(Sum$Scene, levels = unique(Sum$Scene))
pd = position_dodge(.2)

#Plot de Integrator y Scene
ggplot(Sum, aes(x = Scene, y = mean, color = Integrator)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  scale_colour_manual(values=c("black", "red", "green", "yellow")) +
  ylab("Cubic root of t+Integrator")

Summarize(T_cub ~ Integrator+Scene, data = data, digits = 3)








###Integrator y Scene
#Sum = promedios + se
Sum = Summarize(T_cub ~ Accel+Scene, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$Scene = factor(Sum$Scene, levels = unique(Sum$Scene))
pd = position_dodge(.2)

#Plot de Accel y Scene
ggplot(Sum, aes(x = Scene, y = mean, color = Accel)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  scale_colour_manual(values=c("black", "red", "green", "yellow")) +
  ylab("Cubic root of t+Accel")

Summarize(T_cub ~ Accel+Scene, data = data, digits = 3)





###Integrator y Scene
#Sum = promedios + se
Sum = Summarize(T_cub ~ OS+Scene, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$Scene = factor(Sum$Scene, levels = unique(Sum$Scene))
pd = position_dodge(.2)

#Plot de OS y Scene
ggplot(Sum, aes(x = Scene, y = mean, color = OS)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  scale_colour_manual(values=c("black", "red", "green", "yellow", "blue")) +
  ylab("Cubic root of t+OS")

Summarize(T_cub ~ OS+Scene, data = data, digits = 3)



###OS y Scene
#Sum = promediScene + se
Sum = Summarize(T_cub ~ OS+Scene, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$Scene = factor(Sum$Scene, levels = unique(Sum$Scene))
pd = position_dodge(.2)

#Plot de OS y Scene
ggplot(Sum, aes(x = Scene, y = mean, color = OS)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  scale_colour_manual(values=c("black", "red", "green", "yellow", "blue")) +
  ylab("Cubic root of t+OS")

Summarize(T_cub ~ OS, data = data, digits = 3)


###Numerator
#Sum = promedios + se
Sum = Summarize(T_cub ~ Numerator, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$Numerator = factor(Sum$Numerator, levels = unique(Sum$Numerator))
pd = position_dodge(.2)

#Plot de Numerator
ggplot(Sum, aes(x = Numerator, y = mean, color = Numerator)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  ylab("Cubic root of t")



###OS
#Sum = promedios + se
Sum = Summarize(T_cub ~ OS, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$OS = factor(Sum$OS, levels = unique(Sum$OS))
pd = position_dodge(.2)

#Plot de OS
ggplot(Sum, aes(x = OS, y = mean, color = OS)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold"))  +
  scale_colour_manual(values=c("black", "red", "green", "yellow", "blue")) +
  ylab("Cubic root of t")




###OS y Accel
#Sum = promediAccel + se
Sum = Summarize(T_cub ~ OS+Accel, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$Accel = factor(Sum$Accel, levels = unique(Sum$Accel))
pd = position_dodge(.2)

#Plot de OS y Accel
ggplot(Sum, aes(x = Accel, y = mean, color = OS)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  scale_colour_manual(values=c("black", "red", "green", "yellow", "blue")) +
  ylab("Cubic root of t+Accel")





###OS y Integrator
#Sum = promediIntegrator + se
Sum = Summarize(T_cub ~ OS+Integrator, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$Integrator = factor(Sum$Integrator, levels = unique(Sum$Integrator))
pd = position_dodge(.2)

#Plot de OS y Integrator
ggplot(Sum, aes(x = Integrator, y = mean, color = OS)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  scale_colour_manual(values=c("black", "red", "green", "yellow", "blue")) +
  ylab("Cubic root of t+Integrator")



###Scene y OS
#Sum = promedios + se
Sum = Summarize(T_cub ~ Scene+OS, data = data, digits = 3)
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)

#Ordenando
Sum$OS = factor(Sum$OS, levels = unique(Sum$OS))
pd = position_dodge(.2)

#Plot de Scene y OS
ggplot(Sum, aes(x = OS, y = mean, color = Scene)) + 
  geom_errorbar(aes(ymin=mean - se, ymax=mean + se), width=.2, linewidth=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title= element_text(face="bold")) +
  scale_colour_manual(values=c("black", "red", "green", "yellow", "blue")) +
  ylab("Cubic root of t+Scene")

Summarize(T_cub ~ Scene, data = data, digits = 3)









pairwise.t.test(T_cub, data$Scene:data$Integrator, p.adjust.method = "BH")
pairwise.t.test(T_cub, data$Scene:data$Accel, p.adjust.method = "BH")
pairwise.t.test(T_cub, data$Scene:data$OS, p.adjust.method = "BH")

pairwise.t.test(T_cub, data$Accel:data$Integrator, p.adjust.method = "BH")
pairwise.t.test(T_cub, data$Accel:data$OS, p.adjust.method = "BH")

pairwise.t.test(T_cub, data$Integrator:data$OS, p.adjust.method = "BH")


pairwise.t.test(T_cub, data$Accel)
pairwise.t.test(T_cub, data$Integrator)
pairwise.t.test(T_cub, data$OS)
pairwise.t.test(T_cub, data$Scene)




marginal = lsmeans(model, pairwise ~ OS, adjust = "tukey")
marginal

marginal = lsmeans(model, pairwise ~ Integrator, adjust="tukey")
marginal


marginal = lsmeans(model, pairwise~Accel, adjust="tukey")
marginal

marginal = lsmeans(model, pairwise~Scene, adjust="tukey")
marginal

marginal = lsmeans(model, pairwise~Accel*OS*Scene*Integrator, adjust="tukey")
marginal


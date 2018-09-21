library(googlesheets)
library(psych)
library(magrittr) # for %>%
library(car)
library(ggplot2)
library(reshape2)

# 



(my_sheets <- gs_ls())
# my_sheets %>% glimpse()
gs_ls("")[,1]
gs_ls("AgHebdHum_omni")

AgHebdHum_omni <- gs_title("AgHebdHum_omni (réponses)")
AgHebdHum_omni
AgHebdHum_omni %>% gs_browse()
AgHebdHum_omni
gs_ws_ls(AgHebdHum_omni)

AgHebdHum_omni2 <- AgHebdHum_omni %>% gs_read(ws = "Réponses au formulaire 1")
AgHebdHum_omni2
str(AgHebdHum_omni2)
glimpse(AgHebdHum_omni2)
AgHebdHum_omni2 <- gs_read(ws=1)

dim(AgHebdHum_omni2)
class(AgHebdHum_omni2)

AHO_DF <- data.frame(AgHebdHum_omni2)

dim(AHO_DF)

View(AHO_DF)


class(AHO_DF)
colnames(AHO_DF)
AHO_DF$Horodateur2 <- as.Date(AHO_DF$Horodateur, format=c('%d/%m/%Y %H:%M:%S'))
# as.Date(AHO_DF$Horodateur, '%H:%M:%S')

# class(AHO_DF$Horodateur)
# as.POSIXlt(AHO_DF$Horodateur)


##### ID selection

AHO_DF$Identifiant


#### recodage des variables

AHO_DF$Humeur..en.ce.moment.ci. <- car::recode(AHO_DF$Humeur..en.ce.moment.ci., 
              '"0 dépressive extrême"=0; "1 dépressive sévère"=1;
"2 dépressive marquée"=2; "3 dépressive moyenne"=3; 
              "5 humeur normale"=5; "4 dépressive légère"=4;
              "6 manie légère"=6;"7 manie moyenne"=7;
"8 manie marquée"=8; "9 manie sévère"=9;
"10 manie extrême"
              ')

#### conversion of other variables

AHO_DF$Insomnie..la.nuit.passée. <- car::recode(AHO_DF$Insomnie..la.nuit.passée., 
                                               '"Non"=0;
                                                "Oui"=1
                                               ')

AHO_DF$Irritabilité..en.ce.moment.ci. <- car::recode(AHO_DF$Irritabilité..en.ce.moment.ci., 
                                               '"Absente"=0;
                                                "Modérée"=1;
                                                "Sévère"=2
          
                                               
                                                                                 ')
###### fichier long


AHO_DF_long <- melt(data=AHO_DF, 
     id.vars=c("Horodateur",
               "Identifiant",
               "Insomnie..la.nuit.passée.",
               "Irritabilité..en.ce.moment.ci.",
               "Anxiété..en.ce.moment.ci.",
               "Humeur..en.ce.moment.ci.",
               "Nombre.de.changement.d.humeur..depuis.la.dernière.évaluation.",
               "Evènements.particuliers.",
               "Situation.Problème",
               "Pensées",
               "Emotions.Sentiments.Sensations",                               
               "Comportements",                                                
               "Conséquences",                                                
               "Horodateur2"),
     measure.vars=c("Insomnie..la.nuit.passée.",
                    "Irritabilité..en.ce.moment.ci.",
                    "Anxiété..en.ce.moment.ci.",
                    "Humeur..en.ce.moment.ci.",
                    "Nombre.de.changement.d.humeur..depuis.la.dernière.évaluation."),
     variable.name = "variable")

dim(AHO_DF_long)
nrow(AHO_DF_long)/5
nrow(AHO_DF)
nrow(AHO_DF_long)/5 == nrow(AHO_DF) # multiple of the AHO_DF => fractor 5 because 5 DVs

colnames(AHO_DF_long)

AHO_DF_long$variable
AHO_DF_long$value

AHO_DF_long$variable <- car::recode(AHO_DF_long$variable, 
                                                     '"Insomnie..la.nuit.passée."="Insomnie (nuit dernière)";
                                                     "Irritabilité..en.ce.moment.ci."="Irritabilité";
                                                     "Anxiété..en.ce.moment.ci."="Anxiété";
                                                     "Humeur..en.ce.moment.ci."="Humeur";
                                                     "Nombre.de.changement.d.humeur..depuis.la.dernière.évaluation."="Changements d\'humeur"
                                                     ')   

AHO_DF_long$scale_min <- car::recode(AHO_DF_long$variable, 
                                    '"Insomnie (nuit dernière)"=0;
                                    "Irritabilité"=0;
                                    "Anxiété"=0;
                                    "Humeur"=0;
                                    "Changements d\'humeur"=0
                                    ')   
AHO_DF_long$scale_max <- car::recode(AHO_DF_long$variable, 
                                     '"Insomnie (nuit dernière)"=1;
                                     "Irritabilité"=2;
                                     "Anxiété"=10;
                                     "Humeur"=10;
                                     "Changements d\'humeur"=5
                                     ')   

AHO_DF_long$scale_min <- as.numeric(as.character(AHO_DF_long$scale_min))
AHO_DF_long$scale_max <- as.numeric(as.character(AHO_DF_long$scale_max))

##### plot

# plot(c(1:59), AHO_DF_long$value, ylim=c(0,10), type="l")
# plot(AHO_DF$Horodateur2, AHO_DF$Humeur..en.ce.moment.ci., ylim=c(0,10), type="l")


ggplot(AHO_DF, aes(Horodateur2, Humeur..en.ce.moment.ci.)) + 
  geom_point() + 
  coord_cartesian(ylim=c(0,10)) +
  geom_line() +
  geom_hline(yintercept=5, linetype="dashed", color="grey50")


# variable ranges for different scales
# dummy <- data.frame(variable = levels(AHO_DF_long$variable), range = , 
                    # stringsAsFactors=FALSE)


ggplot(AHO_DF_long, aes(Horodateur2, value)) + 
  geom_point() + 
  # coord_cartesian(ylim=c(0,10)) +
  geom_line() +
  # geom_hline(yintercept=5, linetype="dashed", color="grey50") +
  facet_wrap(.~variable, ncol=1, scales = "free_y") +
  geom_blank(ymin=c(0,0,0,0,0),ymax=c(10,5,10,1,2))
  

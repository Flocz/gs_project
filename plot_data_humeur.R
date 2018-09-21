# library(openxlsx)
# library(googlesheets)
library(gsheet)
library(ggplot2)
library(lubridate)
library(dplyr)


### load the data ----
data = gsheet2tbl(path)

# NOTES: find useful info about spreadsheets queries at : construct_download_url {gsheet}


### Add new variables to the data ----
# Date : create date, create hour.

data$date = gsub(' .*', '', data$Horodateur)
data$hour = gsub('.* ', '', data$Horodateur)

# TODO : transforme horodateur in date variables (i.e. with lubridate)

# Nombre de changement d'humeur : change into integer
as.integer(gsub(',', '\\.', data$`Nombre de changement d'humeur (depuis la dernière évaluation)`))


### Create a categorical variable for time by hours ----
# time variable will be considered as factor

hcat <- NA 
for (i in 1:dim(data)[1]) {
  if (data$Hour[i] >= 8 & data$Hour[i] < 12) {
    hcat[i] <- 'Matin [08:12['
  } else if (data$Hour[i] >= 12 & data$Hour[i] < 16) {
    hcat[i] <- 'Midi [12:16['
  } else if (data$Hour[i] >= 16 & data$Hour[i] < 20) {
    hcat[i] <- 'Soir [16:20['
  } else if (data$Hour[i] >= 20 & data$Hour[i] < 23) {
    hcat[i] <- 'Nuit [20:23['
  }
}

data$hcat = factor(hcat, levels = c("Matin [08:12[", "Midi [12:16[", "Soir [16:20[", "Nuit [20:23["))

data$hcatNum = as.numeric(data$hcat)

### Create a continue time variable ----

sdh = strsplit(data$hour, split = ':')
sdh = do.call(rbind, lapply(sdh, as.numeric))
data$continuousTime = c(sdh[,1] + sdh[,2] / 60 + sdh[,3] / 3600)


### Plot ----
# TODO : add a condition whether we want to plot the continuous or the  factorial variables

yVar = '`Anxiété (en ce moment-ci)`' # 'Humeur_NUM' # 
xVar = 'hcatNum' # 'continuousTime' # 

p = ggplot(data, aes_string(x = xVar, y = yVar), color = 'date') +
  theme_minimal()

  if (xVar == 'hcatNum') {
    p = p + 
      geom_boxplot(aes_string(x = factor(hcat))) +
      geom_point(shape = 21, fill = 'white', 
                 position = position_jitter(w = 0.2, h = 0)) +
      geom_smooth(aes_string(x = xVar, y = yVar)) +
      # TODO add x labels from 'hcat' 
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  } else {
    p = p + 
      geom_point(shape = 21, fill = 'white') +
      geom_smooth(method = 'loess')
  }

p = p + 
  coord_cartesian(ylim = c(0, 10)) +
  geom_hline(yintercept = 5, col = 'red')

p

plot(data$continuousTime, data$Humeur_NUM)


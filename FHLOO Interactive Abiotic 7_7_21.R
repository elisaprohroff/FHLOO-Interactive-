################# INTERACTIVE PLOT ####################


################ set working directory ###############################

rm(list = ls())

setwd('C:/Users/e/Desktop/Kirk REU FHLOO Data')

install.packages("cran")
install.packages("zoo")
install.packages('rlang')
install.packages(
  "ggplot2",
  repos = c("http://rstudio.org/_packages",
            "http://cran.rstudio.com")
)

###############load libraries #############################
library(ggplot2)
library(scales)
library(grid)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)
library(plotly)
library(dygraphs)
library(xts)          # To make the conversion data-frame / xts format
library(tidyverse)
library(zoo)
#library(cran)

################ Open FHLOO as CSV ######################

# Read the data, turn nonexistent data to "NaN"
FHLOO1 <- read.csv('NANOOS_FHLOO - Newest - Copy.CSV', na.strings=c("-9999","NaN"))

#FHLOO1 <- read.table("NANOOS_FHLOO - Newest - Copy.CSV", na.strings=c("-9999","NaN"), header=T, sep=",") %>% head(2483)

colnames(FHLOO1)


# Change character string to date&time in POSIXct
FHLOO1$Date <- ymd_hms(FHLOO1$PDT)

class(FHLOO1$Date)


#Change POSIXct string to Date string
#FHLOO1$Date <- as.Date(as.POSIXct(FHLOO1$Date), format='%m-%d-%Y', tz = "")%>% head(2154)

#~~~~~~~~~~~~~~~~~~~ Create Bins for Salinity ~~~~~~~~~~~~~~~~~~~~~~~

# Initialize empty variable first
FHLOO1$Salinity <- NA

#Fill in values (Only 2 Categories)
#FHLOO1$Salinity <- ifelse(FHLOO$SBE37Sal_PSU >= 30, "HighSal", "LowSal")

#Create 3 different thresholds for salinity and separate into high, med, and low
FHLOO1$Salinity <- cut(FHLOO1$SBE37Sal_PSU, breaks = c(-Inf, 27, 30, Inf),
                      labels = c("LowSal", "MedSal", "HighSal"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot TEMP data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

T <- ggplot(data = FHLOO1, aes(x = Date, y = SBE37Temp_C))+ 
  geom_line(color="#69b3a2") +
  ylab("Water Temperature (°C)") +
  ggtitle("Sea Water Temperature Over Time") + 
  ylim(8.5, 13.5) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(T)

# NOTE: Dates start in May 2021 for this file


#Attempt to make TS plot
Temp <- ts(FHLOO1$SBE37Temp_C, start = c(2021,5,7), end = c(2021,6,21), frequency = 2154)

plot(Temp)

view(Temp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot pH data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pH <- ggplot(data = FHLOO1, aes(x = Date, y = pH))+ 
  geom_line(color="#69b3a2") +
  ylab("pH") +
  ggtitle("Sea Water pH Over Time") + 
  ylim(6,8) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(pH)

#Fix date window

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot Dissolved O2 data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO1)

DO <- ggplot(data = FHLOO1, aes(x = Date, y = O2Conc_mgL))+ 
  geom_line(color="#69b3a2") +
  ylab("Dissolved Oxygen (mg/L)") +
  ggtitle("Dissolved Oxygen Over Time") + 
  ylim(7,11) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(DO)

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot CO2 Partial Pressure data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO1)

pCO2 <- ggplot(data = FHLOO1, aes(x = Date, y = CO2_uAtm))+ 
  geom_line(color="#69b3a2") +
  ylab("pCO2 (uAtm)") +
  ggtitle("Partial Pressure of Carbon Dioxide Over Time") + 
  ylim(500, 950) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(pCO2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot Practical Salinity units data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO1)

Sal <- ggplot(data = FHLOO1, aes(x = Date, y = SBE37Sal_PSU))+ 
  geom_line(color="#69b3a2") +
  ylab("Salinity") +
  ggtitle("Salinity Over Time") + 
  ylim(25,31) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(Sal)


#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot Chlorophyll data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO1)

ECO <- ggplot(data = FHLOO1, aes(x = Date, y = ECOChl_ug_l))+ 
  geom_line(color="#69b3a2") +
  ylab("Chlorophyll Fluorescence (ug/L)") +
  ggtitle("Chlorophyll Fluorescence Over Time") + 
  ylim(0,1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(ECO)

#Fix time window on x axis


#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot Turbidity data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO1)

NTU <- ggplot(data = FHLOO1, aes(x = Date, y = ECONTU))+ 
  geom_line(color="#69b3a2") +
  ylab("Turbidity (NTU)") +
  ggtitle("Water Turbidity Over Time") + 
  ylim(0,1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(NTU)

#Fix time window on x axis


#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot O2 Air Saturation data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO1)

AirSat <- ggplot(data = FHLOO1, aes(x = Date, y = O2AirSat_Percent))+ 
  geom_line(color="#69b3a2") +
  ylab("Dissolved O2 (% Saturation)") +
  ggtitle("Dissolved Oxygen Over Time") + 
  ylim(30, 160) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(AirSat)





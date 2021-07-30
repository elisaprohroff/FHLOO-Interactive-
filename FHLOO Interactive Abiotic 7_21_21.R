################# INTERACTIVE PLOT ####################


################ set working directory ###############################

rm(list = ls())

setwd('C:/Users/e/Desktop/Kirk REU FHLOO Data')

install.packages("seacarb")
install.packages("sf")
install.packages("oce")
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
library(seacarb)
#library(sf) This package freezes my computer 
library (oce)
################ Open FHLOO as CSV ######################

# Read the data, turn nonexistent data to "NaN"
FHLOO1 <- read.csv('NANOOS_FHLOO(1).CSV', na.strings=c("-9999","NaN"))

#FHLOO1 <- read.table("NANOOS_FHLOO - Newest - Copy.CSV", 
#na.strings=c("-9999","NaN"), header=T, sep=",") %>% head(2483)

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
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
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
T

ggplotly(T)

# NOTE: Dates start in May 2021 for this file


#Attempt to make TS plot
#Temp <- ts(FHLOO1$SBE37Temp_C, start = c(2021,5,7), end = c(2021,6,21), frequency = 2154)

plot(Temp)

view(Temp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot pH data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pH <- ggplot(data = FHLOO1, aes(x = Date, y = pH))+ 
  geom_line(color="#69b3a2") +
  ylab("pH") +
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d"),breaks = as.Date(from = "2021-06-21", to = "2021-07-21")) +
  scale_y_continuous(breaks = seq(from = 7.5, to = 9.0, by = .2)) +                                                                                        
  ggtitle("Sea Water pH Over Time") + 
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

pH

ggplotly(pH)

#Fix date window

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot DO data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO1)

DO <- ggplot(data = FHLOO1, aes(x = Date, y = O2Conc_mgL))+ 
  geom_line(color="#69b3a2") +
  ylab("Dissolved Oxygen (mg/L)") +
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot CO2 Partial Pressure data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO1)

pCO2 <- ggplot(data = FHLOO1, aes(x = Date, y = CO2_uAtm))+ 
  geom_line(color="#69b3a2") +
  ylab("pCO2 (uAtm)") +
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot Practical Salinity units data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO1)

Sal <- ggplot(data = FHLOO1, aes(x = Date, y = SBE37Sal_PSU))+ 
  geom_line(color="#69b3a2") +
  ylab("Salinity") +
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
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
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
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
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot O2 Air Saturation data using ggplot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO1)

AirSat <- ggplot(data = FHLOO1, aes(x = Date, y = O2AirSat_Percent))+ 
  geom_line(color="#69b3a2") +
  ylab("Dissolved O2 (% Saturation)") +
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Use SeaCarb ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


colnames(Carb)

Carb <- carbfull(flag = 21, FHLOO1$CO2_uAtm, FHLOO1$pH, S=FHLOO1$SBE37Sal_PSU, 
                 T=FHLOO1$SBE37Temp_C, Patm=1, P=0, Pt=0, Sit=0,
         k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential",
         NH4t=0, HSt=0)


# Create dataframe of variables of interest from Carb data frame
data2 <- data.frame(SeaCarb_OmegaAragonite = Carb$OmegaAragonite, 
                    SeaCarb_OmegaCalcite = Carb$OmegaCalcite, 
                    SeaCarb_DIC = Carb$DIC, 
                    SeaCarb_ALK = Carb$ALK)


#Add columns from data2 to FHLOO1 to combine datasets
FHLOO2 <- cbind(FHLOO1, data2)

#Save as CSV file
write.csv(FHLOO2, "FHLOO2.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot Aragonite over time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("reshape")
library(reshape)

df.melted <- melt(FHLOO2, id = c("Date", "SeaCarb_OmegaAragonite", "SeaCarb_OmegaCalcite", 
                                 "SeaCarb_DIC", "SeaCarb_ALK"))

#Subset data 
dm <- subset(df.melted, select = c(Date, SeaCarb_OmegaAragonite, SeaCarb_OmegaCalcite, 
                                   SeaCarb_DIC, SeaCarb_ALK))

dy <- subset(dm, select = c(SeaCarb_OmegaAragonite, SeaCarb_OmegaCalcite, 
                            SeaCarb_DIC, SeaCarb_ALK))

p <- ggplot(data = dm, aes(x = Date, y = SeaCarb_OmegaAragonite, SeaCarb_OmegaCalcite, 
                           SeaCarb_DIC, SeaCarb_ALK)) +
  geom_line() 
#+ facet_wrap( ~ SeaCarb_OmegaAragonite + SeaCarb_OmegaCalcite + 
                           #  SeaCarb_DIC + SeaCarb_ALK, nrow = 4)

ggplotly(p)
  
# 6 figures arranged in 3 rows and 2 columns
par(mfrow=c(2,3), lty = )
plot(FHLOO2$Date, FHLOO2$SBE37Temp_C, main="Temperature")
plot(FHLOO2$Date, FHLOO2$O2Conc_mgL, main="Oxygen")
plot(FHLOO2$Date, FHLOO2$CO2_uAtm, main="pCO2")
plot(FHLOO2$Date, FHLOO2$SBE37Sal_PSU, main="Salinity")
plot(FHLOO2$Date, FHLOO2$pH, main="pH")
plot(FHLOO2$Date, FHLOO2$SeaCarb_OmegaCalcite, main="Calcite")

?par

colnames(FHLOO2)

Arag <- ggplot(data = FHLOO2, aes(x = Date, y = SeaCarb_OmegaAragonite))+ 
  geom_line(color="gold2") +
  ylab("Aragonite Saturation") +
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
  ggtitle("Aragonite Saturation Over Time") + 
  ylim(0, 5) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(Arag)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Calcite Over Time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO2)

Calc <- ggplot(data = FHLOO2, aes(x = Date, y = SeaCarb_OmegaCalcite))+ 
  geom_line(color="darkorange2") +
  ylab("Calcite Saturation") +
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
  ggtitle("Calcite Saturation Over Time") + 
  ylim(0, 7) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(Calc)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ALK Over Time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO2)

Alk <- ggplot(data = FHLOO2, aes(x = Date, y = SeaCarb_ALK))+ 
  geom_line(color="springgreen4") +
  ylab("Alkalinity") +
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
  ggtitle("Alkalinity Over Time") + 
  ylim(0, .005) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(Alk)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DIC Over Time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(FHLOO2)

DIC <- ggplot(data = FHLOO2, aes(x = Date, y = SeaCarb_DIC))+ 
  geom_line(color="coral2") +
  ylab("Dissolved Inorganic Carbon") +
  scale_x_datetime(date_breaks = "4 days", labels = date_format("%b %d")) +
  ggtitle("Dissolved Inorganic Carbon Over Time") + 
  ylim(0, .005) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =12,lineheight=3),
        axis.text.x= element_text(size =10),
        axis.text.y = element_text(size =10),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =12,lineheight=3)) 

ggplotly(DIC)

#Save as Static Image 
library(plotly)
if (!require("processx")) install.packages("processx")

$ chmod +x orca-X.Y.Z-x86_64.AppImage

C:/Users/e/AppData/Local/Programs/orca

fig <- plot_ly(z = ~DIC) %>% add_surface()
orca(fig, "DIC-plot.svg")


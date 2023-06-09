#### DOC STRING ===============================================================
# This script uses the total transmission of single and co-occurring parasites 
# to compare transmissibility in the absence and presence of environmental 
# variability.

# First, this script wrangles and summarizes total transmission data. Second,
# this script plots median total transmission of co-occurring parasites by
# magnitude and frequency of disturbance. Third, this script calculates  
# differences between the total transmission of single and co-occurring parasites,
# and plots said differences as heat maps. Next, this script plots the total 
# transmission of single and co-occurring parasites in the absence of environmental
# variability next to select cases in which the addition of environmental
# variability results in the total transmission of a co-occurring parasite exceeding 
# that of a single parasite.

# Outputs are collected in the "Collected summary objects" section. To see the 
# outputs generated by this script without running it, go to 
# "EnvironmentalVariability_Repo/Outputs/TotalTransmission_SummaryOutputs".
#### END DOC STRING ===========================================================

# Required packages
install.packages("tidyverse"); install.packages("geomtextpath")
library(tidyvserse); library(geomtextpath)

#### Data wrangling ####
# NOTE! If you want to use the outputs you've generated, replace "Outputs/TotalTransmission_Outputs" with "Generated_Outputs".
#### ... Demographically stochastic simulations ####
#### ... ... Single parasite ####
D_LT_Iden_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_LT_Iden_1P_AUC.rds")
D_LT_Semi_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_LT_Semi_1P_AUC.rds")
D_LT_PiAd_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_LT_PiAd_1P_AUC.rds")
D_LT_PjAd_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_LT_PjAd_1P_AUC.rds")
D_HT_Iden_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_HT_Iden_1P_AUC.rds")
D_HT_Semi_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_HT_Semi_1P_AUC.rds")
D_HT_PiAd_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_HT_PiAd_1P_AUC.rds")
D_HT_PjAd_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_HT_PjAd_1P_AUC.rds")
#### ... ... Two parasites ####
D_LT_Iden_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_LT_Iden_2P_AUC.rds")
D_LT_Semi_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_LT_Semi_2P_AUC.rds")
D_LT_PiAd_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_LT_PiAd_2P_AUC.rds")
D_LT_PjAd_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_LT_PjAd_2P_AUC.rds")
D_HT_Iden_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_HT_Iden_2P_AUC.rds")
D_HT_Semi_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_HT_Semi_2P_AUC.rds")
D_HT_PiAd_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_HT_PiAd_2P_AUC.rds")
D_HT_PjAd_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/D_HT_PjAd_2P_AUC.rds")
#### ... Demographically and environmentally stochastic simulations ####
#### ... ... Single parasite ####
DE_LT_Iden_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_LT_Iden_1P_AUC.rds"); DE_LT_Iden_1P_AUC_Medians <- miscTools::colMedians(DE_LT_Iden_1P_AUC[ ,2:ncol(DE_LT_Iden_1P_AUC)])
DE_LT_Semi_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_LT_Semi_1P_AUC.rds"); DE_LT_Semi_1P_AUC_Medians <- miscTools::colMedians(DE_LT_Semi_1P_AUC[ ,2:ncol(DE_LT_Semi_1P_AUC)])
DE_LT_PiAd_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_LT_PiAd_1P_AUC.rds"); DE_LT_PiAd_1P_AUC_Medians <- miscTools::colMedians(DE_LT_PiAd_1P_AUC[ ,2:ncol(DE_LT_PiAd_1P_AUC)])
DE_LT_PjAd_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_LT_PjAd_1P_AUC.rds"); DE_LT_PjAd_1P_AUC_Medians <- miscTools::colMedians(DE_LT_PjAd_1P_AUC[ ,2:ncol(DE_LT_PjAd_1P_AUC)])
DE_HT_Iden_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_HT_Iden_1P_AUC.rds"); DE_HT_Iden_1P_AUC_Medians <- miscTools::colMedians(DE_HT_Iden_1P_AUC[ ,2:ncol(DE_HT_Iden_1P_AUC)])
DE_HT_Semi_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_HT_Semi_1P_AUC.rds"); DE_HT_Semi_1P_AUC_Medians <- miscTools::colMedians(DE_HT_Semi_1P_AUC[ ,2:ncol(DE_HT_Semi_1P_AUC)])
DE_HT_PiAd_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_HT_PiAd_1P_AUC.rds"); DE_HT_PiAd_1P_AUC_Medians <- miscTools::colMedians(DE_HT_PiAd_1P_AUC[ ,2:ncol(DE_HT_PiAd_1P_AUC)])
DE_HT_PjAd_1P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_HT_PjAd_1P_AUC.rds"); DE_HT_PjAd_1P_AUC_Medians <- miscTools::colMedians(DE_HT_PjAd_1P_AUC[ ,2:ncol(DE_HT_PjAd_1P_AUC)])
#### ... ... Two parasites ####
DE_LT_Iden_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_LT_Iden_2P_AUC.rds"); DE_LT_Iden_2P_AUC_Medians <- miscTools::colMedians(DE_LT_Iden_2P_AUC[ ,2:ncol(DE_LT_Iden_2P_AUC)])
DE_LT_Semi_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_LT_Semi_2P_AUC.rds"); DE_LT_Semi_2P_AUC_Medians <- miscTools::colMedians(DE_LT_Semi_2P_AUC[ ,2:ncol(DE_LT_Semi_2P_AUC)])
DE_LT_PiAd_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_LT_PiAd_2P_AUC.rds"); DE_LT_PiAd_2P_AUC_Medians <- miscTools::colMedians(DE_LT_PiAd_2P_AUC[ ,2:ncol(DE_LT_PiAd_2P_AUC)])
DE_LT_PjAd_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_LT_PjAd_2P_AUC.rds"); DE_LT_PjAd_2P_AUC_Medians <- miscTools::colMedians(DE_LT_PjAd_2P_AUC[ ,2:ncol(DE_LT_PjAd_2P_AUC)])
DE_HT_Iden_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_HT_Iden_2P_AUC.rds"); DE_HT_Iden_2P_AUC_Medians <- miscTools::colMedians(DE_HT_Iden_2P_AUC[ ,2:ncol(DE_HT_Iden_2P_AUC)])
DE_HT_Semi_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_HT_Semi_2P_AUC.rds"); DE_HT_Semi_2P_AUC_Medians <- miscTools::colMedians(DE_HT_Semi_2P_AUC[ ,2:ncol(DE_HT_Semi_2P_AUC)])
DE_HT_PiAd_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_HT_PiAd_2P_AUC.rds"); DE_HT_PiAd_2P_AUC_Medians <- miscTools::colMedians(DE_HT_PiAd_2P_AUC[ ,2:ncol(DE_HT_PiAd_2P_AUC)])
DE_HT_PjAd_2P_AUC <- readRDS("Outputs/TotalTransmission_Outputs/DE_HT_PjAd_2P_AUC.rds"); DE_HT_PjAd_2P_AUC_Medians <- miscTools::colMedians(DE_HT_PjAd_2P_AUC[ ,2:ncol(DE_HT_PjAd_2P_AUC)])

SDs_LT <- rep(seq(2, 30, 2), 13); SDs_HT <- rep(seq(2, 40, 2), 13)
Freqs_LT <- rep(c(1, 2, 5, seq(10, 100, 10)), each = 15); Freqs_HT <- rep(c(1, 2, 5, seq(10, 100, 10)), each = 20)
DF_LT <- cbind(SDs_LT, Freqs_LT); DF_HT <- cbind(SDs_HT, Freqs_HT)

DE_LT_Iden_1P_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_Iden_1P_AUC_Medians)))
DE_LT_Semi_1P_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_Semi_1P_AUC_Medians)))
DE_LT_PiAd_1P_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_PiAd_1P_AUC_Medians)))
DE_LT_PjAd_1P_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_PjAd_1P_AUC_Medians)))
DE_HT_Iden_1P_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_Iden_1P_AUC_Medians)))
DE_HT_Semi_1P_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_Semi_1P_AUC_Medians)))
DE_HT_PiAd_1P_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_PiAd_1P_AUC_Medians)))
DE_HT_PjAd_1P_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_PjAd_1P_AUC_Medians)))
Pis <- seq(1, length(DE_LT_Iden_2P_AUC_Medians), 2); Pjs <- seq(2, length(DE_LT_Iden_2P_AUC_Medians), 2)
DE_LT_Iden_2P_Pi_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_Iden_2P_AUC_Medians[Pis])))
DE_LT_Iden_2P_Pj_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_Iden_2P_AUC_Medians[Pjs])))
DE_LT_Semi_2P_Pi_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_Semi_2P_AUC_Medians[Pis])))
DE_LT_Semi_2P_Pj_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_Semi_2P_AUC_Medians[Pjs])))
DE_LT_PiAd_2P_Pi_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_PiAd_2P_AUC_Medians[Pis])))
DE_LT_PiAd_2P_Pj_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_PiAd_2P_AUC_Medians[Pjs])))
DE_LT_PjAd_2P_Pi_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_PjAd_2P_AUC_Medians[Pis])))
DE_LT_PjAd_2P_Pj_HM <- as.data.frame(cbind(DF_LT, as.numeric(DE_LT_PjAd_2P_AUC_Medians[Pjs])))
Pis <- seq(1, length(DE_HT_Iden_2P_AUC_Medians), 2); Pjs <- seq(2, length(DE_HT_Iden_2P_AUC_Medians), 2)
DE_HT_Iden_2P_Pi_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_Iden_2P_AUC_Medians[Pis])))
DE_HT_Iden_2P_Pj_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_Iden_2P_AUC_Medians[Pjs])))
DE_HT_Semi_2P_Pi_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_Semi_2P_AUC_Medians[Pis])))
DE_HT_Semi_2P_Pj_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_Semi_2P_AUC_Medians[Pjs])))
DE_HT_PiAd_2P_Pi_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_PiAd_2P_AUC_Medians[Pis])))
DE_HT_PiAd_2P_Pj_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_PiAd_2P_AUC_Medians[Pjs])))
DE_HT_PjAd_2P_Pi_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_PjAd_2P_AUC_Medians[Pis])))
DE_HT_PjAd_2P_Pj_HM <- as.data.frame(cbind(DF_HT, as.numeric(DE_HT_PjAd_2P_AUC_Medians[Pjs])))

#### Plotting total transmission of co-occurring parasites ####
#### ... DE_LT_Iden_2P_Pi_HM ####
nb.cols <- nrow(DE_LT_Iden_2P_Pi_HM)
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)
ggplot(DE_LT_Iden_2P_Pi_HM, aes(Freqs_LT, SDs_LT, z = V3)) +
  geom_contour_filled(bins = nrow(DE_LT_Iden_2P_Pi_HM), show.legend = FALSE)+
  scale_fill_manual(values = rev(mycolors))+
  labs(x="Frequency of Disturbance", 
       y="Magnitude of Disturbance",
       title=" ",
       subtitle=" ")+
  scale_x_reverse(breaks = c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10 times per day", " ", "Twice per day", "Once per day", "Once every 2 days", "", "", "Once every 5 days", "", "Once per week", "", "", "Once every 10 days"))+
  theme_bw()+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust=1), 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))
#### ... DE_LT_Iden_2P_Pj_HM ####
nb.cols <- nrow(DE_LT_Iden_2P_Pj_HM)
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)
ggplot(DE_LT_Iden_2P_Pj_HM, aes(Freqs_LT, SDs_LT, z = V3)) +
  geom_contour_filled(bins = nrow(DE_LT_Iden_2P_Pj_HM), show.legend = FALSE)+
  scale_fill_manual(values = rev(mycolors))+
  labs(x="Frequency of Disturbance", 
       y="Magnitude of Disturbance",
       title=" ",
       subtitle=" ")+
  scale_x_reverse(breaks = c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10 times per day", " ", "Twice per day", "Once per day", "Once every 2 days", "", "", "Once every 5 days", "", "Once per week", "", "", "Once every 10 days"))+
  theme_bw()+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust=1), 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))
#### ... DE_LT_Semi_2P_Pi_HM ####
nb.cols <- nrow(DE_LT_Semi_2P_Pi_HM)
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)
ggplot(DE_LT_Semi_2P_Pi_HM, aes(Freqs_LT, SDs_LT, z = V3)) +
  geom_contour_filled(bins = nrow(DE_LT_Semi_2P_Pi_HM), show.legend = FALSE)+
  scale_fill_manual(values = rev(mycolors))+
  labs(x="Frequency of Disturbance", 
       y="Magnitude of Disturbance",
       title=" ",
       subtitle=" ")+
  scale_x_reverse(breaks = c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10 times per day", " ", "Twice per day", "Once per day", "Once every 2 days", "", "", "Once every 5 days", "", "Once per week", "", "", "Once every 10 days"))+
  theme_bw()+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust=1), 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))
#### ... DE_LT_Semi_2P_Pj_HM ####
nb.cols <- nrow(DE_LT_Semi_2P_Pj_HM)
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)
ggplot(DE_LT_Semi_2P_Pj_HM, aes(Freqs_LT, SDs_LT, z = V3)) +
  geom_contour_filled(bins = nrow(DE_LT_Semi_2P_Pj_HM), show.legend = FALSE)+
  scale_fill_manual(values = rev(mycolors))+
  labs(x="Frequency of Disturbance", 
       y="Magnitude of Disturbance",
       title=" ",
       subtitle=" ")+
  scale_x_reverse(breaks = c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10 times per day", " ", "Twice per day", "Once per day", "Once every 2 days", "", "", "Once every 5 days", "", "Once per week", "", "", "Once every 10 days"))+
  theme_bw()+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust=1), 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))
#### ... DE_LT_PiAd_2P_Pi_HM ####
nb.cols <- nrow(DE_LT_PiAd_2P_Pi_HM)
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)
ggplot(DE_LT_PiAd_2P_Pi_HM, aes(Freqs_LT, SDs_LT, z = V3)) +
  geom_contour_filled(bins = nrow(DE_LT_PiAd_2P_Pi_HM), show.legend = FALSE)+
  scale_fill_manual(values = rev(mycolors))+
  labs(x="Frequency of Disturbance", 
       y="Magnitude of Disturbance",
       title=" ",
       subtitle=" ")+
  scale_x_reverse(breaks = c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10 times per day", " ", "Twice per day", "Once per day", "Once every 2 days", "", "", "Once every 5 days", "", "Once per week", "", "", "Once every 10 days"))+
  theme_bw()+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust=1), 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))
#### ... DE_LT_PiAd_2P_Pj_HM ####
nb.cols <- nrow(DE_LT_PiAd_2P_Pj_HM)
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)
ggplot(DE_LT_PiAd_2P_Pj_HM, aes(Freqs_LT, SDs_LT, z = V3)) +
  geom_contour_filled(bins = nrow(DE_LT_PiAd_2P_Pj_HM), show.legend = FALSE)+
  scale_fill_manual(values = rev(mycolors))+
  labs(x="Frequency of Disturbance", 
       y="Magnitude of Disturbance",
       title=" ",
       subtitle=" ")+
  scale_x_reverse(breaks = c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10 times per day", " ", "Twice per day", "Once per day", "Once every 2 days", "", "", "Once every 5 days", "", "Once per week", "", "", "Once every 10 days"))+
  theme_bw()+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust=1), 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))
#### ... DE_LT_PjAd_2P_Pi_HM ####
nb.cols <- nrow(DE_LT_PjAd_2P_Pi_HM)
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)
ggplot(DE_LT_PjAd_2P_Pi_HM, aes(Freqs_LT, SDs_LT, z = V3)) +
  geom_contour_filled(bins = nrow(DE_LT_PjAd_2P_Pi_HM), show.legend = FALSE)+
  scale_fill_manual(values = rev(mycolors))+
  labs(x="Frequency of Disturbance", 
       y="Magnitude of Disturbance",
       title=" ",
       subtitle=" ")+
  scale_x_reverse(breaks = c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10 times per day", " ", "Twice per day", "Once per day", "Once every 2 days", "", "", "Once every 5 days", "", "Once per week", "", "", "Once every 10 days"))+
  theme_bw()+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust=1), 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))
#### ... DE_LT_PjAd_2P_Pj_HM ####
nb.cols <- nrow(DE_LT_PjAd_2P_Pj_HM)
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)
ggplot(DE_LT_PjAd_2P_Pj_HM, aes(Freqs_LT, SDs_LT, z = V3)) +
  geom_contour_filled(bins = nrow(DE_LT_PjAd_2P_Pj_HM), show.legend = FALSE)+
  scale_fill_manual(values = rev(mycolors))+
  labs(x="Frequency of Disturbance", 
       y="Magnitude of Disturbance",
       title=" ",
       subtitle=" ")+
  scale_x_reverse(breaks = c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10 times per day", " ", "Twice per day", "Once per day", "Once every 2 days", "", "", "Once every 5 days", "", "Once per week", "", "", "Once every 10 days"))+
  theme_bw()+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust=1), 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))

#### Calculating differences in transmission between single and co-occurring parasites (1P - 2P) #####
## Note: Here, negative outcomes indicate that co-infecting parasite population grew larger than single infection population, so we're looking for negative numbers
#### ... Computing differences ####
Pis <- seq(1, length(DE_LT_Iden_2P_AUC_Medians), 2); Pjs <- seq(2, length(DE_LT_Iden_2P_AUC_Medians), 2)

DE_LT_Iden_2P_AUC_PiMedians <- DE_LT_Iden_2P_AUC_Medians[Pis]; DE_LT_Iden_2P_AUC_PjMedians <- DE_LT_Iden_2P_AUC_Medians[Pjs]
DE_LT_Iden_Diffs_1m2Pi <- DE_LT_Iden_1P_AUC_Medians - DE_LT_Iden_2P_AUC_PiMedians
DE_LT_Iden_Diffs_1m2Pj <- DE_LT_Iden_1P_AUC_Medians - DE_LT_Iden_2P_AUC_PjMedians
DE_LT_Iden_Diffs_1m2Pi_Less0 <- which(DE_LT_Iden_Diffs_1m2Pi <= 0)
DE_LT_Iden_Diffs_1m2Pj_Less0 <- which(DE_LT_Iden_Diffs_1m2Pj <= 0)

DE_LT_Semi_2P_AUC_PiMedians <- DE_LT_Semi_2P_AUC_Medians[Pis]; DE_LT_Semi_2P_AUC_PjMedians <- DE_LT_Semi_2P_AUC_Medians[Pjs]
DE_LT_Semi_Diffs_1m2Pi <- DE_LT_Semi_1P_AUC_Medians - DE_LT_Semi_2P_AUC_PiMedians
DE_LT_Semi_Diffs_1m2Pj <- DE_LT_Semi_1P_AUC_Medians - DE_LT_Semi_2P_AUC_PjMedians
DE_LT_Semi_Diffs_1m2Pi_Less0 <- which(DE_LT_Semi_Diffs_1m2Pi <= 0)
DE_LT_Semi_Diffs_1m2Pj_Less0 <- which(DE_LT_Semi_Diffs_1m2Pj <= 0)

DE_LT_PiAd_2P_AUC_PiMedians <- DE_LT_PiAd_2P_AUC_Medians[Pis]; DE_LT_PiAd_2P_AUC_PjMedians <- DE_LT_PiAd_2P_AUC_Medians[Pjs]
DE_LT_PiAd_Diffs_1m2Pi <- DE_LT_PiAd_1P_AUC_Medians - DE_LT_PiAd_2P_AUC_PiMedians
DE_LT_PiAd_Diffs_1m2Pj <- DE_LT_PiAd_1P_AUC_Medians - DE_LT_PiAd_2P_AUC_PjMedians
DE_LT_PiAd_Diffs_1m2Pi_Less0 <- which(DE_LT_PiAd_Diffs_1m2Pi <= 0)
DE_LT_PiAd_Diffs_1m2Pj_Less0 <- which(DE_LT_PiAd_Diffs_1m2Pj <= 0)

DE_LT_PjAd_2P_AUC_PiMedians <- DE_LT_PjAd_2P_AUC_Medians[Pis]; DE_LT_PjAd_2P_AUC_PjMedians <- DE_LT_PjAd_2P_AUC_Medians[Pjs]
DE_LT_PjAd_Diffs_1m2Pi <- DE_LT_PjAd_1P_AUC_Medians - DE_LT_PjAd_2P_AUC_PiMedians
DE_LT_PjAd_Diffs_1m2Pj <- DE_LT_PjAd_1P_AUC_Medians - DE_LT_PjAd_2P_AUC_PjMedians
DE_LT_PjAd_Diffs_1m2Pi_Less0 <- which(DE_LT_PjAd_Diffs_1m2Pi <= 0)
DE_LT_PjAd_Diffs_1m2Pj_Less0 <- which(DE_LT_PjAd_Diffs_1m2Pj <= 0)

Pis <- seq(1, length(DE_HT_Iden_2P_AUC_Medians), 2); Pjs <- seq(2, length(DE_HT_Iden_2P_AUC_Medians), 2)

DE_HT_Iden_2P_AUC_PiMedians <- DE_HT_Iden_2P_AUC_Medians[Pis]; DE_HT_Iden_2P_AUC_PjMedians <- DE_HT_Iden_2P_AUC_Medians[Pjs]
DE_HT_Iden_Diffs_1m2Pi <- DE_HT_Iden_1P_AUC_Medians - DE_HT_Iden_2P_AUC_PiMedians
DE_HT_Iden_Diffs_1m2Pj <- DE_HT_Iden_1P_AUC_Medians - DE_HT_Iden_2P_AUC_PjMedians
DE_HT_Iden_Diffs_1m2Pi_Less0 <- which(DE_HT_Iden_Diffs_1m2Pi <= 0)
DE_HT_Iden_Diffs_1m2Pj_Less0 <- which(DE_HT_Iden_Diffs_1m2Pj <= 0)

DE_HT_Semi_2P_AUC_PiMedians <- DE_HT_Semi_2P_AUC_Medians[Pis]; DE_HT_Semi_2P_AUC_PjMedians <- DE_HT_Semi_2P_AUC_Medians[Pjs]
DE_HT_Semi_Diffs_1m2Pi <- DE_HT_Semi_1P_AUC_Medians - DE_HT_Semi_2P_AUC_PiMedians
DE_HT_Semi_Diffs_1m2Pj <- DE_HT_Semi_1P_AUC_Medians - DE_HT_Semi_2P_AUC_PjMedians
DE_HT_Semi_Diffs_1m2Pi_Less0 <- which(DE_HT_Semi_Diffs_1m2Pi <= 0)
DE_HT_Semi_Diffs_1m2Pj_Less0 <- which(DE_HT_Semi_Diffs_1m2Pj <= 0)

DE_HT_PiAd_2P_AUC_PiMedians <- DE_HT_PiAd_2P_AUC_Medians[Pis]; DE_HT_PiAd_2P_AUC_PjMedians <- DE_HT_PiAd_2P_AUC_Medians[Pjs]
DE_HT_PiAd_Diffs_1m2Pi <- DE_HT_PiAd_1P_AUC_Medians - DE_HT_PiAd_2P_AUC_PiMedians
DE_HT_PiAd_Diffs_1m2Pj <- DE_HT_PiAd_1P_AUC_Medians - DE_HT_PiAd_2P_AUC_PjMedians
DE_HT_PiAd_Diffs_1m2Pi_Less0 <- which(DE_HT_PiAd_Diffs_1m2Pi <= 0)
DE_HT_PiAd_Diffs_1m2Pj_Less0 <- which(DE_HT_PiAd_Diffs_1m2Pj <= 0)

DE_HT_PjAd_2P_AUC_PiMedians <- DE_HT_PjAd_2P_AUC_Medians[Pis]; DE_HT_PjAd_2P_AUC_PjMedians <- DE_HT_PjAd_2P_AUC_Medians[Pjs]
DE_HT_PjAd_Diffs_1m2Pi <- DE_HT_PjAd_1P_AUC_Medians - DE_HT_PjAd_2P_AUC_PiMedians
DE_HT_PjAd_Diffs_1m2Pj <- DE_HT_PjAd_1P_AUC_Medians - DE_HT_PjAd_2P_AUC_PjMedians
DE_HT_PjAd_Diffs_1m2Pi_Less0 <- which(DE_HT_PjAd_Diffs_1m2Pi <= 0)
DE_HT_PjAd_Diffs_1m2Pj_Less0 <- which(DE_HT_PjAd_Diffs_1m2Pj <= 0)

#### ... Plotting differences in total transmission (heat maps) ####
DE_LT_PjAd_Diffs_1m2Pj_HM <- as.data.frame(cbind(DF_LT, DE_LT_PjAd_Diffs_1m2Pj))
nb.cols <- nrow(DE_LT_PjAd_Diffs_1m2Pj_HM)
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)
ggplot(DE_LT_PjAd_Diffs_1m2Pj_HM, aes(Freqs_LT, SDs_LT, z = DE_LT_PjAd_Diffs_1m2Pj)) +
  geom_contour_filled(bins = nrow(DE_LT_PjAd_Diffs_1m2Pj_HM), show.legend = FALSE)+
  scale_fill_manual(values = mycolors)+
  labs(x="Frequency of Disturbance", 
       y="Magnitude of Disturbance",
       title=" ",
       subtitle=" ")+
  scale_x_reverse(breaks = c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10 times per day", " ", "Twice per day", "Once per day", "Once every 2 days", "", "", "Once every 5 days", "", "Once per week", "", "", "Once every 10 days"))+
  geom_textcontour(bins = 1, size = 5, straight = TRUE, breaks = c(0))+
  theme_bw()+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust=1), 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))

DE_HT_PjAd_Diffs_1m2Pj_HM <- as.data.frame(cbind(DF_HT, DE_HT_PjAd_Diffs_1m2Pj))
nb.cols <- nrow(DE_HT_PjAd_Diffs_1m2Pj_HM)
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)
ggplot(DE_HT_PjAd_Diffs_1m2Pj_HM, aes(Freqs_HT, SDs_HT, z = DE_HT_PjAd_Diffs_1m2Pj)) +
  geom_contour_filled(bins = nrow(DE_HT_PjAd_Diffs_1m2Pj_HM), show.legend = FALSE)+
  scale_fill_manual(values = mycolors)+
  labs(x="Frequency of Disturbance", 
       y="Magnitude of Disturbance",
       title=" ",
       subtitle=" ")+
  scale_x_reverse(breaks = c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10 times per day", " ", "Twice per day", "Once per day", "Once every 2 days", "", "", "Once every 5 days", "", "Once per week", "", "", "Once every 10 days"))+
  geom_textcontour(bins = 1, size = 5, straight = TRUE, breaks = c(0))+
  theme_bw()+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust=1), 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))

#### ... Plotting differences in total transmission (box plots) ####
min(DE_LT_PjAd_Diffs_1m2Pj)
which(DE_LT_PjAd_Diffs_1m2Pj <= min(DE_LT_PjAd_Diffs_1m2Pj)) # Mag20_Freq80_In0
# D_LT_PjAd_1P_AUC
# D_LT_PjAd_2P_AUC
D_LT_PjAd_2P_AUC_Add <- D_LT_PjAd_2P_AUC$Vec_Pi + D_LT_PjAd_2P_AUC$Vec_Pj
DE_LT_PjAd_1P_AUC_BP <- DE_LT_PjAd_1P_AUC["Mag20_Freq80_In0"]
DE_LT_PjAd_2P_AUC_Pi_BP <-DE_LT_PjAd_2P_AUC["Mag20_Freq80_In0_Pi"]
DE_LT_PjAd_2P_AUC_Pj_BP <-DE_LT_PjAd_2P_AUC["Mag20_Freq80_In0_Pj"]
DE_LT_PjAd_2P_AUC_BP <- DE_LT_PjAd_2P_AUC_Pi_BP + DE_LT_PjAd_2P_AUC_Pj_BP

LT_PjAd_BP <- cbind(D_LT_PjAd_1P_AUC$Vec_Pi, D_LT_PjAd_2P_AUC$Vec_Pi, D_LT_PjAd_2P_AUC$Vec_Pj, D_LT_PjAd_2P_AUC_Add, DE_LT_PjAd_1P_AUC_BP, DE_LT_PjAd_2P_AUC_Pi_BP, DE_LT_PjAd_2P_AUC_Pj_BP, DE_LT_PjAd_2P_AUC_BP)
colnames(LT_PjAd_BP) <- c("DemStoch, 1 Parasite", "DemStoch, 2 Parasites (Pi)", "DemStoch, 2 Parasites (Pj)", "DemStoch, 2 Parasites (Additive)", "DemEnvStoch, 1 Parasite", "DemEnvStoch, 2 Parasites (Pi)", "DemEnvStoch, 2 Parasites (Pj)", "DemEnvStoch, 2 Parasites (Additive)")
boxplot(LT_PjAd_BP, outline = FALSE, las = 2,
        col = (c("goldenrod1","darkorange1", "darkorange1", "darkorange1", "plum1", "violetred", "violetred", "violetred")),
        main = "Low Temp., Pj_Advantage",
        sub = "When 2 Parasites: Pi Shown, Intro at 0, Mag. and Freq. of Disturbance = 20, 80",
        ylab = "Lifetime Parasite Load")

min(DE_HT_PjAd_Diffs_1m2Pj)
which(DE_HT_PjAd_Diffs_1m2Pj <= min(DE_HT_PjAd_Diffs_1m2Pj)) # Mag30_Freq80_In0
# D_HT_PjAd_1P_AUC
# D_HT_PjAd_2P_AUC
D_HT_PjAd_2P_AUC_Add <- D_HT_PjAd_2P_AUC$Vec_Pi + D_HT_PjAd_2P_AUC$Vec_Pj
DE_HT_PjAd_1P_AUC_BP <- DE_HT_PjAd_1P_AUC["Mag30_Freq80_In0"]
DE_HT_PjAd_2P_AUC_Pi_BP <-DE_HT_PjAd_2P_AUC["Mag30_Freq80_In0_Pi"]
DE_HT_PjAd_2P_AUC_Pj_BP <-DE_HT_PjAd_2P_AUC["Mag30_Freq80_In0_Pj"]
DE_HT_PjAd_2P_AUC_BP <- DE_HT_PjAd_2P_AUC_Pi_BP + DE_HT_PjAd_2P_AUC_Pj_BP

HT_PjAd_BP <- cbind(D_HT_PjAd_1P_AUC$Vec_Pi, D_HT_PjAd_2P_AUC$Vec_Pi, D_HT_PjAd_2P_AUC$Vec_Pj, D_HT_PjAd_2P_AUC_Add, DE_HT_PjAd_1P_AUC_BP, DE_HT_PjAd_2P_AUC_Pi_BP, DE_HT_PjAd_2P_AUC_Pj_BP, DE_HT_PjAd_2P_AUC_BP)
colnames(HT_PjAd_BP) <- c("DemStoch, 1 Parasite", "DemStoch, 2 Parasites (Pi)", "DemStoch, 2 Parasites (Pj)", "DemStoch, 2 Parasites (Additive)", "DemEnvStoch, 1 Parasite", "DemEnvStoch, 2 Parasites (Pi)", "DemEnvStoch, 2 Parasites (Pj)", "DemEnvStoch, 2 Parasites (Additive)")
boxplot(HT_PjAd_BP, outline = FALSE, las = 2,
        col = (c("goldenrod1","darkorange1", "darkorange1", "darkorange1", "plum1", "violetred", "violetred", "violetred")),
        main = "High Temp., Pj_Advantage",
        sub = "When 2 Parasites: Pi Shown, Intro at 0, Mag. and Freq. of Disturbance = 30, 80",
        ylab = "Lifetime Parasite Load")

#### ... Getting results for differences ####
## DemStoch; 1P
DemStoch_1P_Meds <- c(median(D_LT_Iden_1P_AUC$Vec_Pi), 
                      median(D_LT_Semi_1P_AUC$Vec_Pi), 
                      median(D_LT_PiAd_1P_AUC$Vec_Pi), 
                      median(D_LT_PjAd_1P_AUC$Vec_Pi), 
                      median(D_HT_Iden_1P_AUC$Vec_Pi), 
                      median(D_HT_Semi_1P_AUC$Vec_Pi), 
                      median(D_HT_PiAd_1P_AUC$Vec_Pi), 
                      median(D_HT_PjAd_1P_AUC$Vec_Pi))
min(DemStoch_1P_Meds); max(DemStoch_1P_Meds)
## DemStoch; 2P
DemStoch_2P_Meds <- c(median(D_LT_Iden_2P_AUC$Vec_Pi), 
                      median(D_LT_Semi_2P_AUC$Vec_Pi), 
                      median(D_LT_PiAd_2P_AUC$Vec_Pi), 
                      median(D_LT_PjAd_2P_AUC$Vec_Pi), 
                      median(D_HT_Iden_2P_AUC$Vec_Pi), 
                      median(D_HT_Semi_2P_AUC$Vec_Pi), 
                      median(D_HT_PiAd_2P_AUC$Vec_Pi), 
                      median(D_HT_PjAd_2P_AUC$Vec_Pi),
                      median(D_LT_Iden_2P_AUC$Vec_Pj), 
                      median(D_LT_Semi_2P_AUC$Vec_Pj), 
                      median(D_LT_PiAd_2P_AUC$Vec_Pj), 
                      median(D_LT_PjAd_2P_AUC$Vec_Pj), 
                      median(D_HT_Iden_2P_AUC$Vec_Pj), 
                      median(D_HT_Semi_2P_AUC$Vec_Pj), 
                      median(D_HT_PiAd_2P_AUC$Vec_Pj), 
                      median(D_HT_PjAd_2P_AUC$Vec_Pj))
min(DemStoch_2P_Meds[1:8]); min(DemStoch_2P_Meds[9:16])
max(DemStoch_2P_Meds[1:8]); max(DemStoch_2P_Meds[9:16])

## DemEnvStoch
DemEnvStoch_1P_LT_Meds <- data.frame(DE_LT_Iden_1P_AUC_Medians,
                                  DE_LT_Semi_1P_AUC_Medians,
                                  DE_LT_PiAd_1P_AUC_Medians,
                                  DE_LT_PjAd_1P_AUC_Medians)
DemEnvStoch_1P_HT_Meds <- data.frame(DE_HT_Iden_1P_AUC_Medians,
                                     DE_HT_Semi_1P_AUC_Medians,
                                     DE_HT_PiAd_1P_AUC_Medians,
                                     DE_HT_PjAd_1P_AUC_Medians)
DemEnvStoch_1P_Mins <- c(); DemEnvStoch_1P_Maxes <- c(); DemEnvStoch_1P_Meds <- c()
j = ncol(DemEnvStoch_1P_LT_Meds)
for (i in 1:ncol(DemEnvStoch_1P_LT_Meds)){
  DemEnvStoch_1P_Mins[i] <- min(DemEnvStoch_1P_LT_Meds[ ,i])
  DemEnvStoch_1P_Mins[j] <- min(DemEnvStoch_1P_HT_Meds[ ,i])
  DemEnvStoch_1P_Maxes[i] <- max(DemEnvStoch_1P_LT_Meds[ ,i])
  DemEnvStoch_1P_Maxes[j] <- max(DemEnvStoch_1P_HT_Meds[ ,i])
  j = j + 1
}; min(DemEnvStoch_1P_Mins); max(DemEnvStoch_1P_Maxes)

## First 3 thermal scenarios
DemEnvStoch_1P_LT_Meds <- data.frame(DE_LT_Iden_1P_AUC_Medians,
                                     DE_LT_Semi_1P_AUC_Medians,
                                     DE_LT_PiAd_1P_AUC_Medians)
DemEnvStoch_1P_HT_Meds <- data.frame(DE_HT_Iden_1P_AUC_Medians,
                                     DE_HT_Semi_1P_AUC_Medians,
                                     DE_HT_PiAd_1P_AUC_Medians)
DemEnvStoch_1P_Mins <- c(); DemEnvStoch_1P_Maxes <- c(); DemEnvStoch_1P_Meds <- c()
j = ncol(DemEnvStoch_1P_LT_Meds)
for (i in 1:ncol(DemEnvStoch_1P_LT_Meds)){
  DemEnvStoch_1P_Mins[i] <- min(DemEnvStoch_1P_LT_Meds[ ,i])
  DemEnvStoch_1P_Mins[j] <- min(DemEnvStoch_1P_HT_Meds[ ,i])
  DemEnvStoch_1P_Maxes[i] <- max(DemEnvStoch_1P_LT_Meds[ ,i])
  DemEnvStoch_1P_Maxes[j] <- max(DemEnvStoch_1P_HT_Meds[ ,i])
  j = j + 1
}; min(DemEnvStoch_1P_Mins); max(DemEnvStoch_1P_Maxes)

Pis <- seq(1, length(DE_LT_Iden_2P_AUC_Medians), 2); Pjs <- seq(2, length(DE_LT_Iden_2P_AUC_Medians), 2)
DemEnvStoch_2P_LT_Meds <- data.frame(as.numeric(DE_LT_Iden_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_LT_Semi_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_LT_PiAd_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_LT_PjAd_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_LT_Iden_2P_AUC_Medians[Pjs]),
                                     as.numeric(DE_LT_Semi_2P_AUC_Medians[Pjs]),
                                     as.numeric(DE_LT_PiAd_2P_AUC_Medians[Pjs]),
                                     as.numeric(DE_LT_PjAd_2P_AUC_Medians[Pjs]))
DemEnvStoch_2P_LT_Mins <- c(); DemEnvStoch_2P_LT_Maxes <- c()
for (i in 1:ncol(DemEnvStoch_2P_LT_Meds)){
  DemEnvStoch_2P_LT_Mins[i] <- min(DemEnvStoch_2P_LT_Meds[ ,i])
  DemEnvStoch_2P_LT_Maxes[i] <- max(DemEnvStoch_2P_LT_Meds[ ,i])
}; min(DemEnvStoch_2P_LT_Mins); max(DemEnvStoch_2P_LT_Maxes)

Pis <- seq(1, length(DE_HT_Iden_2P_AUC_Medians), 2); Pjs <- seq(2, length(DE_HT_Iden_2P_AUC_Medians), 2)
DemEnvStoch_2P_HT_Meds <- data.frame(as.numeric(DE_HT_Iden_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_HT_Semi_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_HT_PiAd_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_HT_PjAd_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_HT_Iden_2P_AUC_Medians[Pjs]),
                                     as.numeric(DE_HT_Semi_2P_AUC_Medians[Pjs]),
                                     as.numeric(DE_HT_PiAd_2P_AUC_Medians[Pjs]),
                                     as.numeric(DE_HT_PjAd_2P_AUC_Medians[Pjs]))
DemEnvStoch_2P_HT_Mins <- c(); DemEnvStoch_2P_HT_Maxes <- c()
for (i in 1:ncol(DemEnvStoch_2P_HT_Meds)){
  DemEnvStoch_2P_HT_Mins[i] <- min(DemEnvStoch_2P_HT_Meds[ ,i])
  DemEnvStoch_2P_HT_Maxes[i] <- max(DemEnvStoch_2P_HT_Meds[ ,i])
}; min(DemEnvStoch_2P_HT_Mins); max(DemEnvStoch_2P_HT_Maxes)

## First 3 thermal scenarios
Pis <- seq(1, length(DE_LT_Iden_2P_AUC_Medians), 2); Pjs <- seq(2, length(DE_LT_Iden_2P_AUC_Medians), 2)
DemEnvStoch_2P_LT_Meds <- data.frame(as.numeric(DE_LT_Iden_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_LT_Semi_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_LT_PiAd_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_LT_Iden_2P_AUC_Medians[Pjs]),
                                     as.numeric(DE_LT_Semi_2P_AUC_Medians[Pjs]),
                                     as.numeric(DE_LT_PiAd_2P_AUC_Medians[Pjs]))
DemEnvStoch_2P_LT_Mins <- c(); DemEnvStoch_2P_LT_Maxes <- c()
for (i in 1:ncol(DemEnvStoch_2P_LT_Meds)){
  DemEnvStoch_2P_LT_Mins[i] <- min(DemEnvStoch_2P_LT_Meds[ ,i])
  DemEnvStoch_2P_LT_Maxes[i] <- max(DemEnvStoch_2P_LT_Meds[ ,i])
}
0
Pis <- seq(1, length(DE_HT_Iden_2P_AUC_Medians), 2); Pjs <- seq(2, length(DE_HT_Iden_2P_AUC_Medians), 2)
DemEnvStoch_2P_HT_Meds <- data.frame(as.numeric(DE_HT_Iden_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_HT_Semi_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_HT_PiAd_2P_AUC_Medians[Pis]),
                                     as.numeric(DE_HT_Iden_2P_AUC_Medians[Pjs]),
                                     as.numeric(DE_HT_Semi_2P_AUC_Medians[Pjs]),
                                     as.numeric(DE_HT_PiAd_2P_AUC_Medians[Pjs]))
DemEnvStoch_2P_HT_Mins <- c(); DemEnvStoch_2P_HT_Maxes <- c()
for (i in 1:ncol(DemEnvStoch_2P_HT_Meds)){
  DemEnvStoch_2P_HT_Mins[i] <- min(DemEnvStoch_2P_HT_Meds[ ,i])
  DemEnvStoch_2P_HT_Maxes[i] <- max(DemEnvStoch_2P_HT_Meds[ ,i])
}; min(DemEnvStoch_2P_HT_Mins); max(DemEnvStoch_2P_HT_Maxes)

## Differences
DE_LT_PjAd_2PGreater1P <- as.data.frame(DE_LT_PjAd_Diffs_1m2Pj[DE_LT_PjAd_Diffs_1m2Pj_Less0]); colnames(DE_LT_PjAd_2PGreater1P) <- c("DE_LT_PjAd_2PGreater1P")
DE_HT_PjAd_2PGreater1P <- as.data.frame(DE_HT_PjAd_Diffs_1m2Pj[DE_HT_PjAd_Diffs_1m2Pj_Less0]); colnames(DE_HT_PjAd_2PGreater1P) <- c("DE_HT_PjAd_2PGreater1P")
nrow(DE_LT_PjAd_2PGreater1P); min(DE_LT_PjAd_2PGreater1P) # 62, -340118.7 when Mag = 20/Freq = 80
nrow(DE_HT_PjAd_2PGreater1P); min(DE_HT_PjAd_2PGreater1P) # 60, -255191.1 when Mag = 30/Freq = 80

#### Collected summary objects ####
DE_LT_Iden_Diffs_1m2Pi <- as.data.frame(DE_LT_Iden_Diffs_1m2Pi)
DE_LT_Semi_Diffs_1m2Pi <- as.data.frame(DE_LT_Semi_Diffs_1m2Pi)
DE_LT_PiAd_Diffs_1m2Pi <- as.data.frame(DE_LT_PiAd_Diffs_1m2Pi)
DE_LT_PjAd_Diffs_1m2Pi <- as.data.frame(DE_LT_PjAd_Diffs_1m2Pi)
DE_HT_Iden_Diffs_1m2Pi <- as.data.frame(DE_HT_Iden_Diffs_1m2Pi)
DE_HT_Semi_Diffs_1m2Pi <- as.data.frame(DE_HT_Semi_Diffs_1m2Pi)
DE_HT_PiAd_Diffs_1m2Pi <- as.data.frame(DE_HT_PiAd_Diffs_1m2Pi)
DE_HT_PjAd_Diffs_1m2Pi <- as.data.frame(DE_HT_PjAd_Diffs_1m2Pi)
DE_LT_Iden_Diffs_1m2Pj <- as.data.frame(DE_LT_Iden_Diffs_1m2Pj)
DE_LT_Semi_Diffs_1m2Pj <- as.data.frame(DE_LT_Semi_Diffs_1m2Pj)
DE_LT_PiAd_Diffs_1m2Pj <- as.data.frame(DE_LT_PiAd_Diffs_1m2Pj)
DE_LT_PjAd_Diffs_1m2Pj <- as.data.frame(DE_LT_PjAd_Diffs_1m2Pj)
DE_HT_Iden_Diffs_1m2Pj <- as.data.frame(DE_HT_Iden_Diffs_1m2Pj)
DE_HT_Semi_Diffs_1m2Pj <- as.data.frame(DE_HT_Semi_Diffs_1m2Pj)
DE_HT_PiAd_Diffs_1m2Pj <- as.data.frame(DE_HT_PiAd_Diffs_1m2Pj)
DE_HT_PjAd_Diffs_1m2Pj <- as.data.frame(DE_HT_PjAd_Diffs_1m2Pj)

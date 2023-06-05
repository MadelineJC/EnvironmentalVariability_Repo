#### DOC STRING ===============================================================
# This script (1) counts the number of simulations, per a given permutation, 
# that resulted in non-zero periods of co-occurrence and (2) calculates the 
# percentage of simulations that resulted in longer periods of co-occurrence 
# than did the equivalent deterministic case.

# Outputs are collected in the "Collected summary objects" section.
#### END DOC STRING ===========================================================

Eaches <- c("Each_1", "Each_2", "Each_5", "Each_10", "Each_20", "Each_30", "Each_40", "Each_50", "Each_60", "Each_70", "Each_80", "Each_90", "Each_100")
IntroPt <- seq(0, 49, 1)
Names <- c()
for (i in 1:length(IntroPt)){
  Names[i] <- paste0("Intro_", IntroPt[i])
}

#### Number of simulations (/1000) resulting in non-zero periods of co-occurrence ####
#### ... Magnitude of disturbance varies (LATE) ####
E_LT_Iden_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_LT_Iden_PV.rds")
E_LT_Semi_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_LT_Semi_PV.rds")
E_LT_PiAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_LT_PiAd_PV.rds")
E_LT_PjAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_LT_PjAd_PV.rds")
E_LT_PjAd_vB_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_LT_PjAd_vB_PV.rds")
DE_LT_Iden_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_LT_Iden_PV.rds")
DE_LT_Semi_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_LT_Semi_PV.rds")
DE_LT_PiAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_LT_PiAd_PV.rds")
DE_LT_PjAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_LT_PjAd_PV.rds")
DE_LT_PjAd_vB_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_LT_PjAd_vB_PV.rds")
#### ... ... Environmentally stochastic ####
#### ... ... ... Identical TPCs ####
names(E_LT_Iden_PV) <- Eaches
# Low mag.
TempDF <- data.frame(E_LT_Iden_PV[["Each_10"]][["Intro_0"]][ ,1])
for (i in 2:length(E_LT_Iden_PV[["Each_10"]])){
  temp <- E_LT_Iden_PV[["Each_10"]][[i]][ ,1]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_Iden_Non0_LowMag <- c() # Gives number of simulations out of 1000 that resulted in non-zero periods of co-occurrence
for (i in 1:ncol(TempDF)){
  E_LT_Iden_Non0_LowMag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 1
TempDF <- data.frame(E_LT_Iden_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(E_LT_Iden_PV[["Each_10"]])){
  temp <- E_LT_Iden_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_Iden_Non0_Inter1Mag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_Iden_Non0_Inter1Mag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 2
TempDF <- data.frame(E_LT_Iden_PV[["Each_10"]][["Intro_0"]][ ,10])
for (i in 2:length(E_LT_Iden_PV[["Each_10"]])){
  temp <- E_LT_Iden_PV[["Each_10"]][[i]][ ,10]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_Iden_Non0_Inter2Mag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_Iden_Non0_Inter2Mag[i] <- sum(TempDF[ ,i] != 0)
}
# High mag. 
TempDF <- data.frame(E_LT_Iden_PV[["Each_10"]][["Intro_0"]][ ,15])
for (i in 2:length(E_LT_Iden_PV[["Each_10"]])){
  temp <- E_LT_Iden_PV[["Each_10"]][[i]][ ,15]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_Iden_Non0_HighMag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_Iden_Non0_HighMag[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Semi-Identical TPCs ####
names(E_LT_Semi_PV) <- Eaches
# Low mag.
TempDF <- data.frame(E_LT_Semi_PV[["Each_10"]][["Intro_0"]][ ,1])
for (i in 2:length(E_LT_Semi_PV[["Each_10"]])){
  temp <- E_LT_Semi_PV[["Each_10"]][[i]][ ,1]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_Semi_Non0_LowMag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_Semi_Non0_LowMag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 1
TempDF <- data.frame(E_LT_Semi_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(E_LT_Semi_PV[["Each_10"]])){
  temp <- E_LT_Semi_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_Semi_Non0_Inter1Mag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_Semi_Non0_Inter1Mag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 2
TempDF <- data.frame(E_LT_Semi_PV[["Each_10"]][["Intro_0"]][ ,10])
for (i in 2:length(E_LT_Semi_PV[["Each_10"]])){
  temp <- E_LT_Semi_PV[["Each_10"]][[i]][ ,10]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_Semi_Non0_Inter2Mag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_Semi_Non0_Inter2Mag[i] <- sum(TempDF[ ,i] != 0)
}
# High mag.
TempDF <- data.frame(E_LT_Semi_PV[["Each_10"]][["Intro_0"]][ ,15])
for (i in 2:length(E_LT_Semi_PV[["Each_10"]])){
  temp <- E_LT_Semi_PV[["Each_10"]][[i]][ ,15]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_Semi_Non0_HighMag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_Semi_Non0_HighMag[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pi-Advantage TPCs ####
names(E_LT_PiAd_PV) <- Eaches
# Low mag.
TempDF <- data.frame(E_LT_PiAd_PV[["Each_10"]][["Intro_0"]][ ,1])
for (i in 2:length(E_LT_PiAd_PV[["Each_10"]])){
  temp <- E_LT_PiAd_PV[["Each_10"]][[i]][ ,1]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PiAd_Non0_LowMag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PiAd_Non0_LowMag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 1
TempDF <- data.frame(E_LT_PiAd_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(E_LT_PiAd_PV[["Each_10"]])){
  temp <- E_LT_PiAd_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PiAd_Non0_Inter1Mag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PiAd_Non0_Inter1Mag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 2
TempDF <- data.frame(E_LT_PiAd_PV[["Each_10"]][["Intro_0"]][ ,10])
for (i in 2:length(E_LT_PiAd_PV[["Each_10"]])){
  temp <- E_LT_PiAd_PV[["Each_10"]][[i]][ ,10]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PiAd_Non0_Inter2Mag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PiAd_Non0_Inter2Mag[i] <- sum(TempDF[ ,i] != 0)
}
# High mag.
TempDF <- data.frame(E_LT_PiAd_PV[["Each_10"]][["Intro_0"]][ ,15])
for (i in 2:length(E_LT_PiAd_PV[["Each_10"]])){
  temp <- E_LT_PiAd_PV[["Each_10"]][[i]][ ,15]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PiAd_Non0_HighMag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PiAd_Non0_HighMag[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pj-Advantage, Version A TPCs ####
names(E_LT_PjAd_PV) <- Eaches
# Low mag.
TempDF <- data.frame(E_LT_PjAd_PV[["Each_10"]][["Intro_0"]][ ,1])
for (i in 2:length(E_LT_PjAd_PV[["Each_10"]])){
  temp <- E_LT_PjAd_PV[["Each_10"]][[i]][ ,1]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PjAd_Non0_LowMag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PjAd_Non0_LowMag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 1
TempDF <- data.frame(E_LT_PjAd_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(E_LT_PjAd_PV[["Each_10"]])){
  temp <- E_LT_PjAd_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PjAd_Non0_Inter1Mag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PjAd_Non0_Inter1Mag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 2
TempDF <- data.frame(E_LT_PjAd_PV[["Each_10"]][["Intro_0"]][ ,10])
for (i in 2:length(E_LT_PjAd_PV[["Each_10"]])){
  temp <- E_LT_PjAd_PV[["Each_10"]][[i]][ ,10]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PjAd_Non0_Inter2Mag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PjAd_Non0_Inter2Mag[i] <- sum(TempDF[ ,i] != 0)
}
# High mag.
TempDF <- data.frame(E_LT_PjAd_PV[["Each_10"]][["Intro_0"]][ ,15])
for (i in 2:length(E_LT_PjAd_PV[["Each_10"]])){
  temp <- E_LT_PjAd_PV[["Each_10"]][[i]][ ,15]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PjAd_Non0_HighMag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PjAd_Non0_HighMag[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pj-Advantage, Version B TPCs ####
names(E_LT_PjAd_vB_PV) <- Eaches
# Low mag.
TempDF <- data.frame(E_LT_PjAd_vB_PV[["Each_10"]][["Intro_0"]][ ,1])
for (i in 2:length(E_LT_PjAd_vB_PV[["Each_10"]])){
  temp <- E_LT_PjAd_vB_PV[["Each_10"]][[i]][ ,1]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PjAd_vB_Non0_LowMag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PjAd_vB_Non0_LowMag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 1
TempDF <- data.frame(E_LT_PjAd_vB_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(E_LT_PjAd_vB_PV[["Each_10"]])){
  temp <- E_LT_PjAd_vB_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PjAd_vB_Non0_Inter1Mag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PjAd_vB_Non0_Inter1Mag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 2
TempDF <- data.frame(E_LT_PjAd_vB_PV[["Each_10"]][["Intro_0"]][ ,10])
for (i in 2:length(E_LT_PjAd_vB_PV[["Each_10"]])){
  temp <- E_LT_PjAd_vB_PV[["Each_10"]][[i]][ ,10]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PjAd_vB_Non0_Inter2Mag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PjAd_vB_Non0_Inter2Mag[i] <- sum(TempDF[ ,i] != 0)
}
# High mag.
TempDF <- data.frame(E_LT_PjAd_vB_PV[["Each_10"]][["Intro_0"]][ ,15])
for (i in 2:length(E_LT_PjAd_vB_PV[["Each_10"]])){
  temp <- E_LT_PjAd_vB_PV[["Each_10"]][[i]][ ,15]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_LT_PjAd_vB_Non0_HighMag <- c()
for (i in 1:ncol(TempDF)){
  E_LT_PjAd_vB_Non0_HighMag[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... Demographically and environmentally stochastic ####
#### ... ... ... Identical TPCs ####
names(DE_LT_Iden_PV) <- Eaches
# Low mag.
TempDF <- data.frame(DE_LT_Iden_PV[["Each_10"]][["Intro_0"]][ ,1])
for (i in 2:length(DE_LT_Iden_PV[["Each_10"]])){
  temp <- DE_LT_Iden_PV[["Each_10"]][[i]][ ,1]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_Iden_Non0_LowMag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_Iden_Non0_LowMag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 1
TempDF <- data.frame(DE_LT_Iden_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_LT_Iden_PV[["Each_10"]])){
  temp <- DE_LT_Iden_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_Iden_Non0_Inter1Mag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_Iden_Non0_Inter1Mag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 2
TempDF <- data.frame(DE_LT_Iden_PV[["Each_10"]][["Intro_0"]][ ,10])
for (i in 2:length(DE_LT_Iden_PV[["Each_10"]])){
  temp <- DE_LT_Iden_PV[["Each_10"]][[i]][ ,10]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_Iden_Non0_Inter2Mag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_Iden_Non0_Inter2Mag[i] <- sum(TempDF[ ,i] != 0)
}
# High mag. 
TempDF <- data.frame(DE_LT_Iden_PV[["Each_10"]][["Intro_0"]][ ,15])
for (i in 2:length(DE_LT_Iden_PV[["Each_10"]])){
  temp <- DE_LT_Iden_PV[["Each_10"]][[i]][ ,15]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_Iden_Non0_HighMag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_Iden_Non0_HighMag[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Semi-Identical TPCs ####
names(DE_LT_Semi_PV) <- Eaches
# Low mag.
TempDF <- data.frame(DE_LT_Semi_PV[["Each_10"]][["Intro_0"]][ ,1])
for (i in 2:length(DE_LT_Semi_PV[["Each_10"]])){
  temp <- DE_LT_Semi_PV[["Each_10"]][[i]][ ,1]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_Semi_Non0_LowMag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_Semi_Non0_LowMag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 1
TempDF <- data.frame(DE_LT_Semi_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_LT_Semi_PV[["Each_10"]])){
  temp <- DE_LT_Semi_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_Semi_Non0_Inter1Mag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_Semi_Non0_Inter1Mag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 2
TempDF <- data.frame(DE_LT_Semi_PV[["Each_10"]][["Intro_0"]][ ,10])
for (i in 2:length(DE_LT_Semi_PV[["Each_10"]])){
  temp <- DE_LT_Semi_PV[["Each_10"]][[i]][ ,10]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_Semi_Non0_Inter2Mag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_Semi_Non0_Inter2Mag[i] <- sum(TempDF[ ,i] != 0)
}
# High mag.
TempDF <- data.frame(DE_LT_Semi_PV[["Each_10"]][["Intro_0"]][ ,15])
for (i in 2:length(DE_LT_Semi_PV[["Each_10"]])){
  temp <- DE_LT_Semi_PV[["Each_10"]][[i]][ ,15]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_Semi_Non0_HighMag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_Semi_Non0_HighMag[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pi-Advantage TPCs ####
names(DE_LT_PiAd_PV) <- Eaches
# Low mag.
TempDF <- data.frame(DE_LT_PiAd_PV[["Each_10"]][["Intro_0"]][ ,1])
for (i in 2:length(DE_LT_PiAd_PV[["Each_10"]])){
  temp <- DE_LT_PiAd_PV[["Each_10"]][[i]][ ,1]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PiAd_Non0_LowMag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PiAd_Non0_LowMag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 1
TempDF <- data.frame(DE_LT_PiAd_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_LT_PiAd_PV[["Each_10"]])){
  temp <- DE_LT_PiAd_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PiAd_Non0_Inter1Mag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PiAd_Non0_Inter1Mag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 2
TempDF <- data.frame(DE_LT_PiAd_PV[["Each_10"]][["Intro_0"]][ ,10])
for (i in 2:length(DE_LT_PiAd_PV[["Each_10"]])){
  temp <- DE_LT_PiAd_PV[["Each_10"]][[i]][ ,10]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PiAd_Non0_Inter2Mag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PiAd_Non0_Inter2Mag[i] <- sum(TempDF[ ,i] != 0)
}
# High mag.
TempDF <- data.frame(DE_LT_PiAd_PV[["Each_10"]][["Intro_0"]][ ,15])
for (i in 2:length(DE_LT_PiAd_PV[["Each_10"]])){
  temp <- DE_LT_PiAd_PV[["Each_10"]][[i]][ ,15]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PiAd_Non0_HighMag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PiAd_Non0_HighMag[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pj-Advantage, Version A TPCs ####
names(DE_LT_PjAd_PV) <- Eaches
# Low mag.
TempDF <- data.frame(DE_LT_PjAd_PV[["Each_10"]][["Intro_0"]][ ,1])
for (i in 2:length(DE_LT_PjAd_PV[["Each_10"]])){
  temp <- DE_LT_PjAd_PV[["Each_10"]][[i]][ ,1]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PjAd_Non0_LowMag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PjAd_Non0_LowMag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 1
TempDF <- data.frame(DE_LT_PjAd_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_LT_PjAd_PV[["Each_10"]])){
  temp <- DE_LT_PjAd_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PjAd_Non0_Inter1Mag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PjAd_Non0_Inter1Mag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 2
TempDF <- data.frame(DE_LT_PjAd_PV[["Each_10"]][["Intro_0"]][ ,10])
for (i in 2:length(DE_LT_PjAd_PV[["Each_10"]])){
  temp <- DE_LT_PjAd_PV[["Each_10"]][[i]][ ,10]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PjAd_Non0_Inter2Mag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PjAd_Non0_Inter2Mag[i] <- sum(TempDF[ ,i] != 0)
}
# High mag.
TempDF <- data.frame(DE_LT_PjAd_PV[["Each_10"]][["Intro_0"]][ ,15])
for (i in 2:length(DE_LT_PjAd_PV[["Each_10"]])){
  temp <- DE_LT_PjAd_PV[["Each_10"]][[i]][ ,15]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PjAd_Non0_HighMag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PjAd_Non0_HighMag[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pj-Advantage, Version B TPCs ####
names(DE_LT_PjAd_vB_PV) <- Eaches
# Low mag.
TempDF <- data.frame(DE_LT_PjAd_vB_PV[["Each_10"]][["Intro_0"]][ ,1])
for (i in 2:length(DE_LT_PjAd_vB_PV[["Each_10"]])){
  temp <- DE_LT_PjAd_vB_PV[["Each_10"]][[i]][ ,1]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PjAd_vB_Non0_LowMag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PjAd_vB_Non0_LowMag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 1
TempDF <- data.frame(DE_LT_PjAd_vB_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_LT_PjAd_vB_PV[["Each_10"]])){
  temp <- DE_LT_PjAd_vB_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PjAd_vB_Non0_Inter1Mag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PjAd_vB_Non0_Inter1Mag[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate mag. 2
TempDF <- data.frame(DE_LT_PjAd_vB_PV[["Each_10"]][["Intro_0"]][ ,10])
for (i in 2:length(DE_LT_PjAd_vB_PV[["Each_10"]])){
  temp <- DE_LT_PjAd_vB_PV[["Each_10"]][[i]][ ,10]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PjAd_vB_Non0_Inter2Mag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PjAd_vB_Non0_Inter2Mag[i] <- sum(TempDF[ ,i] != 0)
}
# High mag.
TempDF <- data.frame(DE_LT_PjAd_vB_PV[["Each_10"]][["Intro_0"]][ ,15])
for (i in 2:length(DE_LT_PjAd_vB_PV[["Each_10"]])){
  temp <- DE_LT_PjAd_vB_PV[["Each_10"]][[i]][ ,15]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_LT_PjAd_vB_Non0_HighMag <- c()
for (i in 1:ncol(TempDF)){
  DE_LT_PjAd_vB_Non0_HighMag[i] <- sum(TempDF[ ,i] != 0)
}

#### ... ... Summarasing ####
LT_Iden_Non0 <- data.frame(Det_LT_Iden_PV,
                           E_LT_Iden_Non0_LowMag,
                           E_LT_Iden_Non0_Inter1Mag,
                           E_LT_Iden_Non0_Inter2Mag,
                           E_LT_Iden_Non0_HighMag,
                           DE_LT_Iden_Non0_LowMag,
                           DE_LT_Iden_Non0_Inter1Mag,
                           DE_LT_Iden_Non0_Inter2Mag,
                           DE_LT_Iden_Non0_HighMag)
LT_Semi_Non0 <- data.frame(Det_LT_Semi_PV,
                           E_LT_Semi_Non0_LowMag,
                           E_LT_Semi_Non0_Inter1Mag,
                           E_LT_Semi_Non0_Inter2Mag,
                           E_LT_Semi_Non0_HighMag,
                           DE_LT_Semi_Non0_LowMag,
                           DE_LT_Semi_Non0_Inter1Mag,
                           DE_LT_Semi_Non0_Inter2Mag,
                           DE_LT_Semi_Non0_HighMag)
LT_PiAd_Non0 <- data.frame(Det_LT_PiAd_PV,
                           E_LT_PiAd_Non0_LowMag,
                           E_LT_PiAd_Non0_Inter1Mag,
                           E_LT_PiAd_Non0_Inter2Mag,
                           E_LT_PiAd_Non0_HighMag,
                           DE_LT_PiAd_Non0_LowMag,
                           DE_LT_PiAd_Non0_Inter1Mag,
                           DE_LT_PiAd_Non0_Inter2Mag,
                           DE_LT_PiAd_Non0_HighMag)
LT_PjAd_Non0 <- data.frame(Det_LT_PjAd_PV,
                           E_LT_PjAd_Non0_LowMag,
                           E_LT_PjAd_Non0_Inter1Mag,
                           E_LT_PjAd_Non0_Inter2Mag,
                           E_LT_PjAd_Non0_HighMag,
                           DE_LT_PjAd_Non0_LowMag,
                           DE_LT_PjAd_Non0_Inter1Mag,
                           DE_LT_PjAd_Non0_Inter2Mag,
                           DE_LT_PjAd_Non0_HighMag)
LT_PjAd_vB_Non0 <- data.frame(Det_LT_PjAd_vB_PV,
                              E_LT_PjAd_vB_Non0_LowMag,
                              E_LT_PjAd_vB_Non0_Inter1Mag,
                              E_LT_PjAd_vB_Non0_Inter2Mag,
                              E_LT_PjAd_vB_Non0_HighMag,
                              DE_LT_PjAd_vB_Non0_LowMag,
                              DE_LT_PjAd_vB_Non0_Inter1Mag,
                              DE_LT_PjAd_vB_Non0_Inter2Mag,
                              DE_LT_PjAd_vB_Non0_HighMag)

#### ... Frequency of disturbance varies (HATE) ####
E_HT_Iden_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_HT_Iden_PV.rds")
E_HT_Semi_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_HT_Semi_PV.rds")
E_HT_PiAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_HT_PiAd_PV.rds")
E_HT_PjAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_HT_PjAd_PV.rds")
E_HT_PjAd_vB_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/E_HT_PjAd_vB_PV.rds")
DE_HT_Iden_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_HT_Iden_PV.rds")
DE_HT_Semi_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_HT_Semi_PV.rds")
DE_HT_PiAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_HT_PiAd_PV.rds")
DE_HT_PjAd_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_HT_PjAd_PV.rds")
DE_HT_PjAd_vB_PV <- readRDS("Data/EnvStoch-DemEnvStoch_PV_Outputs/DE_HT_PjAd_vB_PV.rds")

#### ... ... Environmentally stochastic ####
#### ... ... ... Identical TPCs ####
names(E_HT_Iden_PV) <- Eaches
# Low freq.
TempDF <- data.frame(E_HT_Iden_PV[["Each_100"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_Iden_PV[["Each_100"]])){
  temp <- E_HT_Iden_PV[["Each_100"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_Iden_Non0_LowFreq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_Iden_Non0_LowFreq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 1
TempDF <- data.frame(E_HT_Iden_PV[["Each_50"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_Iden_PV[["Each_50"]])){
  temp <- E_HT_Iden_PV[["Each_50"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_Iden_Non0_Inter1Freq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_Iden_Non0_Inter1Freq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 2
TempDF <- data.frame(E_HT_Iden_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_Iden_PV[["Each_10"]])){
  temp <- E_HT_Iden_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_Iden_Non0_Inter2Freq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_Iden_Non0_Inter2Freq[i] <- sum(TempDF[ ,i] != 0)
}
# High freq
TempDF <- data.frame(E_HT_Iden_PV[["Each_1"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_Iden_PV[["Each_1"]])){
  temp <- E_HT_Iden_PV[["Each_1"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_Iden_Non0_HighFreq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_Iden_Non0_HighFreq[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Semi-Identical TPCs ####
names(E_HT_Semi_PV) <- Eaches
# Low freq.
TempDF <- data.frame(E_HT_Semi_PV[["Each_100"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_Semi_PV[["Each_100"]])){
  temp <- E_HT_Semi_PV[["Each_100"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_Semi_Non0_LowFreq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_Semi_Non0_LowFreq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 1
TempDF <- data.frame(E_HT_Semi_PV[["Each_50"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_Semi_PV[["Each_50"]])){
  temp <- E_HT_Semi_PV[["Each_50"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_Semi_Non0_Inter1Freq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_Semi_Non0_Inter1Freq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 2
TempDF <- data.frame(E_HT_Semi_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_Semi_PV[["Each_10"]])){
  temp <- E_HT_Semi_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_Semi_Non0_Inter2Freq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_Semi_Non0_Inter2Freq[i] <- sum(TempDF[ ,i] != 0)
}
# High freq
TempDF <- data.frame(E_HT_Semi_PV[["Each_1"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_Semi_PV[["Each_1"]])){
  temp <- E_HT_Semi_PV[["Each_1"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_Semi_Non0_HighFreq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_Semi_Non0_HighFreq[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pi-Advantage TPCs ####
names(E_HT_PiAd_PV) <- Eaches
# Low freq.
TempDF <- data.frame(E_HT_PiAd_PV[["Each_100"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PiAd_PV[["Each_100"]])){
  temp <- E_HT_PiAd_PV[["Each_100"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PiAd_Non0_LowFreq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PiAd_Non0_LowFreq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 1
TempDF <- data.frame(E_HT_PiAd_PV[["Each_50"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PiAd_PV[["Each_50"]])){
  temp <- E_HT_PiAd_PV[["Each_50"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PiAd_Non0_Inter1Freq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PiAd_Non0_Inter1Freq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 2
TempDF <- data.frame(E_HT_PiAd_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PiAd_PV[["Each_10"]])){
  temp <- E_HT_PiAd_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PiAd_Non0_Inter2Freq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PiAd_Non0_Inter2Freq[i] <- sum(TempDF[ ,i] != 0)
}
# High freq
TempDF <- data.frame(E_HT_PiAd_PV[["Each_1"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PiAd_PV[["Each_1"]])){
  temp <- E_HT_PiAd_PV[["Each_1"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PiAd_Non0_HighFreq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PiAd_Non0_HighFreq[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pj-Advantage, Version A TPCs ####
names(E_HT_PjAd_PV) <- Eaches
# Low freq.
TempDF <- data.frame(E_HT_PjAd_PV[["Each_100"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PjAd_PV[["Each_100"]])){
  temp <- E_HT_PjAd_PV[["Each_100"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PjAd_Non0_LowFreq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PjAd_Non0_LowFreq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 1
TempDF <- data.frame(E_HT_PjAd_PV[["Each_50"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PjAd_PV[["Each_50"]])){
  temp <- E_HT_PjAd_PV[["Each_50"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PjAd_Non0_Inter1Freq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PjAd_Non0_Inter1Freq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 2
TempDF <- data.frame(E_HT_PjAd_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PjAd_PV[["Each_10"]])){
  temp <- E_HT_PjAd_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PjAd_Non0_Inter2Freq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PjAd_Non0_Inter2Freq[i] <- sum(TempDF[ ,i] != 0)
}
# High freq
TempDF <- data.frame(E_HT_PjAd_PV[["Each_1"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PjAd_PV[["Each_1"]])){
  temp <- E_HT_PjAd_PV[["Each_1"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PjAd_Non0_HighFreq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PjAd_Non0_HighFreq[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pj-Advantage, Version B TPCs ####
names(E_HT_PjAd_vB_PV) <- Eaches
# Low freq.
TempDF <- data.frame(E_HT_PjAd_vB_PV[["Each_100"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PjAd_vB_PV[["Each_100"]])){
  temp <- E_HT_PjAd_vB_PV[["Each_100"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PjAd_vB_Non0_LowFreq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PjAd_vB_Non0_LowFreq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 1
TempDF <- data.frame(E_HT_PjAd_vB_PV[["Each_50"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PjAd_vB_PV[["Each_50"]])){
  temp <- E_HT_PjAd_vB_PV[["Each_50"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PjAd_vB_Non0_Inter1Freq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PjAd_vB_Non0_Inter1Freq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 2
TempDF <- data.frame(E_HT_PjAd_vB_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PjAd_vB_PV[["Each_10"]])){
  temp <- E_HT_PjAd_vB_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PjAd_vB_Non0_Inter2Freq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PjAd_vB_Non0_Inter2Freq[i] <- sum(TempDF[ ,i] != 0)
}
# High freq
TempDF <- data.frame(E_HT_PjAd_vB_PV[["Each_1"]][["Intro_0"]][ ,5])
for (i in 2:length(E_HT_PjAd_vB_PV[["Each_1"]])){
  temp <- E_HT_PjAd_vB_PV[["Each_1"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
E_HT_PjAd_vB_Non0_HighFreq <- c()
for (i in 1:ncol(TempDF)){
  E_HT_PjAd_vB_Non0_HighFreq[i] <- sum(TempDF[ ,i] != 0)
}

#### ... ... Demographically and environmentally stochastic ####
#### ... ... ... Identical TPCs ####
names(DE_HT_Iden_PV) <- Eaches
# Low freq.
TempDF <- data.frame(DE_HT_Iden_PV[["Each_100"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_Iden_PV[["Each_100"]])){
  temp <- DE_HT_Iden_PV[["Each_100"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_Iden_Non0_LowFreq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_Iden_Non0_LowFreq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 1
TempDF <- data.frame(DE_HT_Iden_PV[["Each_50"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_Iden_PV[["Each_50"]])){
  temp <- DE_HT_Iden_PV[["Each_50"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_Iden_Non0_Inter1Freq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_Iden_Non0_Inter1Freq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 2
TempDF <- data.frame(DE_HT_Iden_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_Iden_PV[["Each_10"]])){
  temp <- DE_HT_Iden_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_Iden_Non0_Inter2Freq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_Iden_Non0_Inter2Freq[i] <- sum(TempDF[ ,i] != 0)
}
# High freq
TempDF <- data.frame(DE_HT_Iden_PV[["Each_1"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_Iden_PV[["Each_1"]])){
  temp <- DE_HT_Iden_PV[["Each_1"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_Iden_Non0_HighFreq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_Iden_Non0_HighFreq[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Semi-Identical TPCs ####
names(DE_HT_Semi_PV) <- Eaches
# Low freq.
TempDF <- data.frame(DE_HT_Semi_PV[["Each_100"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_Semi_PV[["Each_100"]])){
  temp <- DE_HT_Semi_PV[["Each_100"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_Semi_Non0_LowFreq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_Semi_Non0_LowFreq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 1
TempDF <- data.frame(DE_HT_Semi_PV[["Each_50"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_Semi_PV[["Each_50"]])){
  temp <- DE_HT_Semi_PV[["Each_50"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_Semi_Non0_Inter1Freq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_Semi_Non0_Inter1Freq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 2
TempDF <- data.frame(DE_HT_Semi_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_Semi_PV[["Each_10"]])){
  temp <- DE_HT_Semi_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_Semi_Non0_Inter2Freq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_Semi_Non0_Inter2Freq[i] <- sum(TempDF[ ,i] != 0)
}
# High freq
TempDF <- data.frame(DE_HT_Semi_PV[["Each_1"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_Semi_PV[["Each_1"]])){
  temp <- DE_HT_Semi_PV[["Each_1"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_Semi_Non0_HighFreq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_Semi_Non0_HighFreq[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pi-Advantage TPCs ####
names(DE_HT_PiAd_PV) <- Eaches
# Low freq.
TempDF <- data.frame(DE_HT_PiAd_PV[["Each_100"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PiAd_PV[["Each_100"]])){
  temp <- DE_HT_PiAd_PV[["Each_100"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PiAd_Non0_LowFreq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PiAd_Non0_LowFreq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 1
TempDF <- data.frame(DE_HT_PiAd_PV[["Each_50"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PiAd_PV[["Each_50"]])){
  temp <- DE_HT_PiAd_PV[["Each_50"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PiAd_Non0_Inter1Freq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PiAd_Non0_Inter1Freq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 2
TempDF <- data.frame(DE_HT_PiAd_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PiAd_PV[["Each_10"]])){
  temp <- DE_HT_PiAd_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PiAd_Non0_Inter2Freq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PiAd_Non0_Inter2Freq[i] <- sum(TempDF[ ,i] != 0)
}
# High freq
TempDF <- data.frame(DE_HT_PiAd_PV[["Each_1"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PiAd_PV[["Each_1"]])){
  temp <- DE_HT_PiAd_PV[["Each_1"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PiAd_Non0_HighFreq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PiAd_Non0_HighFreq[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pj-Advantage, Version A TPCs ####
names(DE_HT_PjAd_PV) <- Eaches
# Low freq.
TempDF <- data.frame(DE_HT_PjAd_PV[["Each_100"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PjAd_PV[["Each_100"]])){
  temp <- DE_HT_PjAd_PV[["Each_100"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PjAd_Non0_LowFreq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PjAd_Non0_LowFreq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 1
TempDF <- data.frame(DE_HT_PjAd_PV[["Each_50"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PjAd_PV[["Each_50"]])){
  temp <- DE_HT_PjAd_PV[["Each_50"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PjAd_Non0_Inter1Freq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PjAd_Non0_Inter1Freq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 2
TempDF <- data.frame(DE_HT_PjAd_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PjAd_PV[["Each_10"]])){
  temp <- DE_HT_PjAd_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PjAd_Non0_Inter2Freq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PjAd_Non0_Inter2Freq[i] <- sum(TempDF[ ,i] != 0)
}
# High freq
TempDF <- data.frame(DE_HT_PjAd_PV[["Each_1"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PjAd_PV[["Each_1"]])){
  temp <- DE_HT_PjAd_PV[["Each_1"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PjAd_Non0_HighFreq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PjAd_Non0_HighFreq[i] <- sum(TempDF[ ,i] != 0)
}
#### ... ... ... Pj-Advantage, Version B TPCs ####
names(DE_HT_PjAd_vB_PV) <- Eaches
# Low freq.
TempDF <- data.frame(DE_HT_PjAd_vB_PV[["Each_100"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PjAd_vB_PV[["Each_100"]])){
  temp <- DE_HT_PjAd_vB_PV[["Each_100"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PjAd_vB_Non0_LowFreq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PjAd_vB_Non0_LowFreq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 1
TempDF <- data.frame(DE_HT_PjAd_vB_PV[["Each_50"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PjAd_vB_PV[["Each_50"]])){
  temp <- DE_HT_PjAd_vB_PV[["Each_50"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PjAd_vB_Non0_Inter1Freq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PjAd_vB_Non0_Inter1Freq[i] <- sum(TempDF[ ,i] != 0)
}
# Intermediate freq. 2
TempDF <- data.frame(DE_HT_PjAd_vB_PV[["Each_10"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PjAd_vB_PV[["Each_10"]])){
  temp <- DE_HT_PjAd_vB_PV[["Each_10"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PjAd_vB_Non0_Inter2Freq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PjAd_vB_Non0_Inter2Freq[i] <- sum(TempDF[ ,i] != 0)
}
# High freq
TempDF <- data.frame(DE_HT_PjAd_vB_PV[["Each_1"]][["Intro_0"]][ ,5])
for (i in 2:length(DE_HT_PjAd_vB_PV[["Each_1"]])){
  temp <- DE_HT_PjAd_vB_PV[["Each_1"]][[i]][ ,5]
  TempDF <- data.frame(cbind(TempDF, temp))
}
colnames(TempDF) <- Names
DE_HT_PjAd_vB_Non0_HighFreq <- c()
for (i in 1:ncol(TempDF)){
  DE_HT_PjAd_vB_Non0_HighFreq[i] <- sum(TempDF[ ,i] != 0)
}

#### ... ... Summarising ####
HT_Iden_Non0 <- data.frame(Det_HT_Iden_PV,
                           E_HT_Iden_Non0_LowFreq,
                           E_HT_Iden_Non0_Inter1Freq,
                           E_HT_Iden_Non0_Inter2Freq,
                           E_HT_Iden_Non0_HighFreq,
                           DE_HT_Iden_Non0_LowFreq,
                           DE_HT_Iden_Non0_Inter1Freq,
                           DE_HT_Iden_Non0_Inter2Freq,
                           DE_HT_Iden_Non0_HighFreq)
HT_Semi_Non0 <- data.frame(Det_HT_Semi_PV,
                           E_HT_Semi_Non0_LowFreq,
                           E_HT_Semi_Non0_Inter1Freq,
                           E_HT_Semi_Non0_Inter2Freq,
                           E_HT_Semi_Non0_HighFreq,
                           DE_HT_Semi_Non0_LowFreq,
                           DE_HT_Semi_Non0_Inter1Freq,
                           DE_HT_Semi_Non0_Inter2Freq,
                           DE_HT_Semi_Non0_HighFreq)
HT_PiAd_Non0 <- data.frame(Det_HT_PiAd_PV,
                           E_HT_PiAd_Non0_LowFreq,
                           E_HT_PiAd_Non0_Inter1Freq,
                           E_HT_PiAd_Non0_Inter2Freq,
                           E_HT_PiAd_Non0_HighFreq,
                           DE_HT_PiAd_Non0_LowFreq,
                           DE_HT_PiAd_Non0_Inter1Freq,
                           DE_HT_PiAd_Non0_Inter2Freq,
                           DE_HT_PiAd_Non0_HighFreq)
HT_PjAd_Non0 <- data.frame(Det_HT_PjAd_PV,
                           E_HT_PjAd_Non0_LowFreq,
                           E_HT_PjAd_Non0_Inter1Freq,
                           E_HT_PjAd_Non0_Inter2Freq,
                           E_HT_PjAd_Non0_HighFreq,
                           DE_HT_PjAd_Non0_LowFreq,
                           DE_HT_PjAd_Non0_Inter1Freq,
                           DE_HT_PjAd_Non0_Inter2Freq,
                           DE_HT_PjAd_Non0_HighFreq)
HT_PjAd_vB_Non0 <- data.frame(Det_HT_PjAd_vB_PV,
                              E_HT_PjAd_vB_Non0_LowFreq,
                              E_HT_PjAd_vB_Non0_Inter1Freq,
                              E_HT_PjAd_vB_Non0_Inter2Freq,
                              E_HT_PjAd_vB_Non0_HighFreq,
                              DE_HT_PjAd_vB_Non0_LowFreq,
                              DE_HT_PjAd_vB_Non0_Inter1Freq,
                              DE_HT_PjAd_vB_Non0_Inter2Freq,
                              DE_HT_PjAd_vB_Non0_HighFreq)

#### Percentage of simulations that result in longer periods of co-occurrence than deterministic case ####
Eaches <- c(); Each <- c(1, 2, 5, seq(10, 100, 10))
for (i in 1:length(Each)){
  Eaches[i] <- paste0("Each_", Each[i])
}

#### ... Low-average temperature environment ####
#### ... ... Identical TPCs ####
# Get into form such that organised by intro time
LT_Iden_List <- list()
for (i in 1:50){
  Iden_List_2 <- list()
  for (j in 1:length(E_LT_Iden_PV)){
    Iden_List_2[[j]] <- E_LT_Iden_PV[[j]][[i]]
  }
  names(Iden_List_2) <- Eaches
  LT_Iden_List[[i]] <- Iden_List_2
}
names(LT_Iden_List) <- names
# Divide sims wherein co-occcurrence > deterministic sim
Prop_By_Intro_LT_Iden <- c()
for (i in 1:length(LT_Iden_List)){
  Prop_By_Intro_LT_Iden[i] <- round(sum(unlist(LT_Iden_List[[i]]) > Det_LT_Iden_PV[i])/(1000*15*13), digits = 2) # 1000 sims x 15 SDs x 13 Eaches
}

#### ... ... Semi-Identical TPCs ####
LT_Semi_List <- list()
for (i in 1:50){
  Semi_List_2 <- list()
  for (j in 1:length(E_LT_Semi_PV)){
    Semi_List_2[[j]] <- E_LT_Semi_PV[[j]][[i]]
  }
  names(Semi_List_2) <- Eaches
  LT_Semi_List[[i]] <- Semi_List_2
}
names(LT_Semi_List) <- names
Prop_By_Intro_LT_Semi <- c()
for (i in 1:length(LT_Semi_List)){
  Prop_By_Intro_LT_Semi[i] <- round(sum(unlist(LT_Semi_List[[i]]) > Det_LT_Semi_PV[i])/(1000*15*13), digits = 2)
}

#### ... ... Pi-Advantage TPCs ####
LT_PiAd_List <- list()
for (i in 1:50){
  PiAd_List_2 <- list()
  for (j in 1:length(E_LT_PiAd_PV)){
    PiAd_List_2[[j]] <- E_LT_PiAd_PV[[j]][[i]]
  }
  names(PiAd_List_2) <- Eaches
  LT_PiAd_List[[i]] <- PiAd_List_2
}
names(LT_PiAd_List) <- names
Prop_By_Intro_LT_PiAd <- c()
for (i in 1:length(LT_PiAd_List)){
  Prop_By_Intro_LT_PiAd[i] <- round(sum(unlist(LT_PiAd_List[[i]]) > Det_LT_PiAd_PV[i])/(1000*15*13), digits = 2)
}

#### ... ... Pj-Advantage, Version A TPCs ####
LT_PjAd_List <- list()
for (i in 1:50){
  PjAd_List_2 <- list()
  for (j in 1:length(E_LT_PjAd_PV)){
    PjAd_List_2[[j]] <- E_LT_PjAd_PV[[j]][[i]]
  }
  names(PjAd_List_2) <- Eaches
  LT_PjAd_List[[i]] <- PjAd_List_2
}
names(LT_PjAd_List) <- names
Prop_By_Intro_LT_PjAd <- c()
for (i in 1:length(LT_PjAd_List)){
  Prop_By_Intro_LT_PjAd[i] <- round(sum(unlist(LT_PjAd_List[[i]]) > Det_LT_PjAd_PV[i])/(1000*15*13), digits = 2)
}

#### ... ... Pj-Advantage, Version B TPCs ####
LT_PjAd_vB_List <- list()
for (i in 1:50){
  PjAd_vB_List_2 <- list()
  for (j in 1:length(E_LT_PjAd_vB_PV)){
    PjAd_vB_List_2[[j]] <- E_LT_PjAd_vB_PV[[j]][[i]]
  }
  names(PjAd_vB_List_2) <- Eaches
  LT_PjAd_vB_List[[i]] <- PjAd_vB_List_2
}
names(LT_PjAd_vB_List) <- names
Prop_By_Intro_LT_PjAd_vB <- c()
for (i in 1:length(LT_PjAd_vB_List)){
  Prop_By_Intro_LT_PjAd_vB[i] <- round(sum(unlist(LT_PjAd_vB_List[[i]]) > Det_LT_PjAd_vB_PV[i])/(1000*15*13), digits = 2)
}

#### ... High-average temperature environment ####
#### ... ... Identical TPCs ####
HT_Iden_List <- list()
for (i in 1:50){
  Iden_List_2 <- list()
  for (j in 1:length(E_HT_Iden_PV)){
    Iden_List_2[[j]] <- E_HT_Iden_PV[[j]][[i]]
  }
  names(Iden_List_2) <- Eaches
  HT_Iden_List[[i]] <- Iden_List_2
}
names(HT_Iden_List) <- names
Prop_By_Intro_HT_Iden <- c()
for (i in 1:length(HT_Iden_List)){
  Prop_By_Intro_HT_Iden[i] <- round(sum(unlist(HT_Iden_List[[i]]) > Det_HT_Iden_PV[i])/(1000*20*13), digits = 2) # 1000 sims x 15 SDs x 13 Eaches
}

#### ... ... Semi-Identical TPCs ####
HT_Semi_List <- list()
for (i in 1:50){
  Semi_List_2 <- list()
  for (j in 1:length(E_HT_Semi_PV)){
    Semi_List_2[[j]] <- E_HT_Semi_PV[[j]][[i]]
  }
  names(Semi_List_2) <- Eaches
  HT_Semi_List[[i]] <- Semi_List_2
}
names(HT_Semi_List) <- names
Prop_By_Intro_HT_Semi <- c()
for (i in 1:length(HT_Semi_List)){
  Prop_By_Intro_HT_Semi[i] <- round(sum(unlist(HT_Semi_List[[i]]) > Det_HT_Semi_PV[i])/(1000*20*13), digits = 2)
}

#### ... ... Pi-Advantage TPCs ####
HT_PiAd_List <- list()
for (i in 1:50){
  PiAd_List_2 <- list()
  for (j in 1:length(E_HT_PiAd_PV)){
    PiAd_List_2[[j]] <- E_HT_PiAd_PV[[j]][[i]]
  }
  names(PiAd_List_2) <- Eaches
  HT_PiAd_List[[i]] <- PiAd_List_2
}
names(HT_PiAd_List) <- names
Prop_By_Intro_HT_PiAd <- c()
for (i in 1:length(HT_PiAd_List)){
  Prop_By_Intro_HT_PiAd[i] <- round(sum(unlist(HT_PiAd_List[[i]]) > Det_HT_PiAd_PV[i])/(1000*20*13), digits = 2)
}

#### ... ... Pj-Advantage, Version A TPCs ####
HT_PjAd_List <- list()
for (i in 1:50){
  PjAd_List_2 <- list()
  for (j in 1:length(E_HT_PjAd_PV)){
    PjAd_List_2[[j]] <- E_HT_PjAd_PV[[j]][[i]]
  }
  names(PjAd_List_2) <- Eaches
  HT_PjAd_List[[i]] <- PjAd_List_2
}
names(HT_PjAd_List) <- names
Prop_By_Intro_HT_PjAd <- c()
for (i in 1:length(HT_PjAd_List)){
  Prop_By_Intro_HT_PjAd[i] <- round(sum(unlist(HT_PjAd_List[[i]]) > Det_HT_PjAd_PV[i])/(1000*20*13), digits = 2)
}

#### ... ... Pj-Advantage, Version B TPCs ####
HT_PjAd_vB_List <- list()
for (i in 1:50){
  PjAd_vB_List_2 <- list()
  for (j in 1:length(E_HT_PjAd_vB_PV)){
    PjAd_vB_List_2[[j]] <- E_HT_PjAd_vB_PV[[j]][[i]]
  }
  names(PjAd_vB_List_2) <- Eaches
  HT_PjAd_vB_List[[i]] <- PjAd_vB_List_2
}
names(HT_PjAd_vB_List) <- names
Prop_By_Intro_HT_PjAd_vB <- c()
for (i in 1:length(HT_PjAd_vB_List)){
  Prop_By_Intro_HT_PjAd_vB[i] <- round(sum(unlist(HT_PjAd_vB_List[[i]]) > Det_HT_PjAd_vB_PV[i])/(1000*20*13), digits = 2)
}

#### ... Summarising ####
Prop_By_Intro_DF <- data.frame(Prop_By_Intro_LT_Iden,
                               Prop_By_Intro_LT_Semi,
                               Prop_By_Intro_LT_PiAd,
                               Prop_By_Intro_LT_PjAd,
                               Prop_By_Intro_LT_PjAd_vB,
                               Prop_By_Intro_HT_Iden,
                               Prop_By_Intro_HT_Semi,
                               Prop_By_Intro_HT_PiAd,
                               Prop_By_Intro_HT_PjAd,
                               Prop_By_Intro_HT_PjAd_vB)

#### Collected summary objects ####
#### ... Number of simulations (/1000) resulting in non-zero periods of co-occurrence ####
LT_Non0_List <- list(LT_Iden_Non0, LT_Semi_Non0, LT_PiAd_Non0, LT_PjAd_Non0, LT_PjAd_vB_Non0); names(LT_Non0_List) <- c("LT_Iden_Non0", "LT_Semi_Non0", "LT_PiAd_Non0", "LT_PjAd_Non0", "LT_PjAd_vB_Non0")
HT_Non0_List <- list(HT_Iden_Non0, HT_Semi_Non0, HT_PiAd_Non0, HT_PjAd_Non0, HT_PjAd_vB_Non0); names(HT_Non0_List) <- c("HT_Iden_Non0", "HT_Semi_Non0", "HT_PiAd_Non0", "HT_PjAd_Non0", "HT_PjAd_vB_Non0")
#### ... Percentage of simulations that result in longer periods of co-occurrence than deterministic case ####
Prop_By_Intro_DF

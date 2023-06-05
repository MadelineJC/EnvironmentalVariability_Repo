#### DOC STRING ===============================================================
# This script runs deterministic simulations in each environment (low- and 
# high-average temperature) under each thermal scenario (Identical, Semi-
# Identical, Pi-Advantage, Pj-Advantage (Version A), and Pj-Advantage (Version 
# B)). 

# If you run this script, it will save time series outputs, and persistence vectors 
# (containing the number of time steps for which both parasites populations had 
# abundances greater than) as .rds files in a folder called "Generated_Outputs".

# To see the outputs as used in the manuscript, go to 
# "/EnvironmentalVariability_Repo/Data/Deterministic_Outputs".
#### END DOC STRING============================================================

#### Low-Average Temperature Environment ####
#### ... Identical TPCs ####
## Intro at 0
ri = 2.13; rj = 2.13; k = 0.01; p = 1; o = 1000
Pi <- c(); Pj <- c(); I <- c()
Pi[1] = 1; Pj[1] = 1; I[1] = 1
ts = 0.1; t_stop = seq(0, 50, ts)
for (t in 1:length(t_stop)){
  Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
  Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
  I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
}
Pi <- Pi[1:length(t_stop)]; Pi_DetM <- Pi
Pj <- Pj[1:length(t_stop)]; Pj_DetM <- Pj
I <- I[1:length(t_stop)]; I_DetM <- I
I0DF <- data.frame(t_stop, Pi, Pj, I)

## Intro at 0 PV
for (i in 1:nrow(I0DF)){
  temp_df = I0DF[,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  Intro0PV <- nrow(temp_df)
}
IntroPt <- seq(1, 49, 1)
DetIntros <- vector(mode = "list", length=length(IntroPt))

## Intro after 0
ri = 2.13; rj = 2.13; k = 0.01; p = 1; o = 1000
ts = 0.1 
for (i in 1:length(IntroPt)){
  Pi <- c(); Pj <- c(); I <- c()
  Pi[1] = 1; Pj[1] = 0; I[1] = 1
  t_stop = seq(0, IntroPt[i], ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df <- data.frame(t_stop, Pi, Pj, I)
  
  Pi[1] = det_df$Pi[nrow(det_df)]; Pj[1] = 1; I[1] = det_df$I[nrow(det_df)]
  t_stop = seq((IntroPt[i]+0.1), 50, ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df2 <- data.frame(t_stop, Pi, Pj, I)
  
  df <- data.frame(rbind(det_df, det_df2))
  
  DetIntros[[i]] <- df
}

## Intro after 0 PV
DetPV <- vector()
Intros <- seq(1, 49, 1)
for (i in 1:length(DetIntros)){
  temp_df = DetIntros[[i]][,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  DetPV[i] = nrow(temp_df)
}
DetPV <- c(Intro0PV, DetPV)
Zeros <- which(DetPV == 0)
First0 <- Zeros[1] - 1

## Concat timeseries
Det_Timeseries <- list(); Det_Timeseries[[1]] <- I0DF
for (i in 1:length(DetIntros)){
  Det_Timeseries[[i + 1]] <- DetIntros[[i]]
}
Names <- c()
for (i in 1:50){
  Names[i] <- paste0("Intro_", i-1)
}
names(Det_Timeseries) <- Names

## Save timeseries
saveRDS(Det_Timeseries, file = "Generated_Outputs/Det_LT_Iden.rds")

## Save PV
saveRDS(DetPV, file = "Generated_Outputs/Det_LT_Iden_PV.rds")

#### ... Semi-Identical TPCs ####
ri = 1.79; rj = 1.80; k = 0.01; p = 1; o = 1000
Pi <- c(); Pj <- c(); I <- c()
Pi[1] = 1; Pj[1] = 1; I[1] = 1
ts = 0.1; t_stop = seq(0, 50, ts)
for (t in 1:length(t_stop)){
  Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
  Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
  I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
}
Pi <- Pi[1:length(t_stop)]; Pi_DetM <- Pi
Pj <- Pj[1:length(t_stop)]; Pj_DetM <- Pj
I <- I[1:length(t_stop)]; I_DetM <- I
I0DF <- data.frame(t_stop, Pi, Pj, I)
for (i in 1:nrow(I0DF)){
  temp_df = I0DF[,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  Intro0PV <- nrow(temp_df)
}
IntroPt <- seq(1, 49, 1)
DetIntros <- vector(mode = "list", length=length(IntroPt))
ri = 1.79; rj = 1.80; k = 0.01; p = 1; o = 1000
ts = 0.1 
for (i in 1:length(IntroPt)){
  Pi <- c(); Pj <- c(); I <- c()
  Pi[1] = 1; Pj[1] = 0; I[1] = 1
  t_stop = seq(0, IntroPt[i], ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df <- data.frame(t_stop, Pi, Pj, I)
  
  Pi[1] = det_df$Pi[nrow(det_df)]; Pj[1] = 1; I[1] = det_df$I[nrow(det_df)]
  t_stop = seq((IntroPt[i]+0.1), 50, ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df2 <- data.frame(t_stop, Pi, Pj, I)
  
  df <- data.frame(rbind(det_df, det_df2))
  
  DetIntros[[i]] <- df
}
DetPV <- vector()
Intros <- seq(1, 49, 1)
for (i in 1:length(DetIntros)){
  temp_df = DetIntros[[i]][,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  DetPV[i] = nrow(temp_df)
}
DetPV <- c(Intro0PV, DetPV)
Zeros <- which(DetPV == 0)
First0 <- Zeros[1] - 1
Det_Timeseries <- list(); Det_Timeseries[[1]] <- I0DF
for (i in 1:length(DetIntros)){
  Det_Timeseries[[i + 1]] <- DetIntros[[i]]
}
Names <- c()
for (i in 1:50){
  Names[i] <- paste0("Intro_", i-1)
}
names(Det_Timeseries) <- Names
saveRDS(Det_Timeseries, file = "Generated_Outputs/Det_LT_Semi.rds")
saveRDS(DetPV, file = "Generated_Outputs/Det_LT_Semi_PV.rds")

#### ... Pi-Advantage TPCs ####
ri = 2.12; rj = 1.54; k = 0.01; p = 1; o = 1000
Pi <- c(); Pj <- c(); I <- c()
Pi[1] = 1; Pj[1] = 1; I[1] = 1
ts = 0.1; t_stop = seq(0, 50, ts)
for (t in 1:length(t_stop)){
  Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
  Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
  I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
}
Pi <- Pi[1:length(t_stop)]; Pi_DetM <- Pi
Pj <- Pj[1:length(t_stop)]; Pj_DetM <- Pj
I <- I[1:length(t_stop)]; I_DetM <- I
I0DF <- data.frame(t_stop, Pi, Pj, I)
for (i in 1:nrow(I0DF)){
  temp_df = I0DF[,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  Intro0PV <- nrow(temp_df)
}
IntroPt <- seq(1, 49, 1)
DetIntros <- vector(mode = "list", length=length(IntroPt))
ri = 2.12; rj = 1.54; k = 0.01; p = 1; o = 1000
ts = 0.1 
for (i in 1:length(IntroPt)){
  Pi <- c(); Pj <- c(); I <- c()
  Pi[1] = 1; Pj[1] = 0; I[1] = 1
  t_stop = seq(0, IntroPt[i], ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df <- data.frame(t_stop, Pi, Pj, I)
  
  Pi[1] = det_df$Pi[nrow(det_df)]; Pj[1] = 1; I[1] = det_df$I[nrow(det_df)]
  t_stop = seq((IntroPt[i]+0.1), 50, ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df2 <- data.frame(t_stop, Pi, Pj, I)
  
  df <- data.frame(rbind(det_df, det_df2))
  
  DetIntros[[i]] <- df
}
DetPV <- vector()
Intros <- seq(1, 49, 1)
for (i in 1:length(DetIntros)){
  temp_df = DetIntros[[i]][,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  DetPV[i] = nrow(temp_df)
}
DetPV <- c(Intro0PV, DetPV)
Zeros <- which(DetPV == 0)
First0 <- Zeros[1] - 1
Det_Timeseries <- list(); Det_Timeseries[[1]] <- I0DF
for (i in 1:length(DetIntros)){
  Det_Timeseries[[i + 1]] <- DetIntros[[i]]
}
Names <- c()
for (i in 1:50){
  Names[i] <- paste0("Intro_", i-1)
}
names(Det_Timeseries) <- Names
saveRDS(Det_Timeseries, file = "Generated_Outputs/Det_LT_PiAd.rds")
saveRDS(DetPV, file = "Generated_Outputs/Det_LT_PiAd_PV.rds")

#### ... Pj-Advantage, Version A TPCs ####
ri = 1.36; rj = 1.94; k = 0.01; p = 1; o = 1000
Pi <- c(); Pj <- c(); I <- c()
Pi[1] = 1; Pj[1] = 1; I[1] = 1
ts = 0.1; t_stop = seq(0, 50, ts)
for (t in 1:length(t_stop)){
  Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
  Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
  I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
}
Pi <- Pi[1:length(t_stop)]; Pi_DetM <- Pi
Pj <- Pj[1:length(t_stop)]; Pj_DetM <- Pj
I <- I[1:length(t_stop)]; I_DetM <- I
I0DF <- data.frame(t_stop, Pi, Pj, I)
for (i in 1:nrow(I0DF)){
  temp_df = I0DF[,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  Intro0PV <- nrow(temp_df)
}
IntroPt <- seq(1, 49, 1)
DetIntros <- vector(mode = "list", length=length(IntroPt))
ri = 1.36; rj = 1.94; k = 0.01; p = 1; o = 1000
ts = 0.1 
for (i in 1:length(IntroPt)){
  Pi <- c(); Pj <- c(); I <- c()
  Pi[1] = 1; Pj[1] = 0; I[1] = 1
  t_stop = seq(0, IntroPt[i], ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df <- data.frame(t_stop, Pi, Pj, I)
  
  Pi[1] = det_df$Pi[nrow(det_df)]; Pj[1] = 1; I[1] = det_df$I[nrow(det_df)]
  t_stop = seq((IntroPt[i]+0.1), 50, ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df2 <- data.frame(t_stop, Pi, Pj, I)
  
  df <- data.frame(rbind(det_df, det_df2))
  
  DetIntros[[i]] <- df
}
DetPV <- vector()
Intros <- seq(1, 49, 1)
for (i in 1:length(DetIntros)){
  temp_df = DetIntros[[i]][,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  DetPV[i] = nrow(temp_df)
}
DetPV <- c(Intro0PV, DetPV)
Zeros <- which(DetPV == 0)
First0 <- Zeros[1] - 1
Det_Timeseries <- list(); Det_Timeseries[[1]] <- I0DF
for (i in 1:length(DetIntros)){
  Det_Timeseries[[i + 1]] <- DetIntros[[i]]
}
Names <- c()
for (i in 1:50){
  Names[i] <- paste0("Intro_", i-1)
}
names(Det_Timeseries) <- Names
saveRDS(Det_Timeseries, file = "Generated_Outputs/Det_LT_PjAd.rds")
saveRDS(DetPV, file = "Generated_Outputs/Det_LT_PjAd_PV.rds")

#### ... Pj-Advantage, Version B TPCs ####
ri = 1.54; rj = 2.12; k = 0.01; p = 1; o = 1000
Pi <- c(); Pj <- c(); I <- c()
Pi[1] = 1; Pj[1] = 1; I[1] = 1
ts = 0.1; t_stop = seq(0,50,ts)
for (t in 1:length(t_stop)){
  Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
  Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
  I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
}
Pi <- Pi[1:length(t_stop)]; Pi_DetM <- Pi
Pj <- Pj[1:length(t_stop)]; Pj_DetM <- Pj
I <- I[1:length(t_stop)]; I_DetM <- I
I0DF <- data.frame(t_stop, Pi, Pj, I)
for (i in 1:nrow(I0DF)){
  temp_df = I0DF[,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  Intro0PV <- nrow(temp_df)
}
IntroPt <- seq(1,49,1)
DetIntros <- vector(mode = "list", length=length(IntroPt))
ri = 1.54; rj = 2.12; k = 0.01; p = 1; o = 1000
ts = 0.1
for (i in 1:length(IntroPt)){
  Pi <- c(); Pj <- c(); I <- c()
  Pi[1] = 1; Pj[1] = 0; I[1] = 1
  t_stop = seq(0, IntroPt[i], ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)];
  Pj <- Pj[1:length(t_stop)];
  I <- I[1:length(t_stop)];
  det_df <- data.frame(t_stop, Pi, Pj, I)

  Pi[1] = det_df$Pi[nrow(det_df)]; Pj[1] = 1; I[1] = det_df$I[nrow(det_df)]
  t_stop = seq((IntroPt[i]+0.1), 50, ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)];
  Pj <- Pj[1:length(t_stop)];
  I <- I[1:length(t_stop)];
  det_df2 <- data.frame(t_stop, Pi, Pj, I)

  df <- data.frame(rbind(det_df, det_df2))

  DetIntros[[i]] <- df
}
DetPV <- vector()
Intros <- seq(1,49,1)
for (i in 1:length(DetIntros)){
  temp_df = DetIntros[[i]][,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  DetPV[i] = nrow(temp_df)
}
DetPV <- c(Intro0PV, DetPV)
Zeros <- which(DetPV == 0)
First0 <- Zeros[1] - 1
Det_Timeseries <- list(); Det_Timeseries[[1]] <- I0DF
for (i in 1:length(DetIntros)){
  Det_Timeseries[[i + 1]] <- DetIntros[[i]]
}
Names <- c()
for (i in 1:50){
  Names[i] <- paste0("Intro_", i-1)
}
names(Det_Timeseries) <- Names
saveRDS(Det_Timeseries, file = "Generated_Outputs/Det_LT_PjAd_vB.rds")
saveRDS(DetPV, file = "Generated_Outputs/Det_LT_PjAd_vB_PV.rds")

#### High-Average Temperature Environment ####
#### ... Identical TPCs ####
ri = 2.15; rj = 2.15; k = 0.01; p = 1; o = 1000
Pi <- c(); Pj <- c(); I <- c()
Pi[1] = 1; Pj[1] = 1; I[1] = 1
ts = 0.1; t_stop = seq(0, 50, ts)
for (t in 1:length(t_stop)){
  Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
  Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
  I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
}
Pi <- Pi[1:length(t_stop)]; Pi_DetM <- Pi
Pj <- Pj[1:length(t_stop)]; Pj_DetM <- Pj
I <- I[1:length(t_stop)]; I_DetM <- I
I0DF <- data.frame(t_stop, Pi, Pj, I)
for (i in 1:nrow(I0DF)){
  temp_df = I0DF[,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  Intro0PV <- nrow(temp_df)
}
IntroPt <- seq(1, 49, 1)
DetIntros <- vector(mode = "list", length=length(IntroPt))
ri = 2.15; rj = 2.15; k = 0.01; p = 1; o = 1000
ts = 0.1 
for (i in 1:length(IntroPt)){
  Pi <- c(); Pj <- c(); I <- c()
  Pi[1] = 1; Pj[1] = 0; I[1] = 1
  t_stop = seq(0, IntroPt[i], ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df <- data.frame(t_stop, Pi, Pj, I)
  
  Pi[1] = det_df$Pi[nrow(det_df)]; Pj[1] = 1; I[1] = det_df$I[nrow(det_df)]
  t_stop = seq((IntroPt[i]+0.1), 50, ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df2 <- data.frame(t_stop, Pi, Pj, I)
  
  df <- data.frame(rbind(det_df, det_df2))
  
  DetIntros[[i]] <- df
}
DetPV <- vector()
Intros <- seq(1, 49, 1)
for (i in 1:length(DetIntros)){
  temp_df = DetIntros[[i]][,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  DetPV[i] = nrow(temp_df)
}
DetPV <- c(Intro0PV, DetPV)
Zeros <- which(DetPV == 0)
First0 <- Zeros[1] - 1
Det_Timeseries <- list(); Det_Timeseries[[1]] <- I0DF
for (i in 1:length(DetIntros)){
  Det_Timeseries[[i + 1]] <- DetIntros[[i]]
}
Names <- c()
for (i in 1:50){
  Names[i] <- paste0("Intro_", i-1)
}
names(Det_Timeseries) <- Names
saveRDS(Det_Timeseries, file = "Generated_Outputs/Det_HT_Iden.rds")
saveRDS(DetPV, file = "Generated_Outputs/Det_HT_Iden_PV.rds")

#### ... Semi-Identical TPCs ####
ri = 1.84; rj = 1.82; k = 0.01; p = 1; o = 1000
Pi <- c(); Pj <- c(); I <- c()
Pi[1] = 1; Pj[1] = 1; I[1] = 1
ts = 0.1; t_stop = seq(0, 50, ts)
for (t in 1:length(t_stop)){
  Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
  Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
  I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
}
Pi <- Pi[1:length(t_stop)]; Pi_DetM <- Pi
Pj <- Pj[1:length(t_stop)]; Pj_DetM <- Pj
I <- I[1:length(t_stop)]; I_DetM <- I
I0DF <- data.frame(t_stop, Pi, Pj, I)
for (i in 1:nrow(I0DF)){
  temp_df = I0DF[,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  Intro0PV <- nrow(temp_df)
}
IntroPt <- seq(1, 49, 1)
DetIntros <- vector(mode = "list", length=length(IntroPt))
ri = 1.84; rj = 1.82; k = 0.01; p = 1; o = 1000
ts = 0.1 
for (i in 1:length(IntroPt)){
  Pi <- c(); Pj <- c(); I <- c()
  Pi[1] = 1; Pj[1] = 0; I[1] = 1
  t_stop = seq(0, IntroPt[i], ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df <- data.frame(t_stop, Pi, Pj, I)
  
  Pi[1] = det_df$Pi[nrow(det_df)]; Pj[1] = 1; I[1] = det_df$I[nrow(det_df)]
  t_stop = seq((IntroPt[i]+0.1), 50, ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df2 <- data.frame(t_stop, Pi, Pj, I)
  
  df <- data.frame(rbind(det_df, det_df2))
  
  DetIntros[[i]] <- df
}
DetPV <- vector()
Intros <- seq(1, 49, 1)
for (i in 1:length(DetIntros)){
  temp_df = DetIntros[[i]][,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  DetPV[i] = nrow(temp_df)
}
DetPV <- c(Intro0PV, DetPV)
Zeros <- which(DetPV == 0)
First0 <- Zeros[1] - 1
Det_Timeseries <- list(); Det_Timeseries[[1]] <- I0DF
for (i in 1:length(DetIntros)){
  Det_Timeseries[[i + 1]] <- DetIntros[[i]]
}
Names <- c()
for (i in 1:50){
  Names[i] <- paste0("Intro_", i-1)
}
names(Det_Timeseries) <- Names
saveRDS(Det_Timeseries, file = "Generated_Outputs/Det_HT_Semi.rds")
saveRDS(DetPV, file = "Generated_Outputs/Det_HT_Semi_PV.rds")

#### ... Pi-Advantage TPCs ####
ri = 2.11; rj = 1.61; k = 0.01; p = 1; o = 1000
Pi <- c(); Pj <- c(); I <- c()
Pi[1] = 1; Pj[1] = 1; I[1] = 1
ts = 0.1; t_stop = seq(0, 50, ts)
for (t in 1:length(t_stop)){
  Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
  Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
  I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
}
Pi <- Pi[1:length(t_stop)]; Pi_DetM <- Pi
Pj <- Pj[1:length(t_stop)]; Pj_DetM <- Pj
I <- I[1:length(t_stop)]; I_DetM <- I
I0DF <- data.frame(t_stop, Pi, Pj, I)
for (i in 1:nrow(I0DF)){
  temp_df = I0DF[,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  Intro0PV <- nrow(temp_df)
}
IntroPt <- seq(1, 49, 1)
DetIntros <- vector(mode = "list", length=length(IntroPt))
ri = 2.11; rj = 1.61; k = 0.01; p = 1; o = 1000
ts = 0.1 
for (i in 1:length(IntroPt)){
  Pi <- c(); Pj <- c(); I <- c()
  Pi[1] = 1; Pj[1] = 0; I[1] = 1
  t_stop = seq(0, IntroPt[i], ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df <- data.frame(t_stop, Pi, Pj, I)
  
  Pi[1] = det_df$Pi[nrow(det_df)]; Pj[1] = 1; I[1] = det_df$I[nrow(det_df)]
  t_stop = seq((IntroPt[i]+0.1), 50, ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df2 <- data.frame(t_stop, Pi, Pj, I)
  
  df <- data.frame(rbind(det_df, det_df2))
  
  DetIntros[[i]] <- df
}
DetPV <- vector()
Intros <- seq(1, 49, 1)
for (i in 1:length(DetIntros)){
  temp_df = DetIntros[[i]][,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  DetPV[i] = nrow(temp_df)
}
DetPV <- c(Intro0PV, DetPV)
Zeros <- which(DetPV == 0)
First0 <- Zeros[1] - 1
Det_Timeseries <- list(); Det_Timeseries[[1]] <- I0DF
for (i in 1:length(DetIntros)){
  Det_Timeseries[[i + 1]] <- DetIntros[[i]]
}
Names <- c()
for (i in 1:50){
  Names[i] <- paste0("Intro_", i-1)
}
names(Det_Timeseries) <- Names
saveRDS(Det_Timeseries, file = "Generated_Outputs/Det_HT_PiAd.rds")
saveRDS(DetPV, file = "Generated_Outputs/Det_HT_PiAd_PV.rds")

#### ... Pj-Advantage, Version A TPCs ####
ri = 1.45; rj = 1.96; k = 0.01; p = 1; o = 1000
Pi <- c(); Pj <- c(); I <- c()
Pi[1] = 1; Pj[1] = 1; I[1] = 1
ts = 0.1; t_stop = seq(0, 50, ts)
for (t in 1:length(t_stop)){
  Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
  Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
  I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
}
Pi <- Pi[1:length(t_stop)]; Pi_DetM <- Pi
Pj <- Pj[1:length(t_stop)]; Pj_DetM <- Pj
I <- I[1:length(t_stop)]; I_DetM <- I
I0DF <- data.frame(t_stop, Pi, Pj, I)
for (i in 1:nrow(I0DF)){
  temp_df = I0DF[,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  Intro0PV <- nrow(temp_df)
}
IntroPt <- seq(1, 49, 1)
DetIntros <- vector(mode = "list", length=length(IntroPt))
ri = 1.45; rj = 1.96; k = 0.01; p = 1; o = 1000
ts = 0.1 
for (i in 1:length(IntroPt)){
  Pi <- c(); Pj <- c(); I <- c()
  Pi[1] = 1; Pj[1] = 0; I[1] = 1
  t_stop = seq(0, IntroPt[i], ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df <- data.frame(t_stop, Pi, Pj, I)
  
  Pi[1] = det_df$Pi[nrow(det_df)]; Pj[1] = 1; I[1] = det_df$I[nrow(det_df)]
  t_stop = seq((IntroPt[i]+0.1), 50, ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)]; 
  Pj <- Pj[1:length(t_stop)]; 
  I <- I[1:length(t_stop)]; 
  det_df2 <- data.frame(t_stop, Pi, Pj, I)
  
  df <- data.frame(rbind(det_df, det_df2))
  
  DetIntros[[i]] <- df
}
DetPV <- vector()
Intros <- seq(1, 49, 1)
for (i in 1:length(DetIntros)){
  temp_df = DetIntros[[i]][,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  DetPV[i] = nrow(temp_df)
}
DetPV <- c(Intro0PV, DetPV)
Zeros <- which(DetPV == 0)
First0 <- Zeros[1] - 1
Det_Timeseries <- list(); Det_Timeseries[[1]] <- I0DF
for (i in 1:length(DetIntros)){
  Det_Timeseries[[i + 1]] <- DetIntros[[i]]
}
Names <- c()
for (i in 1:50){
  Names[i] <- paste0("Intro_", i-1)
}
names(Det_Timeseries) <- Names
saveRDS(Det_Timeseries, file = "Generated_Outputs/Det_HT_PjAd.rds")
saveRDS(DetPV, file = "Generated_Outputs/Det_HT_PjAd_PV.rds")

#### ... Pj-Advantage, Version B TPCs ####
ri = 1.61; rj = 2.11; k = 0.01; p = 1; o = 1000
Pi <- c(); Pj <- c(); I <- c()
Pi[1] = 1; Pj[1] = 1; I[1] = 1
ts = 0.1; t_stop = seq(0,50,ts)
for (t in 1:length(t_stop)){
  Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
  Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
  I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
}
Pi <- Pi[1:length(t_stop)]; Pi_DetM <- Pi
Pj <- Pj[1:length(t_stop)]; Pj_DetM <- Pj
I <- I[1:length(t_stop)]; I_DetM <- I
I0DF <- data.frame(t_stop, Pi, Pj, I)
for (i in 1:nrow(I0DF)){
  temp_df = I0DF[,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  Intro0PV <- nrow(temp_df)
}
IntroPt <- seq(1,49,1)
DetIntros <- vector(mode = "list", length=length(IntroPt))
ri = 1.61; rj = 2.11; k = 0.01; p = 1; o = 1000
ts = 0.1
for (i in 1:length(IntroPt)){
  Pi <- c(); Pj <- c(); I <- c()
  Pi[1] = 1; Pj[1] = 0; I[1] = 1
  t_stop = seq(0, IntroPt[i], ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)];
  Pj <- Pj[1:length(t_stop)];
  I <- I[1:length(t_stop)];
  det_df <- data.frame(t_stop, Pi, Pj, I)

  Pi[1] = det_df$Pi[nrow(det_df)]; Pj[1] = 1; I[1] = det_df$I[nrow(det_df)]
  t_stop = seq((IntroPt[i]+0.1), 50, ts)
  for (t in 1:length(t_stop)){
    Pi[t + 1] = (1 + ri*ts - (1-exp(-k*I[t]*ts)))*Pi[t]
    Pj[t + 1] = (1 + rj*ts - (1-exp(-k*I[t]*ts)))*Pj[t]
    I[t + 1] = (1 + p*(Pi[t]/(Pi[t] + o) + Pj[t]/(Pj[t] + o))*ts)*I[t]
  }
  Pi <- Pi[1:length(t_stop)];
  Pj <- Pj[1:length(t_stop)];
  I <- I[1:length(t_stop)];
  det_df2 <- data.frame(t_stop, Pi, Pj, I)

  df <- data.frame(rbind(det_df, det_df2))

  DetIntros[[i]] <- df
}
DetPV <- vector()
Intros <- seq(1,49,1)
for (i in 1:length(DetIntros)){
  temp_df = DetIntros[[i]][,2:3]
  temp_df = temp_df[which(temp_df[,1] > 1 & temp_df[,2] > 1),]
  DetPV[i] = nrow(temp_df)
}
DetPV <- c(Intro0PV, DetPV)
Zeros <- which(DetPV == 0)
First0 <- Zeros[1] - 1
Det_Timeseries <- list(); Det_Timeseries[[1]] <- I0DF
for (i in 1:length(DetIntros)){
  Det_Timeseries[[i + 1]] <- DetIntros[[i]]
}
Names <- c()
for (i in 1:50){
  Names[i] <- paste0("Intro_", i-1)
}
names(Det_Timeseries) <- Names
saveRDS(Det_Timeseries, file = "Generated_Outputs/Det_HT_PjAd_vB.rds")
saveRDS(DetPV, file = "Generated_Outputs/Det_HT_PjAd_vB_PV.rds")

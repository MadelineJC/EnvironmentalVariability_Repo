#### DOC STRING ===============================================================
# This script contains functions for thermal performance curves for each 
# parasite (i and j), under each thermal scenario (Identical, Semi-Identical,
# Pi-Advantage, Pj-Advantage (Version A), and Pj-Advantage (Version B)).
#### END DOC STRING============================================================

#### TEMPERATURE SAMPLING ####
#### Low-Average Temperature Environment ####
#### ... Identical TPCs ####
MTE_TPC <- function(TempK){
  lambda_knot <- 1.4; Tknot <- 14+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-9.5)+K; TzH <- 22.5+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate)
}

av <- 19.9 # Optimal temperature for Pi and Pj

#### ... Semi-Identical TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}

av <- 19.9 # Temperature at which Pi and Pj have identical replication rates

#### ... Pi-Advantage TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}

av <- 18 # Optimal temperature for Pi

#### ... Pj-Advantage, Version A TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}

av <- 20.8530117 # Optimal temperature for Pj

#### ... Pi-Advantage, Version B TPCs ####
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}

av <- 18 # Optimal temperature for Pi

#### High-Average Temperature Environment ####
#### ... Identical TPCs ####
MTE_TPC <- function(TempK){
  lambda_knot <- 1.2; Tknot <- 19+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-2)+K; TzH <- 29.8+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate)
}

av <- 27.1 # Optimal temperature for Pi and Pj

#### ... Semi-Identical TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 0.68; Tknot <- 11.3+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-10)+K; TzH <- 28+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.68; Tknot <- 26+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- 1+K; TzH <- 33+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}

av <- 27.1 # Temperature at which Pi and Pj have identical replication rates

#### ... Pi-Advantage TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 0.68; Tknot <- 11.3+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-10)+K; TzH <- 28+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.68; Tknot <- 26+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- 1+K; TzH <- 33+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}

av <- 25.5 # Optimal temperature for Pi

#### ... Pj-Advantage, Version A TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 0.68; Tknot <- 11.3+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-10)+K; TzH <- 28+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.68; Tknot <- 26+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- 1+K; TzH <- 33+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}

av <- 28.0639755 # Optimal temperature for Pi

#### ... Pj-Advantage, Version B TPCs ####
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 0.68; Tknot <- 11.3+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-10)+K; TzH <- 28+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 1.68; Tknot <- 26+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- 1+K; TzH <- 33+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}

av <- 25.5 # Optimal temperature for Pi

#### PLOTTING ####
K <- 273.15
#### Low-Average Temperature Environment ####
#### ... Identical TPCs ####
MTE_TPC <- function(TempK){
  lambda_knot <- 1.4; Tknot <- 14+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-9.5)+K; TzH <- 22.5+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate)
}
TempK <- seq(0+K, 30+K, 0.1)
RepRate <- MTE_TPC(TempK)
TempC <- seq(0, 30, 0.1)
x <- as.data.frame(cbind(TempC, RepRate))
plot(TempC, RepRate, type = "l", lwd = 3, col = "goldenrod1", xlab = "Temperature (C)", ylab = "Replication Rate", ylim = c(0,3)) # TPC of Pi
lines(TempC, RepRate, lty = 2, lwd = 2, col = "springgreen4") # TPC of Pj
abline(h = c(mean(RepRate)), col = c("goldenrod1"), lty = 1, lwd = 2) # Average replication rate of Pi
abline(h = c(mean(RepRate)), col = c("springgreen4"), lty = 2, lwd = 2) # Average replication rate of Pj
abline(v = 19.9, col = "#FF99FF", lwd = 2) # Average temperature of the system

#### ... Semi-Identical TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}
TempK <- seq(0+K, 30+K, 0.1)
RepRate_Pi <- MTE_TPC_Pi(TempK); RepRate_Pj <- MTE_TPC_Pj(TempK)
TempC <- seq(0, 30, 0.1)
x <- as.data.frame(cbind(TempC, RepRate_Pi, RepRate_Pj))
plot(TempC, RepRate_Pi, type = "l", lwd = 3, col = "goldenrod1", xlab = "Temperature (C)", ylab = "Replication Rate", ylim = c(0,3))
lines(TempC, RepRate_Pj, lwd = 3, col = "springgreen4")
abline(h = c(mean(RepRate_Pi), mean(RepRate_Pj)), col = c("goldenrod1", "springgreen4"), lty = 2, lwd = 2)
abline(v = 19.9, col = "#FF99FF", lty = 1, lwd = 2)

#### ... Pi-Advantage TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}
TempK <- seq(0+K, 30+K, 0.1)
RepRate_Pi <- MTE_TPC_Pi(TempK); RepRate_Pj <- MTE_TPC_Pj(TempK)
TempC <- seq(0, 30, 0.1)
x <- as.data.frame(cbind(TempC, RepRate_Pi, RepRate_Pj))
plot(TempC, RepRate_Pi, type = "l", lwd = 3, col = "goldenrod1", xlab = "Temperature (C)", ylab = "Replication Rate", ylim = c(0,3))
lines(TempC, RepRate_Pj, lwd = 3, col = "springgreen4")
abline(h = c(mean(RepRate_Pi), mean(RepRate_Pj)), col = c("goldenrod1", "springgreen4"), lty = 2, lwd = 2)
abline(v = 18, col = "#FF99FF", lty = 1, lwd = 2)

#### ... Pj-Advantage, Version A TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}
TempK <- seq(0+K, 30+K, 0.1)
RepRate_Pi <- MTE_TPC_Pi(TempK); RepRate_Pj <- MTE_TPC_Pj(TempK)
TempC <- seq(0, 30, 0.1)
x <- as.data.frame(cbind(TempC, RepRate_Pi, RepRate_Pj))
plot(TempC, RepRate_Pi, type = "l", lwd = 3, col = "goldenrod1", xlab = "Temperature (C)", ylab = "Replication Rate", ylim = c(0,3))
lines(TempC, RepRate_Pj, lwd = 3, col = "springgreen4")
abline(h = c(mean(RepRate_Pi), mean(RepRate_Pj)), col = c("goldenrod1", "springgreen4"), lty = 2, lwd = 2)
abline(v = 20.8530117, col = "#FF99FF", lty = 1, lwd = 2)

#### ... Pj-Advantage, Version B TPCs ####
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.5; Tknot <- 13+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-12)+K; TzH <- 20.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 1.54; Tknot <- 18+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-7)+K; TzH <- 25.6+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
TempK <- seq(0+K, 30+K, 0.1)
RepRate_Pi <- MTE_TPC_Pi(TempK); RepRate_Pj <- MTE_TPC_Pj(TempK)
TempC <- seq(0, 30, 0.1)
x <- as.data.frame(cbind(TempC, RepRate_Pi, RepRate_Pj))
plot(TempC, RepRate_Pi, type = "l", lwd = 3, col = "goldenrod1", xlab = "Temperature (C)", ylab = "Replication Rate", ylim = c(0,3))
lines(TempC, RepRate_Pj, lwd = 3, col = "springgreen4")
abline(h = c(mean(RepRate_Pi), mean(RepRate_Pj)), col = c("goldenrod1", "springgreen4"), lty = 2, lwd = 2)
abline(v = 18, col = "#FF99FF", lty = 1, lwd = 2)

#### High-Average Temperature Environment ####
#### ... Identical TPCs ####
MTE_TPC <- function(TempK){
  lambda_knot <- 1.2; Tknot <- 19+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-2)+K; TzH <- 29.8+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate)
}
TempK <- seq(0+K, 40+K, 0.1)
test <- MTE_TPC(TempK)
TempC <- seq(0, 40, 0.1)
x <- as.data.frame(cbind(TempC, test))
plot(TempC, test, type = "l", lwd = 3, col = "goldenrod1", xlab = "Temperature (C)", ylab = "Replication Rate", ylim = c(0,3))
lines(TempC, test, lty = 2, lwd = 2, col = "springgreen4")
abline(h = c(mean(test)), col = c("goldenrod1"), lty = 1, lwd = 2)
abline(h = c(mean(test)), col = c("springgreen4"), lty = 2, lwd = 2)
abline(v = 27.1, col = "#FF99FF", lwd = 2)

#### ... Semi-Identical TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 0.68; Tknot <- 11.3+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-10)+K; TzH <- 28+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.68; Tknot <- 26+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- 1+K; TzH <- 33+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}
TempK <- seq(0+K, 40+K, 0.1)
RepRate_Pi <- MTE_TPC_Pi(TempK); RepRate_Pj <- MTE_TPC_Pj(TempK)
TempC <- seq(0, 40, 0.1)
x <- as.data.frame(cbind(TempC, RepRate_Pi, RepRate_Pj))
plot(TempC, RepRate_Pi, type = "l", lwd = 3, col = "goldenrod1", xlab = "Temperature (C)", ylab = "Replication Rate", ylim = c(0,3))
lines(TempC, RepRate_Pj, lwd = 3, col = "springgreen4")
abline(h = c(mean(RepRate_Pi), mean(RepRate_Pj)), col = c("goldenrod1", "springgreen4"), lty = 2, lwd = 2)
abline(v = 27.1, col = "#FF99FF", lty = 1, lwd = 2)

#### ... Pi-Advantage TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 0.68; Tknot <- 11.3+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-10)+K; TzH <- 28+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.68; Tknot <- 26+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- 1+K; TzH <- 33+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}
TempK <- seq(0+K, 40+K, 0.1)
RepRate_Pi <- MTE_TPC_Pi(TempK); RepRate_Pj <- MTE_TPC_Pj(TempK)
TempC <- seq(0, 40, 0.1)
x <- as.data.frame(cbind(TempC, RepRate_Pi, RepRate_Pj))
plot(TempC, RepRate_Pi, type = "l", lwd = 3, col = "goldenrod1", xlab = "Temperature (C)", ylab = "Replication Rate", ylim = c(0,3))
lines(TempC, RepRate_Pj, lwd = 3, col = "springgreen4")
abline(h = c(mean(RepRate_Pi), mean(RepRate_Pj)), col = c("goldenrod1", "springgreen4"), lty = 2, lwd = 2)
abline(v = 25.5, col = "#FF99FF", lty = 1, lwd = 2)

#### ... Pj-Advantage, Version A TPCs ####
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 0.68; Tknot <- 11.3+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-10)+K; TzH <- 28+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 1.68; Tknot <- 26+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- 1+K; TzH <- 33+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}
TempK <- seq(0+K, 40+K, 0.1)
RepRate_Pi <- MTE_TPC_Pi(TempK); RepRate_Pj <- MTE_TPC_Pj(TempK)
TempC <- seq(0, 40, 0.1)
x <- as.data.frame(cbind(TempC, RepRate_Pi, RepRate_Pj))
plot(TempC, RepRate_Pi, type = "l", lwd = 3, col = "goldenrod1", xlab = "Temperature (C)", ylab = "Replication Rate", ylim = c(0,3))
lines(TempC, RepRate_Pj, lwd = 3, col = "springgreen4")
abline(h = c(mean(RepRate_Pi), mean(RepRate_Pj)), col = c("goldenrod1", "springgreen4"), lty = 2, lwd = 2)
abline(v = 28.0639755, col = "#FF99FF", lty = 1, lwd = 2)

#### ... Pj-Advantage, Version B TPCs ####
MTE_TPC_Pj <- function(TempK){
  lambda_knot <- 0.68; Tknot <- 11.3+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- (-10)+K; TzH <- 28+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pj)
}
MTE_TPC_Pi <- function(TempK){
  lambda_knot <- 1.68; Tknot <- 26+K; Ez <- 0.65; EzL <- (-6.2); EzH <- 6.2; TzL <- 1+K; TzH <- 33+K; k <- 8.62*10^(-5)
  r <- lambda_knot*
    (exp(-(Ez/k)*((1/TempK) - (1/Tknot))))*
    (1 + exp((EzL/k)*((1/TzL) - (1/TempK))) + exp((EzH/k)*((1/TzH) - (1/TempK))))^(-1)
  return(RepRate_Pi)
}
TempK <- seq(0+K, 40+K, 0.1)
RepRate_Pi <- MTE_TPC_Pi(TempK); RepRate_Pj <- MTE_TPC_Pj(TempK)
TempC <- seq(0, 40, 0.1)
x <- as.data.frame(cbind(TempC, RepRate_Pi, RepRate_Pj))
plot(TempC, RepRate_Pi, type = "l", lwd = 3, col = "goldenrod1", xlab = "Temperature (C)", ylab = "Replication Rate", ylim = c(0,3))
lines(TempC, RepRate_Pj, lwd = 3, col = "springgreen4")
abline(h = c(mean(RepRate_Pi), mean(RepRate_Pj)), col = c("goldenrod1", "springgreen4"), lty = 2, lwd = 2)
abline(v = 25.5, col = "#FF99FF", lty = 1, lwd = 2)

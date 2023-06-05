# EnvironmentalVariability_Repo
This repository contains scripts and data to replicate “Environmental variability can promote parasite diversity within hosts and transmission among hosts”. 

To replicate data, start with the “Data_Generation” folder. Please note that replicating these data will take a few weeks.

To replicate analyses with provided data (in the “Data” folder), clone this repository, load “EnvironmentalVariability_Repo.Rproj”, and run each script in the following order:

	⁃	1_ThermalPerformanceCurves.R
	⁃	2_Simulation_Analysis.R
	⁃	3_CompetitiveOutcomes_Analysis.R
	⁃	4_Rarity_Analysis.R
	⁃	5_Transmission_DemStoch_Sim-AUC.R
	⁃	6_Transmisson_SingleParasite_Sim.R
	⁃	7_Transmission_TwoParasite_AUC.R
	⁃	8_Transmission_Analysis.R
	⁃	9_NegRepRate.R

A guide to the “Data” folder:

	1.	Deterministic_Outputs
	  ⁃	Contains time series and persistence vectors (containing the number of time steps for which both parasites populations had abundances greater than one).
	2.	DemStoch_Outputs
	  ⁃	Contains time series and persistence vectors (containing the number of time steps for which both parasites populations had abundances greater than one).
	3.	EnvStoch-DemEnvStoch_PV_Outputs
	  ⁃	Does not contain time series, but does contain persistence vectors (containing the number of time steps for which both parasites populations had abundances greater than one).
	  ⁃	These time series data are very large, and are thus presently housed in the Federalised Research Data Repository (FRDR). Click [here] to access.



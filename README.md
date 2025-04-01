# EnvironmentalVariability_Repo
This repository contains scripts and data to replicate “**Environmental variability can prolong parasite co-occurrence within hosts and promote transmission among hosts**”, by Madeline Jarvis-Cross (madeline.jarvis.cross@mail.utoronto.ca) and Martin Krkošek (martin.krkosek@utoronto.ca). 

Here, we use mathematical models and simulations to ask how environmental variability may affect the viability and longevity of within-host parasite diversity and among-host transmission. 

---

There are three ways to replicate this work:

1. Re-generate data using scripts in the “`Data_Generation`” folder, and use resulting outputs (stored in "`Generated_Outputs`" folder) to run main scripts (`1_ThermalPerformanceCurves.R`, ..., `9_NegRepRate.R`).
	* This option will require you to make a couple adjustments to `3_CompetitiveOutcomes_Analysis.R` and `7_Transmission_TwoParasite_AUC.R`
2. Download data from [Federated Research Data Repository (FRDR)](https://www.frdr-dfdr.ca/repo/dataset/064c6978-3fb1-42ca-b373-365d771e96c7) (doi:10.20383/103.0764) into "`Data`" folder and run main scripts (`1_ThermalPerformanceCurves.R`, ..., `9_NegRepRate.R`).
3. Use data supplied in "`Data`" folder to run select main scripts (`1_ThermalPerformanceCurves.R`, `2_Simulation_Analysis.R`, `4_Rarity_Analysis.R`, `5_Transmission_DemStoch_Sim-AUC.R`, `6_Transmisson_SingleParasite_Sim.R`, `8_Transmission_Analysis.R`, `9_NegRepRate.R`), and view outputs from remaining main scripts (`3_CompetitiveOutcomes_Analysis.R`, `7_Transmission_TwoParasite_AUC.R`) in "`Outputs`" folder.

Regardless of how you choose to replicate the work, clone this repository, and load “`EnvironmentalVariability_Repo.Rproj`”. If you run any of the main scripts (listed below), please do so in numerical order.

* `1_ThermalPerformanceCurves.R`
* `2_Simulation_Analysis.R`
* `3_CompetitiveOutcomes_Analysis.R`
* `4_Rarity_Analysis.R`
* `5_Transmission_DemStoch_Sim-AUC.R`
* `6_Transmisson_SingleParasite_Sim.R`
* `7_Transmission_TwoParasite_AUC.R`
* `8_Transmission_Analysis.R`
* `9_NegRepRate.R`

A guide to the “`Data`” folder:

1. `Deterministic_Outputs`
	* Contains time series and persistence vectors (containing the number of time steps for which both parasites populations had abundances greater than one).
2. `DemStoch_Outputs`
	* Contains time series and persistence vectors (containing the number of time steps for which both parasites populations had abundances greater than one).
3. `EnvStoch-DemEnvStoch_PV_Outputs`
	* Does not contain time series, but does contain persistence vectors (containing the number of time steps for which both parasites populations had abundances greater than one).
		* These time series data files range between 6GB and 22GB, and are presently housed in the **[Federated Research Data Repository (FRDR)]**. Download the data into the `EnvStoch-DemEnvStoch_PV_Outputs` folder to use (`EnvironmentalVariability_Repo/Data/EnvStoch-DemEnvStoch_PV_Outputs`).
		
A guide to the "`Outputs`" folder:

1. `CompetitiveOutcomes_Outputs`
	* Contains outputs from main script 3 (`3_CompetitiveOutcomes_Analysis.R`)
2. `TotalTransmission_Outputs`
	* Contains outputs from main scripts 5, 6, and 7 (`5_Transmission_DemStoch_Sim-AUC.R`, `6_Transmisson_SingleParasite_Sim.R`, `7_Transmission_TwoParasite_AUC.R`)
3. `TotalTransmission_SummaryOutputs`
	* Contains outputs from main script 8 (`8_Transmission_Analysis.R`)
4. `NegativeRepRate_Outputs`
	* Contains outputs from main script 9 (`9_NegRepRate.R`)

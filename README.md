# A329-LucasBarataria
This repository contains code and data for the following manuscript: 
Stagg et al. A modeling framework for assessing the impact of wetland change on landscape-scale net ecosystem carbon balance. In prep.

## Dependencies
The scripts in this repository were built using SyncroSim v3.1.12, with the following SyncroSim packages: ST-Sim v4.3.8, using R v4.5.1 with the following R packages rsyncrosim v2.1.4, tidyverse v2.0.0, terra v1.8-54, sf v1.0-21, and viridis v0.6.5.

## Suggested Workflow
1. __Download SyncroSim:__ SyncroSim v3.1.12 can be downloaded at: https://syncrosim.com/download/.
2. __Install R and rsyncrosim:__ Install R, then install rsyncrosim v2.1.4 from GitHub using this tutorial: https://syncrosim.github.io/rsyncrosim/articles/a06_rsyncrosim_install_github.html.
3. __Update file paths:__ At the top of each script in the "Scripts" folder, update `rootPath`.
4. __Download spatial data__: These scripts rely on publicly available CCAP (2001-2016), LandFire EVT (2016 & 2001), NASA CMS stand age (https://doi.org/10.3334/ORNLDAAC/1829) and PRISM climate data (2000-2022). Once downloaded, update file paths in the Step 6 R script.
5. __Run all scripts__: Run each script in order from Step 1 to Step 13. An ST-Sim SyncroSim library will be saved in the "Models" folder. Model inputs and outputs can be viewed within the SyncroSim desktop by following Step 5 of this tutorial: https://docs.syncrosim.com/getting_started/quickstart.html.
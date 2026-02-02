## ExpBites: Analyze Human Exposure to Mosquito Bites
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15237615.svg)](https://doi.org/10.5281/zenodo.15237615)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ExpBites)](https://cran.r-project.org/package=ExpBites)

### Overview
**ExpBites** is an R package to compute mosquito biting exposure based on hourly human and mosquito behavior. It is designed for entomological modeling and malaria vector control analysis, particularly in the context of insecticide-treated net (ITN) usage. 

### Features 
- Simulate individual human behavior (outdoors, indoors awake, asleep, under bed net) over 24-hour cycles (for test purpose). 
- Simulate mosquito biting activity indoors and outdoors (for test purpose). 
- Estimate exposure to mosquito bites for bednet users and non-users. 
- Calculate proportions of exposure prevented by nets.
- Calculate proportions of exposure indoors or during specified time intervals.
- Visualize exposure and behavior patterns over time. 

### Installation 
You can install the released version of **ExpBites** from
[CRAN](https://CRAN.R-project.org) with :

```R
install.packages("ExpBites")
```
or the development version from GitHub: 

```R 
# install.packages("devtools")
devtools::install_github("Nmoiroux/ExpBites")
``` 

Make sure you have the required dependencies: 

```R 
install.packages(c("dplyr", "tidyr", "ggplot2", "Rdpack")) 
``` 

### Example 
```R 
library(ExpBites) 

# Generate simulated human and mosquito data (for test purpose)
df <- gen_df_human(n_individuals = 100) 
df_bites <- gen_df_mosquito() 

# Calculate hourly exposure to mosquito bites 
exp_result <- calculate_Exp(df, df_bites) 

# Summarize exposure for entire day and a specific time window (e.g., 22h to 5h) 
summary_tbl <- summarise_exposure(exp_result, interval = c(22, 5))

print(summary_tbl) 

# Plot exposure patterns 
plot_exposure(exp_result) 

# Plot mosquito and human behavior patterns 
plot_behaviors(exp_result) 
``` 

## When should I use ecoXCorr?

ecoXCorr is useful when:

- environmental drivers are expected to have delayed effects
- the relevant time scale of these effects is unknown
- you want a global view of lagged associations rather than testing a single lag

Typical applications include:

- vector ecology
- disease ecology
- environmental epidemiology
- climate–biology interactions

### References 
This package builds upon : 
- Curriero FC, Shone SM, Glass GE. (2005) *Cross correlation maps: a tool for visualizing and modeling time lagged associations.* [Vector Borne Zoonotic Dis.](https://doi.org/10.1089/vbz.2005.5.267)


### License 
This package is released under the [GPL-3 License](https://www.gnu.org/licenses/gpl-3.0-standalone.html). 

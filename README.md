## ExpBites: Simulate and Analyze Human Exposure to Mosquito Biting
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15237615.svg)](https://doi.org/10.5281/zenodo.15237615)

### Overview
**ExpBites** is an R package to simulate human and mosquito behavior over time and compute mosquito biting exposure. It is designed for entomological modeling and malaria vector control analysis, particularly in the context of insecticide-treated net (ITN) usage. 

### Features 
- Simulate individual human behavior (outdoors, indoors awake, asleep, under bednet) over 24-hour cycles. 
- Simulate mosquito biting activity indoors and outdoors. 
- Estimate exposure to mosquito bites for bednet users and non-users. 
- Calculate proportions of exposure prevented by nets.
- Calculate proportions of exposure indoors or during specified time intervals.
- Visualize exposure and behavior patterns over time. 

### Installation 
You can install the development version of **ExpBites** from GitHub: 

```R 
# install.packages("devtools")
devtools::install_github("yourusername/ExpBites")
``` 

Make sure you have the required dependencies: 

```R 
install.packages(c("dplyr", "tidyr", "ggplot2", "tibble")) 
``` 

### Example 
```R 
library(ExpBites) 

# Generate simulated human and mosquito data 
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

### References 
This package builds upon and extends the models described in: 
- Killeen et al. (2006). *Quantifying behavioural interactions between humans and mosquitoes...* [BMC Infectious Diseases](https://doi.org/10.1186/1471-2334-6-161) 
- Geissbühler et al. (2007). *Interdependence of domestic malaria prevention measures...* [Malaria Journal](https://doi.org/10.1186/1475-2875-6-126) 
- Moiroux et al. (2014). *Human exposure to early morning Anopheles biting...* [PLoS One](https://doi.org/10.1371/journal.pone.0104967) 

### License 
This package is released under the [GPL-3 License](https://www.gnu.org/licenses/gpl-3.0.html). 

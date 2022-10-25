# Global fishing between jurisdictions with unequal fisheries management

This repository contains the data and code to recreate the analyses in [Klein et al. 2022](https://iopscience.iop.org/article/10.1088/1748-9326/ac97ab) "Global fishing between jurisdictions with unequal fisheries management", published in Environmental Research Letters.


# Data

### FishedWhereV2Summary.csv

This is data from Watson (2017) and Watson and Tidd (2018) estimating the annual volume of fish caught by each fishing country in industrial and non-industrial fisheries.

### Management_data.csv

This is the management effectiveness score developed by Mora et al. (2009).

### FMI_values.csv

This is the Fisheries Management Index data from Melnychuk et al. (2017).

### Carissa1Oct.csv

This data shows the annual volume of fish traded between each country, which is used to present data in the Supplementary Information.


# Scripts

The scripts should be executed in the order designated in the naming structure (1-10). Outputs from the below scripts are saved in the *output_data* folder

- 1_Clean_data.Rmd
This script performs genreal cleaning functions on the data, such as reconciling country names, for further analysis.

- 2_Fishing_displacement.Rmd
This script calculates the amount of fisheries displacement (one country fishing in another country's waters) and unequal displacement (a country fishing in another country's waters that has lower management effectiveness using the data from Mora et al..

- 3_Trade_displacement.Rmd
This script does the same as above, except looking at trade between countries with different management effectiveness. The results for this analysis are showin the Supplementary Information of the publication.

- 4_Fishing_displacement_FMI.Rmd
This script calculates fishing displacement as in 2, but using the Fisheries Management Index from Melnychuk et al. (2017) instead of the Mora et al. data.

- 5_Fishing_displacement_FMI_wadj.Rmd
Same as the script above - but uses the weight adjusted Fisheries management index in Melnychuk et al. This value was not included in the final analysis or text of the manuscript, but is retained for reference.

- 6_Difference_Management_Scores.Rmd
This script calculates the difference between management effectiveness scores between displacing countries. A larger difference in management effectiveness was assumed to be more problematic than a smaller difference.

- 7_Highly_unequal_displacement.Rmd
This script was not used in the final version of the manuscript but is maintained for reference. It explores unequal fishing based on quartiles instead of absolute management effectiveness values in an effort to explore displacement between countries with substantial differences in management effectiveness.

- 8_Supplementary_Table1.Rmd
This script collates the data for Supplementary Table 1

- 9_Figure4_ME_vs_Unequal.Rmd
This script creates Figure 4A of the main text using the Mora data. The other figures in the manuscript were not created using R and are thus not in this repository.

- 10_Figure4_FMI_wadj_vs_Unequal.Rmd
This script creats Figure 4B of the main text using the Melnychuk data.

### Additional scripts

- IUU_fishing.Rmd
This script looks at the amount of IUU fishing that occured during the study period.


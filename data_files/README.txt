
This folder contains the files needed to reproduce the results reported in: 

Hahm, Hyeonho, David Hilpert, and Thomas König. 2022. "Divided We Unite: The Nature of Partyism and the Role of Coalition Partnership in Europe." American Political Science Review.



TABLE OF CONTENTS
Scripts: 
-1_data_preparation.R: To transform the raw data, adding analytical variables, including coalition experience. The script refers to helper_scripts/translations in order to harmonize games in multiple languages
	-input: stacked_raw_data.RData
	-output: aff_pol_df_analy_jan2023.RData
-2_analysis.R: To generate tables and figures included in the manuscript and the appendix
	-input: aff_pol_df_analy_jan2023.RData

At the beginning of each script, please specify the location of the "folder_for_dataverse" using the setwd() command




SYSTEM
iMac, macOS Catalina (version 10.15.7)

R version 3.6.1
R Studio 2022.02.3
libraries:
-lme4 (version 1.1-26)
-MASS (version 7.3-53)
-foreign (version 0.8-71)
-modelsummary (version 0.9.2)
-stargazer (version 5.2.2)
-readstata13 (version 0.9.2)
-tidyr (version 1.1.4)




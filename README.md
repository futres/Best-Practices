# Best Practices Paper
Data and Analyses used for the Best Practices Paper.
To access data, one needs to create an account on the CyVerse Discovery Environment (DE), which is free.

## Scripts Folder
This folder contains two scripts: data cleaning and data analysis.

### Data Cleaning
Original data is linked to the DE (see above).
This script runs the cleaning routine:
  1. Create labeling field, "measurementStatus"
  2. Summarize number of records for each species and each measurementType
  3. Label those species with fewer that 10 records are labeled "too few records" in measurementStatus
  4. For those with adult lifeStage and >10 records, run an outlier detection test
  5. Label outliers as "outliers" in measurement Status
  6. Calculate upper and lower limits, which are 3 standard deviations from the mean for those species with known adult lifeStage and >10 records
  7. Label records greater than the upper limit or lesser than lower limit as "outliers" in measurementStatus
  8. Label remaining records as "possibly good" in measurementStatus

Throughout data cleaning, data visualization plots are created and datasets are saved. This script created Figure 2.

### Data Analyses
Data used in analyses are linked to the DE (see above).
The data is trimmed to exclude known juveniles in lifeStage and suspected outliers in measurementStatus.
The script runs the following analyses:
  1. Calculate the difference between species-level mass values from <a href="https://figshare.com/collections/PanTHERIA_a_species-level_database_of_life_history_ecology_and_geography_of_extant_and_recently_extinct_mammals/3301274" PanTHERIA</a> and the FuTRES data. 
      - This is calculated as the difference between PanTHERIA and FuTRES mass divided by the standard error of mass values from FuTRES. 
      - We adjusted significant levels based on sample size using critical t-values. We also applied a Bonferroni correction.
      - Those greater than 3 standard errors different from FuTRES mass was considered significantly different. 
  2. Estimate mass values for zooarchaeological specimens for which a skeletal measurement is known.
     - This is done by creating a regression of skeletal element v. mass for which we have both measurements.
      - Using the variation in both the x and the y values, we create a more accurate estimation of a range of possible mass values for each specimen.

This script created Figures 3, 4, and supplemental figures.

## Importance
* Dynamic, individual-level data allows users to make informed decisions about quality of data and quality of species-level summaries based on sample size. 
* Additionally, knowing the variation in both the x and y axes for allometric relationships provides a more accurate assessment of mass and can later be used to infer a trait using a transfer funciton.

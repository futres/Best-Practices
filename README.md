# Best Practices Paper
Data and Analyses used for the Best Practices Paper.
To access data, one needs to create an account on the <a href="https://de.cyverse.org/de/">CyVerse Discovery Environment (DE)</a>, which is free.

## Data
### VertNet
The VertNet data was extracted from <a href="https://vertnet.org/">VertNet.org</a> on using <a href="https://github.com/rafelafrance/traiter_vertnet">traiter</a> in November 2021. The R package <i>traiter</i> was used to extract trait data. We kept only the first measurement value for each record. The VertNet data reside in the Data Commons <a href="https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/OriginalData/VertNet/all_mammals_2021-11-09a.csv">DOI</a>.

* inferred values (TRUE) mean that the value was converted to a standard unit (g, mm)
* the original unit was retained in the data under "verbatimUnit"
* estimated values are values where a decision was made about which part of the string was the measurement value

### Other datasets
There were six legacy datasets used for this project that are stored in <a href="https://geome-db.org/">GEOME</a> under the FuTRES Team:
* Project 277 (Aepyceros_Villseñor_Africa_Modern)
* Project 278 (Spermophilus.beecheyi_Blois_NorthAmerica_Modern)
* Project 282 (Florida Museum Environmental Archaeology Program Comparative Collection Data
* Project 294 (Puma.concolor_ODFW_OR_USA_Modern)
* Project 314 (Equid_Bernor_Global_Cenozoic)

The original data are stored in The DE. Validated data are stored in both GEOME and the DE.

### Processed data
Processed data are data that have been triplified and reasoned over. These data are extracted from the <a href="https://futres-data-interface.netlify.app/">FuTRES datastore</a> and are stored on the DE.

### Versioning
The VertNet data extraction before ingestion into FuTRES and the FuTRES data extraction from the FuTRES datastore are in the Data Commons under the DOI:.
This repository has also been versioned as scripts have been significantly modified.

### Data used in analyses
The data that results from Best_Practices_Data_Cleaning.R are stored on the DE and Data Commons.

## Scripts Folder
This folder contains two scripts: data cleaning and data analysis.

### <a href="https://cran.r-project.org/src/contrib/Archive/OutlierDetection/">Outlier Detection package</a>
This package became outdated and was removed from the R package site, but is archived. 
The Maha.R function is called to perform the outlier test.

### Data Cleaning
Original data is linked to the DE (see above).
This script runs the cleaning routine:
  1. Added "origin" column to denote who submitted data for the benefit of analyses in the <a href="https://github.com/futres/Best-Practices/blob/master/scripts/Best_Practices_Analyses.R">Data Analyses</a> script.
  2. Create labeling field, "measurementStatus"
  3. Summarize number of records for each species and each measurementType
  4. Label those species with fewer that 10 records are labeled "too few records" in measurementStatus
  5. For those with adult lifeStage and >10 records, run an outlier detection test
  6. Label outliers as "outliers" in measurement Status
  7. Calculate upper and lower limits, which are 3 standard deviations from the mean for those species with known adult lifeStage and >10 records
  8. Label records greater than the upper limit or lesser than lower limit as "outliers" in measurementStatus
  9. Label remaining records as "possibly good" in measurementStatus

Throughout data cleaning, data visualization plots are created and datasets are saved. This script created Figure 2 and Table 1.

### Data Analyses
Data used in analyses are linked to the DE (see above). We use a file called <a href="https://data.cyverse.org/dav-anon/iplant/home/rwalls/FuTRES_data/Projects/BestPracticesData/BPP.data.csv">"BPP.data.csv"</a>, which is the resulting data file from Data Cleaning script.
The data is trimmed to exclude known juveniles in lifeStage and suspected outliers in measurementStatus.
The script runs the following analyses:
  1. Calculate the difference between species-level mass values from <a href="https://figshare.com/collections/PanTHERIA_a_species-level_database_of_life_history_ecology_and_geography_of_extant_and_recently_extinct_mammals/3301274">PanTHERIA</a> and the FuTRES data. 
      - This is calculated as the difference between PanTHERIA and FuTRES mass divided by the standard error of mass values from FuTRES. 
      - We adjusted significant levels based on sample size using critical t-values. We also applied a Bonferroni correction.
      - Those greater than 3 standard errors different from FuTRES mass was considered significantly different. 
  2. Estimate mass values for zooarchaeological specimens for which a skeletal measurement is known.
     - This is done by creating a regression of skeletal element v. mass for which we have both measurements.
      - Using the variation in both the x and the y values, we create a more accurate estimation of a range of possible mass values for each specimen.

This script created Figures 3, 4, supplemental figures, Tables 2, 3, and supplemental tables.

## Importance
* Dynamic, individual-level data allows users to make informed decisions about quality of data and quality of species-level summaries based on sample size. 
* Additionally, knowing the variation in both the x and y axes for allometric relationships provides a more accurate assessment of mass and can later be used to infer a trait using a transfer funciton.

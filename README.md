# Replication Material

This repository hosts data and R scripts for the paper "[Agricultural Roots of Social Conflict in Southeast Asia](https://doi.org/10.1177/00223433241305990)," co-authored with Justin Hastings, published in the Journal of Peace Research.


## Data

The data used for generating the main results of the study are compiled into the [masterdata.RData](masterdata.RData) file. 

- The *conflict* data are from the Armed Conflict Location & Event Data Project (ACLED) available at: [http://acleddata.com](www.acleddata.com). For details, see: Raleigh, C., A. Linke, H. Hegre, and J. Karlsen (2010). Introducing ACLED: "An Armed Conflict Location and Event Dataset: Special Data Feature." Journal of Peace Research 47(5): 651–660.

- The rice *harvest* data are from the International Food and Policy Research Institute's MapSPAM 2010, obtained from Harvard Dataverse available at: [https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PRFF8V](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PRFF8V). For details, see: Yu, Q., You, L., Wood-Sichra, U., Ru, Y., Joglekar, A.K.B., Fritz, S., Xiong, W., Lu, M., Wu, W., and P., Yang. (2020). [A Cultivated Planet in 2010-Part 2: The Global Gridded Agricultural-Production Maps](https://essd.copernicus.org/articles/12/3545/2020/). Earth System Science Data, 12(4), 3545-3572.

- The harvest *calendar* data are from Sacks, W.J., D. Deryng, J.A. Foley, and N. Ramankutty (2010). Crop planting dates: an analysis of global patterns. Global Ecology and Biogeography 19, 607-620, available at: [https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset/netcdf-0-5-degree/](https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset/netcdf-0-5-degree/)
  
- The *weather* data are the NOAA/CPC Global Unified Gauge-Based Analysis of Daily Precipitation, available at [https://psl.noaa.gov/data/gridded/data.cpc.globalprecip.html](https://psl.noaa.gov/data/gridded/data.cpc.globalprecip.html). We use these data to test harvest-related mechanisms due to good vs bad crops seasons.

- The *cities* data are from the Simple Maps available at [https://simplemaps.com/data/world-cities](https://simplemaps.com/data/world-cities). We use these data to examine the treatment heterogeneity in urban vs rural cells.




## R scripts

### To compile the data

- [01-getacled.r](01-getacled.r): download and store conflict data
- [02-getrainfall.r](02-getrainfall.r): download and store rainfall data
- [03-getcalendar.r](03-getcalendar.r): download and store harvest calendar data
- [04-getspam.r](04-getspam.r): download and store rice harvest area data
- [05-combine.r](05-combine.r): combine the crop harvest and conflict datasets
- [06-masterdata.r](06-masterdata.r): some more data wrangling to finalize the dataset

The replication materials do not include the downloaded data. However, the links to the data and brief instructions on how to download and store them are provided in the corresponding R scripts.


### To replicate the results

- [11-descriptive.r](11-descriptive.r): Table 2, Figures 1-3, Appendix Figures B1-B2
- [12-mainresults.r](12-mainresults.r): Table 3, Figures 4-7, Appendix Tables A1-A2 and A4-A7, Appendix Figures B3-B4 and B6
- [13-specchart.r](13-specchart.r): Appendix Figure B5
- [14-smallres.r](14-smallres.r): Appendix Table A3*

*Note: Appendix Table A3 generates the regression results similar to the main results but using 0.5-degree cells (instead of 1.0-degree cells) as the geographic unit of observation. The dataset is compiled into [masterdata05.RData](masterdata05.RData) file using steps similar to that of the main dataset.


## License

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


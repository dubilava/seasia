# Replication Material

This repository hosts data and R codes for the paper "[Agricultural Shocks and Social Conflict in Southeast Asia](https://arxiv.org/abs/2304.10027)," co-authored with Justin Hastings.

## Data

The data used for generating the main results of the study are stored in the [masterdata.RData](masterdata.RData) file. This file compiles the data from the following sources:

- The *conflict* data are from the Armed Conflict Location & Event Data Project (ACLED) available at: [http://acleddata.com](www.acleddata.com). For details, see: Raleigh, C., A. Linke, H. Hegre, and J. Karlsen (2010). Introducing ACLED: "An Armed Conflict Location and Event Dataset: Special Data Feature." Journal of Peace Research 47(5): 651–660.

- The *rice harvest* data are from the International Food and Policy Research Institute's MapSPAM 2010, obtained from Harvard Dataverse available at: [https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PRFF8V](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PRFF8V). For details, see: Yu, Q., You, L., Wood-Sichra, U., Ru, Y., Joglekar, A.K.B., Fritz, S., Xiong, W., Lu, M., Wu, W., and P., Yang. (2020). [A Cultivated Planet in 2010-Part 2: The Global Gridded Agricultural-Production Maps](https://essd.copernicus.org/articles/12/3545/2020/). Earth System Science Data, 12(4), 3545-3572.

- The *harvest calendar* data are from Monfreda, C., N. Ramankutty, and J.A. Foley (2008), Farming the Planet: 2. Geographic Distribution of Crop Areas, Yields, Physiological Types, and Net Primary Production in the Year 2000, Global Biogeochemical Cycles, 22, GB1022 available at: [http://www.earthstat.org/harvested-area-yield-175-crops/](http://www.earthstat.org/harvested-area-yield-175-crops/)
  
- The *weather* data are the ERA5 reanalysis gridded data on monthly averaged total precipitation from European Centre for Medium-Range Weather Forecasts (ECMWF) Copernicus Project, available at [https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels). We use these data to test harvest-related mechanisms due to good vs bad crops seasons.

- The *cities* data are obtained from the Simple Maps available at [https://simplemaps.com/data/world-cities](https://simplemaps.com/data/world-cities). We use these data to examine the treatment heterogeneity in urban vs rural cells.




## R Codes

The R codes to replicate the study are as follows:

- [01-mainresults.r](01-mainresults.r): Tables 1-8, Appendix Tables B1-B2 and B5, Figure 1, and Appendix Figures A3-A4 and A6
- [02-altresults.r](03-altresults.r): Appendix Tables B3-B4.
- [03-figures.r](03-figures.r): Figures 1-4, and Appendix Figures A1-A2.
- [04-specchart.r](04-specchart.r): Appendix Figure A5.


## License

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


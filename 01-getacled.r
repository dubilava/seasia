library(data.table)
library(acled.api)

rm(list=ls())
gc()

# you will need to have registered to source the data using acled.api package in R 
# refer to the api user guide available at 
# https://www.acleddata.com/wp-content/uploads/dlm_uploads/2017/10/API-User-Guide.pdf

Sys.setenv(ACLED_EMAIL_ADDRESS="<your_email_address>")
Sys.setenv(ACLED_ACCESS_KEY="<your_access_key>")

acled_dt <- as.data.table(acled.api(region=9,start.date="2010-01-01",end.date="2023-12-31",add.variables=c("geo_precision","time_precision","longitude","latitude","notes")))

save(acled_dt,file="acled.RData")


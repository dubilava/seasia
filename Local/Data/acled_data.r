library(acled.api)
library(data.table)

Sys.setenv(EMAIL_ADDRESS="david.ubilava@sydney.edu.au")
Sys.setenv(ACCESS_KEY="YDLW2X*dpTs6aF1wlWq5")

# acled_df <- acled.api(region=9,add.variables=c("geo_precision","time_precision","longitude","latitude","notes"))
# 
# acled_dt <- as.data.table(acled_df)
# 
# save(acled_dt,file="acled_seasia.RData")


acled_df <- acled.api(start.date = "2015-01-01",end.date = "2022-12-31",add.variables=c("geo_precision","time_precision","longitude","latitude"))

acled_dt <- as.data.table(acled_df)

save(acled_dt,file="acled_global_recent.RData")


# Snowbedo
Package for modeling snowmelt and streamflow in semi-arid, snowpack-driven mountainous watersheds.

This package utilizes  streamflow, temperature data, precipitation, shortwave incoming (downwelling) radiation, and white-sky albedo 
to calibrate a streamflow model based on a simplified snowmelt-based process that is influenced by albedo. This calibrated model can 
then be used to explore various scenarios of albedo (which may be influenced by dust and black carbon deposition).


## Functions
### Formatting data
- limitperiod.R (limits data to specific time period)
- dissipate.R (calculates daily precipitation from accumulative precipitation)

### Modeling streamflow
- modelflow.R (calculates flow based on meteorological and remotely sensed land cover parameters)

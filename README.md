# Snowbedo
Package for modeling snowmelt and streamflow in semi-arid, snowpack-driven mountainous watersheds.

This package utilizes  streamflow, temperature data, precipitation, shortwave incoming (downwelling) radiation, and white-sky albedo 
to calibrate a streamflow model based on a simplified snowmelt-based process that is influenced by albedo. This calibrated model can 
then be used to explore various scenarios of albedo (which may be influenced by dust and black carbon deposition).


## Functions included in the Package
### Formatting data
- limitperiod (limits data to specific time period)
- dissipate (calculates daily precipitation from accumulative precipitation)

### Modeling streamflow
- modelflow (calculates flow based on meteorological and remotely sensed land cover parameters). Note that this function is still under development, and has not produced acceptable streamflow results.


## Scripts for Using the Snowbedo Package
- NeuralNetwork.R (reads in the formatted data for an individual watershed, subsets based on user-defined list of parameters, and trains a neural network model for streamflow. Produces a time series of modeled streamflow and saves a file of the neural network model)
- ModelDifferencesInAlbedo.R (reads in a formatted dataset and neural network model (produced by NeuralNetwork.R) and creates different scenarios by adjusting the albedo. Produces time series of streamflow for each of the scenarios)

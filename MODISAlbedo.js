var imageCollection = ee.ImageCollection("MODIS/MOD10A1");

//Load Watershed
var watersheds=ee.FeatureCollection('ft:1Xeary6JY-QxOG1y7rmtESB9v4-OtNj8CkL-kjRBK');
var watershed = watersheds.filter(ee.Filter.eq('name', 'Upper Emigration Creek'));

//Filter Dates
var imagefiltercollection=imageCollection
.filterDate('2005-1-1','2010-1-1');


//Mask all albedo data >100 (to exclude clouds or other non-snow data)
var maskNonSnow = function(image) {
  //Look at the Snow Albedo Score. (0-100) is the snow albedo
  var score = image.select('Snow_Albedo_Daily_Tile');
  //Mask any non-albedo values and clip to the extent of the watersheds
  return image.updateMask(score.lte(100)).clip(watershed);
};

//Apply function to create mask of non-snowdata over the time series
var maskedImages = imagefiltercollection.map(maskNonSnow);
//Select the albedo "band" of albedo values
var maskedAlbedo = maskedImages.select('Snow_Albedo_Daily_Tile');

//Calculate mean albedo in each of the watersheds
var meanreduce = function(img) {
  return img.reduceRegions({
    collection:watersheds,
    reducer: ee.Reducer.mean(),
    scale:500
  })
};
//Apply function to the masked layer
var mean_albedo=maskedAlbedo.map(meanreduce);
Export.table.toDrive(mean_albedo.flatten(), 'WatershedMeanSnowAlbedoTest','GeoJSON')

Map.addLayer(watershed);

var myTimeSeries=
Chart.image.series(maskedAlbedo,watershed,ee.Reducer.mean(),500);
print(myTimeSeries);
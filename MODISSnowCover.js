var watersheds = ee.FeatureCollection('ft:1Xeary6JY-QxOG1y7rmtESB9v4-OtNj8CkL-kjRBK');
var watershed = watersheds.filter(ee.Filter.eq('name', 'Upper City Creek'));
var snow = ee.ImageCollection('MODIS/MYD10A1')
  .filterDate('2002-12-29','2002-12-30')
  .filterBounds(watershed)

Map.centerObject(watershed);
Map.addLayer(watershed);

//Mask all data that is non-snow (anything with fractional snow > 100
var maskNonSnow = function(image) {
  //Look at the Fractional Snow Cover Score (0-100) is snow
  var score = image.select('Fractional_Snow_Cover');
  //Mask any non-albedo values and clip to the extent of the watersheds
  return image.updateMask(score.lte(100)).clip(watershed);
};

//Apply function to create mask of non-snowdata over the time series
var maskedImages = snow.map(maskNonSnow);

//Select the snowcover "band" of albedo values
var maskedSnow = maskedImages.select('Fractional_Snow_Cover');
Map.addLayer(maskedImages);
//Calculate mean snow cover in each of the watersheds
var meanreduce = function(img) {
  return img.reduceRegions({
    collection:watersheds,
    reducer: ee.Reducer.mean(),
    scale:500
  })
};

//Apply function to the masked layer
var mean_snow=maskedSnow.map(meanreduce);

var mcTimeSeries=Chart.image.series(maskedSnow,watershed,ee.Reducer.mean(),500);
print(mcTimeSeries);


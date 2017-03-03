var imageCollection = ee.ImageCollection("MODIS/MOD10A1");
var watersheds=ee.FeatureCollection('ft:1Xeary6JY-QxOG1y7rmtESB9v4-OtNj8CkL-kjRBK');
var watershed = watersheds.filter(ee.Filter.eq('name', 'Upper Big Cottonwood'));
Map.addLayer(watershed);
var images=imageCollection.filterDate('2000-1-1','2015-1-1').select('Snow_Albedo_Daily_Tile');

//Calculate mean cloud cover in each of the watersheds
var cloudcover = images.map(function(image){
  var imagefmask=image.select('Snow_Albedo_Daily_Tile');
  var histogram = imagefmask.reduceRegion({
        reducer: ee.Reducer.frequencyHistogram(),
        geometry: watershed,
        scale: 500
      })

    
  return ee.Feature(null).set(histogram.get('Snow_Albedo_Daily_Tile'))
})

var cloudcoverlist=cloudcover.toList(365);
//print(cloudcoverlist);

Export.table.toDrive(cloudcover);

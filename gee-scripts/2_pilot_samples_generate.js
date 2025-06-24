/**
 * This script processes generates a pilot sample of 1000 locations for verification and parameter testing
 * 
 * 1. Import the pre-exported pilot map of Dynamic World built probability scores
 * 2. Stratify a sample of points within pixels with positive trends
 * 3. Export to Drive to analyse in R
 * 
 * Author: Zander Venter
 */

// Import the intial land take map
var greyTrend = ee.ImageCollection('projects/gee-zander-nina/assets/Arena/grey_trend_2018_2023_v1')
var aoi = greyTrend.geometry().dissolve();
var proj = greyTrend.first().projection();
greyTrend = greyTrend.mosaic();

// define loss threshold - want all values above zero
var lossTresh = 0;

var losses = greyTrend.gt(lossTresh).selfMask()

Map.addLayer(greyTrend, {min:-10, max:10, palette:['#d13328', 'white', '#00d6d5']}, 'trend',0)
Map.addLayer(losses, {palette:['#00d6d5']}, 'loss', 0)

// Define function to get stratified sample

function getStratSample_simple(stratImage, aoi, stratList, sampleSizeList, exportLabel, scale, seed1, seed2){
  
  var sample = stratImage.rename('LC').stratifiedSample({
     seed: seed1,
     numPoints: 0, // 0 points for pixel values not in 'allocation'
     region: aoi,
     classBand: 'LC', // class band name
     classValues: stratList, // pixel values
     classPoints: sampleSizeList, // sample allocation
     tileScale: 2,
     projection: proj,
     scale: scale, 
     geometries: true
  })
  
  sample = sample.randomColumn('PLOTID',seed2)
  sample = sample.map(function(ft){
    var id = ee.String('id_').cat(ee.String(ee.Number(ft.get('PLOTID')).multiply(ee.Number(1e8)).round().int()))
    var lon = ee.Number(ee.List(ft.geometry().coordinates()).get(0)).multiply(100000).round().divide(100000)
    var lat = ee.Number(ee.List(ft.geometry().coordinates()).get(1)).multiply(100000).round().divide(100000)
    return ft
      .set(
        'PLOTID', id,
        'batch', exportLabel,
        'LONGITUDE', lon,
        'LATITUDE', lat,
        'CoordString', ee.String(lat).cat(', ').cat(ee.String(lon)))
  })
  //print(sample.limit(10))
  //print(sample.size())
  //Map.addLayer(sample.style({color:'red'}), {}, 'sample')
  
  Export.table.toAsset({
    collection: sample.sort('PLOTID'), 
    assetId: 'Arena/Sampling/' + exportLabel ,
    description: exportLabel
  })
  
  //Export.table.toDrive({
  //  collection: sample.sort('PLOTID'), 
  //  fileFormat: 'CSV' ,
  //  description: exportLabel
  //})
  
}

// Define strata image
var stratImage = losses;

// generate and export samples
getStratSample_simple(stratImage, aoi, [1], [1000], 'pilot_samples', 10, 123, 456)




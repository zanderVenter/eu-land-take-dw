/**
 * This script processes Dynamic World to calculate linear trend in built-up probability scores over EU
 * 
 * 1. Import the export grid from 'export_grid_generate.js'
 * 2. Define start and end years and months
 * 3. Use the mask from World Cover to mask out ocean areas further than 20km from shoreline
 * 4. Define function to get trend values in Dynamic World built probability scores
 * 5. Iterate over grid, apply function and generate export tasks
 * 
 * Author: Zander Venter
 */

/***
 * Main imports ----------------------------------------------------------------------------------
 */


var projCrs = 'EPSG:3035'

var grid = ee.FeatureCollection('projects/nina/Arena/export_grid')
print(grid.limit(10))
print(grid.size())
print(grid.distinct(['CellCode']).size())
Map.addLayer(grid, {}, 'grid', 0);

// Define the start and end year
var startYear = 2018;
var endYear = 2023;
// Define the months of year to consider - selecting all months
var startMonth = 1;
var endMonth = 12;

// Define mask
var wc = ee.ImageCollection("ESA/WorldCover/v200").mosaic();
var mask = wc.gte(0);
Map.addLayer(mask, {}, 'mask', 0)


// Bring in Dynamic World
var dwRaw = ee.ImageCollection("GOOGLE/DYNAMICWORLD/V1")
  .filter(ee.Filter.calendarRange(startMonth,endMonth,'month'));


/*
  // Define functions /////////////////////////////////////////////////////////////
*/

// Function to get the trend in built-up class and perform masking
function getTrendImg(aoi){
  
  // Filter to area of interest
  var dw = dwRaw.filterBounds(aoi).select(['built']);
  
  // Calculte the linear trend
  var trend = getLinearCoeff(dw);
  trend = trend.multiply(100).round().int()
  
  return trend
  
}

function getLinearCoeff(collection) {
  var start = ee.Date(ee.Image(collection.first()).get('system:time_start')).get('year')
  collection = collection.map(function(img){
     var year = ee.Date(img.get('system:time_start')).get('year').subtract(ee.Number(start))
    return ee.Image(year).byte().addBands(img).set('system:time_start', img.get('system:time_start'))
  });
  var trend = collection.reduce(ee.Reducer.linearFit()).select('scale');
  return trend
}


/*
  // Generate exports /////////////////////////////////////////////////////////////
*/

var list= grid.toList(1000);

for (var i = 0; i<1; i++){
  var cellID = ee.Feature(list.get(i)).get('CellCode').getInfo() 
  var aoi = ee.Feature(list.get(i)).geometry();
  //Map.addLayer(aoi)
  //Map.centerObject(aoi)
  
  var trend = getTrendImg(aoi).set('cellID', cellID);
  trend = trend.updateMask(mask);
  //Map.addLayer(trend.where(trend.lte(0), 0), {min:0, max:10, palette:['000000','ff80f7']}, 'built prob trend',0);
  
  
  Export.image.toAsset({
    image: trend.clip(aoi),
    region: aoi,
    description: 'ID_' + String(cellID),
    assetId: 'Arena/grey_trend_2018_2023_v2/ID_' + String(cellID),
    scale: 10,
    crs: projCrs,
    maxPixels:1e11
  });
}

Map.onClick(function(coords){
  
  var livingatlasURL = 'https://livingatlas.arcgis.com/wayback/#active=25982&mapCenter='+String(coords.lon) +'%2C' +String(coords.lat) +'%2C17'
  print(livingatlasURL)
  
  var pt = ee.Geometry.Point(coords.lon, coords.lat)
  var ptTrans = pt.transform('EPSG:32633').coordinates()
  
  ptTrans.evaluate(function(coordsTrans){
    var norgeibilderURL = 'https://norgeibilder.no/?x=' + String(coordsTrans[0]) + '&y=' + String(coordsTrans[1]) + '&level=14&utm=33&projects=&layers=&plannedOmlop=0&plannedGeovekst=0'
    print(norgeibilderURL)
    
  })
})

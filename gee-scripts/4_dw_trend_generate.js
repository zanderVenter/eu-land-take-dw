/**
 * This script processes Dynamic World to calculate linear trend in built-up probability scores over EU
 * 
 * 1. Import the export grid with months generated from 'dw_months_select.R'
 * 2. Define start and end years and months - will be done iteratively in for loop
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

var grid = ee.FeatureCollection('projects/nina/Arena/export_grid_with_monthsets')
Map.addLayer(grid, {}, 'grid', 0);

// Define the start and end year
var startYear = 2018;
var endYear = 2023;

// Define mask
var wc = ee.ImageCollection("ESA/WorldCover/v200").mosaic();
var mask = wc.gte(0);
Map.addLayer(mask, {}, 'mask', 0)


// Bring in Dynamic World
var dwRaw = ee.ImageCollection("GOOGLE/DYNAMICWORLD/V1")
  .filter(ee.Filter.calendarRange(startYear,endYear,'year'));

/*
  // Define functions /////////////////////////////////////////////////////////////
*/

// Function to get the trend in built-up class and perform masking
function getTrendImg(aoi, start_month, end_month, includeBare){
  
  // Filter to area of interest
  var dw = dwRaw.filterBounds(aoi).filter(ee.Filter.calendarRange(start_month, end_month, 'month'))
  
  var dwToTrend;
  
  if (includeBare){
    dwToTrend = dw.map(changeTypology);
  } else {
    dwToTrend = dw.select(['built']);
  }
  
  // Calculte the linear trend
  var trend = getLinearCoeff(dwToTrend);
  trend = trend.multiply(100).round().int()
  
  return trend
  
}

// Function to get linear trend slope for each pixel
function getLinearCoeff(collection) {
  var start = ee.Date(ee.Image(collection.first()).get('system:time_start')).get('year')
  collection = collection.map(function(img){
     var year = ee.Date(img.get('system:time_start')).get('year').subtract(ee.Number(start))
    return ee.Image(year).byte().addBands(img).set('system:time_start', img.get('system:time_start'))
  });
  var trend = collection.reduce(ee.Reducer.linearFit()).select('scale');
  return trend
}

// Helper function to add built and bare probability scores if that is desirable
function changeTypology(img){
    var grey = ee.Image(img.select('built').add(img.select('bare'))).rename('built');
    return grey.copyProperties(img, img.propertyNames())
}


/*
  // Generate exports /////////////////////////////////////////////////////////////
*/

var list= grid.toList(1000);

var list= grid.reduceColumns(ee.Reducer.toList(), ['CellCode']).get('list').evaluate(function(list){
  print(list)
  
  for (var i = 947; i<947; i++){
    var cellID = list[i];
    var feature = grid.filter(ee.Filter.eq('CellCode', cellID)).first();
    var aoi = feature.geometry();
    //Map.addLayer(aoi)
    //Map.centerObject(aoi)
    
    var start_month = feature.get("start");
    var end_month = feature.get("end");
    
    var trend = getTrendImg(aoi, start_month, end_month, false).set('cellID', cellID);
    trend = trend.updateMask(mask);
    //Map.addLayer(trend.where(trend.lte(0), 0), {min:0, max:10, palette:['000000','ff80f7']}, 'built prob trend',0);
    
    
    Export.image.toAsset({
      image: trend.clip(aoi),
      region: aoi,
      description: 'ID_' + String(cellID),
      assetId: 'Arena/grey_trend_2018_2023_v3/ID_' + String(cellID),
      scale: 10,
      crs: projCrs,
      maxPixels:1e11
    });
  }
  
})



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


/**
 * This script extracts full Dynamic World built probability score time series for parameter testing in R
 * 
 * 1. Import the samples generated by previous script
 * 2. Extract Dynamic World time series for each point
 * 3. Export to Drive to analyse in R
 * 
 * Author: Zander Venter
 */

//// Extract Dynamic World time series for sample locations
var samples = ee.FeatureCollection('projects/nina/Arena/Sampling/pilot_sample')
print(samples.size())

// Define the start and end year
var startYear = 2017;
var endYear = 2024;

// Bring in Dynamic World
var dwRaw = ee.ImageCollection("GOOGLE/DYNAMICWORLD/V1")
  .filterBounds(geometry)
  .filter(ee.Filter.calendarRange(startYear,endYear,'year'))
  .select(['built', 'bare']);

var tableOut = samples.map(function(ft){
  
  var redCol = dwRaw.filterBounds(ft.geometry()).map(function(img){
    var red = img.sampleRegions({
      collection: ee.FeatureCollection([ft]), 
      scale:10, 
      geometries:false
    })
    return red.map(function(x){return x.set('date', img.date())})
  })
  
  return redCol.flatten()
}).flatten().select(['date', 'PLOTID', 'built', 'bare']);
print(tableOut.limit(10))

Export.table.toDrive({
  collection: tableOut,
  description: 'pilot_dw_timeseries',
  fileFormat:'CSV'
})


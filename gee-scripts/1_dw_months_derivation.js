/**
 * This script extracts monthly Dynamic World built probability scores for stratifeid sample of points
 * 
 * 1. Import the grid used for iterating exports
 * 2. Define urban mask for stratification of points
 * 3. Stratify points
 * 4. Extract Dynamic World time series for each point
 * 3. Export to Drive to analyse in R
 * 
 * Author: Zander Venter
 */

// Load the grid to iterate over and sort by 'CellCode'
var grid = ee.FeatureCollection('projects/nina/Arena/export_grid')
print("Grid size:", grid.size())

// Define urban mask used to stratify random samples for deriving DW built probability scores
var wc = ee.ImageCollection("ESA/WorldCover/v100").mosaic()
var proj = ee.ImageCollection("ESA/WorldCover/v100").first().projection()
var urban_mask = wc.eq(50).selfMask()

// Define the start and end year for filtering DW
var start_year = 2020
var end_year = 2022

// Load Dynamic World data for the selected years
var dw_raw = ee.ImageCollection("GOOGLE/DYNAMICWORLD/V1").filter(
    ee.Filter.calendarRange(start_year, end_year, "year")
).select(["built"]);

// Define a stratified sampling function
function getStratSamlpe(stratImage, aoi, stratList, sampleSizeList, scale, proj, seed1){
  
  var sample = stratImage.rename('stratum')
    .reproject(proj.atScale(scale))
    .stratifiedSample({
     seed: seed1,
     numPoints: 0, // 0 points for pixel values not in 'allocation'
     region: aoi,
     classBand: 'stratum', // class band name
     classValues: stratList, // pixel values
     classPoints: sampleSizeList, // sample allocation
     tileScale: 2,
     projection: proj,
     scale: scale, 
     geometries: true
  })
  
  sample = sample.map(function(ft){
    var lon = ee.Number(ee.List(ft.geometry().coordinates()).get(0)).multiply(1e7).round().divide(1e7)
    var lat = ee.Number(ee.List(ft.geometry().coordinates()).get(1)).multiply(1e7).round().divide(1e7)
    return ft
      .set(
        'PLOTID', ee.String('a').cat(ee.String(lon).replace('\\.', '-')).cat(ee.String('_')).cat(ee.String(lat).replace('\\.', '-')),
        'LONGITUDE', lon,
        'LATITUDE', lat
        )
  })
  
  return sample.sort('PLOTID')
  
}

// Loop over the grid and export
var list= grid.reduceColumns(ee.Reducer.toList(), ['CellCode']).get('list').evaluate(function(list){
  print(list)
  
  for (var i = 0; i<1; i++){
    var id = list[i]
    var aoi = grid.filter(ee.Filter.eq('CellCode', id)).geometry();

    //Clip the urban mask to the AOI
    var strata_img_grid = urban_mask.clip(aoi)

    //Generate stratified samples
    var strat_samples = getStratSamlpe(strata_img_grid, aoi, [1], [30], 10, proj, 123)

    // Process Dynamic World time series
    var timeseries = dw_raw.filterBounds(aoi).map(function(img){
      var red = img.reduceRegions({
        collection: strat_samples,
        reducer: ee.Reducer.first(),
        scale: 10
      }).filter(ee.Filter.notNull(["first"]))
      red = red.map(function(ft){return ft.set("date", img.date().format("YYYY-MM-dd")).setGeometry(null)})
      return red
    }).flatten()
    //print(timeseries.limit(100))
    
    Export.table.toDrive({
      collection: timeseries.select(["PLOTID", "date", "first"]),
      fileFormat: 'CSV',
      description: 'dw_urban_timeseries_' + String(id),
      folder: 'dw_urban_timeseries'
    })
    
  }
})








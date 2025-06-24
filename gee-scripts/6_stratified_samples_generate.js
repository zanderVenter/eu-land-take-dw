/**
 * This script generates a stratified samples of land take X country X cropland/nature
 * 
 * 1. Define countries to iterate over
 * 2. Define strata image which is a combination of CLC+ 2018 and DW land take from DW built trend scores
 * 3. Get stratified samples per country using grid iteration and export to to Drive -> R
 * 
 * Author: Zander Venter
 */
 
/***
 * 1. Define countries which we will iterate overa------------------------------------+-------------------------------
 */

//// Countries ---------------------------------------
var countries = ee.FeatureCollection('users/zandersamuel/Global_misc/GISCO_CNT_RG_01M_2024');
Map.addLayer(countries, {}, 'countries raw', 0)
var selectedEEA = [
    "ALB",
    "AUT",
    "BEL",
    "BIH",
    "BGR",
    "CZE",
    "CYP",
    "DEU",
    "DNK",
    "ESP",
    "EST",
    "FIN",
    "FRA",
    "GBR",
    "GRC",  // "GRE", "GRC", // some datasets have either ISO code
    "HRV",
    "HUN",
    "IRL",
    "ITA",
    "ISL",
    "LIE",
    "LTU",
    "LUX",
    "LVA",
    "MKD",
    "MLT",
    "MNE",
    "NLD",
    "NOR",
    "POL",
    "PRT",
    "ROU",
    "SRB",
    "SVK",
    "SVN",
    "SWE",
    "CHE",
    "TUR",
    "XKX",
    "CHE"]
// Define a geometry that excludes grid cells in South America and Africa
var excludeArea = /* color: #98ff00 */ee.Geometry({
      "type": "GeometryCollection",
      "geometries": [
        {
          "type": "Polygon",
          "coordinates": [
            [
              [
                1.7527057855936556,
                81.31775845273751
              ],
              [
                3.3347370355936556,
                73.74409530440026
              ],
              [
                38.315205785593655,
                72.88632759705337
              ],
              [
                40.776143285593655,
                79.53999751306542
              ],
              [
                39.369893285593655,
                81.31775845273751
              ]
            ]
          ],
          "evenOdd": true
        },
        {
          "type": "Polygon",
          "coordinates": [
            [
              [
                -63.50510371029064,
                24.63642145462176
              ],
              [
                -74.75510371029064,
                11.29627646126898
              ],
              [
                -54.36447871029064,
                -2.1644626175286628
              ],
              [
                -46.45432246029064,
                0.12017854256283018
              ],
              [
                -46.98166621029064,
                6.609377574331028
              ]
            ]
          ],
          "evenOdd": true
        },
        {
          "type": "Polygon",
          "coordinates": [
            [
              [
                42.83377898436505,
                -7.730745165510744
              ],
              [
                42.83377898436505,
                -15.980493877672831
              ],
              [
                56.19315398436504,
                -24.385270757831574
              ],
              [
                59.88456023436504,
                -20.15718470553273
              ]
            ]
          ],
          "geodesic": true,
          "evenOdd": true
        }
      ],
      "coordinates": []
    });
countries = countries.filter(ee.Filter.inList('ISO3_CODE', selectedEEA))
countries = countries.map(function(ft){return ft.difference(excludeArea)})
Map.addLayer(countries, {}, 'countries filtered', 0)

/*
Export.table.toDrive({
  collection: countries,
  fileFormat: 'GeoJSON',
  description: 'countries',
})
*/


/***
 * 2. Define strata image -------------------------------------------------------------------
 */

var greyTrend = ee.Image('projects/nina/Arena/grey_trend_2018_2023_v3');
var proj =  greyTrend.projection();

var lossTreshLow = 3;
var lossTreshOptimal = 5;

var losses = greyTrend.gt(lossTreshOptimal).selfMask()

Map.addLayer(greyTrend, {min:-10, max:10, palette:['#d13328', 'white', '#00d6d5']}, 'trend',0)
Map.addLayer(losses, {palette:['#e8ea1e']}, 'loss', 0)

// Define land mask
var wc = ee.ImageCollection("ESA/WorldCover/v200").mosaic();
var landMask = wc.neq(80);
Map.addLayer(landMask, {}, 'landMask', 0)

// CLC+ backbone for 2018 baseline
var lcVizDict = {
  "names": [
    "Sealed", //1
    "Woody", //2
    "Low-growing woody", //3
    "Permanent herbaceous",//4
    "Periodically herbaceous", //5
    "Sparesely vegetated", //6
    "Water", //7
  ],
  "colors": [
    "#CC0303", 
    "#235123",
    "#B76124", 
    "#92AF1F",
    "#CDB400", 
    "#F7E174",
    "#2019A4",
  ]};

var clcplus = ee.Image('projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1');
clcplus = clcplus.remap(
  [1,2,3,4,5,6,7,8,9,10,11],
  [1,2,2,2,3,4,5,6,6, 7, 7]);
Map.addLayer(clcplus, {min:1, max:7, palette: lcVizDict.colors}, 'clcplus', 0);

var nature = clcplus.neq(5).and(clcplus.neq(1));
Map.addLayer(nature, {min:0, max:1}, 'nature', 0);

var cropland = clcplus.eq(5)
Map.addLayer(cropland, {min:0, max:1}, 'cropland', 0);

var built = clcplus.eq(1)
Map.addLayer(built, {min:0, max:1}, 'built', 0);

// Create strata image for generating samples - buffer included as single stratum
var strataImg = ee.Image(0)
  .where(nature, 1) // stable nature
  .where(built.or(cropland), 2) // stable other
  .where(greyTrend.gt(lossTreshLow).and(nature), 3) // loss buffer - nature
  .where(greyTrend.gt(lossTreshLow).and(nature.eq(0)), 4) // loss buffer - other
  .where(greyTrend.gt(lossTreshOptimal).and(nature), 5) // loss - nature
  .where(greyTrend.gt(lossTreshOptimal).and(cropland), 6) // loss - cropland
// Apply land mask
strataImg = strataImg.updateMask(landMask).selfMask();
Map.addLayer(strataImg, {min:1, max:6, palette:['white', 'grey', 'orange', 'red', 'black', 'pink']}, 'strataImg', 0)



/***
 * 3. Get strat samples and export to Drive -> R ------------------------------------------------------
 */

// Function to get stratified sample

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
    var lon = ee.Number(ee.List(ft.geometry().coordinates()).get(0)).multiply(1e14).round().divide(1e14)
    var lat = ee.Number(ee.List(ft.geometry().coordinates()).get(1)).multiply(1e14).round().divide(1e14)
    return ft
      .set(
        'PLOTID', ee.String('a').cat(ee.String(lon).replace('\\.', '-')).cat(ee.String('_')).cat(ee.String(lat).replace('\\.', '-')),
        'LONGITUDE', lon,
        'LATITUDE', lat
        )
  })
  
  return sample.sort('PLOTID')
  
}


var list= countries.reduceColumns(ee.Reducer.toList(), ['ISO3_CODE']).get('list').evaluate(function(list){
  print(list)
  
  //list = [41473908]
  
  for (var i = 0; i<1; i++){
    var id = list[i]
    var aoi = countries.filter(ee.Filter.eq('ISO3_CODE', id)).geometry();
    
    var strataImgGrid = strataImg.clip(aoi)
    
    var proj = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED").filterBounds(aoi).first().select(1).projection()
    
    //Need to fill in sample allocations from R script below - replacing with 150 as dummy
    var stratSamples =  getStratSamlpe(strataImgGrid, aoi, [1,2,3,4,5,6], [150,150,150,150,150,150], 10, proj, 123)
    
    stratSamples = stratSamples.map(function(ft){
      return ft.set('country', id)
    })
    
    Export.table.toDrive({
      collection: stratSamples,
      fileFormat: 'GeoJSON',
      description: 'samples_' + String(id),
      folder: 'sample_countries'
    })
    
  }
})


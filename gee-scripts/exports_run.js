 /**
 * This script vectorizes and exports built-up expansion areas from the 'de_trend_generate.js' script
 * 
 * 1. Import the built trends grid from 'de_trend_generate.js'
 * 2. Define countries which we will iterate over
 * 3. Import ancilliary data layers for defining strata of interest:
 *    - clc+ and ELC10 for definining baseline 2018 land cover
 *    - wetland layer
 *    - protected area status
 *    - conservation biodiversity, carbon and water priority
 * 4. Iterate over countries, vectorize, and export
 * 
 * Author: Zander Venter
 */
 
/***
 * Main imports ----------------------------------------------------------------------------------
 */

var greyTrend = ee.ImageCollection('projects/gee-zander-nina/assets/Arena/grey_trend_2018_2023_v2').mosaic();

var lossTresh = 5;

var losses = greyTrend.gt(lossTresh).selfMask()

Map.addLayer(greyTrend, {min:-10, max:10, palette:['#d13328', 'white', '#00d6d5']}, 'trend',0)
Map.addLayer(losses, {palette:['#00d6d5']}, 'loss', 0)


// Countries = EEA 30 
var selectedEEA = [
    "AUT",
    "BEL",
    "BGR",
    "CZE",
    "DEU",
    "DNK",
    "ESP",
    "EST",
    "FIN",
    "FRA",
    "GBR",
    "GRE", "GRC", // some datasets have either ISO code
    "HRV",
    "HUN",
    "IRL",
    "ITA",
    "ISL",
    "LIE",
    "LTU",
    "LUX",
    "LVA",
    "MLT",
    "NLD",
    "NOR",
    "POL",
    "PRT",
    "ROU",
    "SVK",
    "SVN",
    "SWE"]

// Selected extra - for journalistic interest
var extra = [
    "CHE",
    "TUR",
    "UKR",
  ]

// Join both list for spatial grid
var selectedISO = selectedEEA.concat(extra)

// Import a detalied geometry - for exports later
var countries = ee.FeatureCollection('projects/earthengine-legacy/assets/projects/sat-io/open-datasets/geoboundaries/HPSCGS-ADM0');
countries = countries.filter(ee.Filter.inList('shapeISO', selectedISO))
Map.addLayer(countries, {}, 'countries', 0);

// Define visualization dictionary for land cover maps

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

/***
 * Polygon metadata / strata ----------------------------------------------------------------------------------
 */

//// ELC10 - https://www.mdpi.com/2072-4292/13/12/2301
/* ELC10 lookup
    "Artificial land", //1
    "Cropland", //5
    "Woodland", //2
    "Shrubland",//3
    "Grassland", //4
    "Bare land", //6
    "Water", //7
    "Wetland",//7
*/

var elc10 = ee.ImageCollection('projects/nina/Arena/elc10_ukr_tur').mosaic();
elc10 = elc10.remap(
  [1,2,3,4,5,6,7,8], 
  [1,5,2,3,4,6,7,7]);
Map.addLayer(elc10, {min:1, max:7, palette: lcVizDict.colors}, 'elc10', 0)

//// CLC+ - https://land.copernicus.eu/en/products/clc-backbone
/* CLC+ lookup
    "Sealed", //1
    "Woody needle leaved trees", //2
    "Woody Broadleaved deciduous trees", //2
    "Woody Broadleaved evergreen trees", //2
    "Low-growing woody plands", //3
    "Permanent herbaceous", //4
    "Periodically herbaceous", //5
    "Lichens and mosses", //6
    "Non and sparsely vegetated", //6
    "Water", //7
    "Snow and ice", //7
*/

var clcplus = ee.Image('projects/nina/Europe_misc/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1')
clcplus = clcplus.remap(
  [1,2,3,4,5,6,7,8,9,10,11],
  [1,2,2,2,3,4,5,6,6, 7, 7]);
clcplus = clcplus.unmask(elc10)

Map.addLayer(clcplus, {min:1, max:7, palette: lcVizDict.colors}, 'clcplus', 0)


//// Global wetlands map - https://doi.org/10.5194/essd-15-265-2023
/*
180 - Non-wetland (#CCCCCC) Light gray
181 - Permanent water (#0000FF) Deep Blue
182 - Swamp (#006400) Dark green
183 - Marsh (#00FF00) Light green
184 - Flooded flat (#00FFFF) Aqua blue
185 - Saline (#CC99FF) Light purple
186 - Mangrove forest (#556B2F) Olive green
187 - Salt marsh (#FFFF99) Pale yellow
188 - Tidal flat (#D2B48C) Light brown
*/
var wetlands = ee.Image('projects/sat-io/open-datasets/GWL_FCS30/GWL_FCS30_2018').gte(182);
Map.addLayer(wetlands, {}, 'wetlands', 0)

// (Ia) Strict Nature Reserves: protected areas designed to preserve biodiversity and all geological features. Limited human use (e.g., scientific study, education) is allowed and carefully monitored. Strict nature reserves are often used to understand the impact of indirect human disturbance (e.g., burning fossil fuels) because of the area’s high level of preservation.
// (Ib) Wilderness Areas: protected areas managed to preserve ecosystem processes with limited human use. Wilderness areas cannot contain modern infrastructure (e.g., a visitor’s center), but they allow for local indigenous groups to maintain subsistence lifestyles. These areas are often established to restore disturbed environments.
// (II) National Parks: protected areas designed to preserve large-scale ecosystems and support human visitation. With conservation as a priority, these areas allow infrastructure and contribute to the local economy by providing opportunities for environmental educational and recreation.
var pas = ee.FeatureCollection("WCMC/WDPA/current/polygons")
  .filterBounds(greyTrend.geometry())
  .filter(ee.Filter.neq('STATUS', 'Proposed'));
var pasImg1 = ee.Image(0).byte().paint(pas.filter(ee.Filter.inList('IUCN_CAT', ['Ia'])), 1);
var pasImg2 = ee.Image(0).byte().paint(pas.filter(ee.Filter.inList('IUCN_CAT', ['Ib'])), 1);
var pasImg3 = ee.Image(0).byte().paint(pas.filter(ee.Filter.inList('IUCN_CAT', [ 'II'])), 1);

//// GLobal areas conservation priority - https://www.nature.com/articles/s41559-021-01528-7
// app scripts: https://code.earthengine.google.com/444c391a71e76a9dfc5285d379261a2c
var biodivcarbonwater_cons = ee.Image("users/Uploads/naturemap/biodiversitycarbonwater/minshort_speciestargets_biome_withPA_carbon__water__esh10km_repruns10_ranked");
// identify areas with 30% coverage target
biodivcarbonwater_cons = biodivcarbonwater_cons.lte(30);
Map.addLayer(biodivcarbonwater_cons, {min:0, max:1}, 'biodivcarbonwater_cons', 0);


/***
 * Export vector data ----------------------------------------------------------------------------------
 */
var lossMask = ee.Image(1).updateMask(losses).rename('label').addBands(greyTrend);

var strataStack = clcplus.eq(2).rename('tree')
  .addBands(clcplus.eq(3).rename('bush'))
  .addBands(clcplus.eq(4).rename('grass'))
  .addBands(clcplus.eq(5).rename('cropland'))
  .addBands(clcplus.eq(6).rename('bare'))
  .addBands(clcplus.eq(7).rename('water'))
  .addBands(wetlands.rename('wetland'))
  .addBands(pasImg1.rename('pa_Ia'))
  .addBands(pasImg2.rename('pa_Ib'))
  .addBands(pasImg3.rename('pa_II'))
  .addBands(biodivcarbonwater_cons.rename('cons_priority'))

var exportStack = lossMask.addBands(strataStack);
    
var list = countries.toList(1000)
for (var i = 0; i<1; i++){
  var sel = selectedISO[i]
  var aoi = countries.filter(ee.Filter.eq('shapeISO', sel)).geometry();
  
  var vec = exportStack
    .reduceToVectors({
      reducer: ee.Reducer.mean(), 
      geometry: aoi, 
      scale: 15, 
      //bestEffort: true,
      maxPixels: 1e11,
      //tileScale: 4,
      geometryInNativeProjection: true
    });
  //Map.addLayer(vec)
  Export.table.toDrive({
    collection: vec, 
    description: 'builtup_gain_2018_2023_v2_' + String(sel),
    fileFormat: 'GeoJSON'
  })
}

/// Ukraine and Turkiye
var selected = ['UKR', 'TUR']
var countries2 = countries.filter(ee.Filter.inList('shapeISO', selected))
Map.addLayer(countries2, {}, 'countries2', 0);

var list = countries2.toList(1000)
for (var i = 0; i<0; i++){
  var sel = selected[i]
  var aoi = countries2.filter(ee.Filter.eq('shapeISO', sel)).geometry();
  

  var aoi1 = aoi.difference(geometry, 1)
  var aoi2 = aoi.intersection(geometry, 1)
  
  var vec1 = exportStack.reduceToVectors({
      reducer: ee.Reducer.mean(), 
      geometry: aoi1, 
      scale: 10, 
      //bestEffort: true,
      maxPixels: 1e11,
      //tileScale: 4,
      geometryInNativeProjection: true
    }).select(['mean']);
    
  Export.table.toDrive({
    collection: vec1, 
    description: 'builtup_gain_2018_2023_v2_' +'1_' + String(sel) ,
    fileFormat: 'GeoJSON'
  })
  
  var vec2 = exportStack.reduceToVectors({
      reducer: ee.Reducer.mean(), 
      geometry: aoi2, 
      scale: 10, 
      //bestEffort: true,
      maxPixels: 1e11,
      //tileScale: 4,
      geometryInNativeProjection: true
    }).select(['mean']);
    
  Export.table.toDrive({
    collection: vec2, 
    description: 'builtup_gain_2018_2023_v2_' + '2_' +String(sel) ,
    fileFormat: 'GeoJSON'
  })
}

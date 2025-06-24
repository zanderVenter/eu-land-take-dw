/**
 * This script generates a strata areas of land take X country X cropland/nature
 * 
 * 1. Define countries to iterate over
 * 2. Define strata image which is a combination of CLC+ 2018 and DW land take from DW built trend scores
 * 3. Get strata areas per country using grid iteration and export to to Drive -> R
 * 4. Aggregate to grid for visualization in R
 * 5. Get overlap with CLC Accounting Layers
 * 6. Get overlap with RESOLVE Ecoregions
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
print(proj)

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
 * 3. Get strata areas and export to Drive -> R ------------------------------------------------------
 */

// Function to get strata area

function getStratAreas(stratImage, aoi,  scale, proj){
  
  // Calculate strata areas and group them
  var strataAreas = ee.Image.pixelArea().addBands(stratImage)
    .reproject(proj.atScale(scale))
    .reduceRegion({
      reducer: ee.Reducer.sum().group(1),
      geometry: aoi,
      scale: scale,
      maxPixels: 1e14,
      bestEffort: true
  });
  
  // Process groups to extract information server-side
  var groups = ee.List(strataAreas.get('groups'));
  
  var strataInfo = groups.map(function(group) {
    var dict = ee.Dictionary(group);
    dict = dict.rename(['sum', 'group'], ['area', 'stratum'])
    //var area = ee.Number(dict.get('sum'));
    return ee.Feature(null, dict);
  });
  strataInfo = ee.FeatureCollection(strataInfo)
  
  return strataInfo
  
}


var list= countries.reduceColumns(ee.Reducer.toList(), ['ISO3_CODE']).get('list').evaluate(function(list){
  print(list)
  
  //list = [41473908]
  
  for (var i = 0; i<0; i++){
    var id = list[i]
    var aoi = countries.filter(ee.Filter.eq('ISO3_CODE', id)).geometry();
    
    var strataImgGrid = strataImg.clip(aoi)
    
    var proj = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED").filterBounds(aoi).first().select(1).projection()
    
    var stratAreas = getStratAreas(strataImgGrid, aoi,  10, proj);
    
    stratAreas = stratAreas.map(function(ft){
      return ft.set('country', id)
    })
    
    Export.table.toDrive({
      collection: stratAreas,
      fileFormat: 'CSV',
      description: 'areas_' + String(id),
      folder: 'sample_areas_countries_v3'
    })
    
    
  }
})


/***
 * 4. Aggregate to grid for visualization in R ------------------------------------------------------
 */
var aoi = greyTrend.geometry().bounds();
var grid50km = aoi.coveringGrid(proj, 50000).filterBounds(countries);
grid50km = grid50km.randomColumn('id', 123).map(function(ft){
  ft = ft.setGeometry(ft.geometry().transform(ee.Projection('EPSG:4326'),1))
  return ft.set('id', ee.Number(ft.get('id')).multiply(1e10).round())
})
Map.addLayer(grid50km, {}, 'grid50km', 0)
/*
Export.table.toDrive({
  collection: grid50km,
  fileFormat: 'GeoJSON',
  description: 'grid50km'
})
*/
var gridAreas = grid50km.map(function(ft){
  
  var stratAreas = getStratAreas(strataImg, ft.geometry(),  20, proj);
  stratAreas = stratAreas.map(function(i){ return i.set('id', ft.get('id'))})
  
  return stratAreas
}).flatten()
/*
Export.table.toDrive({
  collection: gridAreas,
  fileFormat: 'CSV',
  description: 'areas_grid_v3'
})
*/

/***
 * 5. Areas of overlap with CLC AL ------------------------------------------------------
 */

// Here we will loop over export grid and not countries
var grid = ee.FeatureCollection('projects/nina/Arena/export_grid')
Map.addLayer(grid, {}, 'export_grid', 0)

var clc18 = ee.Image('projects/nina/Europe_misc/CLC2018ACC_V2018_20');
Map.addLayer(clc18.randomVisualizer(), {}, 'clc 2018', 0)

var strataImgLoss = clc18.updateMask(losses).updateMask(landMask);
//Map.addLayer(strataImgLoss.gte(331).and(strataImgLoss.lte(335)).focal_max().selfMask(), {palette:['#ed02f0']})

var strataImg = clc18.updateMask(landMask);

// Areas ----
var list= grid.reduceColumns(ee.Reducer.toList(), ['CellCode']).get('list').evaluate(function(list){
  print(list)
  
  //list = [18594054]
  
  for (var i = 947; i<947; i++){
    var id = list[i]
    var aoi = grid.filter(ee.Filter.eq('CellCode', id)).geometry();
    var strataImgGrid = strataImg.clip(aoi)
    var strataImgLossGrid = strataImgLoss.clip(aoi)
    
    var proj = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED").filterBounds(aoi).first().select(1).projection()
    
    var areas = getStratAreas(strataImgGrid, aoi,  10, proj);
    var areasLoss = getStratAreas(strataImgLossGrid, aoi,  10, proj);
    areas = areas.map(function(ft){
      return ft.set( 'CellCode', id, 'type', 'allLand')
    })
    areasLoss = areasLoss.map(function(ft){
      return ft.set( 'CellCode', id, 'type', 'loss')
    })
    var stratAreas = areas.merge(areasLoss)
    
    Export.table.toDrive({
      collection: stratAreas,
      fileFormat: 'CSV',
      description: 'areas_' + String(id),
      folder: 'clc_overlap_areas_v3'
    })
    
  }
})

/***
 * 5. Get overlap with RESOLVE Ecoregions ------------------------------------------------------
 */
var ecoregion = ee.FeatureCollection('projects/nina/Arena/Ecoregions2017')
var ecoregionImg = ecoregion.reduceToImage(['ECO_ID'],ee.Reducer.first());
Map.addLayer(ecoregionImg.randomVisualizer(), {}, 'ecoregionImg', 0)

var strataImgLoss = ecoregionImg.updateMask(losses).updateMask(landMask);
//Map.addLayer(strataImgLoss.gte(331).and(strataImgLoss.lte(335)).focal_max().selfMask(), {palette:['#ed02f0']})

var strataImg = ecoregionImg.updateMask(landMask);


// Areas ----
var list= grid.reduceColumns(ee.Reducer.toList(), ['CellCode']).get('list').evaluate(function(list){
  print(list)
  
  //list = [18594054]
  
  for (var i = 947; i<947; i++){
    var id = list[i]
    var aoi = grid.filter(ee.Filter.eq('CellCode', id)).geometry();
    var strataImgGrid = strataImg.clip(aoi)
    var strataImgLossGrid = strataImgLoss.clip(aoi)
    
    var proj = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED").filterBounds(aoi).first().select(1).projection()
    
    var areas = getStratAreas(strataImgGrid, aoi,  10, proj);
    var areasLoss = getStratAreas(strataImgLossGrid, aoi,  10, proj);
    areas = areas.map(function(ft){
      return ft.set( 'CellCode', id, 'type', 'allLand')
    })
    areasLoss = areasLoss.map(function(ft){
      return ft.set( 'CellCode', id, 'type', 'loss')
    })
    var stratAreas = areas.merge(areasLoss)
    
    Export.table.toDrive({
      collection: stratAreas,
      fileFormat: 'CSV',
      description: 'areas_' + String(id),
      folder: 'ecoregion_overlap_areas_v3'
    })
    
  }
})


//// Map interaction
Map.onClick(handleMapClick)
function handleMapClick(coords){
  
  
  coords = [coords.lon, coords.lat]
  var googleURL = 'https://earth.google.com/web/@'+String(coords[1]) +','+String(coords[0]) +',156.58634283a,1165.64764158d,35y,0h,0t,0r'
  var livingatlasURL = 'https://livingatlas.arcgis.com/wayback/#active=25982&mapCenter='+String(coords[0]) +'%2C' +String(coords[1]) +'%2C17'
  var planetURL = 'https://www.planet.com/basemaps/#/mode/compare/mosaic/global_quarterly_2018q3_mosaic/comparison/global_quarterly_2023q3_mosaic/center/'+String(coords[0]) +','+String(coords[1]) +'/zoom/17'
  
  
  var panel = ui.Panel()
  panel.add(ui.Label('googleURL',null,googleURL))
  .add(ui.Label('livingatlasURL',null,livingatlasURL))
  .add(ui.Label('planetURL',null,planetURL))
  Map.widgets().reset([panel]);
}

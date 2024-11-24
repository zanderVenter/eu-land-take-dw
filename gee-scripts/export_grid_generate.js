  
/**
 * This script produces an export grid which is used to iterate Dynamic World processing and export tasks
 * 
 * 1. Define the set of countries we want to include in the study area
 * 2. Exclude grid cells that are in areas outside the main EU - e.g. islands in Atlantic
 * 3. Filter out grid cells in the middle of ocean by filtering by country boundaries
 * 4. Assign a unique ID for each cell
 * 5. Export to GEE Assets
 * 
 * Author: Zander Venter
 */

var projCrs = 'EPSG:3035'

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

// Selected extra - for journalistic interest, and to complete the covering grid
var extra = [
    "CHE",
    "TUR",
    "UKR",
    "SRB",
    "ALB",
    "BIH"
  ]

// Join both list for spatial grid
var selectedGrid = selectedEEA.concat(extra)

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

// Import a simple geometry country dataset for creating covering grid
var countriesSimple = ee.FeatureCollection('users/zandersamuel/Global_misc/world_countries').filter(ee.Filter.inList('adm0_a3', selectedGrid));
Map.addLayer(countriesSimple, {}, 'countriesSimple', 0);

var grid = countriesSimple.geometry().coveringGrid(ee.Projection(projCrs), 100000);
grid = grid.filter(ee.Filter.intersects('.geo', excludeArea).not());
grid = grid.randomColumn('CellCode', 123).sort('CellCode');
grid = grid.map(function(ft){return ft.set('CellCode', ee.Number(ft.get('CellCode')).multiply(100000000).round())});

print(grid.limit(10))
print(grid.size())
Map.addLayer(grid, {}, 'grid', 0);

Export.table.toAsset({
  collection: grid,
  assetId: 'Arena/export_grid',
  description: 'export_grid'
})

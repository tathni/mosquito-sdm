// This code is for extracting environmental covariates for use in a species distribution model of mosquitoes
// Temperature covariates are excluded from this script, and are calculated in the "temperature_covariates" script

// Import environmental datasets
var precipitation = ee.ImageCollection("ECMWF/ERA5/DAILY");
var EVI = ee.ImageCollection("MODIS/006/MOD13A2");
var forestCover = ee.ImageCollection("MODIS/006/MOD44B");
var populationDensity = ee.ImageCollection([
  ee.Image("users/tathni/GHS_Pop_2000"),
  ee.Image("users/tathni/GHS_Pop_2015")]);
var cattleDensity = ee.Image("users/tathni/Cattle_2010_Aw");
var windSpeed = ee.ImageCollection("IDAHO_EPSCOR/TERRACLIMATE");
var surfaceWater = ee.Image("JRC/GSW1_3/GlobalSurfaceWater");
var countries = ee.FeatureCollection("USDOS/LSIB_SIMPLE/2017");



// Identify distinct geographical regions and define masks to clip landmasses
// North America polygon is created manually, due to issues regarding export
var countriesUnique = countries.aggregate_array("wld_rgn").flatten().distinct();
print(countriesUnique);

var Asia = countries.filter(ee.Filter.inList("wld_rgn", ["N Asia","S Asia","E Asia","SE Asia","SW Asia",
                                                         "Central Asia","Indian Ocean"]));
var bordersAsia = Asia.geometry().dissolve().bounds();

var Africa = countries.filter(ee.Filter.inList("wld_rgn", ["Africa"]));
var bordersAfrica = Africa.geometry().dissolve().bounds();

var Europe = countries.filter(ee.Filter.inList("wld_rgn", ["Europe"]));
var bordersEurope = Europe.geometry().dissolve().bounds();

var CentralAmerica = countries.filter(ee.Filter.inList("wld_rgn", ["Caribbean","Central America"]));
var bordersCentralAmerica = CentralAmerica.geometry().dissolve().bounds();

var SouthAmerica = countries.filter(ee.Filter.inList("wld_rgn", ["South America","South Atlantic"]));
var bordersSouthAmerica = SouthAmerica.geometry().dissolve().bounds();

var Oceania = countries.filter(ee.Filter.inList("wld_rgn", ["Oceania","Australia"]));
var bordersOceania = Oceania.geometry().dissolve().bounds();

var bordersNorthAmerica = ee.Geometry.Polygon(
        [[[-134.71962177972188, 51.79168228105622],
          [-130.50087177972188, 41.718093909106074],
          [-159.68055927972185, 16.567839160193564],
          [-147.37587177972185, 19.575397983342405],
          [-124.17274677972186, 33.66499591672636],
          [-111.78694734033277, 16.593752215004137],
          [-87.17757234033277, 8.015375850162053],
          [-78.65218171533277, 4.871705437640687],
          [-74.43343171533277, 12.597119270319542],
          [-65.46858796533277, 16.76213880995453],
          [-60.45882234033277, 11.910017446968578],
          [-56.67952546533277, 19.269341037051426],
          [-3.044355402589076, 73.25076703394421],
          [-1.0886571784392607, 84.08619809731579],
          [-51.713657178439256, 83.78923057273694],
          [-98.11990717843926, 82.71766784334287],
          [-133.97928217843926, 72.91199372404995],
          [-164.91678217843926, 72.70417124886384],
          [-173.35428217843926, 48.673786354648705]]]);



// Define Greenland separately to use as mask to convert NAs to 0s for Cattle Density covariate
var Greenland = countries.filter(ee.Filter.inList("country_na", ["Greenland"]));
var bordersGreenland = Greenland.geometry().dissolve().bounds();


// Set limits for taking averages
var startDate = ee.Date("2000-01-01");
var endDate = ee.Date("2019-12-31");
var startYear = 2000;
var endYear = 2019;
var years = ee.List.sequence(startYear, endYear);
var months = ee.List.sequence(1, 12);
var months_rep = months.cat(months);
var weeks = ee.List.sequence(1, 52);
var weeks_rep = weeks.cat(weeks);


// Calculate precipitation in the wettest and driest quarter, set of 3 consecutive months with most or least precipitation, respectively
// This algorithm uses year flow-over. For example, if the quarter begins with Dec'00, it includes Jan'01, Feb'01
var precipMainQuarters = ee.ImageCollection.fromImages(ee.List.sequence(0, 9).map(function(m){ // Canonical first 10 quarters in one calendar year
  var q_start = ee.Number(months_rep.get(m));
  var q_end = ee.Number(months_rep.get(ee.Number(m).add(2)));
  var precipByQuarter = precipitation.filter(ee.Filter.calendarRange(q_start, q_end, "month")); // For each set of 3 months (defined as a quarter)
  return ee.ImageCollection.fromImages(years.map(function(y){
     return precipByQuarter.filter(ee.Filter.calendarRange(y, y, "year"))
                           .select("total_precipitation")
                           .toBands()
                           .resample("bilinear")
                           .reproject("EPSG:4326", null, 1000) // Reproject to a 1 km scale
                           .reduce(ee.Reducer.sum()); // Sum the total precip for each year within the quarter specified above
  })).toBands()
     .reduce(ee.Reducer.mean()) // Average the total quarterly precip over the total 20-year range by quarter (Jan-Mar, Feb-Apr, etc.)
     .rename(ee.Number(m).format("%d"));
}));

var precipNovQuarter = ee.ImageCollection.fromImages(ee.List.sequence(10, 10).map(function(m){ // "November quarter": Nov&Dec[year] + Jan[year+1]
  var q_start = ee.Number(months_rep.get(m));
  var q_end = ee.Number(months_rep.get(ee.Number(m).add(1)));
  var precipQuarterFirstHalf = precipitation.filter(ee.Filter.calendarRange(q_start, q_end, "month")); // Nov&Dec
  var precipQuarterSecondHalf = precipitation.filter(ee.Filter.calendarRange(1, 1, "month")); // Jan
  var precipNet = ee.ImageCollection.fromImages(years.map(function(y){
     var tempFirstHalf = precipQuarterFirstHalf.filter(ee.Filter.calendarRange(y, y, "year")) // Nov&Dec[year]
                                               .select("total_precipitation")
                                               .toBands()
                                               .resample("bilinear")
                                               .reproject("EPSG:4326", null, 1000);
     var tempSecondHalf = precipQuarterSecondHalf.filter(ee.Filter.calendarRange(ee.Number(y).add(1), ee.Number(y).add(1), "year")) // Jan[year+1]
                                                 .select("total_precipitation")
                                                 .toBands()
                                                 .resample("bilinear")
                                                 .reproject("EPSG:4326", null, 1000);
     var stackedBands = tempFirstHalf.addBands(tempSecondHalf); // Combine Nov&Dec[year] and Jan[year+1]
     return stackedBands.reduce(ee.Reducer.sum()); // Reduce and create a summation image
  })).toBands()
     .reduce(ee.Reducer.mean()) // Average the total 20-year precip for the "November quarter": Nov&Dec[year] and Jan[year+1]
     .rename(ee.Number(m).format("%d"));
  return precipNet;
}));

var precipDecQuarter = ee.ImageCollection.fromImages(ee.List.sequence(11, 11).map(function(m){ // "December quarter": Dec[year] + Jan&Feb[year+1]
  var precipQuarterFirstHalf = precipitation.filter(ee.Filter.calendarRange(m, m, "month")); // Dec
  var precipQuarterSecondHalf = precipitation.filter(ee.Filter.calendarRange(1, 2, "month")); // Jan&Feb
  var precipNet = ee.ImageCollection.fromImages(years.map(function(y){
     var firstHalf = precipQuarterFirstHalf.filter(ee.Filter.calendarRange(y, y, "year")) // Dec[year]
                                           .select("total_precipitation")
                                           .toBands()
                                           .resample("bilinear")
                                           .reproject("EPSG:4326", null, 1000);
     var secondHalf = precipQuarterSecondHalf.filter(ee.Filter.calendarRange(ee.Number(y).add(1), ee.Number(y).add(1), "year")) // Jan&Feb[year+1]
                                             .select("total_precipitation")
                                             .toBands()
                                             .resample("bilinear")
                                             .reproject("EPSG:4326", null, 1000);
     var stackedBands = firstHalf.addBands(secondHalf); // Combine Dec[year] and Jan&Feb[year+1]
     return stackedBands.reduce(ee.Reducer.sum()); // Reduce and create a summation image
  })).toBands()
     .reduce(ee.Reducer.mean()) // Average the total 20-year precip for the "December quarter": Nov&Dec[year] and Jan[year+1]
     .rename(ee.Number(m).format("%d"));
  return precipNet;
}));

var mergeExtraQuarters = precipNovQuarter.merge(precipDecQuarter); // Merge the canonical 10 quarters + Nov/Dec quarters
var mergeAllQuarters = mergeExtraQuarters.merge(precipMainQuarters);
var precipWettestQuarter = mergeAllQuarters.toBands() // Highest composite (wettest quarter)
                                           .reduce(ee.Reducer.max()).clip(countries);
var precipDriestQuarter = mergeAllQuarters.toBands() // Lowest composite (driest quarter)
                                          .reduce(ee.Reducer.min()).clip(countries);
print(precipWettestQuarter); // PWQ
print(precipDriestQuarter); // PDQ



// Calculate mean EVI within a year, then 20-year average
var meanEVI = ee.ImageCollection.fromImages(years.map(function(y){
  return EVI.filter(ee.Filter.calendarRange(y, y, "year"))
            .select("EVI")
            .toBands()
            .resample("bilinear")
            .reproject("EPSG:4326", null, 1000)
            .multiply(0.0001)
            .reduce(ee.Reducer.mean());
})).toBands().reduce(ee.Reducer.mean()).clip(countries);
print(meanEVI); // EVIM



// Calculate standard deviation of the EVI within a year, then 20-year average
var stdDevEVI = ee.ImageCollection.fromImages(years.map(function(y){
  return EVI.filter(ee.Filter.calendarRange(y, y, "year"))
            .select("EVI")
            .toBands()
            .resample("bilinear")
            .reproject("EPSG:4326", null, 1000)
            .multiply(0.0001)
            .reduce(ee.Reducer.stdDev());
})).toBands().reduce(ee.Reducer.mean()).clip(countries);
print(stdDevEVI); // EVISD



// Calculate forest cover
var forestCoverPercent = ee.ImageCollection.fromImages(years.map(function(y){
  return forestCover.filter(ee.Filter.calendarRange(y, y, "year"))
            .select("Percent_Tree_Cover")
            .toBands()
            .reduceResolution({
              reducer: ee.Reducer.mean(),
              maxPixels: 4096 })
            .reproject("EPSG:4326", null, 1000)
            .reduce(ee.Reducer.mean());
})).toBands().reduce(ee.Reducer.mean()).clip(countries);
print(forestCoverPercent); // FC



// Calculate mean annual human population density, then average over years available for our study period
var meanHPD = populationDensity.mean().clip(countries);
print(meanHPD); // HPD



// Calculate cattle density
var cattleDensityBand = cattleDensity.select("b1")
                                     .resample("bilinear")
                                     .reproject("EPSG:4326", null, 1000)
                                     .clip(countries);
print(cattleDensityBand); // CD



// Calculate mean annual wind speed, then 20-year average
var meanWindSpeed = ee.ImageCollection.fromImages(years.map(function(y){
  return windSpeed.filter(ee.Filter.calendarRange(y, y, "year"))
                        .select("vs")
                        .toBands()
                        .resample("bilinear")
                        .reproject("EPSG:4326", null, 1000)
                        .reduce(ee.Reducer.mean());
})).toBands().reduce(ee.Reducer.mean()).clip(countries);
print(meanWindSpeed); // WS



// Calculate surface water
var surfaceWaterBand = surfaceWater.select("seasonality")
                                   .unmask(0)
                                   .reduceResolution({
                                     reducer: ee.Reducer.mean(),
                                     maxPixels: 4096 })
                                   .reproject("EPSG:4326", null, 1000)
                                   .clip(countries);
print(surfaceWaterBand); // SW




// Define an image export function
var exportImage = function(image, bordersRegion, desc){
  Export.image.toDrive({
  image: image,
  folder: "Mosquito SDM MaxEnt Mechanistic (Tejas)",
  description: desc,
  maxPixels: 1e13,
  scale: 1000,
  region: bordersRegion
  });
};



/*
// Precipitation of wettest quarter, image export
exportImage(precipWettestQuarter, bordersAfrica, "PWQ_Africa");
exportImage(precipWettestQuarter, bordersAsia, "PWQ_Asia");
exportImage(precipWettestQuarter, bordersEurope, "PWQ_Europe");
exportImage(precipWettestQuarter, bordersNorthAmerica, "PWQ_NorthAmerica");
exportImage(precipWettestQuarter, bordersCentralAmerica, "PWQ_CentralAmerica");
exportImage(precipWettestQuarter, bordersSouthAmerica, "PWQ_SouthAmerica");
exportImage(precipWettestQuarter, bordersOceania, "PWQ_Oceania");


// Precipitation of driest quarter, image export
exportImage(precipDriestQuarter, bordersAfrica, "PDQ_Africa");
exportImage(precipDriestQuarter, bordersAsia, "PDQ_Asia");
exportImage(precipDriestQuarter, bordersEurope, "PDQ_Europe");
exportImage(precipDriestQuarter, bordersNorthAmerica, "PDQ_NorthAmerica");
exportImage(precipDriestQuarter, bordersCentralAmerica, "PDQ_CentralAmerica");
exportImage(precipDriestQuarter, bordersSouthAmerica, "PDQ_SouthAmerica");
exportImage(precipDriestQuarter, bordersOceania, "PDQ_Oceania");
exportImage(precipDriestQuarter, bordersGreenland, "PDQ_Greenland_Mask"); // For masking of CD


// Mean EVI, image export
exportImage(meanEVI, bordersAfrica, "EVIM_Africa");
exportImage(meanEVI, bordersAsia, "EVIM_Asia");
exportImage(meanEVI, bordersEurope, "EVIM_Europe");
exportImage(meanEVI, bordersNorthAmerica, "EVIM_NorthAmerica");
exportImage(meanEVI, bordersCentralAmerica, "EVIM_CentralAmerica");
exportImage(meanEVI, bordersSouthAmerica, "EVIM_SouthAmerica");
exportImage(meanEVI, bordersOceania, "EVIM_Oceania");


// Standard deviation of EVI, image export
exportImage(stdDevEVI, bordersAfrica, "EVISD_Africa");
exportImage(stdDevEVI, bordersAsia, "EVISD_Asia");
exportImage(stdDevEVI, bordersEurope, "EVISD_Europe");
exportImage(stdDevEVI, bordersNorthAmerica, "EVISD_NorthAmerica");
exportImage(stdDevEVI, bordersCentralAmerica, "EVISD_CentralAmerica");
exportImage(stdDevEVI, bordersSouthAmerica, "EVISD_SouthAmerica");
exportImage(stdDevEVI, bordersOceania, "EVISD_Oceania");


// Forest cover, image export
exportImage(forestCoverPercent, bordersAfrica, "FC_Africa");
exportImage(forestCoverPercent, bordersAsia, "FC_Asia");
exportImage(forestCoverPercent, bordersEurope, "FC_Europe");
exportImage(forestCoverPercent, bordersNorthAmerica, "FC_NorthAmerica");
exportImage(forestCoverPercent, bordersCentralAmerica, "FC_CentralAmerica");
exportImage(forestCoverPercent, bordersSouthAmerica, "FC_SouthAmerica");
exportImage(forestCoverPercent, bordersOceania, "FC_Oceania");


// Human population density, image export
exportImage(meanHPD, bordersAfrica, "HPD_Africa");
exportImage(meanHPD, bordersAsia, "HPD_Asia");
exportImage(meanHPD, bordersEurope, "HPD_Europe");
exportImage(meanHPD, bordersNorthAmerica, "HPD_NorthAmerica");
exportImage(meanHPD, bordersCentralAmerica, "HPD_CentralAmerica");
exportImage(meanHPD, bordersSouthAmerica, "HPD_SouthAmerica");
exportImage(meanHPD, bordersOceania, "HPD_Oceania");


// Cattle density, image export
exportImage(cattleDensityBand, bordersAfrica, "CD_Africa");
exportImage(cattleDensityBand, bordersAsia, "CD_Asia");
exportImage(cattleDensityBand, bordersEurope, "CD_Europe");
exportImage(cattleDensityBand, bordersNorthAmerica, "CD_NorthAmerica");
exportImage(cattleDensityBand, bordersCentralAmerica, "CD_CentralAmerica");
exportImage(cattleDensityBand, bordersSouthAmerica, "CD_SouthAmerica");
exportImage(cattleDensityBand, bordersOceania, "CD_Oceania");


// Wind speed, image export
exportImage(meanWindSpeed, bordersAfrica, "WS_Africa");
exportImage(meanWindSpeed, bordersAsia, "WS_Asia");
exportImage(meanWindSpeed, bordersEurope, "WS_Europe");
exportImage(meanWindSpeed, bordersNorthAmerica, "WS_NorthAmerica");
exportImage(meanWindSpeed, bordersCentralAmerica, "WS_CentralAmerica");
exportImage(meanWindSpeed, bordersSouthAmerica, "WS_SouthAmerica");
exportImage(meanWindSpeed, bordersOceania, "WS_Oceania");


// Surface water, image export
exportImage(surfaceWaterBand, bordersAfrica, "SW_Africa");
exportImage(surfaceWaterBand, bordersAsia, "SW_Asia");
exportImage(surfaceWaterBand, bordersEurope, "SW_Europe");
exportImage(surfaceWaterBand, bordersNorthAmerica, "SW_NorthAmerica");
exportImage(surfaceWaterBand, bordersCentralAmerica, "SW_CentralAmerica");
exportImage(surfaceWaterBand, bordersSouthAmerica, "SW_SouthAmerica");
exportImage(surfaceWaterBand, bordersOceania, "SW_Oceania");
*/

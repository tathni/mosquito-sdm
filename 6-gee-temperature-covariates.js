// This code is for extracting temperature covariates for use in a species distribution model of mosquitoes
// Specifically, temperature covariates are defined for mosquitoes' activity season (temporal restriction)

// Photoperiod- diapausing mosquitoes: < 9 hrs sunlight
// Precipitation- diapausing mosquitoes: < 50 mm precipitation in the last 30 days

// Daylengths calculation are based on Forsythe et al 1995

var temp = ee.ImageCollection("ECMWF/ERA5/DAILY");
var precipitation = ee.ImageCollection("ECMWF/ERA5/DAILY");
var countries = ee.FeatureCollection("USDOS/LSIB_SIMPLE/2017");

// Set limits for data extraction
var startDate = ee.Date("2000-01-01");
var endDate = ee.Date("2019-12-31");
var startYear = 2000;
var endYear = 2019;
var years = ee.List.sequence(startYear, endYear);




// Identify distinct geographical regions and define masks to clip landmasses
// North America polygon is created manually, due to issues regarding export
// Africa is exported in 2 halves for the PrecipASTM and PrecipASTSD variables
var countriesUnique = countries.aggregate_array("wld_rgn").flatten().distinct();

var Asia = countries.filter(ee.Filter.inList("wld_rgn", ["N Asia","S Asia","E Asia","SE Asia","SW Asia",
                                                         "Central Asia","Indian Ocean"]));
var bordersAsia = Asia.geometry().dissolve().bounds();

var Africa = countries.filter(ee.Filter.inList("wld_rgn", ["Africa"]));
var bordersAfrica = Africa.geometry().dissolve().bounds();

var Europe = countries.filter(ee.Filter.inList("wld_rgn", ["Europe"]));
var bordersEurope = Europe.geometry().dissolve().bounds();

var CentralAmerica = countries.filter(ee.Filter.inList("wld_rgn", ["Central America","Caribbean"]));
var bordersCentralAmerica = CentralAmerica.geometry().dissolve().bounds();

/*
// Pieces of Central America to export PrecipASTM and PrecipASTSD in smaller chunks
print(CentralAmerica.aggregate_array("country_co").flatten().distinct());
var bordersCentralAmerica1 = countries.filter(ee.Filter.inList("country_co", ["CU","RQ","VQ","VI","AC",
                                                                       "TB","NL","AV","RN","NN","SC","MH"]))
                                      .geometry().dissolve().bounds();
var bordersCentralAmerica2 = countries.filter(ee.Filter.inList("country_co", ["GP","DO"]))
                                      .geometry().dissolve().bounds();
var bordersCentralAmerica3 = countries.filter(ee.Filter.inList("country_co", ["TD"]))
                                      .geometry().dissolve().bounds();
var bordersCentralAmerica4 = countries.filter(ee.Filter.inList("country_co", ["GJ"]))
                                      .geometry().dissolve().bounds();
var bordersCentralAmerica5 = countries.filter(ee.Filter.inList("country_co", ["MB","ST"]))
                                      .geometry().dissolve().bounds();
var bordersCentralAmerica6 = countries.filter(ee.Filter.inList("country_co", ["VC","BB"]))
                                      .geometry().dissolve().bounds();
var bordersCentralAmerica7 = countries.filter(ee.Filter.inList("country_co", ["UC","AA"]))
                                      .geometry().dissolve().bounds();
var bordersCentralAmerica8 = countries.filter(ee.Filter.inList("country_co", ["DR"]))
                                      .geometry().dissolve().bounds();
var bordersCentralAmerica9 = countries.filter(ee.Filter.inList("country_co", ["HA","JM","BQ","CJ","ES","GT"]))
                                      .geometry().dissolve().bounds();
var bordersCentralAmerica10 = countries.filter(ee.Filter.inList("country_co", ["PM","CS","NU","HO","BH"]))
                                      .geometry().dissolve().bounds();
*/

var SouthAmerica = countries.filter(ee.Filter.inList("wld_rgn", ["South America","South Atlantic"]));
var bordersSouthAmerica = SouthAmerica.geometry().dissolve().bounds();

var Oceania = countries.filter(ee.Filter.inList("wld_rgn", ["Oceania","Australia"]));
var bordersOceania = Oceania.geometry().dissolve().bounds();

var bordersNorthAmerica = /* color: #d63000 */ee.Geometry.Polygon(
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




// Outer loop over days, inner loop over years, inner calculation of previous 30-day rolling total precipitation
var rollingPrecip = ee.ImageCollection.fromImages(ee.List.sequence(0, 364).map(function(d){ // Loop over each day of year
  return ee.ImageCollection.fromImages(years.map(function(y){ // Loop over each of the 20 years
    if(y % 4 === 0 && d >= 59) { // Assumption: skip Feb. 29 on leap years
      d = d + 1;
    }
    var d_end = ee.Date.fromYMD(y, 1, 1).advance(d, "day");
    var d_start = d_end.advance(-30, "day");
    return precipitation.filter(ee.Filter.date(d_start, d_end))
                        .select("total_precipitation")
                        .toBands()
                        .multiply(ee.Image.constant(1000))
                        //.resample("bilinear")
                        //.reproject("EPSG:4326", null, 1000) // Reproject to a 1 km scale
                        .reduce(ee.Reducer.sum()); // Return the summed precipitation for the year
  })).toBands()
     .reduce(ee.Reducer.mean()) // Average the summed rolling precip over 20-year period by each day
}));

// Create a binary mask, with 1's if the last 30-day precipitation >50 mm (wet season), 0 if otherwise (dry season)
var precipMask = rollingPrecip.toBands().gte(50);



// Calculate daylength
var lat = ee.Image.pixelLonLat().select("latitude");
var p = 0;  // Degrees below horizon that we consider end of day
var daylengths = ee.List.sequence(1, 365).map(function(doy){
  var theta = ee.Number(doy).subtract(186)
                            .multiply(0.00860)
                            .tan()
                            .multiply(0.9671396)
                            .atan()
                            .multiply(2)
                            .add(0.2163108);
  var phi = theta.cos()
                 .multiply(0.39795)
                 .asin();
  var num = lat.multiply(ee.Image.constant(Math.PI))
             .divide(ee.Image.constant(180))
             .sin()
             .multiply(ee.Image.constant(phi.sin()))
             .add(ee.Image.constant(ee.Number(p).multiply(Math.PI).divide(180)));
  var denom = lat.multiply(ee.Image.constant(Math.PI))
                 .divide(ee.Image.constant(180))
                 .cos()
                 .multiply(ee.Image.constant(phi.cos()));
  var X = num.divide(denom).clamp(-1.0, 1.0);
  var D = X.acos()
           .divide(ee.Image.constant(-Math.PI))
           .add(ee.Image.constant(1))
           .multiply(ee.Image.constant(24))
           .select(["latitude"], ["daylength"])
           .set(ee.Dictionary.fromLists(["date", "day", "month"],
                                        [ee.Date.fromYMD(2019, 12, 31).advance(doy, "day"),
                                         ee.Date.fromYMD(2019, 12, 31).advance(doy, "day").get("day"),
                                         ee.Date.fromYMD(2019, 12, 31).advance(doy, "day").get("month")]));


  return(D);

});

// Create a binary mask, with 1's if photoperiod is greater than the specified hrs, 0 if otherwise
var diapause = 9;
var diapauseMask = ee.ImageCollection.fromImages(daylengths).toBands().gte(diapause);



// Outer loop for each day, calculate avg temp on that day over the 20 year period inner loop, and then apply the mask
// If y is multiple of 4, and d = 60 (feb 29 leap day), d=d+1 ... built into the temps averaging also
// This results in the avg temp moqsuitoes are experiencing on the days that they are, on average, active
// In other words, an avg activity ssn mask applied to an avg temp by day for 20-year image collection


// Calculation of temperature annual mean (TAM), no activity season mask applied
var TAM = ee.ImageCollection.fromImages(ee.List.sequence(0, 364).map(function(d){ // Loop over each day of year
  return ee.ImageCollection.fromImages(years.map(function(y){ // Loop over each of the 20 years
    if(y % 4 === 0 && d >= 59) { // Assumption: skip Feb. 29 on leap years
      d = d + 1;
    }
    var d_start = ee.Date.fromYMD(y, 1, 1).advance(d, "day");
    return temp.filter(ee.Filter.date(d_start))
               .select(["mean_2m_air_temperature"])
               .toBands()
               .add(ee.Image.constant(-273.15))
               .resample("bilinear")
               .reproject("EPSG:4326", null, 1000);
  })).toBands()
     .reduce(ee.Reducer.mean())
})).toBands().reduce(ee.Reducer.mean());


// Calculation of temperature annual standard deviation (TASD), no activity season mask applied
var TASD = ee.ImageCollection.fromImages(ee.List.sequence(0, 364).map(function(d){ // Loop over each day of year
  return ee.ImageCollection.fromImages(years.map(function(y){ // Loop over each of the 20 years
    if(y % 4 === 0 && d >= 59) { // Assumption: skip Feb. 29 on leap years
      d = d + 1;
    }
    var d_start = ee.Date.fromYMD(y, 1, 1).advance(d, "day");
    return temp.filter(ee.Filter.date(d_start))
               .select(["mean_2m_air_temperature"])
               .toBands()
               .add(ee.Image.constant(-273.15))
               .resample("bilinear")
               .reproject("EPSG:4326", null, 1000);
  })).toBands()
     .reduce(ee.Reducer.stdDev())
})).toBands().reduce(ee.Reducer.mean());



// Calculation of photoperiod activity season temperature mean (PhotoASTM)
var PhotoASTM = ee.ImageCollection.fromImages(ee.List.sequence(0, 364).map(function(d){ // Loop over each day of year
  return ee.ImageCollection.fromImages(years.map(function(y){ // Loop over each of the 20 years
    if(y % 4 === 0 && d >= 59) { // Assumption: skip Feb. 29 on leap years
      d = d + 1;
    }
    var d_start = ee.Date.fromYMD(y, 1, 1).advance(d, "day");
    return temp.filter(ee.Filter.date(d_start))
               .select(["mean_2m_air_temperature"])
               .toBands()
               .add(ee.Image.constant(-273.15))
               .resample("bilinear")
               .reproject("EPSG:4326", null, 1000);
  })).toBands()
     .reduce(ee.Reducer.mean())
})).toBands().updateMask(diapauseMask).reduce(ee.Reducer.mean());


// Calculation of photoperiod activity season temperature standard deviation (PhotoASTSD)
var PhotoASTSD = ee.ImageCollection.fromImages(years.map(function(y){ // Loop over each day of year
  return ee.ImageCollection.fromImages(ee.List.sequence(0, 364).map(function(d){ // Loop over each of the 20 years
    if(y % 4 === 0 && d >= 59) { // Assumption: skip Feb. 29 on leap years
      d = d + 1;
    }
    var d_start = ee.Date.fromYMD(y, 1, 1).advance(d, "day");
    return temp.filter(ee.Filter.date(d_start))
               .select(["mean_2m_air_temperature"])
               .toBands()
               .add(ee.Image.constant(-273.15))
               .resample("bilinear")
               .reproject("EPSG:4326", null, 1000);
  })).toBands()
     .updateMask(diapauseMask)
     .reduce(ee.Reducer.stdDev()) // Variation across all days in a given year
})).toBands().reduce(ee.Reducer.mean()); // Average the daily variations for every year




// Calculation of precipitation activity season temperature mean (PrecipASTM)
var PrecipASTM = ee.ImageCollection.fromImages(ee.List.sequence(0, 364).map(function(d){ // Loop over each day of year
  return ee.ImageCollection.fromImages(years.map(function(y){ // Loop over each of the 20 years
    if(y % 4 === 0 && d >= 59) { // Assumption: skip Feb. 29 on leap years
      d = d + 1;
    }
    var d_start = ee.Date.fromYMD(y, 1, 1).advance(d, "day");
    return temp.filter(ee.Filter.date(d_start))
               .select(["mean_2m_air_temperature"])
               .toBands()
               .add(ee.Image.constant(-273.15))
  })).toBands()
     .reduce(ee.Reducer.mean())
})).toBands()
   .updateMask(precipMask)
   .reduce(ee.Reducer.mean())
   .resample("bilinear")
   .reproject("EPSG:4326", null, 1000);



// Calculation of precipitation activity season temperature standard deviation (PrecipASTSD)
var PrecipASTSD = ee.ImageCollection.fromImages(years.map(function(y){ // Loop over each day of year
  return ee.ImageCollection.fromImages(ee.List.sequence(0, 364).map(function(d){ // Loop over each of the 20 years
    if(y % 4 === 0 && d >= 59) { // Assumption: skip Feb. 29 on leap years
      d = d + 1;
    }
    var d_start = ee.Date.fromYMD(y, 1, 1).advance(d, "day");
    return temp.filter(ee.Filter.date(d_start))
               .select(["mean_2m_air_temperature"])
               .toBands()
               .add(ee.Image.constant(-273.15))
  })).toBands()
     .updateMask(precipMask)
     .reduce(ee.Reducer.stdDev()) // Variation across all days in a given year
})).toBands()
   .reduce(ee.Reducer.mean())
   .resample("bilinear")
   .reproject("EPSG:4326", null, 1000); // Average the daily variations for every year




var activitySSN_diapause_length = diapauseMask.selfMask().reduce(ee.Reducer.count());
var activitySSN_precip_length = precipMask.selfMask().reduce(ee.Reducer.count());
var dateImage = ee.Image.constant(ee.List.sequence(1,365));



// Masks multiplied by a NEW mask with constant 1-365 bands
var activitySSN_diapause_first = diapauseMask.selfMask().multiply(dateImage).reduce(ee.Reducer.firstNonNull());
var activitySSN_diapause_last = diapauseMask.selfMask().multiply(dateImage).reduce(ee.Reducer.lastNonNull());
var activitySSN_precip_first = precipMask.selfMask().multiply(dateImage).reduce(ee.Reducer.firstNonNull());
var activitySSN_precip_last = precipMask.selfMask().multiply(dateImage).reduce(ee.Reducer.lastNonNull());




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
// Photoperiod activity season start day, end day, and length, image export
exportImage(activitySSN_diapause_first, bordersAfrica, "PhotoperiodAS_FirstDay_Africa")
exportImage(activitySSN_diapause_last, bordersAfrica, "PhotoperiodAS_LastDay_Africa")
exportImage(activitySSN_diapause_length, bordersAfrica, "PhotoperiodAS_Length_Africa")

exportImage(activitySSN_diapause_first, bordersAsia, "PhotoperiodAS_FirstDay_Asia")
exportImage(activitySSN_diapause_last, bordersAsia, "PhotoperiodAS_LastDay_Asia")
exportImage(activitySSN_diapause_length, bordersAsia, "PhotoperiodAS_Length_Asia")

exportImage(activitySSN_diapause_first, bordersEurope, "PhotoperiodAS_FirstDay_Europe")
exportImage(activitySSN_diapause_last, bordersEurope, "PhotoperiodAS_LastDay_Europe")
exportImage(activitySSN_diapause_length, bordersEurope, "PhotoperiodAS_Length_Europe")

exportImage(activitySSN_diapause_first, bordersNorthAmerica, "PhotoperiodAS_FirstDay_NorthAmerica")
exportImage(activitySSN_diapause_last, bordersNorthAmerica, "PhotoperiodAS_LastDay_NorthAmerica")
exportImage(activitySSN_diapause_length, bordersNorthAmerica, "PhotoperiodAS_Length_NorthAmerica")

exportImage(activitySSN_diapause_first, bordersCentralAmerica, "PhotoperiodAS_FirstDay_CentralAmerica")
exportImage(activitySSN_diapause_last, bordersCentralAmerica, "PhotoperiodAS_LastDay_CentralAmerica")
exportImage(activitySSN_diapause_length, bordersCentralAmerica, "PhotoperiodAS_Length_CentralAmerica")

exportImage(activitySSN_diapause_first, bordersSouthAmerica, "PhotoperiodAS_FirstDay_SouthAmerica")
exportImage(activitySSN_diapause_last, bordersSouthAmerica, "PhotoperiodAS_LastDay_SouthAmerica")
exportImage(activitySSN_diapause_length, bordersSouthAmerica, "PhotoperiodAS_Length_SouthAmerica")

exportImage(activitySSN_diapause_first, bordersOceania, "PhotoperiodAS_FirstDay_Oceania")
exportImage(activitySSN_diapause_last, bordersOceania, "PhotoperiodAS_LastDay_Oceania")
exportImage(activitySSN_diapause_length, bordersOceania, "PhotoperiodAS_Length_Oceania")


// Precipitation activity season start day, end day, and length, image export
exportImage(activitySSN_precip_first, bordersAfrica, "PrecipAS_FirstDay_Africa")
exportImage(activitySSN_precip_last, bordersAfrica, "PrecipAS_LastDay_Africa")
exportImage(activitySSN_precip_length, bordersAfrica, "PrecipAS_Length_Africa")



// TAM, image export
exportImage(TAM, bordersAfrica, "TAM_Africa");
exportImage(TAM, bordersAsia, "TAM_Asia");
exportImage(TAM, bordersEurope, "TAM_Europe");
exportImage(TAM, bordersNorthAmerica, "TAM_NorthAmerica");
exportImage(TAM, bordersCentralAmerica, "TAM_CentralAmerica");
exportImage(TAM, bordersSouthAmerica, "TAM_SouthAmerica");
exportImage(TAM, bordersOceania, "TAM_Oceania");


// TASD, image export
exportImage(TASD, bordersAfrica, "TASD_Africa");
exportImage(TASD, bordersAsia, "TASD_Asia");
exportImage(TASD, bordersEurope, "TASD_Europe");
exportImage(TASD, bordersNorthAmerica, "TASD_NorthAmerica");
exportImage(TASD, bordersCentralAmerica, "TASD_CentralAmerica");
exportImage(TASD, bordersSouthAmerica, "TASD_SouthAmerica");
exportImage(TASD, bordersOceania, "TASD_Oceania");


// PhotoASTM, image export
exportImage(PhotoASTM, bordersAfrica, "PhotoASTM_Africa");
exportImage(PhotoASTM, bordersAsia, "PhotoASTM_Asia");
exportImage(PhotoASTM, bordersEurope, "PhotoASTM_Europe");
exportImage(PhotoASTM, bordersNorthAmerica, "PhotoASTM_NorthAmerica");
exportImage(PhotoASTM, bordersCentralAmerica, "PhotoASTM_CentralAmerica");
exportImage(PhotoASTM, bordersSouthAmerica, "PhotoASTM_SouthAmerica");
exportImage(PhotoASTM, bordersOceania, "PhotoASTM_Oceania");


// PhotoASTSD, image export
exportImage(PhotoASTSD, bordersAfrica, "PhotoASTSD_Africa");
exportImage(PhotoASTSD, bordersAsia, "PhotoASTSD_Asia");
exportImage(PhotoASTSD, bordersEurope, "PhotoASTSD_Europe");
exportImage(PhotoASTSD, bordersNorthAmerica, "PhotoASTSD_NorthAmerica");
exportImage(PhotoASTSD, bordersCentralAmerica, "PhotoASTSD_CentralAmerica");
exportImage(PhotoASTSD, bordersSouthAmerica, "PhotoASTSD_SouthAmerica");
exportImage(PhotoASTSD, bordersOceania, "PhotoASTSD_Oceania");

// PrecipASTM, image export
exportImage(PrecipASTM, bordersAfrica, "PrecipASTM_Africa");
exportImage(PrecipASTM, bordersAsia, "PrecipASTM_Asia");
exportImage(PrecipASTM, bordersEurope, "PrecipASTM_Europe");
exportImage(PrecipASTM, bordersNorthAmerica, "PrecipASTM_NorthAmerica");
exportImage(PrecipASTM, bordersCentralAmerica, "PrecipASTM_CentralAmerica");
exportImage(PrecipASTM, bordersSouthAmerica, "PrecipASTM_SouthAmerica");
exportImage(PrecipASTM, bordersOceania, "PrecipASTM_Oceania");


// PrecipASTSD, image export
exportImage(PrecipASTSD, bordersAfrica, "PrecipASTSD_Africa");
exportImage(PrecipASTSD, bordersAsia, "PrecipASTSD_Asia");
exportImage(PrecipASTSD, bordersEurope, "PrecipASTSD_Europe");
exportImage(PrecipASTSD, bordersNorthAmerica, "PrecipASTSD_NorthAmerica");
exportImage(PrecipASTSD, bordersCentralAmerica, "PrecipASTSD_CentralAmerica");
exportImage(PrecipASTSD, bordersSouthAmerica, "PrecipASTSD_SouthAmerica");
exportImage(PrecipASTSD, bordersOceania, "PrecipASTSD_Oceania");
*/

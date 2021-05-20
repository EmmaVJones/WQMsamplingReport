# WQS information for functions
# From: 9VAC25-260-50. Numerical Criteria for Dissolved Oxygen, Ph, and Maximum Temperature
# https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
WQSvalues <- tibble(CLASS_BASIN = c('I',"II","II_7","III","IV","V","VI","VII"),
                    CLASS = c('I',"II","II","III","IV","V","VI","VII"),
                    `Description Of Waters` = c('Open Ocean', 'Tidal Waters in the Chowan Basin and the Atlantic Ocean Basin',
                                                'Tidal Waters in the Chesapeake Bay and its tidal tributaries',
                                                'Nontidal Waters (Coastal and Piedmont Zone)','Mountainous Zone Waters',
                                                'Stockable Trout Waters','Natural Trout Waters','Swamp Waters'),
                    `Dissolved Oxygen Min (mg/L)` = c(5,4,NA,4,4,5,6,NA),
                    `Dissolved Oxygen Daily Avg (mg/L)` = c(NA,5,NA,5,5,6,7,NA),
                    `pH Min` = c(6,6,6.0,6.0,6.0,6.0,6.0,3.7),
                    `pH Max` = c(9.0,9.0,9.0,9.0,9.0,9.0,9.0,8.0),
                    `Max Temperature (C)` = c(NA, NA, NA, 32, 31, 21, 20, NA)) %>%
  mutate(CLASS_DESCRIPTION = paste0(CLASS, " | ", `Description Of Waters`))

# pH range Exceedance Function
pHSpecialStandardsCorrection <- function(x){
  z <- filter(x, str_detect(as.character(SPSTDS), '6.5-9.5'))
  if(nrow(z) > 0){
    return(
      mutate(x, `pH Min` = case_when(str_detect(as.character(SPSTDS), '6.5-9.5') ~ 6.5, TRUE ~ `pH Min`),
             `pH Max` = case_when(str_detect(as.character(SPSTDS), '6.5-9.5') ~ 9.5, TRUE ~ `pH Max`)))
  }else{return(x)}
}


# Calculate daily thermocline depth and designate Epilimnion vs Hypolimnion
thermoclineDepth <- function(stationData){
  stationData <- stationData %>%
    mutate(SampleDate = as.Date(Fdt_Date_Time)) %>%
    group_by(Fdt_Sta_Id, SampleDate) 
  
  dailyThermDepth <- dplyr::select(stationData, Fdt_Sta_Id, SampleDate, Fdt_Depth, Fdt_Temp_Celcius) %>%
    mutate(DepthDiff = c(NA, diff(Fdt_Depth)),
           TempDiff = c(NA, diff(Fdt_Temp_Celcius))) %>%
    filter(DepthDiff == 1) # get rid of changes less than 1 meter depth
  # Alt route in case shallow lake
  if(nrow(dailyThermDepth) > 0){
    dailyThermDepth <- filter(dailyThermDepth, TempDiff <= -1)
    # one more catch if no thermocline established
    if(nrow(dailyThermDepth) > 0){
      dailyThermDepth <- summarise(dailyThermDepth, ThermoclineDepth = min(Fdt_Depth) - 0.5) %>% ungroup() 
    } else {
      dailyThermDepth <- summarise(stationData, ThermoclineDepth = NA) %>% ungroup()  }
  } else {
    dailyThermDepth <- summarise(stationData, ThermoclineDepth = NA) %>% ungroup() }
  
  
  full_join(stationData, dailyThermDepth, by = c('Fdt_Sta_Id', 'SampleDate')) %>%
    mutate(LakeStratification= ifelse(Fdt_Depth < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>% ungroup() 
}
# stationData %>% thermoclineDepth()

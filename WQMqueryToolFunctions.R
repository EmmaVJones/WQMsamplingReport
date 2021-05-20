
# Organize field and analyte info into prettier table
stationFieldAnalyteDataPretty <- function(stationAnalyteDataRaw, stationFieldDataRaw, averageResults){
  if(averageResults == TRUE){
    if(nrow(stationAnalyteDataRaw) > 0){
      y <- stationAnalyteDataRaw %>%
        group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc) %>%
        dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Com_Code, #Ana_Sam_Mrs_Lcc_Parm_Group_Cd,
                      Pg_Parm_Name, Ana_Uncensored_Value) %>% 
        mutate(`Associated Analyte Records` = 1:n(),
               LabComments = paste0(Pg_Parm_Name,' RMK'))
      y1 <- y %>%
        dplyr::select(Pg_Parm_Name, Ana_Uncensored_Value) %>% 
        pivot_wider(names_from = Pg_Parm_Name, #names_sep = " | ", 
                    values_from = "Ana_Uncensored_Value",
                    values_fn = list(Ana_Uncensored_Value = mean) ) %>% 
        left_join(y %>% ungroup() %>% group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, LabComments) %>%  
                    mutate(Ana_Com_Code2 = paste(Ana_Com_Code, sep = " ")) %>%
                    dplyr::select(LabComments, Ana_Com_Code2) %>% 
                    distinct() %>% 
                    pivot_wider(names_from = LabComments, values_from = Ana_Com_Code2),
                  by = c("Ana_Sam_Fdt_Id", "Fdt_Sta_Id", "Fdt_Date_Time", "Ana_Sam_Mrs_Container_Id_Desc")) %>% 
        dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, sort(names(.)))
    } else {
      y1 <- stationAnalyteDataRaw %>%
        dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc)
    }
    
    suppressWarnings(
      full_join(stationFieldDataRaw, y1, by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) %>%
        arrange(Fdt_Sta_Id, Fdt_Date_Time))
    # original method
    # suppressWarnings(
    # full_join(stationFieldDataRaw,
    #            stationAnalyteDataRaw %>%
    #              filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
    #              group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc) %>%
    #              dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, #Ana_Sam_Mrs_Lcc_Parm_Group_Cd,
    #                            Ana_Parameter_Name, Ana_Uncensored_Value) %>%
    #              pivot_wider(names_from = c('Ana_Parameter_Name'), names_sep = " | ", 
    #                          values_from = "Ana_Uncensored_Value",
    #                          values_fn = list(Ana_Uncensored_Value = mean)),
    #            by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) %>%
    #   arrange(Fdt_Sta_Id, Fdt_Date_Time))
  } else {
    if(nrow(stationAnalyteDataRaw) > 0){
      z <- stationAnalyteDataRaw %>%
        group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Pg_Parm_Name, Ana_Lab_Seq_Num) %>%
        dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Pg_Parm_Name, Ana_Com_Code, Ana_Uncensored_Value) %>%
        mutate(`Associated Analyte Records` = 1:n(),
               LabComments = paste0(Pg_Parm_Name,' RMK'))
      z1 <-  z %>% 
        dplyr::select(`Associated Analyte Records`, Pg_Parm_Name, Ana_Uncensored_Value) %>% 
        pivot_wider(names_from = Pg_Parm_Name, values_from = Ana_Uncensored_Value) %>%
        left_join(z %>% ungroup() %>% group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc,Ana_Lab_Seq_Num) %>%  
                    dplyr::select(`Associated Analyte Records`, LabComments, Ana_Com_Code) %>% 
                    pivot_wider(names_from = LabComments, values_from = Ana_Com_Code),
                  by = c("Ana_Sam_Fdt_Id", "Fdt_Sta_Id", "Fdt_Date_Time", "Ana_Sam_Mrs_Container_Id_Desc", "Ana_Lab_Seq_Num", 
                         "Associated Analyte Records")) %>% 
        dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Lab_Seq_Num, 
                      `Associated Analyte Records`, sort(names(.)))
    } else {
      z1 <- stationAnalyteDataRaw %>% 
        mutate(`Associated Analyte Records` = NA) %>% 
        dplyr::select( Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Lab_Seq_Num, 
                       `Associated Analyte Records`)   }
    suppressWarnings(
      full_join(stationFieldDataRaw, 
                z1,
                # stationAnalyteDataRaw %>%
                #   filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
                #   group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Pg_Parm_Name) %>%
                #   dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Lab_Seq_Num, Pg_Parm_Name , Ana_Uncensored_Value) %>%
                #   mutate(`Associated Analyte Records` = 1:n()) %>% 
                #   pivot_wider(names_from = 'Pg_Parm_Name',values_from = "Ana_Uncensored_Value") ,
                by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) %>% 
        dplyr::select(`Associated Analyte Records`, everything()) %>%
        arrange(Fdt_Sta_Id, Fdt_Date_Time) ) }
  # full_join(stationFieldDataRaw,
  #            stationAnalyteDataRaw %>%
  #              filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
  #              group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd) %>%
  #              dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd, Ana_Parameter_Name, Ana_Uncensored_Value) %>%
  #              pivot_wider(names_from = c('Ana_Parameter_Name','Ana_Sam_Mrs_Lcc_Parm_Group_Cd'), names_sep = " | ", 
  #                          values_from = "Ana_Uncensored_Value"),
  #            by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) )}
}



# Concatenate multiple columns to one
concatenateCols <- function(df, containString){
  x <- dplyr::select(df, contains(containString))  %>% 
    dplyr::select(-contains('RMK'))
  if(length(x) > 0){
    mutate_if(x, is.numeric, as.character) %>%
      na_if('NA') %>%
      unite(newCol, contains(containString), na.rm = TRUE) %>%
      mutate(newCol = as.numeric(newCol)) %>%
      pull()} else {as.numeric(rep(NA, nrow(x)))}
}

concatenateCols2 <- function(df, containString){
  x <- dplyr::select(df, contains(containString))
  if(length(x) == 0){as.numeric(rep(NA, nrow(x)))}
  if(length(x) == 1){as.numeric(x %>% pull()) }
  if(length(x) > 1){
    mutate_if(x, is.numeric, as.character) %>%
      na_if('NA') %>%
      unite(newCol, contains(containString), na.rm = TRUE) %>%
      mutate(newCol = as.numeric(newCol)) %>%
      pull()} 
}


basicSummaryPlusSpatial <- function(windowInfo, windowInfoAnalytes, WQM_Stations_Spatial, WQSlookup, WQSvalues){
  
  
  stationFieldAnalyte <- stationFieldAnalyteDataPretty(windowInfoAnalytes, windowInfo, averageResults = TRUE) %>% 
    left_join(WQM_Stations_Spatial, by = c("Fdt_Sta_Id" = "StationID")) %>% 
    left_join(WQSlookup, by = c('Fdt_Sta_Id'='StationID')) %>%
    mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
    mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
    # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
    left_join(WQSvalues, by = 'CLASS_BASIN') %>%
    dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
    rename('CLASS' = 'CLASS.x') %>% 
    pHSpecialStandardsCorrection() %>% 
    thermoclineDepth()
    
  
  
  suppressWarnings(stationFieldAnalyte %>% 
    # left_join(stationFieldAnalyte, 
    #           dplyr::select(windowInfoWQS, Fdt_Id, Latitude:LakeStratification ), by = 'Fdt_Id') %>% 
      mutate(blankColForSelect = NA, # placeholder to enable selection below
           StationID = Fdt_Sta_Id,
           `Collection Date` = Fdt_Date_Time,
           Comments = Fdt_Comment, 
           `Collector ID` = Fdt_Collector_Id,
           `Run ID` = Fdt_Run_Id,
           `SPG Code` = Fdt_Spg_Code,
           `SPG Description` = Spg_Description,
           Depth = Fdt_Depth,
           `Weather Code` = Fdt_Weather_Code,
           `Tide Code` = Fdt_Tide_Code,
           Temperature = Fdt_Temp_Celcius,
           pH = Fdt_Field_Ph,
           `Dissolved Oxygen` = case_when(!is.na(Fdt_Do_Probe) ~ Fdt_Do_Probe,
                                          !is.na(Fdt_Do_Optical) ~ Fdt_Do_Optical,
                                          !is.na(Fdt_Do_Winkler) ~ Fdt_Do_Winkler,
                                          TRUE ~ as.numeric(NA)),
           `DO Percent Saturation` = Fdt_Do_Satr_Per,
           `Specific Conductance` = Fdt_Specific_Conductance,
           Salinity = Fdt_Salinity,
           Turbidity = Fdt_Turbidity,
           `Secchi Depth` = Fdt_Secchi_Depth, 
           Hardness = concatenateCols(stationFieldAnalyte, 'HARDNESS, TOTAL (MG/L AS CACO3)'),
           Ecoli = concatenateCols(stationFieldAnalyte, 'E.COLI BY COLILERT SM 9223-B'),
           Enterococci = concatenateCols(stationFieldAnalyte, 'ENTEROCOCCI- ME-MF N0/100ML'),
           `Fecal Coliform` = concatenateCols(stationFieldAnalyte, 'FECAL COLIFORM,MEMBR FILTER,M-FC BROTH,44.5 C'),
           `Total Nitrogen` = concatenateCols(stationFieldAnalyte, 'NITROGEN, TOTAL (MG/L AS N)'),
           `Total Nitrate Nitrogen` = concatenateCols(stationFieldAnalyte, 'NITRITE NITROGEN, TOTAL (MG/L AS N)'),
           `Total Kjeldahl Nitrogen` = concatenateCols(stationFieldAnalyte, 'NITROGEN, KJELDAHL, TOTAL, (MG/L AS N)'),
           `Ammonia` = concatenateCols(stationFieldAnalyte, 'NITROGEN, AMMONIA, TOTAL (MG/L AS N)'),
           `Total Phosphorus` = concatenateCols(stationFieldAnalyte, 'PHOSPHORUS, TOTAL (MG/L AS P)'),
           `Ortho Phosphorus` = concatenateCols(stationFieldAnalyte, 'PHOSPHORUS, DISSOLVED ORTHOPHOSPHATE (MG/L AS P)'),
           `Chlorophyll a` = concatenateCols(stationFieldAnalyte, 'CHLOROPHYLL-A UG/L SPECTROPHOTOMETRIC ACID. METH'),
           Turbidity = concatenateCols(stationFieldAnalyte, 'TURBIDITY,LAB NEPHELOMETRIC TURBIDITY UNITS, NTU'),
           `Total Dissolved Solids` = concatenateCols(stationFieldAnalyte, 'TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L'),
           `Total Suspended Solids` = concatenateCols(stationFieldAnalyte, 'SUSP. SED. CONC. TOTAL, MG/L,(Method B)'),
           `Suspended Sediment Concentration Coarse` = concatenateCols(stationFieldAnalyte, 'SUSP. SED. CONC. - >62 um,MG/L, (Method C)'),
           `Suspended Sediment Concentration Fine` = concatenateCols(stationFieldAnalyte, 'SUSP. SED. CONC. - <62 um,MG/L, (Method C)'),
           `Calcium` = concatenateCols(stationFieldAnalyte, 'CALCIUM, DISSOLVED (MG/L AS CA)'),
           `Magnesium` = concatenateCols(stationFieldAnalyte, 'MAGNESIUM, DISSOLVED (MG/L AS MG)'),
           `Sodium` = concatenateCols(stationFieldAnalyte, 'SODIUM, DISSOLVED (MG/L AS NA)'),
           `Potassium` = concatenateCols(stationFieldAnalyte, 'POTASSIUM, DISSOLVED (MG/L AS K)'),
           `Chloride` = concatenateCols(stationFieldAnalyte, 'CHLORIDE, DISSOLVED IN WATER MG/L'),
           `Sulfate` = concatenateCols(stationFieldAnalyte, 'SULFATE, DISSOLVED (MG/L AS SO4)'),
           `Arsenic` = concatenateCols(stationFieldAnalyte, "ARSENIC, DISSOLVED  (UG/L AS AS)"),
           `Barium` = concatenateCols(stationFieldAnalyte, "BARIUM, DISSOLVED (UG/L AS BA)"),
           `Beryllium` = concatenateCols(stationFieldAnalyte, "BERYLLIUM, DISSOLVED (UG/L AS BE)"),
           `Cadmium` = concatenateCols(stationFieldAnalyte,  "CADMIUM, DISSOLVED (UG/L AS CD)"),
           `Chromium` = concatenateCols(stationFieldAnalyte, "CHROMIUM, DISSOLVED (UG/L AS CR)"),
           `Copper` = concatenateCols(stationFieldAnalyte, "COPPER, DISSOLVED (UG/L AS CU)"),
           `Iron` = concatenateCols(stationFieldAnalyte, "IRON, DISSOLVED (UG/L AS FE)"),
           `Lead` = concatenateCols(stationFieldAnalyte, "LEAD, DISSOLVED (UG/L AS PB)"), 
           `Manganese` = concatenateCols(stationFieldAnalyte, "MANGANESE, DISSOLVED (UG/L AS MN)"),
           `Thallium` = concatenateCols(stationFieldAnalyte, "THALLIUM, DISSOLVED (UG/L AS TL)"),
           `Nickel` = concatenateCols(stationFieldAnalyte, "NICKEL, DISSOLVED (UG/L AS NI)"),
           `Silver` = concatenateCols(stationFieldAnalyte, "SILVER, DISSOLVED (UG/L AS AG)"),
           `Strontium` = concatenateCols(stationFieldAnalyte, "STRONTIUM, DISSOLVED (UG/L AS SR)"),
           `Zinc` = concatenateCols(stationFieldAnalyte, "ZINC, DISSOLVED (UG/L AS ZN)"),
           `Antimony` = concatenateCols(stationFieldAnalyte, "ANTIMONY, DISSOLVED (UG/L AS SB)"),
           `Aluminum` = concatenateCols(stationFieldAnalyte, "ALUMINUM, DISSOLVED (UG/L AS AL)"),
           `Selenium` = concatenateCols(stationFieldAnalyte, "SELENIUM, DISSOLVED (UG/L AS SE)"),
           `Fecal Coliform` = concatenateCols(stationFieldAnalyte, "FECAL COLIFORM,MEMBR FILTER,M-FC BROTH,44.5 C"),                       
           `Total Organic Carbon` = concatenateCols(stationFieldAnalyte, 'CARBON, TOTAL ORGANIC (MG/L AS C)'),
           `Dissolved Organic Carbon` = concatenateCols(stationFieldAnalyte, 'CARBON, DISSOLVED ORGANIC (MG/L AS C)'),
           `Benthic Ash Free Dry Mass` = concatenateCols(stationFieldAnalyte, 'BENTHIC ASH FREE DRY MASS, GM/M2'),
           `Benthic Chlorophyll a` = concatenateCols(stationFieldAnalyte, 'BENTHIC CHLOROPHYLL A, MG/M2'), # billy addition
           `Benthic Chlorophyll b` = concatenateCols(stationFieldAnalyte, 'BENTHIC, CHLOROPHYLL B, MG/M2'),# billy addition
           `Benthic Pheophytin a` = concatenateCols(stationFieldAnalyte, 'BENTHIC, PHEOPHYTIN A, MG/M2')) %>% # billy addition
      dplyr::select(StationID, `Collection Date`, Comments, `Collector ID`, `Run ID`, `SPG Code`, `SPG Description`,
                    Depth, `Weather Code`, `Tide Code`, Temperature, pH, `Dissolved Oxygen`, `DO Percent Saturation`,
                    `Specific Conductance`, Salinity, Turbidity, `Secchi Depth`, Hardness, Ecoli, Enterococci, `Fecal Coliform`,
                    `Total Nitrogen`, `Total Nitrate Nitrogen`, `Total Kjeldahl Nitrogen`, `Ammonia`, `Total Phosphorus`, `Ortho Phosphorus`, 
                    `Chlorophyll a`, Turbidity, `Total Dissolved Solids`, `Total Suspended Solids`, `Suspended Sediment Concentration Coarse`, 
                    `Suspended Sediment Concentration Fine`, `Calcium`, `Magnesium`, `Sodium`, `Potassium`, `Chloride`, `Sulfate`, `Arsenic`, 
                    `Barium`, `Beryllium`, `Cadmium`, `Chromium`, `Copper`, `Iron`, `Lead`, `Manganese`, `Thallium`, `Nickel`, `Silver`, 
                    `Strontium`, `Zinc`, `Antimony`, `Aluminum`, `Selenium`, `Fecal Coliform`, `Total Organic Carbon`, `Dissolved Organic Carbon`, 
                    `Benthic Ash Free Dry Mass`,`Benthic Chlorophyll a`, `Benthic Chlorophyll b`, `Benthic Pheophytin a`, 
                    Latitude:LakeStratification )   )
}


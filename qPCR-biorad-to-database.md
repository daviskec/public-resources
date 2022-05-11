qPCR\_BioRad\_data\_to\_database
================
Katie Davis
11 May 2022

-   [Load Packages](#load-packages)
-   [Create a Dataframe to Support Your qPCR
    Data](#create-a-dataframe-to-support-your-qpcr-data)
-   [Read in Data from Bio-Rad Proprietary Software
    Output](#read-in-data-from-bio-rad-proprietary-software-output)
-   [Remove Records as Needed](#remove-records-as-needed)
-   [Save Data to a Database](#save-data-to-a-database)
-   [Manipulate Data to Create a Summary Table of Species
    Presence/Absence](#manipulate-data-to-create-a-summary-table-of-species-presenceabsence)

Goal: Pull qPCR output data from BioRad proprietary software into a
dataframe which can then be sent to a database or manipulated

Data: .csv files produced when selecting Export -&gt; Export all
datasheets -&gt; CSV in Bio-Rad CFX manager 3.1

## Load Packages

``` r
# Load Packages
require(RSQLite)
```

    ## Loading required package: RSQLite

``` r
require(sf)
```

    ## Loading required package: sf

    ## Linking to GEOS 3.9.1, GDAL 3.2.1, PROJ 7.2.1; sf_use_s2() is TRUE

``` r
require(tidyverse)
```

    ## Loading required package: tidyverse

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
require(readxl)
```

    ## Loading required package: readxl

``` r
require(readxl)
```

## Create a Dataframe to Support Your qPCR Data

``` r
#we create an empty data frame in r
 Multi1<- data.frame(LabID = 1:96, #a qPCR plate will only contain 96 wells; this gets replaced with your SampleID in the loop below
                     PlateName = NA,
                     Well = NA,
                     Date = NA,
                     Notes = NA,
                     PSMACall = NA,
                     PSMAcq = NA,
                     PSMAmaxfl = NA,
                     AMMACall = NA,
                     AMMAcq = NA,
                     AMMAmaxfl = NA,
                     ANBOCall = NA,
                     ANBOcq = NA,
                     ANBOmaxfl = NA,
                     LIPICall = NA,
                     LIPIcq = NA,
                     LIPImaxfl = NA,
                     IntConCall = NA,
                     IntConcq = NA,
                     IntConmaxfl = NA
)

#if you only ran a few plates, you could presumably add all output onto this dataframe and do any queries/analysis right here in R
#the dataframe is named Multi1 because we are working with the Murphy lab's Multiplex 1 (combination of species and fluorescence that work well together)
#we have a second multiplex that contains different combinations of species
```

## Read in Data from Bio-Rad Proprietary Software Output

``` r
setwd(paste0(here::here(), "/qPCR_Data/23Feb22")) #set your working directory to the folder where you have saved the BioRad output .csv files
#I recommend a master folder qPCR_data/ followed by subfolders for each plate (e.g. home/qPCR_data/GYPlate1, something like that)
#so here you will need to define the subfolder for the plate, as we are only loading data for one plate at a time (presumably immediately after running the plate and visually inspecting the data, adjusting thresholds, etc.)

PlateName = "23Feb22GYE" #change this to your plate naming scheme
Date = "23 Feb 2022" #change this to the date the qPCR was run
fileprefix = "20220223_143030_CT013542_RT_QPCR_MULT" #copy this from the prefix of your exported csv files (everything before the space and -)

data <- read.csv(paste0(fileprefix, " -  Quantification Summary_0", ".csv"))  #do not change this, this file contains the cq values (cycle where fluorescence was first detected)

PSMA <- read.csv(paste0(fileprefix, " -  End Point Results_FAM", ".csv"))  #do not change these, these files are specific to each fluorescence, and we'll pull two values from them
LIPI <- read.csv(paste0(fileprefix, " -  End Point Results_Quasar 705", ".csv"))  #you should change the names of the objects though
IntCon <- read.csv(paste0(fileprefix, " -  End Point Results_VIC", ".csv"))  #i have the objects named as the species code of the species to which each fluor. corresponds
ANBO <- read.csv(paste0(fileprefix, " -  End Point Results_Cy5", ".csv"))  #since BioRad can only read five fluorescences, you shouldn't have more than 5
AMMA <- read.csv(paste0(fileprefix, " -  End Point Results_Cal Red 610", ".csv"))  #don't change the filenames, just the R object names

wells <- c("A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09", "B10", "B11", "B12", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10", "D11", "D12", "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12", "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12", "G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12", "E01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12") #do not change this


Multi1$Well <- wells #do not change this

for (i in 1:nrow(Multi1)) {
  Multi1$PlateName[i] <- PlateName
  Multi1$Date[i] <- Date
  Multi1$LabID[i] <- filter(data, Well == Multi1$Well[i])$Sample #MAKE SURE YOUR SAMPLE NAMES ARE IN THE BIORAD PLATE FILE YOU CREATE (before exporting everything as a .csv)
  Multi1$PSMAcq[i] <- filter(data, Well == Multi1$Well[i], Fluor == "FAM")$Cq #FAM fluorescence corresponds to Pseudacris maculata (PSMA)
  Multi1$AMMAcq[i] <- filter(data, Well == Multi1$Well[i], Fluor == "Cal Red 610")$Cq #Cal Red fluorescence corresponds to Ambystoma mavortium (AMMA)
  Multi1$ANBOcq[i] <- filter(data, Well == Multi1$Well[i], Fluor == "Cy5")$Cq #Cy5 fluorescence corresponds to Anaxyrus boreas (ANBO)
  Multi1$LIPIcq[i] <- filter(data, Well == Multi1$Well[i], Fluor == "Quasar 705")$Cq #Quasar fluorescence corresponds to Lithobates pipiens (LIPI)
  Multi1$IntConcq[i] <- filter(data, Well == Multi1$Well[i], Fluor == "VIC")$Cq #VIC fluorescence corresponds to the Internal Control (Intcon)
  #change names in data frame above to correspond to your species, then change throughout this loop as well
  #Cq gives us the cycle at which fluorescence is first detected
  
  Multi1$PSMACall[i] <- filter(PSMA, Well == Multi1$Well[i])$CallType #this gives us the BioRad program's call of positive, negative, or no call
  Multi1$PSMAmaxfl[i] <- filter(PSMA, Well == Multi1$Well[i])$End.RFU #this gives us the maximum relative fluorescence for each sample, each fluorescence
  
  Multi1$ANBOCall[i] <- filter(ANBO, Well == Multi1$Well[i])$CallType #repeat, next fluorescence
  Multi1$ANBOmaxfl[i] <- filter(ANBO, Well == Multi1$Well[i])$End.RFU
  
  Multi1$AMMACall[i] <- filter(AMMA, Well == Multi1$Well[i])$CallType #repeat, next fluorescence
  Multi1$AMMAmaxfl[i] <- filter(AMMA, Well == Multi1$Well[i])$End.RFU
  
  Multi1$LIPICall[i] <- filter(LIPI, Well == Multi1$Well[i])$CallType #repeat, next fluorescence
  Multi1$LIPImaxfl[i] <- filter(LIPI, Well == Multi1$Well[i])$End.RFU
  
  Multi1$IntConCall[i] <- filter(IntCon, Well == Multi1$Well[i])$CallType #repeat, next fluorescence
  Multi1$IntConmaxfl[i] <- filter(IntCon, Well == Multi1$Well[i])$End.RFU
  
  Multi1[Multi1 == "NaN"] <- NA #replace BioRad's "NaN" with NA
  Multi1[Multi1 == ""] <- NA #Replace empty character strings with NA
}
```

## Remove Records as Needed

``` r
#I have a well that I need to remove because I know it was contaminated
Multi1 <- Multi1  %>% filter(Well != "E01")

#I also don't want to keep my positive and negative controls in this part of my database
Multi1 <- Multi1 %>% filter(is.na(LabID) == FALSE)

#I also have one sample in this plate that is part of another project, so I don't need to keep it in this database
Multi1 <- Multi1 %>% filter(LabID != "RC0086")
```

## Save Data to a Database

``` r
dbConn <- dbConnect(SQLite(), paste0(here::here(), "/Davis_GYA_data.gpkg")) #path to your database (mine is a geopackage)

dbWriteTable(dbConn, name = "tblqPCRoutput", Multi1, append = TRUE) # do not run this more than once. It adds data to the table, so if you repeat with the same dataframe,  you will duplicate all of your information
#if you want to upload results from another plate, you'll have to go change PlateName, Date, fileprefix, and probably your working directory, depending on how you have it set up

#if you are not using a database, here you could write yours results as R objects or .csv files, and combine results from multiple plates with further data wrangling
```

## Manipulate Data to Create a Summary Table of Species Presence/Absence

``` r
#using R, read your table with all of your qPCR data in and create a summary table that calls Presence/Absence based on if you have at least 2 positive calls form biorad
tbl(dbConn, "tblqPCRoutput") %>%
  select(LabID, PSMACall, AMMACall, ANBOCall, LIPICall) %>% 
  group_by(LabID) %>% 
  summarize(PSMA = ifelse(sum(PSMACall == "Positive")>1, "Present", "Absent"),
            AMMA = ifelse(sum(AMMACall == "Positive")>1, "Present", "Absent"),
            ANBO = ifelse(sum(ANBOCall == "Positive")>1, "Present", "Absent"),
            LIPI = ifelse(sum(LIPICall == "Positive")>1, "Present", "Absent"))
```

    ## Warning: Missing values are always removed in SQL.
    ## Use `SUM(x, na.rm = TRUE)` to silence this warning
    ## This warning is displayed only once per session.

    ## # Source:   lazy query [?? x 5]
    ## # Database: sqlite 3.38.0
    ## #   [C:\Users\Katie\Documents\Repositories\PhD\Davis_GYA_data.gpkg]
    ##    LabID   PSMA    AMMA    ANBO    LIPI  
    ##    <chr>   <chr>   <chr>   <chr>   <chr> 
    ##  1 GY00001 Present Present Absent  Absent
    ##  2 GY00002 Absent  Present Absent  Absent
    ##  3 GY00003 Present Present Absent  Absent
    ##  4 GY00004 Absent  Absent  Absent  Absent
    ##  5 GY00005 Present Present Present Absent
    ##  6 GY00006 Present Present Absent  Absent
    ##  7 GY00008 Present Absent  Absent  Absent
    ##  8 GY00009 Present Present Absent  Absent
    ##  9 GY00010 Present Present Absent  Absent
    ## 10 GY00011 Present Present Absent  Absent
    ## # ... with more rows

``` r
#or, if you want to build a self-updating table in your database, use the show_query function to get the SQL code for this same query
show_query(tbl(dbConn, "tblqPCRoutput") %>%
  select(LabID, PSMACall, AMMACall, ANBOCall, LIPICall) %>% 
  group_by(LabID) %>% 
  summarize(PSMA = ifelse(sum(PSMACall == "Positive")>1, "Present", "Absent"),
            AMMA = ifelse(sum(AMMACall == "Positive")>1, "Present", "Absent"),
            ANBO = ifelse(sum(ANBOCall == "Positive")>1, "Present", "Absent"),
            LIPI = ifelse(sum(LIPICall == "Positive")>1, "Present", "Absent")))
```

    ## <SQL>
    ## SELECT `LabID`, CASE WHEN (SUM(`PSMACall` = 'Positive') > 1.0) THEN ('Present') WHEN NOT(SUM(`PSMACall` = 'Positive') > 1.0) THEN ('Absent') END AS `PSMA`, CASE WHEN (SUM(`AMMACall` = 'Positive') > 1.0) THEN ('Present') WHEN NOT(SUM(`AMMACall` = 'Positive') > 1.0) THEN ('Absent') END AS `AMMA`, CASE WHEN (SUM(`ANBOCall` = 'Positive') > 1.0) THEN ('Present') WHEN NOT(SUM(`ANBOCall` = 'Positive') > 1.0) THEN ('Absent') END AS `ANBO`, CASE WHEN (SUM(`LIPICall` = 'Positive') > 1.0) THEN ('Present') WHEN NOT(SUM(`LIPICall` = 'Positive') > 1.0) THEN ('Absent') END AS `LIPI`
    ## FROM (SELECT `LabID`, `PSMACall`, `AMMACall`, `ANBOCall`, `LIPICall`
    ## FROM `tblqPCRoutput`)
    ## GROUP BY `LabID`

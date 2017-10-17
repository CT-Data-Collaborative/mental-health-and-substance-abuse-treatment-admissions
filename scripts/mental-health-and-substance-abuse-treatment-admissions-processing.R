library(dplyr)
library(datapkg)
library(readxl)

##################################################################
#
# Processing Script for Mental Health and Substance Abuse Treatment Admissions
# Created by Jenna Daly
# On 10/16/2017
#
##################################################################

source('./scripts/getPopulation.R')

sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
x10_15_file <- dir(path_to_raw, recursive=T, pattern = "xlsx")
all_csvs <- dir(path_to_raw, recursive=T, pattern = ".csv")
adm_csvs <- grep("fy", all_csvs, value=T)

#Bring in x10-15 file

#read in entire xls file (all sheets)
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets(paste0(path_to_raw, "/", x10_15_file))

for (i in 1:length(mysheets)) {
  get_year <- names(mysheets[i])
  current_sheet_file <- mysheets[[i]] 
  current_sheet_file$Year <- as.numeric(get_year)
  assign(paste0("adm", "_", get_year), current_sheet_file)
}

#Bring in files after 15
# for (i in 1:length(adm_csvs)) {
#   current_file <- read.csv(paste0(path_to_raw, "/", adm_csvs[1]), stringsAsFactors=F, header=T, check.names=F)
#   get_year <- unlist(gsub("[^0-9]", "", unlist(adm_csvs[1])), "")
#   get_year <- as.numeric(get_year) + 2000
#   current_file$Year <- get_year 
#   assign(paste0("adm", "_", get_year), current_file)
# }

#Find all df with "adm_"
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
adm_files <- grep("adm_", dfs, value=T)

#Combine all adm dfs
adm_data <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(adm_files)) {
  current_file <- get(adm_files[i])
  adm_data <- rbind(adm_data, current_file)
}

#Use xwalk for sub-towns
subtowns <- read.csv(paste0(path_to_raw, "/", "subtowns.csv"), stringsAsFactors=F, header=T, check.names=F)

names(adm_data)[names(adm_data) == "Town"] <- "Subtown"

adm_data_merged <- merge(adm_data, subtowns, by = "Subtown", all.x=T)

#if Town is blank, copy over Subtown column
adm_data_merged$Town[which(is.na(adm_data_merged$Town))] <- adm_data_merged$Subtown[which(is.na(adm_data_merged$Town))]

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

#filter using fips list
adm_data_merged_fips <- merge(fips, adm_data_merged, by="Town", all.x=T)

#Remove subtown column
adm_data_merged_fips$Subtown <- NULL

#Recode all '-' as NAs
cols <- c("MH Only", "SA Only", "MH & SA", "Total")
adm_data_merged_fips[cols][adm_data_merged_fips[cols] == "-"] <- NA

#Set these same columns to numeric
adm_data_merged_fips[cols] <- sapply(adm_data_merged_fips[cols],as.numeric)

#Roll up subtowns
adm_data_merged_fips <- adm_data_merged_fips %>% 
  group_by(Town, Year) %>% 
  mutate(`Mental Health` = sum(`MH Only`), 
         `Substance Abuse` = sum(`SA Only`), 
         `Mental Health and Substance Abuse` = sum(`MH & SA`), 
         `Total Num` = sum(Total)) %>% 
  select(Town, FIPS, `Mental Health`, `Substance Abuse`, `Mental Health and Substance Abuse`, `Total Num`)

adm_data_merged_fips <- unique(adm_data_merged_fips)

#Backfill years (for those towns that do not have complete years data)
years <- c("2010", 
           "2011", 
           "2012", 
           "2013",
           "2014",
           "2015"
           # ,
           # "2016"
           )

backfill <- expand.grid(
  `Town` = unique(fips$`Town`),
  `Year` = years
)

adm_data_backfill <- merge(adm_data_merged_fips, backfill, all.y=T)

#Merge FIPS back in
adm_data_backfill_fips <- merge(adm_data_backfill, fips, by = "Town", all.x=T)

#Clean up columns and rows (remove CT) and calculate percents
adm_data_clean <- adm_data_backfill_fips %>% 
  select(Town, FIPS.y, Year, `Mental Health`, `Substance Abuse`, `Mental Health and Substance Abuse`, `Total Num`) %>% 
  filter(Town != "Connecticut") %>% 
  rename(FIPS = FIPS.y, Total = `Total Num`) %>% 
  mutate(`Mental Health Percent` = round((`Mental Health` / Total)*100, 2),
         `Substance Abuse Percent` = round((`Substance Abuse` / Total)*100, 2),
         `Mental Health and Substance Abuse Percent` = round((`Mental Health and Substance Abuse` / Total)*100, 2))

##################################################################################
## read population data for denominators in rate calculations
pop <- read.csv(paste0(path_to_raw, "/", "populations.csv"), stringsAsFactors=F, header=T, check.names=F)

pop$FIPS <- gsub("^", "0", pop$FIPS)

adm_data_clean_pop <- merge(adm_data_clean, pop, by = c("FIPS", "Year"), all.x=T)

# Helper function for MOE
calcMOE <- function(x, y, moex, moey) {
  moex2 <- moex^2
  moey2 <- moey^2
  d <- x/y
  d2 <- d^2
  
  radicand <- ifelse(
    moex2 < (d2 * moey2),
    moex2 + (d2 * moey2),
    moex2 - (d2 * moey2)
  )
  
  return(sqrt(radicand)/y)
}

#rates are calculated per 10,000
adm_data_clean_pop <- adm_data_clean_pop %>% 
  mutate(Pop = (Pop/1e4), 
         MOE = (MOE/1e4))

# calculate rates with population denominators,
# keep MOES, calculating appropriately
adm_data_clean_pop_rate <- adm_data_clean_pop %>% 
  mutate(`MH Rate` = round((`Mental Health` / Pop), 2),
         `MH Rate MOE` = round((calcMOE(`Mental Health`, Pop, 0, MOE)), 2),
         `SA Rate` = round((`Substance Abuse` / Pop), 2),
         `SA Rate MOE` = round((calcMOE(`Substance Abuse`, Pop, 0, MOE)), 2),
         `MHSA Rate` = round((`Mental Health and Substance Abuse` / Pop), 2),
         `MHSA Rate MOE` = round((calcMOE(`Mental Health and Substance Abuse`, Pop, 0, MOE)), 2), 
         `Total Rate` = round((`Total` / Pop), 2),
         `Total Rate MOE` = round((calcMOE(`Total`, Pop, 0, MOE)), 2))
         
nulls <- c("Age Range", "Pop", "MOE")
adm_data_clean_pop_rate[nulls] <- NULL

# melt adm_data_clean_pop_rate
adm_data_long <- melt(
  adm_data_clean_pop_rate,
  id.vars = c("Town", "FIPS", "Year"),
  variable.name = "Type",
  variable.factor = F,
  value.name = "Value",
  value.factor = F
)

adm_data_long$Type <- as.character(adm_data_long$Type)

#Assign Admission Type, Measure Type, and Variable columns

adm_data_long$`Admission Type` <- NA
find_mh <- c("Mental Health", "MH ")
adm_data_long$`Admission Type`[grep(paste(find_mh, collapse="|"), adm_data_long$Type)] <- "Mental Health"
find_sa <- c("^Substance Abuse", "SA ")
adm_data_long$`Admission Type`[grep(paste(find_sa, collapse="|"), adm_data_long$Type)] <- "Substance Abuse"
find_mhsa <- c("^Mental Health and Substance Abuse", "MHSA ")
adm_data_long$`Admission Type`[grep(paste(find_mhsa, collapse="|"), adm_data_long$Type)] <- "Mental Health and Substance Abuse"
adm_data_long$`Admission Type`[grep("Total", adm_data_long$Type)] <- "Total"

adm_data_long$`Measure Type` <- "Number"
adm_data_long$`Measure Type`[grep("Percent", adm_data_long$Type)] <- "Percent"
adm_data_long$`Measure Type`[grep("Rate", adm_data_long$Type)] <- "Rate (per 10,000)"

adm_data_long$Variable <- "DMHAS Admissions"
adm_data_long$Variable[grep("MOE", adm_data_long$Type)] <- "Margins of Error"

#Order and sort columns
adm_data_long <- adm_data_long %>% 
  select(Town, FIPS, Year, `Admission Type`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Admission Type`)

# Write to File
write.table(
  adm_data_long,
  file.path(getwd(), "data", "dmhas_admissions_2015.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)


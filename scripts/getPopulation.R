library(acs)
source('./scripts/acsHelpers.R')

# ACS B01001
# Get geography object for CT and subcounty divisions
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2015,
    table = "B01001"
)

pops <- data.table()
for (data in acsdata) {
    year <- data@endyear

    pop.total <- acsSum(data, 1, "Total")

    datafips <- data.table(fips = getACSFips(data))

    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(pop.total)
    )

    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "Pop",
        value.factor = F
    )
    
    estimates$`Age Range`[estimates$`Age Range` == "HD01_VD01.Estimate; Total:"] <- "Total"

    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(pop.total) * 1.645
    )

    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "MOE",
        value.factor = F
    )
    
    moes$`Age Range`[moes$`Age Range` == "HD01_VD01.Estimate; Total:"] <- "Total"
    
    setkey(estimates, FIPS, Year, `Age Range`)
    setkey(moes, FIPS, Year, `Age Range`)

    pops <- rbind(pops, estimates[moes])
}

pops <- pops[pops$FIPS != "0900100000",]

# Write to File
write.table(
    pops,
    file.path(getwd(), "raw", "populations.csv"),
    sep = ",",
    row.names = F
)
## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----PackageCheck, echo = FALSE-----------------------------------------------
required <- c("tidyverse", "MortalityLaws", "reshape2")
if (!all(sapply(required, 
                function(pkg) requireNamespace(pkg, quietly = TRUE)))) {
  message(paste("This vignette needs the followig packages:\n\t", 
                paste(required, collapse = " "), 
                "\nSince not all are installed, code will not be executed: "))
  knitr::opts_chunk$set(eval = FALSE)
}

## ----setup--------------------------------------------------------------------
library(tidyverse, quietly = TRUE)
library(MortalityTables)
mortalityTables.load("Austria_Census")

## -----------------------------------------------------------------------------
mortalityTables.load("Austria_Annuities_RR67")
plotMortalityTables(RR67, mort.AT.census.1961.male)

## ----EROMF85, results="hide"--------------------------------------------------
mortalityTables.load("Austria_Annuities_EROMF")
EROM85.male
EROF85.female

EROM.G1950.male
EROM.G1950.male.av
EROF.G1950.female
EROF.G1950.female.av

## ----AVOe1996R, results="hide"------------------------------------------------
mortalityTables.load("Austria_Annuities_AVOe1996R")
AVOe1996R
AVOe1996R.male
AVOe1996R.male.av325
AVOe1996R.male.group
AVOe1996R.female
AVOe1996R.female.av325
AVOe1996R.female.group

## ----AVOe2005R, results="hide"------------------------------------------------
mortalityTables.load("Austria_Annuities_AVOe2005R")
AVOe2005R
AVOe2005R.male
AVOe2005R.male.group
AVOe2005R.male.av
AVOe2005R.male.group.av
AVOe2005R.male.unloaded
AVOe2005R.male.nodamping
AVOe2005R.male.nodamping.group
AVOe2005R.male.nodamping.unloaded

AVOe2005R.female
AVOe2005R.female.group
AVOe2005R.female.av
AVOe2005R.female.group.av
AVOe2005R.female.unloaded
AVOe2005R.female.nodamping
AVOe2005R.female.nodamping.group
AVOe2005R.female.nodamping.unloaded

AVOe2005R.unisex
AVOe2005R.unisex.group
AVOe2005R.unisex.av
AVOe2005R.unisex.group.av
AVOe2005R.unisex.nodamping
AVOe2005R.unisex.nodamping.group

## ----AT.Annuity.Comparison----------------------------------------------------
plotMortalityTables(RR67, EROM85.male, EROF85.female, EROM.G1950.male.av, EROF.G1950.female.av, AVOe1996R[, "Einzel"], AVOe2005R[c("m", "w"), "Einzel","loaded"],
                    aes = aes(color = year), Period = 2020, title = "Austrian Annuity Tables, Period 2020", 
                    legend.position = "right"
) + facet_grid(sex ~ .)

plotMortalityTables(RR67, EROM85.male, EROF85.female, EROM.G1950.male.av, EROF.G1950.female.av, AVOe1996R[, "Einzel"],AVOe2005R[c("m", "w"), "Einzel","loaded"],
                    aes = aes(color = year), YOB = 1965, title = "Austrian Annuity Tables, Cohort 1965", 
                    legend.position = "right"
) + facet_grid(sex ~ .)


## ----OeVSt--------------------------------------------------------------------
mortalityTables.load("Austria_Census")
mort.AT.census

## ----AT.PopulationForecast, results="hide"------------------------------------
mortalityTables.load("Austria_PopulationForecast")
mort.AT.forecast
mortalityTables.load("Austria_PopulationObserved")

## ----AT.PopulationForecastMCMC, results="hide"--------------------------------
mortalityTables.load("Austria_PopulationMCMC")
mort.AT.MCMC

## ----AT.Observation, results="hide"-------------------------------------------
mortalityTables.load("Austria_PopulationObserved")
mort.AT.observed.male
mort.AT.observed.female
mort.AT.observed.unisex


## ----AT.Census.Comparison-----------------------------------------------------
plotMortalityTables(
  mort.AT.census[c("m", "w"),],
  
  aes = aes(color = table),
  legend.position = "right", legend.key.width = unit(2, "lines"),
  title = "Comparison of Austrian Census Mortalities"
) + labs(color = NULL) + facet_grid(sex ~ .)

## ----AT.Census.Projections----------------------------------------------------
plotMortalityTables(
  mort.AT.census[c("m", "w"),"2011"],
  mort.AT.forecast %>% mT.setDimInfo(table = "Official forecast"),
  mort.AT.MCMC[c("m", "w")] %>% mT.setDimInfo(table = "MCMC forecast"),

  YOB = 1980,
  aes = aes(color = table),
  legend.position = "right", legend.key.width = unit(2, "lines"),
  title = "Projected Austrian Population Mortalities, YOB 1980"
  
) + labs(color = NULL) + facet_grid(sex ~ .)

## ----AT.population.History----------------------------------------------------
plotMortalityTables(
  mort.AT.observed.male,
  mort.AT.observed.female,
  mort.AT.observed.unisex,
  
  YOB = 1940,
  aes = aes(color = sex),
  legend.position = "right", legend.key.width = unit(2, "lines"),
  title = "Historic Austrian Population Mortalities (yearly raw observations), YOB 1940"
)

## ----EttlPager, results="hide"------------------------------------------------
# pensionTables.load("Austria_EttlPagler")
# EttlPagler.male
# EttlPagler.female

## ----AVOe1999P, results="hide"------------------------------------------------
# pensionTables.load("Austria_AVOe1999P")
# AVOe1999P

## ----AVOe2008P, results="hide"------------------------------------------------
# pensionTables.load("Austria_AVOe2008P")
# AVOe2008P

## ----AVOe2018P, results="hide"------------------------------------------------
# pensionTables.load("Austria_AVOe2018P")
# AVOe2018P

## ----VUGesamtbestand, results = "hide"----------------------------------------
mortalityTables.load("Austria_VUGesamtbestand_2012-16")
VU.Gesamtbestand
VU.Gesamtbestand.Storno
VU.Gesamtbestand.Detail

## ----VUGesamtbestand.vergleich------------------------------------------------
plotMortalityTables(VU.Gesamtbestand, legend.position = c(0.01, 0.99), legend.justification = c(0,1), title = "Austrian insurance portfolio moratlities 2012-16")

## ----PKGesamtbestand, results = "hide"----------------------------------------
# mortalityTables.load("Austria_PK-Bestand_2010-16")
# PKBestandstafel.2010.16

## ----PKBestand.vergleich------------------------------------------------------
# plotMortalityTables(PKBestandstafel.2010.16[,,"qx", "raw"], legend.position = "right", title = "Austrian pension fund mortalities 2010-16", aes = aes(color = type)) +
  # facet_grid(sex ~ .) + labs(color = "Collective")

## ----DAV1994R, results = "hide"-----------------------------------------------
mortalityTables.load("Germany_Annuities_DAV1994R")
DAV1994R.male
DAV1994R.male.av
DAV1994R.female
DAV1994R.female.av

## ----DAV2004R, results="hide"-------------------------------------------------
mortalityTables.load("Germany_Annuities_DAV2004R")
DAV2004R.male
DAV2004R.male.2Ord
DAV2004R.male.av
DAV2004R.male.av.2Ord
DAV2004R.female
DAV2004R.female.2Ord
DAV2004R.female.av
DAV2004R.female.av.2Ord

## ----DE.Annuities.Comparison--------------------------------------------------
plotMortalityTables(
  DAV1994R.male, DAV1994R.female, 
  DAV2004R.male, DAV2004R.male.2Ord, 
  DAV2004R.female, DAV2004R.female.2Ord,
  
  Period = 2000, aes = aes(linetype = sex, color = interaction(year, data)), 
  legend.position = c(0.01, 0.99), legend.justification = c(0,1), legend.key.width = unit(2, "lines"),
  title = "Comparison of German Annuity Tables, Period 2000"
) + labs(linetype = NULL, color = NULL)

plotMortalityTrend(
  DAV1994R.male, DAV1994R.female, 
  DAV2004R.male, DAV2004R.male.2Ord, 
  DAV2004R.female, DAV2004R.female.2Ord,

  Period = 2000, aes = aes(linetype = sex, color = interaction(year, data)), 
  legend.position = c(0.99, 0.99), legend.justification = c(1,1), legend.key.width = unit(2, "lines"), 
  title = "Age-specific trends of German Annuity Tables, Period 2000"
) + labs(linetype = NULL, color = NULL) + theme(legend.box.just = "right")

## ----ADSt---------------------------------------------------------------------
mortalityTables.load("Germany_Census")
mort.DE.census

## ----DE.Census.Comparison-----------------------------------------------------
plotMortalityTables(
  mort.DE.census,
  
  aes = aes(color = table),
  legend.position = "right", legend.key.width = unit(2, "lines"),
  title = "Comparison of German Census Mortalities"
) + labs(color = NULL) + facet_grid(sex ~ .)

## ----DAV1994T,results="hide"--------------------------------------------------
mortalityTables.load("Germany_Endowments_DAV1994T")
DAV1994T.male
DAV1994T.male.2Ord
DAV1994T.female
DAV1994T.female.2Ord


## ----DAV2008T,results="hide"--------------------------------------------------
mortalityTables.load("Germany_Endowments_DAV2008T")
DAV2008T.male
DAV2008T.male.2Ord
DAV2008T.male.nonsmoker
DAV2008T.male.nonsmoker.2Ord
DAV2008T.male.smoker
DAV2008T.male.smoker.2Ord

DAV2008T.female
DAV2008T.female.2Ord
DAV2008T.female.nonsmoker
DAV2008T.female.nonsmoker.2Ord
DAV2008T.female.smoker
DAV2008T.female.smoker.2Ord

## ----DAV-T.comparison---------------------------------------------------------
plotMortalityTables(
  DAV1994T.male, DAV1994T.male.2Ord, DAV2008T.male, DAV2008T.male.2Ord, DAV2008T.male.nonsmoker,  DAV2008T.male.smoker,
  DAV1994T.female, DAV1994T.female.2Ord, DAV2008T.female, DAV2008T.female.2Ord, DAV2008T.female.nonsmoker,  DAV2008T.female.smoker,
  aes = aes(color = interaction(year, collar), linetype = data), legend.position = "right", legend.key.width = unit(1.5, "lines"),
  title = "German whole life mortality tables"
) + facet_grid(sex ~ .)


Reproducible Research Project 2
================

In the Last 20 years, Floods have had the greatest economic impact while Heat/Droughts have led to the greatest loss of life
============================================================================================================================

Introduction
============

This analysis examines the most recent 20 year history of NOAA Storm Events Databse to answer the questions of which events are the most economically damaging and which are the greatest threat to human health.

The data spans from 1950-2011, but only the most recent 20 years data was used for 3 reasons: 1. Earlier data is less complete and there are many less instances recorded 2. Inflation over time means comparing economic damage in 1950 to 2011 would be misleading without adjusting for inflation 3. General standards for safety and building construction have been improving over time, thus for forward planning decisions we should look at the more recent data which is reflective of this

The analysis shows that Heat/Drought events have led to hte greatest loss of life while Flood events have caused the greatest economic damage.

Data Processing
===============

Load the Packages and Pull the Data
-----------------------------------

``` r
#load packages
library(ggplot2)
library(dplyr)
library(data.table)
library(downloader)
library(R.utils)

# Load the file

Data <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

download(Data, "repdata%2Fdata%2fStormData.csv", mode="wb")

#unzip the file
bunzip2("repdata%2Fdata%2fStormData.csv", "NOAAData.csv", remove=FALSE)
file.info("NOAAData.csv")
```

    ##                   size isdir mode               mtime               ctime
    ## NOAAData.csv 561637449 FALSE  666 2018-09-22 15:19:22 2018-09-22 15:18:36
    ##                            atime exe
    ## NOAAData.csv 2018-09-22 15:18:36  no

``` r
#read the file as a csv
NOAAData <- read.csv("NOAAData.csv", comment.char = "")
knitr::opts_chunk$set(echo = TRUE)
```

Change the units of economic damage
-----------------------------------

"B" units are replaced with billions so the numbers can be aggregated

``` r
#change proerpt damage unit to a dollar amount
NOAAData$PropertyDamageMultiplier <- with(NOAAData, ifelse(
  PROPDMGEXP=="H", 100, ifelse(
  PROPDMGEXP=="K", 1000, ifelse(
    PROPDMGEXP == "M", 1000000, ifelse(
      PROPDMGEXP == "B", 1000000000,
      1)
    )
  )
)
)

#create a new variable with the total damage
NOAAData$PropertyDamage <- NOAAData$PROPDMG * NOAAData$PropertyDamageMultiplier


#change crop damage unit to a dollar amount
NOAAData$CropDamageMultiplier <- with(NOAAData, ifelse(
  CROPDMGEXP=="H", 100, ifelse(
  CROPDMGEXP=="K", 1000, ifelse(
    CROPDMGEXP == "M", 1000000, ifelse(
      CROPDMGEXP == "B", 1000000000,
      1)
  )
)
)
)
#create a new variable with the total damage
NOAAData$CropDamage <- NOAAData$CROPDMG*NOAAData$CropDamageMultiplier

knitr::opts_chunk$set(echo = TRUE)
```

Select only the data from 1991-2011
-----------------------------------

Data wil be analyzed from the most recent 20 years (1991-2011) due to reasons stated in the introduction

``` r
#format dates
NOAAData$Date <- as.Date(NOAAData$BGN_DATE, format = "%m/%d/%Y")

#extract year from date
NOAAData$Year <- year(NOAAData$Date)

#subset for year 1991 onward
NOAADataRecent <- subset(NOAAData, Year >1990)

knitr::opts_chunk$set(echo = TRUE)
```

Consolidate Event Types
-----------------------

There are many Event Types in the file. The following code consolidate similar Event Types into aggreates (ex: Blizzard, Snow, Frost = Winter Weather/Storms). Any leftover event with less than 100 instances is put into an "Other" group.

``` r
#change all text to be upper case
NOAADataRecent$EVTYPE <- toupper(NOAADataRecent$EVTYPE)

#remove excess spaces
NOAADataRecent$EVTYPE <- gsub(" +"," ",NOAADataRecent$EVTYPE)


#consolidate event types based on text
NOAADataRecent$EVTYPE[grep("TORNADO|WATERSPOUT|FUNNEL CLOUD", NOAADataRecent$EVTYPE)] <- "TORNADO"

NOAADataRecent$EVTYPE[grep("HURRICANE|TYPHOON|TROPICAL", NOAADataRecent$EVTYPE)] <- "HURRICANE/TROP STORM"

NOAADataRecent$EVTYPE[grep("THUNDER|TSTM|GUSTNADO|LIGHTNING", NOAADataRecent$EVTYPE)] <- "THUNDERSTORM"

NOAADataRecent$EVTYPE[grep("ICE|BLIZZARD|SNOW|CHILL|WINTER|COLD|HYPOTHERMIA|SLEET|FROST|FREEZING|FREEZE|ICY|WINTRY", NOAADataRecent$EVTYPE)] <- "WINTER WEATHER/STORM"

NOAADataRecent$EVTYPE[grep("FLOOD|FLD", NOAADataRecent$EVTYPE)] <- "FLOOD"

NOAADataRecent$EVTYPE[grep("WIND", NOAADataRecent$EVTYPE)] <- "HEAVY WIND"

NOAADataRecent$EVTYPE[grep("RAIN|PRECIP|PRECIPIRATION|SHOWER|WET", NOAADataRecent$EVTYPE)] <- "HEAVY RAIN"

NOAADataRecent$EVTYPE[grep("HAIL", NOAADataRecent$EVTYPE)] <- "HAIL"

NOAADataRecent$EVTYPE[grep("FIRE|SMOKE", NOAADataRecent$EVTYPE)] <- "FIRE"

NOAADataRecent$EVTYPE[grep("HEAT|WARMTH|DROUGHT|DUST|DRY|HOT|HIGH", NOAADataRecent$EVTYPE)] <- "HEAT/DROUGHT"

NOAADataRecent$EVTYPE[grep("RIP|SURF|SURGE|WATER|SWELLS|SEAS|TSUNAMI|STREAM|TIDE|WAVE", NOAADataRecent$EVTYPE)] <- "OCEAN CONDITIONS"

NOAADataRecent$EVTYPE[grep("FOG", NOAADataRecent$EVTYPE)] <- "FOG"

NOAADataRecent$EVTYPE[grep("AVALANCHE|LANDSLIDE|ROCKSLIDE|MUD", NOAADataRecent$EVTYPE)] <- "AVALANCHE/LANDSLIDE"

#extract the Event Types that rarely occur and aggregate them into an "other" group
EVs <- sort(table(NOAADataRecent$EVTYPE), decreasing=TRUE)
SmallEvents <- names(EVs[EVs<100])
NOAADataRecent$EVTYPE[NOAADataRecent$EVTYPE %in% SmallEvents] <- "other"

#add a count variable for easy summarising of instances
NOAADataRecent$count <- 1


knitr::opts_chunk$set(echo = TRUE)
```

Results
=======

Health Impacts
--------------

A table summarising the health impact of Events is created to show injuries and fatalities. The figure below will focus on fatalities due to the importance of mitigating fatalities for any policy decision.

``` r
#create a summary of the Injuries and Fatalities by Event
EventHealthSummary <- NOAADataRecent %>%
  group_by(EVTYPE) %>%
  summarise(instances=sum(count), Fatalities=sum(FATALITIES), Injuries=sum(INJURIES))

#Order the Event Types by number of Fatalities
EventHealthSummary <- data.frame(EventHealthSummary)
EventHealthSummary <- EventHealthSummary[order(-EventHealthSummary$Fatalities),]
EventHealthSummary$EVTYPE <- factor(EventHealthSummary$EVTYPE, ordered=TRUE)
EventHealthSummary$EVTYPE <- with(EventHealthSummary, reorder(EVTYPE, -Fatalities))

#Create a column chart to show which Events have caused the largest amount of Fatalities
HealthPlot <- ggplot(data=EventHealthSummary, aes(x=EVTYPE, y=Fatalities,fill=EVTYPE),color=EVTYPE) + geom_col() + ggtitle("Fatalities by Event Type, 1991-2011") + ylab("Fatalities")
HealthPlotlabel <- HealthPlot+theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.text = element_text(size=6), legend.title = element_blank())
HealthPlotlabel
```

![](NOAA_Storm_Event_Report_files/figure-markdown_github/Health%20Charts-1.png)

As we can see from the above, Heat/Drought Events have led to the most fatalities, followed by Tornados and Floods

Economic Impacts
----------------

Crop and Property Damage data is combined to generate an overall economic impact number in dollars. Data is summarised by Event Type.

``` r
#summarise the data by total proerpty and crop damage and Event Type
EventEconSummary <- NOAADataRecent %>%
  group_by(EVTYPE) %>%
  summarise(instances=sum(count), PropertyAndCropDMG=sum(PropertyDamage+CropDamage))

#Set Event Types as factors and sort in order from most damage to least
EventEconSummary <- data.frame(EventEconSummary)
EventEconSummary <- EventEconSummary[order(-EventEconSummary$PropertyAndCropDMG),]
EventEconSummary$EVTYPE <- factor(EventEconSummary$EVTYPE, ordered=TRUE)
EventEconSummary$EVTYPE <- with(EventEconSummary, reorder(EVTYPE, -PropertyAndCropDMG))

#plot the Economic Damage data
EconPlot <- ggplot(data=EventEconSummary, aes(x=EVTYPE, y=PropertyAndCropDMG/1000000000,fill=EVTYPE),color=EVTYPE) + geom_col() + ggtitle("Property Damage by Event Type, 1991-2011") + ylab("Property and Crop Damage (Billions)")
EconPlotlabel <- EconPlot+theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.text = element_text(size=6), legend.title = element_blank())
EconPlotlabel
```

![](NOAA_Storm_Event_Report_files/figure-markdown_github/Economic%20Impacts-1.png)

As we can observe from the Economic Impact graph, Floods cause the most economic damage, followed by Hurricanes/Typhoons/Tropical Storms and Ocean Conditions.

#............................
# Load libraries ####
#............................
setwd("~/Desktop/Spring 2020/EPID 886 - Independent Study/")
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(sf)
library(classInt)
library(scales)
library(RColorBrewer)
library(viridis)
library(htmlwidgets)
library(sunburstR)
library(d3r)
library(htmltools)
library(patchwork)

ledger = read_csv("DixLedgerDeidentified_clean.csv")
options(scipen=999)

#............................
# Ledger data cleaning ####
#............................

# variable formatting
as.factor(ledger$occupationcategory)
as.factor(ledger$marital)

# cleaning residence; rename to countyofresidence
ledger$nc_county <- grepl("County", ledger$residence)
ledger = ledger %>%
mutate(residence = case_when(
  nc_county == TRUE ~ residence,
  FALSE ~ NA_character_
) %>% as_factor()
)
ledger = ledger %>% rename(countyofresidence = residence)
table(ledger$countyofresidence, useNA = "always")

# cleaning length of stay
ledger$lengthofstay <- interval(ymd(ledger$dateofadmission), ymd(ledger$dateofdischarge))
ledger$lengthofstay <- time_length(as.duration(ledger$lengthofstay), "days")

# cleaning final condition
ledger$finalcondition2 <- str_split(ledger$finalcondition, "; ")
ledger$finalcondition3 <- !grepl(",", ledger$finalcondition2)
ledger = ledger %>%
  mutate(finalcondition2 = case_when(
    finalcondition3 == FALSE ~ substring(finalcondition, regexpr("; ", finalcondition) + 1),
    TRUE ~ NA_character_
  ) %>% as_factor()
  )
ledger$finalcondition <- str_replace_all(ledger$finalcondition, ";.*", "")
ledger$finalcondition2 <- str_replace_all(ledger$finalcondition2, " ", "")

# creating censusyear
ledger$censusyear = str_remove(ledger$decade, "s")
table(ledger$censusyear)

# cleaning numberofpreviousattacks
table(ledger$numberofpreviousattacks, useNA="always")

# cleaning supposed cause
ledger$supposedcausecleaned2 <- str_split(ledger$supposedcausecleaned1, "; ")
ledger$sc3 <- !grepl(",", ledger$supposedcausecleaned2)
ledger = ledger %>%
  mutate(supposedcausecleaned2 = case_when(
    sc3 == FALSE ~ substring(supposedcausecleaned1, regexpr("; ", supposedcausecleaned1) + 1),
    TRUE ~ NA_character_
  ) %>% as_factor()
  )

ledger$supposedcausecleaned1 <- str_replace_all(ledger$supposedcausecleaned1, "Unknown", NA_character_)

ledger$supposedcausecleaned1 <- str_replace_all(ledger$supposedcausecleaned1, ";.*", "")
ledger$supposedcausecleaned2 <- str_replace_all(ledger$supposedcausecleaned2, " ", "")

# recoding supposed cause category
str(ledger$supposedcausecategory1)
ledger$supposedcausecategory1 <- recode(ledger$supposedcausecategory1, "SItuational"="Situational")
table(ledger$supposedcausecategory1, ledger$supposedcausecleaned1, useNA = "always")

# delete unnecessary variables
ledger <- subset(ledger, select = -c(sc3, supposedcausecategory2, finalcondition3, nc_county))

# exporting cleaned dataset
write_csv(ledger, "ledger_cleaned_with_R.csv")

#............................
# Incorporating census data ####
#............................

censuspop = read_csv("nc_census_1850-1910.csv")
names(censuspop) = tolower(names(censuspop))

ptresidence <- ledger %>% group_by(censusyear, countyofresidence) %>% summarize(n()) 
ptresidence = ptresidence %>% rename(ptsincounty = "n()")
ptresidence$countyofresidence <- str_replace_all(ptresidence$countyofresidence, " County", "")

censuspop <- gather(censuspop, censusyear, population, countypop_1850:countypop_1910)
censuspop$censusyear <- str_sub(censuspop$censusyear, 11)
censuspop$county <- str_replace_all(censuspop$county, " County", "")

#............................
# Population maps ####
#............................

# 1850
nc_1850 <- st_read("nhgis0002_shape/nhgis0002_shapefile_tl2000_us_county_1850/US_county_1850.shp", 
                   stringsAsFactors=F) %>% st_transform(4326)
nc_1850 <- subset(nc_1850, STATENAM=="North Carolina")
st_crs(nc_1850)

censuspop_1850 <- subset(censuspop, censusyear==1850)
nc_1850 <- left_join(nc_1850, censuspop_1850, by = c("GISJOIN" = "gisjoin"))

# 1860
nc_1860 <- st_read("nhgis0002_shape/nhgis0002_shapefile_tl2000_us_county_1860/US_county_1860.shp", 
                   stringsAsFactors=F) %>% st_transform(4326)
nc_1860 <- subset(nc_1860, STATENAM=="North Carolina")
st_crs(nc_1860)

censuspop_1860 <- subset(censuspop, censusyear==1860)
nc_1860 <- left_join(nc_1860, censuspop_1860, by = c("GISJOIN" = "gisjoin"))

# 1870
nc_1870 <- st_read("nhgis0002_shape/nhgis0002_shapefile_tl2000_us_county_1870/US_county_1870.shp", 
                   stringsAsFactors=F) %>% st_transform(4326)
nc_1870 <- subset(nc_1870, STATENAM=="North Carolina")
st_crs(nc_1870)

censuspop_1870 <- subset(censuspop, censusyear==1870)
nc_1870 <- left_join(nc_1870, censuspop_1870, by = c("GISJOIN" = "gisjoin"))

# 1880
nc_1880 <- st_read("nhgis0002_shape/nhgis0002_shapefile_tl2000_us_county_1880/US_county_1880.shp", 
                   stringsAsFactors=F) %>% st_transform(4326)
nc_1880 <- subset(nc_1880, STATENAM=="North Carolina")
st_crs(nc_1880)

censuspop_1880 <- subset(censuspop, censusyear==1880)
nc_1880 <- left_join(nc_1880, censuspop_1880, by = c("GISJOIN" = "gisjoin"))

# 1890
nc_1890 <- st_read("nhgis0002_shape/nhgis0002_shapefile_tl2000_us_county_1890/US_county_1890.shp", 
                   stringsAsFactors=F) %>% st_transform(4326)
nc_1890 <- subset(nc_1890, STATENAM=="North Carolina")
st_crs(nc_1890)

censuspop_1890 <- subset(censuspop, censusyear==1890)
nc_1890 <- left_join(nc_1890, censuspop_1890, by = c("GISJOIN" = "gisjoin"))

# 1900 
nc_1900 <- st_read("nhgis0002_shape/nhgis0002_shapefile_tl2000_us_county_1900/US_county_1900.shp", 
                   stringsAsFactors=F) %>% st_transform(4326)
nc_1900 <- subset(nc_1900, STATENAM=="North Carolina")
st_crs(nc_1900)

censuspop_1900 <- subset(censuspop, censusyear==1900)
nc_1900 <- left_join(nc_1900, censuspop_1900, by = c("GISJOIN" = "gisjoin"))

# 1910
nc_1910 <- st_read("nhgis0002_shape/nhgis0002_shapefile_tl2000_us_county_1910/US_county_1910.shp", 
                   stringsAsFactors=F) %>% st_transform(4326)
nc_1910 <- subset(nc_1910, STATENAM=="North Carolina")
st_crs(nc_1910)

censuspop_1910 <- subset(censuspop, censusyear==1910)
nc_1910 <- left_join(nc_1910, censuspop_1910, by = c("GISJOIN" = "gisjoin"))

#............................
# Patient maps ####
#............................

# 1850
head(nc_1850)

nc_1850_pt <- left_join(nc_1850, ptresidence, by = c("DECADE" = "censusyear", "NHGISNAM" = "countyofresidence"))
nc_1850_pt$pop_thousand <- nc_1850_pt$population / 1000
nc_1850_pt$pt_prop <- (nc_1850_pt$ptsincounty / nc_1850_pt$pop_thousand) 
nc_1850_pt <- nc_1850_pt %>% select(censusyear, pt_prop, ptsincounty, population, county, geometry)

prop1850 <- ggplotly(ggplot(nc_1850_pt, aes(fill=pt_prop, text=paste("County:", county, "<br>", "Admissions: ", pt_prop))) + geom_sf() + theme_void() + scale_fill_gradientn(colors=brewer.pal(5, "GnBu"), na.value="grey") +
  labs(title="Number of admissions per 1,000 county residents - 1850s", fill="Admissions"), tooltip="text")

# 1860
nc_1860_pt <- left_join(nc_1860, ptresidence, by = c("DECADE" = "censusyear", "NHGISNAM" = "countyofresidence"))
nc_1860_pt$pop_thousand <- nc_1860_pt$population / 1000
nc_1860_pt$pt_prop <- nc_1860_pt$ptsincounty / nc_1860_pt$pop_thousand
nc_1860_pt <- nc_1860_pt %>% select(censusyear, pt_prop, ptsincounty, population, county, geometry)

prop1860 <- ggplotly(ggplot(nc_1860_pt, aes(fill=pt_prop, text=paste("County:", county, "<br>", "Admissions: ", pt_prop))) + geom_sf() + theme_void() + scale_fill_gradientn(colors=brewer.pal(5, "GnBu"), na.value="grey") +
  labs(title="Number of admissions per 1,000 county residents - 1860s", fill="Admissions"),
tooltip="text")

# 1870
nc_1870_pt <- left_join(nc_1870, ptresidence, by = c("DECADE" = "censusyear", "NHGISNAM" = "countyofresidence"))
nc_1870_pt$pop_thousand <- nc_1870_pt$population / 1000
nc_1870_pt$pt_prop <- nc_1870_pt$ptsincounty / nc_1870_pt$pop_thousand
nc_1870_pt <- nc_1870_pt %>% select(censusyear, pt_prop, ptsincounty, population, county, geometry)

prop1870 <- ggplotly(ggplot(nc_1870_pt, aes(fill=pt_prop, text=paste("County:", county, "<br>", "Admissions: ", pt_prop))) + geom_sf() + theme_void() + scale_fill_gradientn(colors=brewer.pal(5, "GnBu"), na.value="grey") +
  labs(title="Number of admissions per 1,000 county residents - 1870s", fill="Admissions"),
tooltip="text")

# 1880
nc_1880_pt <- left_join(nc_1880, ptresidence, by = c("DECADE" = "censusyear", "NHGISNAM" = "countyofresidence"))
nc_1880_pt$pop_thousand <- nc_1880_pt$population / 1000
nc_1880_pt$pt_prop <- nc_1880_pt$ptsincounty / nc_1880_pt$pop_thousand
nc_1880_pt <- nc_1880_pt %>% select(censusyear, pt_prop, ptsincounty, population, county, geometry)

prop1880 <- ggplotly(ggplot(nc_1880_pt, aes(fill=pt_prop, text=paste("County:", county, "<br>", "Admissions: ", pt_prop))) + geom_sf() + theme_void() + scale_fill_gradientn(colors=brewer.pal(5, "GnBu"), na.value="grey") +
  labs(title="Number of admissions per 1,000 county residents - 1880s", fill="Admissions"),
       tooltip="text")

# 1890
nc_1890_pt <- left_join(nc_1890, ptresidence, by = c("DECADE" = "censusyear", "NHGISNAM" = "countyofresidence"))
nc_1890_pt$pop_thousand <- nc_1890_pt$population / 1000
nc_1890_pt$pt_prop <- nc_1890_pt$ptsincounty / nc_1890_pt$pop_thousand
nc_1890_pt <- nc_1890_pt %>% select(censusyear, pt_prop, ptsincounty, population, county, geometry)

prop1890 <- ggplotly(ggplot(nc_1890_pt, aes(fill=pt_prop, text=paste("County:", county, "<br>", "Admissions: ", pt_prop))) + geom_sf() + theme_void() + scale_fill_gradientn(colors=brewer.pal(5, "GnBu"), na.value="grey") +
  labs(title="Number of admissions per 1,000 county residents - 1890s", fill="Admissions"),
       tooltip="text")

# 1900
nc_1900_pt <- left_join(nc_1900, ptresidence, by = c("DECADE" = "censusyear", "NHGISNAM" = "countyofresidence"))
nc_1900_pt$pop_thousand <- nc_1900_pt$population / 1000
nc_1900_pt$pt_prop <- nc_1900_pt$ptsincounty / nc_1900_pt$pop_thousand
nc_1900_pt <- nc_1900_pt %>% select(censusyear, pt_prop, ptsincounty, population, county, geometry)

prop1900 <- ggplotly(ggplot(nc_1900_pt, aes(fill=pt_prop, text=paste("County:", county, "<br>", "Admissions: ", pt_prop))) + geom_sf() + theme_void() + scale_fill_gradientn(colors=brewer.pal(5, "GnBu"), na.value="grey") +
  labs(title="Number of admissions per 1,000 county residents - 1900s", fill="Admissions"),
       tooltip="text")

# 1910
ptresidence$censusyear <- as.character(ptresidence$censusyear)
nc_1910_pt <- left_join(nc_1910, ptresidence, by = c("DECADE" = "censusyear", "NHGISNAM" = "countyofresidence"))
nc_1910_pt$pop_thousand <- nc_1910_pt$population / 1000
nc_1910_pt$pt_prop <- nc_1910_pt$ptsincounty / nc_1910_pt$pop_thousand
nc_1910_pt <- nc_1910_pt %>% select(censusyear, pt_prop, ptsincounty, population, county, geometry)

prop1910 <- ggplotly(ggplot(nc_1910_pt, aes(fill=pt_prop, text=paste("County:", county, "<br>", "Admissions: ", pt_prop))) + geom_sf() + theme_void() + scale_fill_gradientn(colors=brewer.pal(5, "GnBu"), na.value="grey") +
  labs(title="Number of admissions per 1,000 county residents - 1910s", fill="Admissions"),
  tooltip="text")

prop1910

# Merging ptprop years into one df and combining patient admissions visualizations
totalprop <- bind_rows(nc_1850_pt, nc_1860_pt, nc_1870_pt, nc_1880_pt, nc_1890_pt, nc_1900_pt, nc_1910_pt)
totalprop <- totalprop %>% group_by(county, censusyear)
totalprop_fig <- ggplot(totalprop, aes(fill=pt_prop, text=paste("County:", county, "<br>", "Admissions: ", pt_prop))) + geom_sf() + theme_void() + scale_fill_gradientn("Admissions", colors=brewer.pal(5, "GnBu"), na.value="grey") + labs(title="Number of admissions per 1,000 county residents by year")
totalprop_fig <- totalprop_fig + facet_wrap(~ censusyear, ncol=2)
totalprop_fig <- ggplotly(totalprop_fig, tooltip="text")
totalprop_fig

# Overall, where were patients from?
ptres_all <- ledger %>% group_by(countyofresidence) %>% summarize(n()) 
ptres_all = ptres_all %>% rename(ptsincounty = "n()")
ptres_all$countyofresidence <- str_replace_all(ptres_all$countyofresidence, " County", "")
ptcounties_all <- left_join(nc_1910, ptres_all, by = c("county" = "countyofresidence"))

ptcountiesoverall <- ggplotly(ggplot(ptcounties_all, (aes(fill=ptsincounty, text=paste("County: ", county, "<br>", "Number of Patients: ", ptsincounty)))) 
         + geom_sf() + theme_void() + 
  scale_fill_distiller(palette="Blues", na.value="grey", direction=1) +
  labs(title="Patients' Counties of Residence", fill="Patients", 
       caption="Based on data available for n admissions. \n County of admission was missing for n patients and is not represented. \n N residents of counties outside of NC are also not represented."),
  tooltip="text"
)

print(ptcountiesoverall)


#............................
# Data visualizations ####
#............................

# sunbursts - based on sunburstR http://www.buildingwidgets.com/blog/2015/7/2/week-26-sunburstr 
??sunburstR

# diagnoses by gender - male
ledger_male <- subset(ledger, sex=="male")
ledger_male <- ledger_male %>% select(supposedcausecategory1, supposedcausecleaned1, supposedcausecleaned2)
# ledger_male$supposedcausecleaned1 <- str_c(ledger_male$supposedcausecategory1, ledger_male$supposedcausecleaned1, sep="-")
# ledger_male$supposedcausecleaned2 <- str_c(ledger_male$supposedcausecleaned1, ledger_male$supposedcausecleaned2, sep ="-")

ledger_male <- ledger_male %>% group_by(supposedcausecategory1, supposedcausecleaned1, supposedcausecleaned2) %>% summarize(n()) 
ledger_male = ledger_male %>% rename(count = "n()")

ledger_male = ledger_male %>% mutate(path = paste(supposedcausecategory1, supposedcausecleaned1, supposedcausecleaned2, sep="-"))
ledger_male$path <- gsub("-NA", "-", ledger_male$path)
ledger_male <- head(ledger_male, -1)

ledger_male <- ungroup(ledger_male)
ledger_male_sb <- ledger_male %>% arrange(desc(count)) %>% select(path, count)

sum(ledger_male_sb$count)
sund2b(ledger_male_sb)

# sb_male <- sunburst(data=ledger_male_sb, legend=FALSE)
# sb_male <- ledger_male %>% arrange(desc(count)) %>% mutate(path = paste(supposedcausecategory1, supposedcausecleaned1, supposedcausecleaned2, sep='-')) %>% select(path, count) %>% sunburst(breadcrumb = list(w=200))
# sb_male

p1 <- sund2b(ledger_male_sb)

# diagnoses by gender - female
ledger_female <- subset(ledger, sex=="female")
ledger_female <- ledger_female %>% select(supposedcausecategory1, supposedcausecleaned1, supposedcausecleaned2)

ledger_female <- ledger_female %>% group_by(supposedcausecategory1, supposedcausecleaned1, supposedcausecleaned2) %>% summarize(n()) 
ledger_female = ledger_female %>% rename(count = "n()")

ledger_female = ledger_female %>% mutate(path = paste(supposedcausecategory1, supposedcausecleaned1, supposedcausecleaned2, sep="-"))
ledger_female$path <- gsub("-NA", "-", ledger_female$path)
ledger_female <- head(ledger_female, -1)

ledger_female <- ungroup(ledger_female)
ledger_female_sb <- ledger_female %>% arrange(desc(count)) %>%select(path, count)

# sb_female <- sunburst(data=ledger_female_sb, legend=FALSE)
# sb_female <- ledger_female %>% arrange(desc(count)) %>% mutate(path = paste(supposedcausecategory1, supposedcausecleaned1, supposedcausecleaned2, sep='-')) %>% select(path, count) %>% sunburst(breadcrumb = list(w=200))
# sb_female

p2 <- sund2b(ledger_female_sb)


# below needs work

# male + female together
# shinyApp(ui = ui, server = server)

# # an example with d2b sunburst and Shiny
# library(shiny)
# library(sunburstR)

# # use a sample of the sequences csv data
# sequences <- read.csv(
#   system.file("examples/visit-sequences.csv",package="sunburstR")
#   ,header = FALSE
#   ,stringsAsFactors = FALSE
# )[1:200,]

# # create a d2b sunburst
# s2b <- sund2b(sequences)
# 
# options(shiny.trace=TRUE)
# ui <- sund2bOutput("s2b")
# server <- function(input, output, session) {
#   output$s2b <- renderSund2b({
#     add_shiny(s2b)
#   })
# }
# shinyApp(ui, server)


#............................
# Old code ####
#............................

# merging ptprop years into one df
# totalprop <- bind_rows(nc_1850_pt, nc_1860_pt, nc_1870_pt, nc_1890_pt, nc_1900_pt, nc_1910_pt
# test <- test %>% group_by(county, censusyear)

# # shiny app - dropdown
# library(shiny)
# 
# choice_data <- list("prop1850", "prop1860", "prop1870", "prop1880", "prop1890", "prop1900", "prop1910")
# ui <- shinyUI(fluidPage(selectInput("select", "Select a year", choices=choice_data), 
#                         plotlyOutput("1850", width="900",height = "400px"),
#                         plotlyOutput("1850", width="900",height = "400px"),
#                         plotlyOutput("1870", width="900",height = "400px"),
#                         plotlyOutput("1880", width="900",height = "400px"),
#                         plotlyOutput("1890", width="900",height = "400px"),
#                         plotlyOutput("1900", width="900",height = "400px"),
#                         plotlyOutput("1910", width="900",height = "400px")
#                         ))
# 
# choice_data <- list("Census_Year_1850", "Census_Year_1860", "Census_Year_1870", "Census_Year_1880", "Census_Year_1890", "Census_Year_1900", "Census_Year_1910")
# ui <- shinyUI(fluidPage(selectInput("select", "Select a year", choices=choice_data),
#                         plotlyOutput("Census_Year_1850", width="900",height = "400px"),
#                         plotlyOutput("Census_Year_1860", width="900",height = "400px"),
#                         plotlyOutput("Census_Year_1870", width="900",height = "400px"),
#                         plotlyOutput("Census_Year_1880", width="900",height = "400px"),
#                         plotlyOutput("Census_Year_1890", width="900",height = "400px"),
#                         plotlyOutput("Census_Year_1900", width="900",height = "400px"),
#                         plotlyOutput("Census_Year_1910", width="900",height = "400px")
#                         ))
# 
# server <- function(input, output){    
#   output$Census_Year_1850 <- renderPlotly(prop1850)
#   
#   output$Census_Year_1860 <- renderPlotly(prop1860)
#   
#   output$Census_Year_1870 <- renderPlotly(prop1870)
#   
#   output$Census_Year_1880 <- renderPlotly(prop1880)
#   
#   output$Census_Year_1890 <- renderPlotly(prop1890)
#   
#   output$Census_Year_1900 <- renderPlotly(prop1900)
#   
#   output$Census_Year_1910 <- renderPlotly(prop1910)
# }
# 
# shinyApp(ui,server)

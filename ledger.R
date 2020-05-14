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
library(chorddiag)

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

# cleaning form
ledger$formcleaned <- gsub("Dementia Precox", "Dementia Praecox", ledger$formcleaned)
ledger_form <- data.frame(table(ledger$formcleaned, useNA="always"))
ledger_form$Var1 <- as.character(ledger_form$Var1)
ledger_form$Var1[is.na(ledger_form$Var1)] <- "Unknown"
ledger_form$Var1 <- factor(ledger_form$Var1)
ledger_form = ledger_form %>% rename(Form = "Var1", Frequency="Freq")

ledger_form$Form2 <- str_split(ledger_form$Form, "; ")
ledger_form$form3 <- !grepl(",", ledger_form$Form2)
ledger_form = ledger_form %>%
  mutate(Form2 = case_when(
    form3 == FALSE ~ substring(Form, regexpr("; ", Form) + 1),
    TRUE ~ NA_character_
  ) %>% as_factor()
  )

ledger_form$Form <- str_replace_all(ledger_form$Form, "Unknown", NA_character_)
ledger_form$Form <- str_replace_all(ledger_form$Form, ";.*", "")
ledger_form$Form2 <- str_replace_all(ledger_form$Form2, " ", "")

ledger_form$Form3 <- str_split(ledger_form$Form2, ";")
ledger_form$form4 <- !grepl(",", ledger_form$Form3)
ledger_form = ledger_form %>%
  mutate(Form3 = case_when(
    form4 == FALSE ~ substring(Form2, regexpr(";", Form2) + 1),
    TRUE ~ NA_character_
  ) %>% as_factor()
  )

ledger_form$Form2 <- str_replace_all(ledger_form$Form2, "Unknown", NA_character_)
ledger_form$Form2 <- str_replace_all(ledger_form$Form2, ";.*", "")
ledger_form$Form3 <- str_replace_all(ledger_form$Form3, " ", "")

# cleaning occupation 
ledger$occupationcleaned <- str_replace_all(ledger$occupationcleaned, "no entry", "unknown")
ledger$occ_value_cleaned <- word(ledger$occupationcleaned, 1, sep="\\;")
ledger$occ_value_cleaned <- as.character(ledger$occ_value_cleaned)
ledger$occ_value_cleaned <- gsub("None", "none", ledger$occ_value_cleaned)
ledger$occ_value_cleaned[is.na(ledger$occ_value_cleaned)] <- "unknown"
ledger$occ_value_cleaned <- factor(ledger$occ_value_cleaned)

ledger = ledger %>%
  mutate(occ_category_cleaned = case_when(
    occ_value_cleaned == "merchant" | occ_value_cleaned == "salesman" | occ_value_cleaned == "broker" ~ "sales",
    occ_value_cleaned == "accountant" | occ_value_cleaned == "lawyer" | occ_value_cleaned == "engineer" | occ_value_cleaned == "surveyor" | occ_value_cleaned == "botanist" | occ_value_cleaned == "agent" | occ_value_cleaned == "architect" | occ_value_cleaned == "editor" | occ_value_cleaned == "printer" | occ_value_cleaned == "reporter" ~ "professional",
    occ_value_cleaned == "painter" | occ_value_cleaned == "florist" | occ_value_cleaned == "musician" | occ_value_cleaned == "jeweler" | occ_value_cleaned == "author" | occ_value_cleaned == "decorator" | occ_value_cleaned == "artist" ~ "creative",
    occ_value_cleaned == "dentist" | occ_value_cleaned == "physician" | occ_value_cleaned == "pharmacist" | occ_value_cleaned == "nurse" | occ_value_cleaned == "optician" ~ "healthcare",
    occ_value_cleaned == "domestic" | occ_value_cleaned == "housewife" | occ_value_cleaned == "housekeeper" ~ "household",
    occ_value_cleaned == "clerk" | occ_value_cleaned == "stenographer" | occ_value_cleaned == "auditor" | occ_value_cleaned == "banker" | occ_value_cleaned == "inspector" | occ_value_cleaned == "teller" | occ_value_cleaned == "grader" ~ "clerical",
    occ_value_cleaned == "farmer" ~ "agriculture",
    occ_value_cleaned == "laborer" | occ_value_cleaned ==  "worker" | occ_value_cleaned == "watchman" | occ_value_cleaned == "janitor" | occ_value_cleaned == "miner" | occ_value_cleaned == "paper folder" | occ_value_cleaned == "road builder" ~ "unskilled labor",
    occ_value_cleaned == "lineman" | occ_value_cleaned == "fisherman" | occ_value_cleaned == "boatman" | occ_value_cleaned == "operator" | occ_value_cleaned == "manufacturer" | occ_value_cleaned == "lumberman" | occ_value_cleaned == "oysterman" | occ_value_cleaned == "grinder" | occ_value_cleaned == "layer" | occ_value_cleaned == "plasterer" | occ_value_cleaned == "carpenter" | occ_value_cleaned == "flagman" | occ_value_cleaned == "pressman" | occ_value_cleaned == "sailor" | occ_value_cleaned == "sampler" ~ "semi-skilled labor",
    occ_value_cleaned == "maker" | occ_value_cleaned == "butcher" | occ_value_cleaned == "fireman" | occ_value_cleaned == "contractor" | occ_value_cleaned == "mechanic" | occ_value_cleaned == "piano tuner" | occ_value_cleaned == "weaver" | occ_value_cleaned == "seamstress" | occ_value_cleaned == "blacksmith" | occ_value_cleaned == "section master" | occ_value_cleaned == "tailor" | occ_value_cleaned == "upholsterer" | occ_value_cleaned == "machinist" | occ_value_cleaned == "tinsmith" | occ_value_cleaned == "silversmith" | occ_value_cleaned == "captain" | occ_value_cleaned == "electrician" | occ_value_cleaned == "mason" | occ_value_cleaned == "millwright" | occ_value_cleaned == "plumber" | occ_value_cleaned == "undertaker" | occ_value_cleaned == "baker" ~ "skilled labor",
    occ_value_cleaned == "cook" | occ_value_cleaned == "keeper" | occ_value_cleaned == "bartender" | occ_value_cleaned == "liveryman" | occ_value_cleaned == "carrier" | occ_value_cleaned == "dairyman" | occ_value_cleaned == "barber" | occ_value_cleaned == "waiter" | occ_value_cleaned == "attendant" | occ_value_cleaned == "laundryman" | occ_value_cleaned == "occultist" | occ_value_cleaned == "photographer" | occ_value_cleaned == "prostitute" | occ_value_cleaned == "porter" | occ_value_cleaned == "driver" | occ_value_cleaned == "conductor" ~ "service",
    occ_value_cleaned == "law enforcement officer" | occ_value_cleaned == "justice of the peace" ~ "government",
    occ_value_cleaned == "minister" ~ "religion",
    occ_value_cleaned == "soldier" ~ "military",
    occ_value_cleaned == "student" | occ_value_cleaned == "teacher" | occ_value_cleaned == "professor" ~ "education",
    occ_value_cleaned == "none" ~ "none"
  ))

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
# ledger_male$path <- gsub("-NA", "-", ledger_male$path)
ledger_male <- head(ledger_male, -1)

ledger_male <- ungroup(ledger_male)
ledger_male_sb <- ledger_male %>% arrange(desc(count)) %>% select(path, count)

sum(ledger_male_sb$count)
sund2b(ledger_male_sb, valueField="count")

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
# ledger_female$path <- gsub("-NA", "-", ledger_female$path)
ledger_female <- head(ledger_female, -1)

ledger_female <- ungroup(ledger_female)
ledger_female_sb <- ledger_female %>% arrange(desc(count)) %>%select(path, count)

# sb_female <- sunburst(data=ledger_female_sb, legend=FALSE)
# sb_female <- ledger_female %>% arrange(desc(count)) %>% mutate(path = paste(supposedcausecategory1, supposedcausecleaned1, supposedcausecleaned2, sep='-')) %>% select(path, count) %>% sunburst(breadcrumb = list(w=200))
# sb_female

p2 <- sund2b(ledger_female_sb)
p2

# form
ledger_form <- ledger_form %>% select(Form, Form2, Form3, Frequency)
ledger_form = ledger_form %>% mutate(path = paste(Form, Form2, Form3, sep="-"))
ledger_form <- head(ledger_form, -2)
ledger_form <- ungroup(ledger_form)
ledger_form <- ledger_form %>% arrange(desc(Frequency)) %>% select(path, Frequency)

sund2b(ledger_form)

# occupation 
occ_bar <- subset(ledger, select=c(occ_value_cleaned, occ_category_cleaned))
occ_bar <- occ_bar %>% group_by(occ_category_cleaned, occ_value_cleaned) %>% summarize(n()) 
occ_bar = occ_bar %>% rename(count = "n()")
occ_bar <- head(occ_bar, -1)


occupationplot <- ggplotly(ggplot(occ_bar, aes(fill=occ_value_cleaned, x=occ_category_cleaned, y=count, text=paste("Occupation Category:", occ_category_cleaned, "<br>", "Occupation:", occ_value_cleaned, "<br>", "Count:", count))) + geom_bar(position="stack", stat="identity") + coord_flip()
         + labs(y="Occupation Category", x="Count") + theme_minimal() + theme(legend.position="none"), tooltip="text")


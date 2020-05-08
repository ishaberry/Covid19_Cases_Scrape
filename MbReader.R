#COVID-19 Canada Open Data Working Group 
#Automates Manitoba case data update from table on website: https://www.gov.mb.ca/covid19/updates/index.html
#Written By: VH, IB
#Date Modified: April 18th 2020

## This code does not currently run given MB switch to tableau dashboard, without machine readable data table

library(dplyr)
library(stringr)
library(rvest)
library(xml2)
library(lubridate)

#Pull into tibble
webpage_url <- "https://www.gov.mb.ca/covid19/updates/index.html"
webpage <- xml2::read_html(webpage_url)
case_summary <- rvest::html_table(webpage)[[1]] %>% tibble::as_tibble()

#The date in MB is not in the table itself but is rather in an emphasized HTML tag above the table 
#Example: <em>Last updated: April 18, 2020 - 1:00 p.m.</em>
#It is the 3rd emphasized element on the page 
em_text <- rvest::html_nodes(webpage, "em")[3] %>% toString()
#Month starts on character 18
dateString <- substring(em_text, 18) 
#Drop everything after the day
dateString <- word(dateString, sep=",")
report_date<-as.Date(dateString, format="%B %d")

#Clean original tibble:
    # - turn first row into col headings
    # - pull out only total cases
    # - rename to NumCases as in QC script
    # - cast cases into numeric 
    # - rename any discrepanices to match our dataset
names(case_summary) <- case_summary %>% slice(1) %>% unlist()
case_summary <- case_summary %>% slice(-1) %>% 
                                 select("Regions","Total Cases") %>% 
                                 rename("NumCases"="Total Cases") %>%
                                 mutate(NumCases = as.numeric(NumCases)) %>%
                                 mutate(Regions = recode(Regions, "Southern" = "Southern Health"))

#Download files in a dataframe from public sheet
casest_1 <- read.csv("https://docs.google.com/spreadsheets/u/1/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/export?format=csv&id=1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo&gid=942958991",
                     header = TRUE,
                     stringsAsFactors = FALSE,
                     skip = 3)

#filter by province
casest_1 <- casest_1 %>%
            select(province,health_region) %>%
            filter(province=="Manitoba")

#make table for t-1
casest_1 <- casest_1 %>%
            select(health_region) %>%
            group_by(health_region) %>%
            summarise(casest_1 = n()) 

#rename to Region
casest_1 <- casest_1 %>% rename(Regions = health_region)

#Save the last provincial_case_id 
id_start <- sum(casest_1$casest_1)

#Merge the two dataframes and subtract
merge <- merge(case_summary,casest_1, by ="Regions")
merge$dif <- merge$NumCases - merge$casest_1

#Take the case_summary from the website and turn it into a long format
cases_long <- data.frame(region=character(), stringsAsFactors=TRUE)
for (row in 1:nrow(merge)){
  region <- merge$Regions[row]
  numCasesInRegion <- merge$dif[row]
  if (numCasesInRegion>0)
  {
    linelisting <- data.frame(HealthRegion=matrix(ncol = 1, nrow = numCasesInRegion))
    linelisting[,1] <- region
    cases_long <-rbind(cases_long, linelisting)
  }
}

#Check the counts per health region (hence why its nice to keep the regions as a factor)
table(cases_long$HealthRegion)

#Format dataframe to match google sheets 
#provincial case id
cases_long$provincial_case_id<-(1:nrow(cases_long))+id_start

#health region
cases_long<- cases_long %>% 
  rename(health_region = HealthRegion)

#age, sex, travel_yn (not reported)
cases_long$age<-c("Not Reported")
cases_long$sex<-c("Not Reported")
cases_long$travel_yn<-c("Not Reported")

#province and country
cases_long$province<-c("Manitoba")
cases_long$country<-c("Canada")

# Report Date
cases_long$date_report<-format(Sys.Date(), format="%d-%m-%Y")

# Report Week
cases_long$report_week<-format(floor_date(Sys.Date(), "week"), format="%d-%m-%Y")

#Lat/Long -- TO BE UPDATED
cases_long$latitude<-c("")
cases_long$longitude<-c("")

#source (webpage url)
cases_long$case_source<-webpage_url

#method_note (0)
cases_long$method_note<-0

#blanks
cases_long$case_id<-c("")
cases_long$healthcare_facility<-c("")
cases_long$city<-c("")
cases_long$date_onset_symptom<-c("")
cases_long$date_admission_hospital<-c("")
cases_long$date_confirmation<-c("")
cases_long$symptom_list<-c("")
cases_long$travel_history_date<-c("")
cases_long$travel_history_country<-c("")
cases_long$travel_history_location<-c("")
cases_long$locally_acquired<-c("")
cases_long$chronic_disease_yn<-c("")
cases_long$chronic_condition_list<-c("")
cases_long$healthcare_worker_yn<-c("")
cases_long$nosocomial_yn<-c("")
cases_long$transmission_link_yn<-c("")
cases_long$transmission_link_province<-c("")
cases_long$transmission_link_id<-c("")
cases_long$syndromic_flag<-c("")
cases_long$active_flag<-c("")
cases_long$additional_info<-c("")
cases_long$additional_source<-c("")

#Reorder to match googlesheets format
col_order<-c("case_id", "provincial_case_id", "age", "sex", "healthcare_facility",
             "city", "health_region", "province", "country", "latitude", "longitude",
             "date_onset_symptom", "date_admission_hospital", "date_confirmation",
             "date_report", "report_week", "symptom_list", "travel_yn", "travel_history_date",
             "travel_history_country", "travel_history_location", "locally_acquired",
             "chronic_disease_yn", "chronic_condition_list", "healthcare_worker_yn", "nosocomial_yn",
             "transmission_link_yn", "transmission_link_province", "transmission_link_id",
             "syndromic_flag", "active_flag", "case_source", "additional_info", "additional_source",
             "method_note")
cases_long<-cases_long[,col_order]

#write to csv-> figure out how to export csv with special characters....
write.csv(cases_long, "/Users/vinyas/Desktop/mb_cases.csv", row.names = FALSE, fileEncoding = "UTF-8")



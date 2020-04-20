#COVID-19 Canada Open Data Working Group 
#Automates Quebec case data update from table on website: https://www.quebec.ca/en/health/health-issues/a-z/2019-coronavirus/situation-coronavirus-in-quebec/
#Written By: VH, IB
#Date Modified: April 4th 2020

library(dplyr)
library(stringr)
library(rvest)
library(xml2)
library(lubridate)

#Pull into tibble
webpage_url <- "https://www.quebec.ca/en/health/health-issues/a-z/2019-coronavirus/situation-coronavirus-in-quebec/"
webpage <- xml2::read_html(webpage_url)
case_summary <- rvest::html_table(webpage)[[1]] %>% tibble::as_tibble()

#Grab date before any modifications; currently formatted as: "Number of confirmed cases, on April 4, 1 p.m."
dateline <- colnames(case_summary)[2]
#"Number of confirmed cases, on " this is 30 chars long so start from 31st char
dateString <- substring(dateline, 31) 
# Drop chars after the second comma
dateString <- word(dateString, sep=",")
#Cast to date
report_date<-as.Date(dateString, format="%B %d")


#Clean original tibble into something nicer:
    # - drop first row because it's just a disclaimer (note this may change...)
    # - rename second column to a simple string from a string with weird characters
    # - change cases in second column from chr to numeric format
    # - remove spaces in all rows in second column
    # - change the second column to numeric format
    # - remove leading characters from regions
    # - remove final line which is total (note this may change too...)
case_summary <- case_summary %>% slice(-1) %>% 
                rename(NumCases = 2) %>%
                mutate(NumCases = str_replace_all(NumCases, "[^[:alnum:]]","")) %>% 
                mutate(NumCases = as.numeric(NumCases)) %>%
                mutate(Regions = str_replace(Regions, "[0-9]+ - ","")) %>%
                slice(-19)

#recode health regions to match
case_summary<- case_summary %>%
  mutate(Regions = recode(Regions, "Saguenay – Lac-Saint-Jean" = "Saguenay"))

case_summary<- case_summary %>%
  mutate(Regions = recode(Regions, "Mauricie-et-Centre-du-Québec" = "Mauricie"))

#download files in a dataframe from public sheet
casest_1 <- read.csv("https://docs.google.com/spreadsheets/u/1/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/export?format=csv&id=1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo&gid=942958991",
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  skip = 3)

#filter by province
casest_1<- casest_1 %>%
  select(province,health_region) %>%
  filter(province=="Quebec")

#make table for t-1
casest_1 <- casest_1 %>%
  select(health_region) %>%
  group_by(health_region) %>%
  summarise(casest_1 = n()) 

#rename to Region
casest_1<- casest_1 %>% 
  rename(Regions = health_region)

#Save the last provincial_case_id 
id_start<-sum(casest_1$casest_1)

#merge the two dataframes and subtract
merge<-merge(case_summary,casest_1, by ="Regions")
merge$dif<-merge$NumCases-merge$casest_1


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
cases_long$province<-c("Quebec")
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
write.csv(cases_long, "/Users/ib/Desktop/qc_cases.csv", row.names = FALSE, fileEncoding = "UTF-8")


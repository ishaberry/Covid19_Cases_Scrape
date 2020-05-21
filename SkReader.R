#COVID-19 Canada Open Data Working Group 
#Automates Saskatchewan case data update from table on website: https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/cases-and-risk-of-covid-19-in-saskatchewan
#Written By: VH, IB
#Date Modified: April 19th 2020

library(dplyr)
library(stringr)
library(rvest)
library(xml2)
library(lubridate)
library(RSelenium)

#Pull into tibble
#Note that SK is using Javascript on the site so need to use Selenium to run a headless browser to pull the tables
rd <- rsDriver()

rd$client$navigate('https://dashboard.saskatchewan.ca/health-wellness/covid-19/cases#cumulative-cases-tab')
h <- rd$client$getPageSource()
h <- h[[1]] %>% read_html()

rd$client$close()
rd$server$stop()
rm(rd)

case_summary <- h %>% html_node(xpath='//*[@id="table1-2"]/table') %>% html_table() %>% as_tibble()
table_title <- h %>% html_node(xpath='//*[@id="table1-2"]/h5') %>% html_text() %>% toString()

#Month starts on character 57
dateString <- substring(table_title, 57) 
#Drop everything after the day
dateString <- word(dateString, sep=",")
report_date<-as.Date(dateString, format="%B %d")

#Clean original tibble:
# - pull out only total cases
# - rename to NumCases as in QC script
# - use regex to remove any chars used for special notes (e.g. *, yen, superscript)
# - cast cases into numeric 
# - rename any discrepanices to match our dataset
case_summary <- case_summary %>% slice(1:6) %>% 
                                 select("Region","Total Cases") %>% 
                                 rename(c("Regions"="Region","NumCases"="Total Cases")) %>%
                                 mutate(NumCases = str_replace_all(NumCases, "\\*¥+" ,"")) %>% 
                                 mutate(NumCases = str_replace_all(NumCases, "[\u2070-\u209f\u00b0-\u00be]+" ,"")) %>% 
                                 mutate(NumCases = as.numeric(NumCases)) %>%
                                 mutate(Regions = recode(Regions, "Central (excluding Saskatoon)" = "Central", "South (excluding Regina)" = "South"))

#Download files in a dataframe from public sheet
casest_1 <- read.csv("https://docs.google.com/spreadsheets/u/1/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/export?format=csv&id=1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo&gid=942958991",
                     header = TRUE,
                     stringsAsFactors = FALSE,
                     skip = 3)

#Filter by province
casest_1 <- casest_1 %>%
  select(province,health_region) %>%
  filter(province=="Saskatchewan")

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
cases_long$province<-c("Saskatchewan")
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
write.csv(cases_long, "/Users/vinyas/Desktop/sk_cases.csv", row.names = FALSE, fileEncoding = "UTF-8")


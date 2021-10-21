#set working directory to project directory

#names of datasets
excel_names<- c("co2percap.xls","gdpconstantpercap.xls",
                "intarrival.xls", "rweengery.xls",
                "urban.xls")
path_db<- c("data_bases/raw_data_bases")
#relative path, important does not work unless you set the working directory to project directory
paths<- paste0(path_db,excel_names)
#import all datasets into list
require(readxl)
raw_dsets<- lapply(paths, function(x){
  read_excel(x, skip = 3)
})
#eliminate some columns, data into tidy format and filter
require(tidyverse)
raw_dsets_tidy<- lapply(raw_dsets, function(x){
  a<- select(x, -c("Country Code","Indicator Code"))
  b<- pivot_longer(a, cols = -c("Country Name","Indicator Name"), 
                   names_to = c("Year"),
                   values_to = c(a$"Indicator Name"[1]))
  c<- select(b, -"Indicator Name")
  d<- filter(c, Year %in% c(1995:2014))
  e<- rename(d,"Country"='Country Name') %>%
    filter(., Country %in% c(
    "Argentina","Brazil","Bolivia","Chile","Colombia","Ecuador",
    "Peru","Paraguay","Uruguay"))
})
#join all the data sets that are in list into 1 tibble, set new names, set correct class
require(purrr)
tidy_dset<- reduce(raw_dsets_tidy, left_join, by = c("Country" = "Country","Year"="Year"))
new_names<- c("Country","Year","CO2","GDP","Tourism",
              "Renewable","Urban")
Tidy_k_tur<- setNames(tidy_dset,new_names) %>% mutate(.,Country=as.factor(Country),Year=as.integer(Year))

#save this df in csv
write.csv(Tidy_k_tur,file = "data_bases/tidy_data_base/WB_tidy.csv")

#transform variables to its natural log
CAK_logs<- Tidy_k_tur %>%
  mutate(CO2_log=log(CO2),GDP_log=log(GDP),Tourism_log=log(Tourism),
         Tur_square=(log(Tourism))^2)%>%
  select(- c("CO2","GDP","Tourism"))
View(CAK_logs)

#add categories to countries based on the arrivals of 2014
CAK_logs_category<- CAK_logs %>%
  mutate(Category = case_when(
    .$Country %in% c("Argentina","Brazil") ~ "High_Tourism",
    .$Country %in% c("Bolivia","Ecuador","Paraguay") ~ "Low_Tourism",
     TRUE ~ "Mid_Tourism"
  ))
View(CAK_logs_category)



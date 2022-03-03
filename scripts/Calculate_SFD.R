#Seafood Footprint Displacement Calculation
#Caitie Kuempel
#Sept. 14, 2018


#Set wd
setwd("~/Dropbox/Collaborations/Fisheries_impact_displacement/September_2018")
#setwd("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/September_2018")


#load libraries
library(dplyr)
library(countrycode)
library(reshape2)
library(stringr)
library(data.table)
library(zoo)

#Load data
imex_dat<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/September_2018/Input_data/Carissa1Oct.csv", header = T)
fishloc<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/September_2018/Input_data/FishedWhereV2Summary.csv")


#Decide whether to include Aquaculture and IUU fishing or not
IncludeAqua = "No"
IncludeIUU = "No"


######################################################Clean data########################################################

#Change export country names to ISO3 for matching
imex_dat$Export_ISO3<-countrycode(imex_dat$ExportCountry, "country.name", "iso3c")
imex_dat$ExportCountry<-ifelse(imex_dat$ExportCountry == "Dominican Rp", "Dominican Republic",
                               ifelse(imex_dat$ExportCountry == "Czechoslovak", "Czechoslovakia",
                                      ifelse(imex_dat$ExportCountry == "Fr Guiana", "French Guiana",
                                             ifelse(imex_dat$ExportCountry == "Fr Polynesia", "French Polynesia",
                                                    ifelse(imex_dat$ExportCountry == "NethAntilles", "Netherlands Antilles",
                                                           ifelse(imex_dat$ExportCountry == "St Pier Mq", "Saint-Pierre Martinique",
                                                                  ifelse(imex_dat$ExportCountry == "Cura\xe7ao", "Curacao",
                                                                         ifelse(imex_dat$ExportCountry == "Untd Arab Em", "United Arab Emirates", as.character(imex_dat$ExportCountry)))))))))

imex_dat$Export_ISO3<-countrycode(imex_dat$ExportCountry, "country.name", "iso3c")

#Czechoslovakia split in 1993 into Czech republic and Slovakia
imex_dat$Export_ISO3<-ifelse(imex_dat$ExportCountry == "Czechoslovakia", "CZE", imex_dat$Export_ISO3)
imex_dat$Export_ISO3<-ifelse(imex_dat$ExportCountry == "Netherlands Antilles", "ANT", imex_dat$Export_ISO3)
imex_dat$Export_ISO3<-ifelse(imex_dat$ExportCountry == "Micronesia", "FSM", imex_dat$Export_ISO3)
imex_dat$Export_ISO3<-ifelse(imex_dat$ExportCountry == "Yugoslavia", "SCG", imex_dat$Export_ISO3)


#Change import country names to ISO3 for matching                         
imex_dat$Import_ISO3<-countrycode(imex_dat$ImportCountry, "country.name", "iso3c")

imex_dat$ImportCountry<-ifelse(imex_dat$ImportCountry == "Cent Afr Rep", "Central African Republic",
                               ifelse(imex_dat$ImportCountry == "Czechoslovak", "Czechoslovakia",
                                      ifelse(imex_dat$ImportCountry == "Dominican Rp", "Dominican Republic",
                                             ifelse(imex_dat$ImportCountry == "Fr Guiana", "French Guiana",
                                                    ifelse(imex_dat$ImportCountry == "Fr Polynesia", "French Polynesia",
                                                           ifelse(imex_dat$ImportCountry == "NethAntilles", "Netherlands Antilles",
                                                                  ifelse(imex_dat$ImportCountry == "St Pier Mq", "Saint-Pierre Martinique",
                                                                         ifelse(imex_dat$ImportCountry == "Cura\xe7ao", "Curacao",
                                                                                ifelse(imex_dat$ImportCountry == "Untd Arab Em", "United Arab Emirates", as.character(imex_dat$ImportCountry))))))))))

imex_dat$Import_ISO3<-countrycode(imex_dat$ImportCountry, "country.name", "iso3c")

imex_dat$Import_ISO3<-ifelse(imex_dat$ImportCountry == "Czechoslovakia", "CZE", imex_dat$Import_ISO3)
imex_dat$Import_ISO3<-ifelse(imex_dat$ImportCountry == "Netherlands Antilles", "ANT", imex_dat$Import_ISO3)
imex_dat$Import_ISO3<-ifelse(imex_dat$ImportCountry == "Yugoslavia", "SCG", imex_dat$Import_ISO3)
imex_dat$Import_ISO3<-ifelse(imex_dat$ImportCountry == "Micronesia", "FSM", imex_dat$Import_ISO3)


#Get Source country ISO3
imex_dat<-imex_dat[!imex_dat$SourceCountry == "Channel Is",]
imex_dat$Source_ISO3<-countrycode(imex_dat$SourceCountry, "country.name", "iso3c")

imex_dat$SourceCountry<-ifelse(imex_dat$SourceCountry == "Cent Afr Rep", "Central African Republic",
                               ifelse(imex_dat$SourceCountry == "Czechoslovak", "Czechoslovakia",
                                      ifelse(imex_dat$SourceCountry == "Dominican Rp", "Dominican Republic",
                                             ifelse(imex_dat$SourceCountry == "Fr Guiana", "French Guiana",
                                                    ifelse(imex_dat$SourceCountry == "Fr Polynesia", "French Polynesia",
                                                           ifelse(imex_dat$SourceCountry == "NethAntilles", "Netherlands Antilles",
                                                                  ifelse(imex_dat$SourceCountry == "St Pier Mq", "Saint-Pierre Martinique",
                                                                         ifelse(imex_dat$SourceCountry == "Cura\xe7ao", "Curacao",
                                                                                ifelse(imex_dat$SourceCountry == "Untd Arab Em", "United Arab Emirates", as.character(imex_dat$SourceCountry))))))))))

imex_dat$Source_ISO3<-countrycode(imex_dat$SourceCountry, "country.name", "iso3c")

imex_dat$Source_ISO3<-ifelse(imex_dat$SourceCountry == "Netherlands Antilles", "ANT", imex_dat$Source_ISO3)
imex_dat$Source_ISO3<-ifelse(imex_dat$SourceCountry == "Yugoslavia", "SCG", imex_dat$Source_ISO3)
imex_dat$Source_ISO3<-ifelse(imex_dat$SourceCountry == "Micronesia", "FSM", imex_dat$Source_ISO3)

##

fishloc$Fishing_ISO3<-countrycode(fishloc$FishingCountry, "country.name", "iso3c")

#Remove Channel Islands and Other nei
fishloc<-fishloc[!fishloc$FishingCountry == "Other nei",]
fishloc<-fishloc[!fishloc$FishingCountry == "Channel Is",]

fishloc$FishingCountry<-ifelse(fishloc$FishingCountry == "Amer Samoa", "American Samoa",
                               ifelse(fishloc$FishingCountry == "Br Ind Oc Tr", "British Indian Ocean Territory",
                                      ifelse(fishloc$FishingCountry == "Br Virgin Is", "British Virgin Islands",
                                             ifelse(fishloc$FishingCountry == "Channel Is", "Channel Islands",
                                                    ifelse(fishloc$FishingCountry == "Dominican Rp", "Dominican Republic",
                                                           ifelse(fishloc$FishingCountry == "Fr Guiana", "French Guiana",
                                                                  ifelse(fishloc$FishingCountry == "Fr Polynesia", "French Polynesia",
                                                                         ifelse(fishloc$FishingCountry == "St Pier Mq", "Saint-Pierre Martinique",
                                                                                ifelse(fishloc$FishingCountry == "Untd Arab Em", "United Arab Emirates",
                                                                                       ifelse(fishloc$FishingCountry == "Cura\xe7ao", "Curacao",
                                                                                              ifelse(fishloc$FishingCountry == "US Virgin Is", "United States Virgin Islands", as.character(fishloc$FishingCountry))))))))))))



fishloc$Fishing_ISO3<-countrycode(fishloc$FishingCountry, "country.name", "iso3c")

fishloc$Fishing_ISO3<-ifelse(fishloc$FishingCountry == "Micronesia", "FSM",
                             ifelse(fishloc$FishingCountry == "NethAntilles", "ANT",
                                    ifelse(fishloc$FishingCountry == "Yugoslavia", "SCG", as.character(fishloc$Fishing_ISO3))))




fishloc$Fished_ISO3<-countrycode(fishloc$FishedCountry, "country.name", "iso3c")

fishloc<-fishloc[!fishloc$FishedCountry == "Kerguelen Is",]
fishloc<-fishloc[!fishloc$FishedCountry == "Channel Is",]

fishloc$FishedCountry<-ifelse(fishloc$FishedCountry == "Amer Samoa", "American Samoa",
                              ifelse(fishloc$FishedCountry == "Br Ind Oc Tr", "British Indian Ocean Territory",
                                     ifelse(fishloc$FishedCountry == "Br Virgin Is", "British Virgin Islands",
                                            ifelse(fishloc$FishedCountry == "Channel Is", "Channel Islands",
                                                   ifelse(fishloc$FishedCountry == "Dominican Rp", "Dominican Republic",
                                                          ifelse(fishloc$FishedCountry == "Fr Guiana", "French Guiana",
                                                                 ifelse(fishloc$FishedCountry == "Fr Polynesia", "French Polynesia",
                                                                        ifelse(fishloc$FishedCountry == "St Pier Mq", "Saint-Pierre Martinique",
                                                                               ifelse(fishloc$FishedCountry == "Untd Arab Em", "United Arab Emirates",
                                                                                      ifelse(fishloc$FishedCountry == "US Virgin Is", "United States Virgin Islands", as.character(fishloc$FishedCountry)))))))))))



fishloc$Fished_ISO3<-countrycode(fishloc$FishedCountry, "country.name", "iso3c")

fishloc$Fished_ISO3<-ifelse(fishloc$FishedCountry == "Micronesia", "FSM",
                            ifelse(fishloc$FishedCountry == "NethAntilles", "ANT",
                                   ifelse(fishloc$FishedCountry == "Yugoslavia", "SCG", 
                                          ifelse(fishloc$FishedCountry == "West Sahara", "ESH",
                                                 ifelse(fishloc$FishedCountry == "High Seas", "HighSeas", as.character(fishloc$Fished_ISO3))))))


#Decide whether to include aquaculture or not

if(IncludeAqua == "No"){
  imex<-imex_dat[!imex_dat$Source == "Domestic Aqua",]
  imex<-imex[!imex$Source == "ReExport Domestic Aqua",]
}else{
  imex<-imex_dat
}

#Check to make sure the correct variables are there (i.e. aquaculture)
unique(imex$Source)

#This info is not in the data anymore?
#Remove river eels, misc freshwater fishes, from imex data?
#drops<-c("River eels", "Misc Freshwater Fishes")
#imex$CommonName2<-as.character(imex$CommonName)
#imex<-imex[!imex$CommonName2 %in% drops,]

fishloc$Reported2<-as.numeric(fishloc$Reported)
fishloc$IUU2<-as.numeric(fishloc$IUU)

if(IncludeIUU == "Yes"){
  imex$Total_imports<-imex$Tonnage + imex$IUUAssocited 
  fishloc$Total_fished<-fishloc$Reported2 + fishloc$IUU2  ####Change this if you don't want to include IUU
}else{
  imex$Total_imports<-imex$Tonnage
  fishloc$Total_fished<-fishloc$Reported2####Change this if you don't want to include IUU
}


#(Caitie: P, R, H come from the “Fished WhereV2” spreadsheet 
#(For each one, sum up the columns for ‘reported’, ‘IUU’, and ‘Discards’  
#for one scenario…then simply use ‘reported’ for another scenario). Q comes 
#from the “Trade” spreadsheet.

################################Quantity of seafood imported to country i from country j (Q)######################
#Seafood imported to Vanuatu from other countries
q_imports<-imex %>% group_by(Import_ISO3,Export_ISO3, Year) %>% 
  summarise(Qimports = sum(Total_imports)) %>% 
  as.data.frame()

####################################Quantity of seafood caught by country i in country j (R) #########################
#Seafood caught by vanuatu in another country
q_fished<-fishloc %>% 
  filter(!Fished_ISO3 == "HighSeas") %>% 
  filter(!Fishing_ISO3 == Fished_ISO3) %>% 
  group_by(Fishing_ISO3, Fished_ISO3, IYear) %>% 
  summarise(Qfished = sum(Total_fished)) %>% 
  as.data.frame()

#####################################Quantity of seafood caught by country i in the High Seas (H)######################
q_highseas<-fishloc %>% 
  filter(Fished_ISO3 == "HighSeas") %>% 
  group_by(Fishing_ISO3, IYear) %>% 
  summarise(Qhighseas = sum(Total_fished)) %>% 
  as.data.frame()

###############################Quantity of seafood produced in country i (Pi) by country i##################################

#Not including High Seas or including High Seas??? For now I do not because otherwise
#we would double count it.

#Only include seafood caught or produced by country i in country i
q_catch<-fishloc %>% 
  filter(!Fished_ISO3 == "HighSeas", !Fishing_ISO3 == "HighSeas") %>%
  filter(Fishing_ISO3 == Fished_ISO3) %>% 
  group_by(Fishing_ISO3, IYear) %>% 
  summarise(Qcaught = sum(Total_fished)) %>% 
  as.data.frame()

#aqua<-imex %>% filter(Source == "Domestic Aqua")
#reaqua<-imex %>% filter(Source == "ReExport Domestic Aqua")
q_aqua<- imex %>% 
  filter(Source == "Domestic Aqua") %>% 
  group_by(Export_ISO3, Year) %>% 
  summarise(QAqua = sum(Total_imports)) %>% 
  as.data.frame()
q_reaqua<-imex %>% 
  filter(Source == "ReExport Domestic Aqua") %>% 
  group_by(Export_ISO3, Year) %>% 
  summarise(QReAqua = sum(Total_imports)) %>% 
  as.data.frame()

q_production<-left_join(q_catch, q_aqua, by = c("Fishing_ISO3" = "Export_ISO3", "IYear" = "Year")) %>%
  left_join(., q_reaqua, by = c("Fishing_ISO3" = "Export_ISO3", "IYear" = "Year"))
q_production[is.na(q_production)]<-0

q_aqua_import<-imex %>% 
  filter(Source == "Domestic Aqua") %>%  
  group_by(Import_ISO3, Year) %>% 
  summarise(QAqua_Im = sum(Total_imports))
q_reaqua_import<-imex %>% 
  filter(Source == "ReExport Domestic Aqua") %>% 
  group_by(Import_ISO3, Year) %>% 
  summarise(QReAqu_Im = sum(Total_imports))
q_all_im<-imex %>% 
  group_by(Import_ISO3,Year) %>%
  summarise(Qimports_total = sum(Total_imports))


q_all_aq_import<-left_join(q_all_im,q_aqua_import, by = c("Import_ISO3", "Year")) %>%
  left_join(.,q_reaqua_import, by = c("Import_ISO3", "Year")) 

  

q_all_aq_import[is.na(q_all_aq_import)]<-0
q_all_aq_import$all_aqua<-q_all_aq_import$QAqua_Im+q_all_aq_import$QReAqu_Im
q_all_aq_import$prop_aqua_im<-q_all_aq_import$all_aqua/q_all_aq_import$Qimports_total

if(IncludeAqua == "Yes"){
  q_production$QProd<-q_production$Qcaught+q_production$QAqua + q_production$QReAqua
  q_production$prop_aqua_prod<-(q_production$QAqua + q_production$QReAqua)/(q_production$Qcaught+q_production$QAqua + q_production$QReAqua)
  aqua_all<-left_join(q_all_aq_import, q_production, by = c("Import_ISO3" = "Fishing_ISO3", "Year" = "IYear"))
  #fwrite(aqua_all, "./Output_data/Proportion_aquaculture_imports.csv")
}else{
  q_production$QProd<-q_production$Qcaught
  q_production$prop_aqua<-0
}

colnames(q_production)<-c("prod_country", "Year", "Qcaught", 'QAqua', "QReAqua", 'QProd', 'prop_aqua')


###############Quantity of fish caught in each country by other countries#####################

w_fished<-fishloc %>% 
  filter(!Fishing_ISO3 == Fished_ISO3) %>% 
  group_by(Fished_ISO3, IYear) %>% 
  summarise(Total_wfished = sum(Total_fished, na.rm = T)) %>% 
  as.data.frame()

w_fished_ISO3<- fishloc %>% 
  filter(!Fishing_ISO3 == Fished_ISO3) %>% 
  group_by(Fishing_ISO3,Fished_ISO3, IYear) %>% 
  summarise(wfished = sum(Total_fished, na.rm = T)) %>% 
  as.data.frame()

w_final<-left_join(w_fished_ISO3, w_fished, by = c("Fished_ISO3", "IYear"))

fwrite(w_final,"C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Where_tonnes_displaced.csv")

##################################Combine data to calculate SDF#############################
#imex_fish<-left_join(imex, fishloc, by = c("Import_ISO3" = "Fishing_ISO3"))

#CT<-unique(imex$Import_ISO3)

imex_countries<-unique(imex$Import_ISO3)
fish_countries<-fishloc %>% 
  filter(!Fished_ISO3 == "HighSeas", !Fishing_ISO3 == "HighSeas")
fish_countries<-unique(fish_countries$Fishing_ISO3)

#Join data together
#Assume that the data is correct. So if there is no trade data, just assume no trade was conducted. 
#Or, similarly, if there is no fishing data, just assume no fishing was conducted.

all_dat_me<-full_join(q_imports, q_fished, by = c("Import_ISO3" = "Fishing_ISO3", "Export_ISO3" = "Fished_ISO3", "Year" = "IYear")) %>% 
  filter(!Import_ISO3 == Export_ISO3) %>% 
  full_join(., q_highseas, by = c("Import_ISO3" = "Fishing_ISO3", "Year" = "IYear")) %>% 
  full_join(., q_production, by = c("Import_ISO3" = "prod_country", "Year")) %>% 
  select(Import_ISO3, Export_ISO3, Year, Qimports, Qfished, Qhighseas, QProd)

all_dat_me[is.na(all_dat_me)]<-0

all_dat<-full_join(q_imports, q_fished, by = c("Import_ISO3" = "Fishing_ISO3", "Export_ISO3" = "Fished_ISO3", "Year" = "IYear")) %>% 
  filter(!Import_ISO3 == Export_ISO3) %>% 
  group_by(Import_ISO3, Year) %>% 
  summarise(Q_imports = sum(Qimports, na.rm = T), Q_fish = sum(Qfished, na.rm = T))%>% 
  full_join(., q_highseas, by = c("Import_ISO3" = "Fishing_ISO3", "Year" = "IYear")) %>% 
  full_join(., q_production, by = c("Import_ISO3" = "prod_country", "Year")) %>% 
  select(Import_ISO3, Year, Q_imports, Q_fish, Qhighseas, QProd)

#This does not include country/years where all values = 0
all_dat[is.na(all_dat)]<-0

#Calculate SDF with and without high seas
all_dat<-all_dat %>% 
  mutate(sdf = (Q_imports + Q_fish + Qhighseas)/(Q_imports + Q_fish + Qhighseas + QProd)) %>% 
  mutate(sdf_nohs = (Q_imports + Q_fish)/(Q_imports + Q_fish + QProd))

#Calculate proportion of production in high seas
prop_prod_high_seas<-unique(all_dat[,c("Import_ISO3", "Year", "Qhighseas", "QProd")])
prop_prod_high_seas$prop_hs<-prop_prod_high_seas$Qhighseas/prop_prod_high_seas$QProd
test<-prop_prod_high_seas[complete.cases(prop_prod_high_seas),]


if(IncludeAqua == "Yes"){
  fwrite(test, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Ratio_high_seas_prod_Aqua.csv")
  fwrite(all_dat_me, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/ME_sdf_dat_Aqua.csv")
}else{
  fwrite(test, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Ratio_high_seas_prod_NoAqua.csv")
  fwrite(all_dat_me, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/ME_sdf_dat_noAqua.csv")
}

if(IncludeAqua == "Yes"){
  fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Seafood_displacement_footprint_data_Aqua.csv")
}else{
  fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Seafood_displacement_footprint_data_NoAqua.csv")
}

##########################################Moving Average Calc##############################

if(IncludeAqua == "Yes"){
  all_sdf<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Seafood_displacement_footprint_data_Aqua.csv")
}else{
  all_sdf<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Seafood_displacement_footprint_data_NoAqua.csv")
}


countries<-unique(all_sdf$Import_ISO3)

all_dat<-c()

for(x in 1:length(countries)){
  #done<-c()
  print(x)
  cname<-as.character(countries[x])
  sub<-all_sdf[all_sdf$Import_ISO3 == cname,]
  if(nrow(sub)<=5){
    next
  }else{
    
    min_year<-min(sub$Year)
    max_year<-max(sub$Year)  
    time_period<-c(min_year:max_year)
    time_df<-data.frame(years =time_period)
    sub_t<-left_join(time_df, sub, by = c("years" = "Year"))
    #m.av<-rollapply(sub_t$SDF, 5, mean)
    m.av<-rollapply(sub_t$sdf, 5, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
    
    #For SO of the rollapply check here
    #https://stackoverflow.com/questions/17765001/using-rollmean-when-there-are-missing-values-na
    
    #m.av<-rollmean(sub_t$SDF, 5,fill = list(NA, NULL, NA))  
    #moving_avg<-movavg(sub$SDF, n = 5, type = "s")
    
    #seq_years<-seq(min_year, max_year,1)
    
    n_yrs<-max_year -min_year
    done<-data.frame(country = cname, max_yr = max_year, min_yr = min_year, total_years = n_yrs, mov_avg=m.av, time = time_period)
    all_dat<-rbind(all_dat, done)
    #return(all_dat)
  }
}

if(IncludeAqua == "Yes"){
  
  fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_SDF_Aqua.csv")
  
}else{
  
  fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_SDF_NoAqua.csv")
}


##########################################Moving Average Calc No High Seas##############################

if(IncludeAqua == "Yes"){
  all_sdf<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Seafood_displacement_footprint_data_Aqua.csv")
}else{
  all_sdf<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Seafood_displacement_footprint_data_NoAqua.csv")
}


countries<-unique(all_sdf$Import_ISO3)

all_dat<-c()

for(x in 1:length(countries)){
  #done<-c()
  print(x)
  cname<-as.character(countries[x])
  sub<-all_sdf[all_sdf$Import_ISO3 == cname,]
  if(nrow(sub)<=5){
    next
  }else{
    
    min_year<-min(sub$Year)
    max_year<-max(sub$Year)  
    time_period<-c(min_year:max_year)
    time_df<-data.frame(years =time_period)
    sub_t<-left_join(time_df, sub, by = c("years" = "Year"))
    #m.av<-rollmean(sub_t$SDF_nohs, 5,fill = list(NA, NULL, NA)) 
    m.av<-rollapply(sub_t$sdf_nohs, 5, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
    
    #moving_avg<-movavg(sub$SDF2, n = 5, type = "s")
    
    #seq_years<-seq(min_year, max_year,1)
    
    n_yrs<-max_year -min_year
    done<-data.frame(country = cname, max_yr = max_year, min_yr = min_year, total_years = n_yrs, mov_avg=m.av, time = time_period)
    all_dat<-rbind(all_dat, done)
    #return(all_dat)
  }
}

if(IncludeAqua == "Yes"){
  
  fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_SDFnoHS_Aqua.csv")
  
}else{
  
  fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_SDFnoHS_NoAqua.csv")
}

####################################Proportion of seafood imported##########################


if(IncludeAqua == "Yes"){
  prod_imp<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Seafood_displacement_footprint_data_Aqua.csv")
}else{
  prod_imp<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Seafood_displacement_footprint_data_NoAqua.csv")
}

prod_imp$Domestic_supply<-prod_imp$QProd +prod_imp$Q_imports + prod_imp$Q_fish
prod_imp$Prop_import<-prod_imp$Q_imports/prod_imp$Domestic_supply

prod_imp<-prod_imp[complete.cases(prod_imp),]


countries<-unique(prod_imp$Import_ISO3)

all_dat<-c()

for(x in 1:length(countries)){
  #done<-c()
  print(x)
  cname<-as.character(countries[x])
  sub<- prod_imp[prod_imp$Import_ISO3 == cname,]
  if(nrow(sub)<=5){
    next
  }else{
    
    min_year<-min(sub$Year)
    max_year<-max(sub$Year)  
    time_period<-c(min_year:max_year)
    time_df<-data.frame(years =time_period)
    sub_t<-left_join(time_df, sub, by = c("years" = "Year"))
    #m.av<-rollmean(sub_t$Prop_import, 5,fill = list(NA, NULL, NA))  
    m.av<-rollapply(sub_t$Prop_import, 5, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
    
    #moving_avg<-movavg(sub$SDF2, n = 5, type = "s")
    
    #seq_years<-seq(min_year, max_year,1)
    
    n_yrs<-max_year -min_year
    done<-data.frame(country = cname, max_yr = max_year, min_yr = min_year, total_years = n_yrs, mov_avg=m.av, time = time_period)
    all_dat<-rbind(all_dat, done)
    #return(all_dat)
  }
}

if(IncludeAqua == "Yes"){
  
  fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_Prop_import_Aqua.csv")
  
}else{
  
  fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_Prop_import_NoAqua.csv")
}

  ####################################Total seafood imported##########################
  
  
  if(IncludeAqua == "Yes"){
    sdf_dat<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Seafood_displacement_footprint_data_Aqua.csv")
  }else{
    sdf_dat<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Seafood_displacement_footprint_data_NoAqua.csv")
  }
  
  
  imports<- sdf_dat %>% group_by(Year) %>% summarise(Total_imports = sum(Q_imports)) %>% as.data.frame()
  min_year<-min(imports$Year)
  max_year<-max(imports$Year)  
  time_period<-c(min_year:max_year)
  time_df<-data.frame(years =time_period)
  sub_t<-left_join(time_df, imports, by = c("years" = "Year"))
  m.av<-rollapply(sub_t$Total_import, 5, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
      
      #m.av<-rollmean(sub_t$Total_import, 5,fill = list(NA, NULL, NA))  
      #moving_avg<-movavg(sub$SDF2, n = 5, type = "s")
      
      #seq_years<-seq(min_year, max_year,1)
      
      n_yrs<-max_year -min_year
      done<-data.frame(max_yr = max_year, min_yr = min_year, total_years = n_yrs, mov_avg=m.av, time = time_period)
  
  if(IncludeAqua == "Yes"){
    
    fwrite(done, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_Total_import_Aqua.csv")
    
  }else{
    
    fwrite(done, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_Total_import_NoAqua.csv")
  }
  
  
  ##########################################High Seas Moving Average Calc##############################
  
  if(IncludeAqua == "Yes"){
    hs_dat<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Ratio_high_seas_prod_Aqua.csv")
  }else{
    hs_dat<-fread("C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Ratio_high_seas_prod_NoAqua.csv")
  }
  
  
  countries<-unique(hs_dat$Import_ISO3)
  
  all_dat<-c()
  
  for(x in 1:length(countries)){
    #done<-c()
    print(x)
    cname<-as.character(countries[x])
    sub<- hs_dat[hs_dat$Import_ISO3 == cname,]
    if(nrow(sub)<=5){
      next
    }else{
      
      min_year<-min(sub$Year)
      max_year<-max(sub$Year)  
      time_period<-c(min_year:max_year)
      time_df<-data.frame(years =time_period)
      sub_t<-left_join(time_df, sub, by = c("years" = "Year"))
      #m.av<-rollmean(sub_t$prop_hs, 5,fill = list(NA, NULL, NA)) 
      m.av<-rollapply(sub_t$prop_hs, 5, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
      
      #moving_avg<-movavg(sub$SDF2, n = 5, type = "s")
      
      #seq_years<-seq(min_year, max_year,1)
      
      n_yrs<-max_year -min_year
      done<-data.frame(country = cname, max_yr = max_year, min_yr = min_year, total_years = n_yrs, mov_avg=m.av, time = time_period)
      all_dat<-rbind(all_dat, done)
      #return(all_dat)
    }
  }
  
  if(IncludeAqua == "Yes"){
    
    fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_Ratio_high_seas_Aqua.csv")
    
  }else{
    
    fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_Ratio_high_seas_NoAqua.csv")
  }
  
  
  
  ##########################################Aquaculture Moving Average Calc##############################
  
  #Proportion of imports
  aq_dat<-aqua_all[complete.cases(aqua_all),]
  
  countries<-unique(aqua_all$Import_ISO3)
  countries<-countries[!countries == ""]
  
  all_dat<-c()
  
  for(x in 1:length(countries)){
    #done<-c()
    print(x)
    cname<-as.character(countries[x])
    sub<-aqua_all[aqua_all$Import_ISO3 == cname,]
    if(nrow(sub)<=5){
      next
    }else{
      
      min_year<-min(sub$Year)
      max_year<-max(sub$Year)  
      time_period<-c(min_year:max_year)
      time_df<-data.frame(years =time_period)
      sub_t<-left_join(time_df, sub, by = c("years" = "Year"))
      #m.av<-rollmean(sub_t$prop_aqua_im, 5,fill = list(NA, NULL, NA)) 
      m.av<-rollapply(sub_t$prop_aqua_im, 5, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
      
      #moving_avg<-movavg(sub$SDF2, n = 5, type = "s")
      
      #seq_years<-seq(min_year, max_year,1)
      
      n_yrs<-max_year -min_year
      done<-data.frame(country = cname, max_yr = max_year, min_yr = min_year, total_years = n_yrs, mov_avg=m.av, time = time_period)
      all_dat<-rbind(all_dat, done)
      #return(all_dat)
    }
  }
  
  fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_Prop__Aqua_Imports.csv")
  
  
  #Proportion of production
  all_dat<-c()
  
  for(x in 1:length(countries)){
    #done<-c()
    print(x)
    cname<-as.character(countries[x])
    sub<-aqua_all[aqua_all$Import_ISO3 == cname,]
    if(nrow(sub)<=5){
      next
    }else{
      
      min_year<-min(sub$Year)
      max_year<-max(sub$Year)  
      time_period<-c(min_year:max_year)
      time_df<-data.frame(years =time_period)
      sub_t<-left_join(time_df, sub, by = c("years" = "Year"))
      #m.av<-rollmean(sub_t$prop_aqua_prod, 5,fill = list(NA, NULL, NA))  
      m.av<-rollapply(sub_t$prop_aqua_prod, 5, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
      
      #moving_avg<-movavg(sub$SDF2, n = 5, type = "s")
      
      #seq_years<-seq(min_year, max_year,1)
      
      n_yrs<-max_year -min_year
      done<-data.frame(country = cname, max_yr = max_year, min_yr = min_year, total_years = n_yrs, mov_avg=m.av, time = time_period)
      all_dat<-rbind(all_dat, done)
      #return(all_dat)
    }
  }
  
  fwrite(all_dat, "C:/Users/uqckuemp/Dropbox/Collaborations/Fisheries_impact_displacement/May_2020/Output_data/Moving_average_Prop__Aqua_Production.csv")
  
  ##########################################Aquaculture Moving Average Calc##############################
  
  #Proportion of imports
  aq_dat<-aqua_all[complete.cases(aqua_all),]
  
  countries<-unique(aqua_all$Import_ISO3)
  countries<-countries[!countries == ""]
  
  all_dat<-c()
  
  for(x in 1:length(countries)){
    #done<-c()
    print(x)
    cname<-as.character(countries[x])
    sub<-aqua_all[aqua_all$Import_ISO3 == cname,]
    if(nrow(sub)<=5){
      next
    }else{
      
      min_year<-min(sub$Year)
      max_year<-max(sub$Year)  
      time_period<-c(min_year:max_year)
      time_df<-data.frame(years =time_period)
      sub_t<-left_join(time_df, sub, by = c("years" = "Year"))
      #m.av<-rollmean(sub_t$SDF2, 5,fill = list(NA, NULL, NA))  
      m.av<-rollapply(sub_t$SDF2, 5, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
      #moving_avg<-movavg(sub$SDF2, n = 5, type = "s")
      
      #seq_years<-seq(min_year, max_year,1)
      
      n_yrs<-max_year -min_year
      done<-data.frame(country = cname, max_yr = max_year, min_yr = min_year, total_years = n_yrs, mov_avg=m.av, time = time_period)
      all_dat<-rbind(all_dat, done)
      #return(all_dat)
    }
  }
  
  fwrite(all_dat, "./Output_data/Moving_average_Prop__Aqua_Imports.csv")
  
  
  #Proportion of production
  all_dat<-c()
  
  for(x in 1:length(countries)){
    #done<-c()
    print(x)
    cname<-as.character(countries[x])
    sub<-aqua_all[aqua_all$Import_ISO3 == cname,]
    if(nrow(sub)<=5){
      next
    }else{
      
      min_year<-min(sub$Year)
      max_year<-max(sub$Year)  
      time_period<-c(min_year:max_year)
      time_df<-data.frame(years =time_period)
      sub_t<-left_join(time_df, sub, by = c("years" = "Year"))
      #m.av<-rollmean(sub_t$prop_aqua_prod, 5,fill = list(NA, NULL, NA))
      m.av<-rollapply(sub_t$prop_aqua_prod, 5, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
      
      #moving_avg<-movavg(sub$SDF2, n = 5, type = "s")
      
      #seq_years<-seq(min_year, max_year,1)
      
      n_yrs<-max_year -min_year
      done<-data.frame(country = cname, max_yr = max_year, min_yr = min_year, total_years = n_yrs, mov_avg=m.av, time = time_period)
      all_dat<-rbind(all_dat, done)
      #return(all_dat)
    }
  }
  
  fwrite(all_dat, "./Output_data/Moving_average_Prop__Aqua_Production.csv")
  
  
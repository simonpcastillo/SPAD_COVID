#' datacook - internal use
#' @description function for internal use
#' @source [Source]{https://github.com/DrFabach/Corona}
dataCook<- function(data, pop, countries){
  data$`Country/Region`<-as.character(data$`Country/Region`)
  data$`Country/Region`[data$`Country/Region`=="Macau"]<- "Macao"
  data$`Country/Region`[data$`Country/Region`=="Mainland China"]<- "China"
  data$`Country/Region`[data$`Country/Region`=="South Korea"]<- "South Korea"
  data$`Country/Region`[data$`Country/Region`=="North Macedonia"]<- "Macedonia"
  data$`Country/Region`[data$`Country/Region`=="Czech Republic"]<- "Czechia"
  data$`Country/Region`[data$`Country/Region`=="Dominican Republic"]<- "Dominican Rep."
  data$`Country/Region`[data$`Country/Region`=="UK"]<- "United Kingdom"
  data$`Country/Region`[data$`Country/Region`=="Gibraltar"]<- "United Kingdom"
  data$`Country/Region`[data$`Country/Region`=="US"]<- "United States"
  data$`Country/Region`[data$`Country/Region`=="Saint Barthelemy"]<- "St-Barthélemy"

  data$`Country/Region`[data$`Country/Region`=="Faroe Islands"]<- "Faeroe Is."
  data$`Country/Region`[data$`Country/Region`=="Bosnia and Herzegovina"]<- "Bosnia and Herz."
  data$`Country/Region`[data$`Country/Region`=="Vatican City"]<- "Vatican"
  data$`Country/Region`[data$`Country/Region`=="Korea, South"]<- "South Korea"
  data$`Country/Region`[data$`Country/Region`=="Republic of Ireland"]<- "Ireland"
  data$`Country/Region`[data$`Country/Region`=="Taiwan*"]<-"Taiwan"

  data$`Country/Region`[data$`Country/Region`=="Congo (Kinshasa)"]<-"Congo"
  data$`Country/Region`[data$`Country/Region`=="Cote d'Ivoire"]<-"Côte d'Ivoire"
  data$`Country/Region`[data$`Country/Region`=="Reunion"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Martinique"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="French Guiana"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Holy See"]<-"Vatican"
  data$`Country/Region`[data$`Country/Region`=="Cayman Islands"]<-"Cayman Is."
  data$`Country/Region`[data$`Country/Region`=="Guadeloupe"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Antigua and Barbuda"]<-"Antigua and Barb."

  data$`Country/Region`[data$`Country/Region`=="Curacao"]<-"Curaçao"
  data$`Country/Region`[data$`Country/Region`=="Guadeloupe"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="occupied Palestinian territory"]<-"Palestine"
  data$`Country/Region`[data$`Country/Region`=="Congo (Brazzaville)"]<-"Congo"
  data$`Country/Region`[data$`Country/Region`=="Equatorial Guinea"]<-"Guinea"
  data$`Country/Region`[data$`Country/Region`=="Central African Republic"]<-"Central African Rep."

  data$`Country/Region`[data$`Country/Region`=="South Sudan"]<-"S. Sudan"
  data$`Country/Region`[data$`Country/Region`=="Eswatini"]<-"eSwatini"
  data$`Country/Region`[data$`Country/Region`=="Western Sahara"]<-"W. Sahara"
  data$`Country/Region`[data$`Country/Region`=="Burma"]<-"Myanmar"
  data$`Country/Region`[data$`Country/Region`=="Sao Tome and Principe"]<-"São Tomé and Principe"
  data$`Country/Region`[data$`Country/Region`=="Saint Vincent and the Grenadines"]<-"St. Vin. and Gren."
  data$`Country/Region`[data$`Country/Region`=="Saint Kitts and Nevis"]<-"St. Kitts and Nevis"


  # countries$NAME<-as.character(countries$NAME)
  # countries$NAME[is.na(countries$NAME)]<-"Côte d'Ivoire"
  data$Pays<-as.character(unique(countries$NAME)[charmatch(data$`Country/Region`,unique(countries$NAME))])
  #print(data$`Country/Region`[is.na(data$Pays)])
  dataPays<- data%>%dplyr::select(-`Province/State`, -Lat, -Long,-`Country/Region`)%>%group_by(Pays)%>%summarise_each(sum)
  dataPays$Pays<-as.character(dataPays$Pays)

  return(dataPays)
}#ElFin

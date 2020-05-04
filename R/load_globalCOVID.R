#' Load global data
#' @author Simon P Castillo \email{spcastil@@uc.cl}.
#' @description  Load global data of SARS_CoV-2. This function obtain data from [CSSEGI-Johns Hopkins]{(https://github.com/CSSEGISandData/COVID-19} and it is built from other repos. [like this]{https://github.com/DrFabach/Corona}
#' @examples load_globalCOVID()
#' @return This function returns different objects to your Global Environment.
#' @return data_deaths: dataframe. The number of global deaths by day due to COVID.
#' @return data_positives: dataframe. The number of SARS-CoV-2(+).
#' @return data_recovered: dataframe. The number of recovered from SARS-CoV-2(+).
#' @return inc_matrix: dataframe. The global active cases by country and day.
#' @return propab_matrix:  dataframe The global active cases/country population by country and day.
#' @return inc_df: dataframe. A three column dataframe with country, date and the number of active cases.
#' @return propab_df: dataframe. A three column dataframe with country, date and the proportional abundance of active cases.
#' @return sumCases: numeric vector. The global sum of SARS-CoV-2(+) by day.
#' @return avCases: numeric vector. The global average of SARS-CoV-2(+) by day.
#'
#'
#'

load_globalCOVID <- function(){

   pacman::p_load(reshape2,tidyverse, rgdal, RCurl)

  #'%ni%' <- Negate('%in%')

  # World shapefile
  #download.file(url="https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
   #             destfile = "data/ne_50m_admin_0_countries.zip")
  #unzip(zipfile = "data/ne_50m_admin_0_countries.zip",exdir = "data" )
  #countries <- readOGR("data/ne_50m_admin_0_countries.shp",encoding = "utf-8",use_iconv = T, verbose = FALSE)

  #save(countries, file="data/shapeFile.RData")
  load("data/shapeFile.RData")

  # World population
  download.file(url = "https://raw.githubusercontent.com/DrFabach/Corona/master/pop.csv", destfile = "data/pop.csv")
  population<- read.csv2("data/pop.csv",stringsAsFactors = F)
  population$pays<-as.character(unique(countries$NAME)[charmatch(population$Country,unique(countries$NAME))])

  # World coronavirus records
  download.file ("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                 destfile = "data/confirmed.csv")
  download.file ("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                 destfile = "data/deaths.csv")
  download.file ("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                 destfile = "data/recovered.csv")
  confirmed <- read.csv("data/confirmed.csv", check.names = F)
  deaths <- read.csv("data/deaths.csv", check.names = F)
  recovered <- read.csv("data/recovered.csv", check.names = F)

  dataCases<- dataCook(deaths, population, countries) # correct country names
  po= population[complete.cases(population),]
  names(po) = c(names(po)[1:2], "Pays")
  po$Country= NULL
  df_d = merge(po, dataCases, by="Pays")
  data_deaths <<- df_d

  dataCases<- dataCook(confirmed, population, countries) # correct country names
  po= population[complete.cases(population),]
  names(po) = c(names(po)[1:2], "Pays")
  po$Country= NULL
  df_p = merge(po, dataCases, by="Pays")
  data_positives <<- df_p

  dataCases<- dataCook(recovered, population, countries) # correct country names
  po= population[complete.cases(population),]
  names(po) = c(names(po)[1:2], "Pays")
  po$Country= NULL
  df_r = merge(po, dataCases, by="Pays")
  data_recovered <<- df_r


  coles = ncol(df_p)
  #View(dataCases[dataCases$Pays %ni% df0$Pays,])
  Pays= df_r$Pays
  pop= df_r$Population
  d0 = df_p[,3:ncol(df_p)]-df_r[,3:ncol(df_r)]-df_d[,3:ncol(df_d)]
  df0 = data.frame(Pays= Pays, Population=pop, d0)
  names(df0) = names(df_p)
  for (i in 1:nrow(df0)) {
    popCountry = df0$Population[i]
    for(j in 3:(coles)){
      df0[i,(coles+j-2)] = df0[i,j] / popCountry

    }
  }

  df0b = df0[,c(1,3:coles)]  ### temporal series of number of infected
  Ncases = colSums(df0[,c(3:coles)])
  Mcases = colSums(df0[,c(3:coles)])/colSums(df0[,c(3:coles)]>0)

  df1 = df0[, c(1, (coles+1):ncol(df0))] ### temporal series of proportional abundance of infected
  names(df0b) = names(df1) = names(df0)[c(1,3:coles)]

  df2 = melt(df1, id="Pays") # proportional abundance
  names(df2) = c("Country", "Day", "propAb")
  df2$date = as.Date(df2$Day, "%m/%d/%y")

  df2b = melt(df0b, id="Pays") # raw abundance
  names(df2b) = c("Country", "Day", "Incidence")
  df2b$date = as.Date(df2b$Day, "%m/%d/%y")

  #breaksdate = seq(range(df2b$date)[1],range(df2b$date)[2], length.out = 5) #c(min(df2b$date),as.Date("2020-02-28"),as.Date("2020-02-29"),as.Date("2020-04-14"),max(df2b$date))#

  inc_matrix <<- df0b
  propab_matrix <<- df1
  inc_df <<- df2b
  propab_df <<- df2
  sumCases <<- Ncases
  avCases <<- Mcases

} #ElFin

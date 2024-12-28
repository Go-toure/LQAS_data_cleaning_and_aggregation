
setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/LQAS_raw/")
library(tidyverse)
library(dplyr)
library(sf)
library(flextable)
library(stringr)
library(stringi)
library(lubridate)
library(readxl)


# AB <- read_excel("ANG_SIA_LQAS.xlsx")
 AB <- read_csv("ANG_SIA_LQAS.csv")
 names(AB)
 AC<- AB |> 
   # for Ghana
   # mutate(Country = "GHA") |>
   drop_na(Country)
#for Zambia
#District = district,
 #for BWT

 #Region = District, District = district,
#remove rows with NA in x or y column
AC <- AC[!(is.na(AC$Country)) & !(is.na(AC$roundNumber)), ]
AD <- AC |>
  filter(Country !="n/a", `Response` !="n/a",`roundNumber` !="n/a", Count_HH_count !="n/a") |>
  select(`today`, `deviceid`, `Response`, `roundNumber`, `Country`, Region, District, `Date_of_LQAS`, `Count_HH_count`, `Count_HH[1]/Sex_Child`, `Count_HH[2]/Sex_Child`, `Count_HH[3]/Sex_Child`, `Count_HH[4]/Sex_Child`, `Count_HH[5]/Sex_Child`, `Count_HH[6]/Sex_Child`,  `Count_HH[7]/Sex_Child`, `Count_HH[8]/Sex_Child`, `Count_HH[9]/Sex_Child`, `Count_HH[10]/Sex_Child`, `Count_HH[1]/FM_Child`, `Count_HH[2]/FM_Child`, `Count_HH[3]/FM_Child`, `Count_HH[4]/FM_Child`, `Count_HH[5]/FM_Child`, `Count_HH[6]/FM_Child`, `Count_HH[7]/FM_Child`, `Count_HH[8]/FM_Child`, `Count_HH[9]/FM_Child`, `Count_HH[10]/FM_Child`,`Count_HH[1]/Reason_Not_FM`,`Count_HH[2]/Reason_Not_FM`,`Count_HH[3]/Reason_Not_FM`,`Count_HH[4]/Reason_Not_FM`,`Count_HH[5]/Reason_Not_FM`,`Count_HH[6]/Reason_Not_FM`,`Count_HH[7]/Reason_Not_FM`,`Count_HH[8]/Reason_Not_FM`,`Count_HH[9]/Reason_Not_FM`,`Count_HH[10]/Reason_Not_FM`,`Count_HH[1]/Reason_NC_NFM`,`Count_HH[2]/Reason_NC_NFM`,`Count_HH[3]/Reason_NC_NFM`,`Count_HH[4]/Reason_NC_NFM`,`Count_HH[5]/Reason_NC_NFM`,`Count_HH[6]/Reason_NC_NFM`,`Count_HH[7]/Reason_NC_NFM`,`Count_HH[8]/Reason_NC_NFM`,`Count_HH[9]/Reason_NC_NFM`,`Count_HH[10]/Reason_NC_NFM`,`Count_HH[1]/Reason_ABS_NFM`,`Count_HH[2]/Reason_ABS_NFM`,`Count_HH[3]/Reason_ABS_NFM`,`Count_HH[4]/Reason_ABS_NFM`,`Count_HH[5]/Reason_ABS_NFM`,`Count_HH[6]/Reason_ABS_NFM`,`Count_HH[7]/Reason_ABS_NFM`,`Count_HH[8]/Reason_ABS_NFM`,`Count_HH[9]/Reason_ABS_NFM`,`Count_HH[10]/Reason_ABS_NFM`,`Count_HH[1]/Care_Giver_Informed_SIA`,`Count_HH[2]/Care_Giver_Informed_SIA`,`Count_HH[3]/Care_Giver_Informed_SIA`,`Count_HH[4]/Care_Giver_Informed_SIA`,`Count_HH[5]/Care_Giver_Informed_SIA`,`Count_HH[6]/Care_Giver_Informed_SIA`,`Count_HH[7]/Care_Giver_Informed_SIA`,`Count_HH[8]/Care_Giver_Informed_SIA`,`Count_HH[9]/Care_Giver_Informed_SIA`,`Count_HH[10]/Care_Giver_Informed_SIA`, Cluster) 
AD$Cluster[!is.na(AD$Cluster)] <- 1
AD <- AD |> 
  mutate(Cluster = as.numeric(Cluster))
#Female sampled_structure
AD$`Count_HH[1]/Sex_Child`[AD$`Count_HH[1]/Sex_Child` == "F"] <- 1
AD$`Count_HH[2]/Sex_Child`[AD$`Count_HH[2]/Sex_Child` == "F"] <- 1
AD$`Count_HH[3]/Sex_Child`[AD$`Count_HH[3]/Sex_Child` == "F"] <- 1
AD$`Count_HH[4]/Sex_Child`[AD$`Count_HH[4]/Sex_Child` == "F"] <- 1
AD$`Count_HH[5]/Sex_Child`[AD$`Count_HH[5]/Sex_Child` == "F"] <- 1
AD$`Count_HH[6]/Sex_Child`[AD$`Count_HH[6]/Sex_Child` == "F"] <- 1
AD$`Count_HH[7]/Sex_Child`[AD$`Count_HH[7]/Sex_Child` == "F"] <- 1
AD$`Count_HH[8]/Sex_Child`[AD$`Count_HH[8]/Sex_Child` == "F"] <- 1
AD$`Count_HH[9]/Sex_Child`[AD$`Count_HH[9]/Sex_Child` == "F"] <- 1
AD$`Count_HH[10]/Sex_Child`[AD$`Count_HH[10]/Sex_Child` == "F"] <- 1
AD$`Count_HH[1]/Sex_Child`[AD$`Count_HH[1]/Sex_Child` == "M"] <- 0
AD$`Count_HH[2]/Sex_Child`[AD$`Count_HH[2]/Sex_Child` == "M"] <- 0
AD$`Count_HH[3]/Sex_Child`[AD$`Count_HH[3]/Sex_Child` == "M"] <- 0
AD$`Count_HH[4]/Sex_Child`[AD$`Count_HH[4]/Sex_Child` == "M"] <- 0
AD$`Count_HH[5]/Sex_Child`[AD$`Count_HH[5]/Sex_Child` == "M"] <- 0
AD$`Count_HH[6]/Sex_Child`[AD$`Count_HH[6]/Sex_Child` == "M"] <- 0
AD$`Count_HH[7]/Sex_Child`[AD$`Count_HH[7]/Sex_Child` == "M"] <- 0
AD$`Count_HH[8]/Sex_Child`[AD$`Count_HH[8]/Sex_Child` == "M"] <- 0
AD$`Count_HH[9]/Sex_Child`[AD$`Count_HH[9]/Sex_Child` == "M"] <- 0
AD$`Count_HH[10]/Sex_Child`[AD$`Count_HH[10]/Sex_Child` == "M"] <- 0

#CHIL VACCINATED
AD$`Count_HH[1]/FM_Child`[AD$`Count_HH[1]/FM_Child` == "Yes"] <- 1
AD$`Count_HH[1]/FM_Child`[AD$`Count_HH[1]/FM_Child` == "YES"] <- 1
AD$`Count_HH[1]/FM_Child`[AD$`Count_HH[1]/FM_Child` == "yes"] <- 1
AD$`Count_HH[1]/FM_Child`[AD$`Count_HH[1]/FM_Child` == "Y"] <- 1
AD$`Count_HH[2]/FM_Child`[AD$`Count_HH[2]/FM_Child` == "Yes"] <- 1
AD$`Count_HH[2]/FM_Child`[AD$`Count_HH[2]/FM_Child` == "YES"] <- 1
AD$`Count_HH[2]/FM_Child`[AD$`Count_HH[2]/FM_Child` == "yes"] <- 1
AD$`Count_HH[2]/FM_Child`[AD$`Count_HH[2]/FM_Child` == "Y"] <- 1
AD$`Count_HH[3]/FM_Child`[AD$`Count_HH[3]/FM_Child` == "Yes"] <- 1
AD$`Count_HH[3]/FM_Child`[AD$`Count_HH[3]/FM_Child` == "YES"] <- 1
AD$`Count_HH[3]/FM_Child`[AD$`Count_HH[3]/FM_Child` == "yes"] <- 1
AD$`Count_HH[3]/FM_Child`[AD$`Count_HH[3]/FM_Child` == "Y"] <- 1
AD$`Count_HH[4]/FM_Child`[AD$`Count_HH[4]/FM_Child` == "Yes"] <- 1
AD$`Count_HH[4]/FM_Child`[AD$`Count_HH[4]/FM_Child` == "YES"] <- 1
AD$`Count_HH[4]/FM_Child`[AD$`Count_HH[4]/FM_Child` == "yes"] <- 1
AD$`Count_HH[4]/FM_Child`[AD$`Count_HH[4]/FM_Child` == "Y"] <- 1
AD$`Count_HH[5]/FM_Child`[AD$`Count_HH[5]/FM_Child` == "Yes"] <- 1
AD$`Count_HH[5]/FM_Child`[AD$`Count_HH[5]/FM_Child` == "YES"] <- 1
AD$`Count_HH[5]/FM_Child`[AD$`Count_HH[5]/FM_Child` == "yes"] <- 1
AD$`Count_HH[5]/FM_Child`[AD$`Count_HH[5]/FM_Child` == "Y"] <- 1
AD$`Count_HH[6]/FM_Child`[AD$`Count_HH[6]/FM_Child` == "Yes"] <- 1
AD$`Count_HH[6]/FM_Child`[AD$`Count_HH[6]/FM_Child` == "YES"] <- 1
AD$`Count_HH[6]/FM_Child`[AD$`Count_HH[6]/FM_Child` == "yes"] <- 1
AD$`Count_HH[6]/FM_Child`[AD$`Count_HH[6]/FM_Child` == "Y"] <- 1
AD$`Count_HH[7]/FM_Child`[AD$`Count_HH[7]/FM_Child` == "Yes"] <- 1
AD$`Count_HH[7]/FM_Child`[AD$`Count_HH[7]/FM_Child` == "YES"] <- 1
AD$`Count_HH[7]/FM_Child`[AD$`Count_HH[7]/FM_Child` == "yes"] <- 1
AD$`Count_HH[7]/FM_Child`[AD$`Count_HH[7]/FM_Child` == "Y"] <- 1
AD$`Count_HH[8]/FM_Child`[AD$`Count_HH[8]/FM_Child` == "Yes"] <- 1
AD$`Count_HH[8]/FM_Child`[AD$`Count_HH[8]/FM_Child` == "YES"] <- 1
AD$`Count_HH[8]/FM_Child`[AD$`Count_HH[8]/FM_Child` == "yes"] <- 1
AD$`Count_HH[8]/FM_Child`[AD$`Count_HH[8]/FM_Child` == "Y"] <- 1
AD$`Count_HH[9]/FM_Child`[AD$`Count_HH[9]/FM_Child` == "Yes"] <- 1
AD$`Count_HH[9]/FM_Child`[AD$`Count_HH[9]/FM_Child` == "YES"] <- 1
AD$`Count_HH[9]/FM_Child`[AD$`Count_HH[9]/FM_Child` == "yes"] <- 1
AD$`Count_HH[9]/FM_Child`[AD$`Count_HH[9]/FM_Child` == "Y"] <- 1
AD$`Count_HH[10]/FM_Child`[AD$`Count_HH[10]/FM_Child` == "Yes"] <- 1
AD$`Count_HH[10]/FM_Child`[AD$`Count_HH[10]/FM_Child` == "YES"] <- 1
AD$`Count_HH[10]/FM_Child`[AD$`Count_HH[10]/FM_Child` == "yes"] <- 1
AD$`Count_HH[10]/FM_Child`[AD$`Count_HH[10]/FM_Child` == "Y"] <- 1
AD$`Count_HH[1]/FM_Child`[AD$`Count_HH[1]/FM_Child` == "NO"] <- 0
AD$`Count_HH[1]/FM_Child`[AD$`Count_HH[1]/FM_Child` == "No"] <- 0
AD$`Count_HH[1]/FM_Child`[AD$`Count_HH[1]/FM_Child` == "no"] <- 0
AD$`Count_HH[1]/FM_Child`[AD$`Count_HH[1]/FM_Child` == "N"] <- 0
AD$`Count_HH[2]/FM_Child`[AD$`Count_HH[2]/FM_Child` == "NO"] <- 0
AD$`Count_HH[2]/FM_Child`[AD$`Count_HH[2]/FM_Child` == "No"] <- 0
AD$`Count_HH[2]/FM_Child`[AD$`Count_HH[2]/FM_Child` == "no"] <- 0
AD$`Count_HH[2]/FM_Child`[AD$`Count_HH[2]/FM_Child` == "N"] <- 0
AD$`Count_HH[3]/FM_Child`[AD$`Count_HH[3]/FM_Child` == "NO"] <- 0
AD$`Count_HH[3]/FM_Child`[AD$`Count_HH[3]/FM_Child` == "No"] <- 0
AD$`Count_HH[3]/FM_Child`[AD$`Count_HH[3]/FM_Child` == "no"] <- 0
AD$`Count_HH[3]/FM_Child`[AD$`Count_HH[3]/FM_Child` == "N"] <- 0
AD$`Count_HH[4]/FM_Child`[AD$`Count_HH[4]/FM_Child` == "NO"] <- 0
AD$`Count_HH[4]/FM_Child`[AD$`Count_HH[4]/FM_Child` == "No"] <- 0
AD$`Count_HH[4]/FM_Child`[AD$`Count_HH[4]/FM_Child` == "no"] <- 0
AD$`Count_HH[4]/FM_Child`[AD$`Count_HH[4]/FM_Child` == "N"] <- 0
AD$`Count_HH[5]/FM_Child`[AD$`Count_HH[5]/FM_Child` == "NO"] <- 0
AD$`Count_HH[5]/FM_Child`[AD$`Count_HH[5]/FM_Child` == "No"] <- 0
AD$`Count_HH[5]/FM_Child`[AD$`Count_HH[5]/FM_Child` == "no"] <- 0
AD$`Count_HH[5]/FM_Child`[AD$`Count_HH[5]/FM_Child` == "N"] <- 0
AD$`Count_HH[6]/FM_Child`[AD$`Count_HH[6]/FM_Child` == "NO"] <- 0
AD$`Count_HH[6]/FM_Child`[AD$`Count_HH[6]/FM_Child` == "No"] <- 0
AD$`Count_HH[6]/FM_Child`[AD$`Count_HH[6]/FM_Child` == "no"] <- 0
AD$`Count_HH[6]/FM_Child`[AD$`Count_HH[6]/FM_Child` == "N"] <- 0
AD$`Count_HH[7]/FM_Child`[AD$`Count_HH[7]/FM_Child` == "NO"] <- 0
AD$`Count_HH[7]/FM_Child`[AD$`Count_HH[7]/FM_Child` == "No"] <- 0
AD$`Count_HH[7]/FM_Child`[AD$`Count_HH[7]/FM_Child` == "no"] <- 0
AD$`Count_HH[7]/FM_Child`[AD$`Count_HH[7]/FM_Child` == "N"] <- 0
AD$`Count_HH[8]/FM_Child`[AD$`Count_HH[8]/FM_Child` == "NO"] <- 0
AD$`Count_HH[8]/FM_Child`[AD$`Count_HH[8]/FM_Child` == "No"] <- 0
AD$`Count_HH[8]/FM_Child`[AD$`Count_HH[8]/FM_Child` == "no"] <- 0
AD$`Count_HH[8]/FM_Child`[AD$`Count_HH[8]/FM_Child` == "N"] <- 0
AD$`Count_HH[9]/FM_Child`[AD$`Count_HH[9]/FM_Child` == "NO"] <- 0
AD$`Count_HH[9]/FM_Child`[AD$`Count_HH[9]/FM_Child` == "No"] <- 0
AD$`Count_HH[9]/FM_Child`[AD$`Count_HH[9]/FM_Child` == "no"] <- 0
AD$`Count_HH[9]/FM_Child`[AD$`Count_HH[9]/FM_Child` == "N"] <- 0
AD$`Count_HH[10]/FM_Child`[AD$`Count_HH[10]/FM_Child` == "NO"] <- 0
AD$`Count_HH[10]/FM_Child`[AD$`Count_HH[10]/FM_Child` == "No"] <- 0
AD$`Count_HH[10]/FM_Child`[AD$`Count_HH[10]/FM_Child` == "no"] <- 0
AD$`Count_HH[10]/FM_Child`[AD$`Count_HH[10]/FM_Child` == "N"] <- 0
#Care_Giver_Informed_SIA
AD$`Count_HH[1]/Care_Giver_Informed_SIA`[AD$`Count_HH[1]/Care_Giver_Informed_SIA` == "Y"] <- 1
AD$`Count_HH[1]/Care_Giver_Informed_SIA`[AD$`Count_HH[1]/Care_Giver_Informed_SIA` == "1"] <- 1
AD$`Count_HH[1]/Care_Giver_Informed_SIA`[AD$`Count_HH[1]/Care_Giver_Informed_SIA` == "N"] <- 0
AD$`Count_HH[1]/Care_Giver_Informed_SIA`[AD$`Count_HH[1]/Care_Giver_Informed_SIA` == "0"] <- 0
AD$`Count_HH[2]/Care_Giver_Informed_SIA`[AD$`Count_HH[2]/Care_Giver_Informed_SIA` == "Y"] <- 1
AD$`Count_HH[2]/Care_Giver_Informed_SIA`[AD$`Count_HH[2]/Care_Giver_Informed_SIA` == "1"] <- 1
AD$`Count_HH[2]/Care_Giver_Informed_SIA`[AD$`Count_HH[2]/Care_Giver_Informed_SIA` == "N"] <- 0
AD$`Count_HH[2]/Care_Giver_Informed_SIA`[AD$`Count_HH[2]/Care_Giver_Informed_SIA` == "0"] <- 0
AD$`Count_HH[3]/Care_Giver_Informed_SIA`[AD$`Count_HH[3]/Care_Giver_Informed_SIA` == "Y"] <- 1
AD$`Count_HH[3]/Care_Giver_Informed_SIA`[AD$`Count_HH[3]/Care_Giver_Informed_SIA` == "1"] <- 1
AD$`Count_HH[3]/Care_Giver_Informed_SIA`[AD$`Count_HH[3]/Care_Giver_Informed_SIA` == "N"] <- 0
AD$`Count_HH[3]/Care_Giver_Informed_SIA`[AD$`Count_HH[3]/Care_Giver_Informed_SIA` == "0"] <- 0
AD$`Count_HH[4]/Care_Giver_Informed_SIA`[AD$`Count_HH[4]/Care_Giver_Informed_SIA` == "Y"] <- 1
AD$`Count_HH[4]/Care_Giver_Informed_SIA`[AD$`Count_HH[4]/Care_Giver_Informed_SIA` == "1"] <- 1
AD$`Count_HH[4]/Care_Giver_Informed_SIA`[AD$`Count_HH[4]/Care_Giver_Informed_SIA` == "N"] <- 0
AD$`Count_HH[4]/Care_Giver_Informed_SIA`[AD$`Count_HH[4]/Care_Giver_Informed_SIA` == "0"] <- 0
AD$`Count_HH[5]/Care_Giver_Informed_SIA`[AD$`Count_HH[5]/Care_Giver_Informed_SIA` == "Y"] <- 1
AD$`Count_HH[5]/Care_Giver_Informed_SIA`[AD$`Count_HH[5]/Care_Giver_Informed_SIA` == "1"] <- 1
AD$`Count_HH[5]/Care_Giver_Informed_SIA`[AD$`Count_HH[5]/Care_Giver_Informed_SIA` == "N"] <- 0
AD$`Count_HH[5]/Care_Giver_Informed_SIA`[AD$`Count_HH[5]/Care_Giver_Informed_SIA` == "0"] <- 0
AD$`Count_HH[6]/Care_Giver_Informed_SIA`[AD$`Count_HH[6]/Care_Giver_Informed_SIA` == "Y"] <- 1
AD$`Count_HH[6]/Care_Giver_Informed_SIA`[AD$`Count_HH[6]/Care_Giver_Informed_SIA` == "1"] <- 1
AD$`Count_HH[6]/Care_Giver_Informed_SIA`[AD$`Count_HH[6]/Care_Giver_Informed_SIA` == "N"] <- 0
AD$`Count_HH[6]/Care_Giver_Informed_SIA`[AD$`Count_HH[6]/Care_Giver_Informed_SIA` == "0"] <- 0
AD$`Count_HH[7]/Care_Giver_Informed_SIA`[AD$`Count_HH[7]/Care_Giver_Informed_SIA` == "Y"] <- 1
AD$`Count_HH[7]/Care_Giver_Informed_SIA`[AD$`Count_HH[7]/Care_Giver_Informed_SIA` == "1"] <- 1
AD$`Count_HH[7]/Care_Giver_Informed_SIA`[AD$`Count_HH[7]/Care_Giver_Informed_SIA` == "N"] <- 0
AD$`Count_HH[7]/Care_Giver_Informed_SIA`[AD$`Count_HH[7]/Care_Giver_Informed_SIA` == "0"] <- 0
AD$`Count_HH[8]/Care_Giver_Informed_SIA`[AD$`Count_HH[8]/Care_Giver_Informed_SIA` == "Y"] <- 1
AD$`Count_HH[8]/Care_Giver_Informed_SIA`[AD$`Count_HH[8]/Care_Giver_Informed_SIA` == "1"] <- 1
AD$`Count_HH[8]/Care_Giver_Informed_SIA`[AD$`Count_HH[8]/Care_Giver_Informed_SIA` == "N"] <- 0
AD$`Count_HH[8]/Care_Giver_Informed_SIA`[AD$`Count_HH[8]/Care_Giver_Informed_SIA` == "0"] <- 0
AD$`Count_HH[9]/Care_Giver_Informed_SIA`[AD$`Count_HH[9]/Care_Giver_Informed_SIA` == "Y"] <- 1
AD$`Count_HH[9]/Care_Giver_Informed_SIA`[AD$`Count_HH[9]/Care_Giver_Informed_SIA` == "1"] <- 1
AD$`Count_HH[9]/Care_Giver_Informed_SIA`[AD$`Count_HH[9]/Care_Giver_Informed_SIA` == "N"] <- 0
AD$`Count_HH[9]/Care_Giver_Informed_SIA`[AD$`Count_HH[9]/Care_Giver_Informed_SIA` == "0"] <- 0
AD$`Count_HH[10]/Care_Giver_Informed_SIA`[AD$`Count_HH[10]/Care_Giver_Informed_SIA` == "Y"] <- 1
AD$`Count_HH[10]/Care_Giver_Informed_SIA`[AD$`Count_HH[10]/Care_Giver_Informed_SIA` == "1"] <- 1
AD$`Count_HH[10]/Care_Giver_Informed_SIA`[AD$`Count_HH[10]/Care_Giver_Informed_SIA` == "N"] <- 0
AD$`Count_HH[10]/Care_Giver_Informed_SIA`[AD$`Count_HH[10]/Care_Giver_Informed_SIA` == "0"] <- 0

#Reason for not vaccinated structure
AC<-AD |> 
  mutate(
    R_House_not_visited1= case_when(
      `Count_HH[1]/Reason_Not_FM`=="House_not_visited"~1),
    R_House_not_visited2= case_when(
      `Count_HH[2]/Reason_Not_FM`=="House_not_visited"~1),
    R_House_not_visited3= case_when(
      `Count_HH[3]/Reason_Not_FM`=="House_not_visited"~1),
    R_House_not_visited4= case_when(
      `Count_HH[4]/Reason_Not_FM`=="House_not_visited"~1),
    R_House_not_visited5= case_when(
      `Count_HH[5]/Reason_Not_FM`=="House_not_visited"~1),
    R_House_not_visited6= case_when(
      `Count_HH[6]/Reason_Not_FM`=="House_not_visited"~1),
    R_House_not_visited7= case_when(
      `Count_HH[7]/Reason_Not_FM`=="House_not_visited"~1),
    R_House_not_visited8= case_when(
      `Count_HH[8]/Reason_Not_FM`=="House_not_visited"~1),
    R_House_not_visited9= case_when(
      `Count_HH[9]/Reason_Not_FM`=="House_not_visited"~1),
    R_House_not_visited10= case_when(
      `Count_HH[10]/Reason_Not_FM`=="House_not_visited"~1),
    R_childabsent1= case_when(
      `Count_HH[1]/Reason_Not_FM`=="childabsent"~1),
    R_childabsent2= case_when(
      `Count_HH[2]/Reason_Not_FM`=="childabsent"~1),
    R_childabsent3= case_when(
      `Count_HH[3]/Reason_Not_FM`=="childabsent"~1),
    R_childabsent4= case_when(
      `Count_HH[4]/Reason_Not_FM`=="childabsent"~1),
    R_childabsent5= case_when(
      `Count_HH[5]/Reason_Not_FM`=="childabsent"~1),
    R_childabsent6= case_when(
      `Count_HH[6]/Reason_Not_FM`=="childabsent"~1),
    R_childabsent7= case_when(
      `Count_HH[7]/Reason_Not_FM`=="childabsent"~1),
    R_childabsent8= case_when(
      `Count_HH[8]/Reason_Not_FM`=="childabsent"~1),
    R_childabsent9= case_when(
      `Count_HH[9]/Reason_Not_FM`=="childabsent"~1),
    R_childabsent10= case_when(
      `Count_HH[10]/Reason_Not_FM`=="childabsent"~1),
    R_Vaccinated_but_not_FM1= case_when(
      `Count_HH[1]/Reason_Not_FM`=="Vaccinated_but_not_FM"~1),
    R_Vaccinated_but_not_FM2= case_when(
      `Count_HH[2]/Reason_Not_FM`=="Vaccinated_but_not_FM"~1),
    R_Vaccinated_but_not_FM3= case_when(
      `Count_HH[3]/Reason_Not_FM`=="Vaccinated_but_not_FM"~1),
    R_Vaccinated_but_not_FM4= case_when(
      `Count_HH[4]/Reason_Not_FM`=="Vaccinated_but_not_FM"~1),
    R_Vaccinated_but_not_FM5= case_when(
      `Count_HH[5]/Reason_Not_FM`=="Vaccinated_but_not_FM"~1),
    R_Vaccinated_but_not_FM6= case_when(
      `Count_HH[6]/Reason_Not_FM`=="Vaccinated_but_not_FM"~1),
    R_Vaccinated_but_not_FM7= case_when(
      `Count_HH[7]/Reason_Not_FM`=="Vaccinated_but_not_FM"~1),
    R_Vaccinated_but_not_FM8= case_when(
      `Count_HH[8]/Reason_Not_FM`=="Vaccinated_but_not_FM"~1),
    R_Vaccinated_but_not_FM9= case_when(
      `Count_HH[9]/Reason_Not_FM`=="Vaccinated_but_not_FM"~1),
    R_Vaccinated_but_not_FM10= case_when(
      `Count_HH[10]/Reason_Not_FM`=="Vaccinated_but_not_FM"~1),
    R_Non_Compliance1= case_when(
      `Count_HH[1]/Reason_Not_FM`=="Non_Compliance"~1),
    R_Non_Compliance2= case_when(
      `Count_HH[2]/Reason_Not_FM`=="Non_Compliance"~1),
    R_Non_Compliance3= case_when(
      `Count_HH[3]/Reason_Not_FM`=="Non_Compliance"~1),
    R_Non_Compliance4= case_when(
      `Count_HH[4]/Reason_Not_FM`=="Non_Compliance"~1),
    R_Non_Compliance5= case_when(
      `Count_HH[5]/Reason_Not_FM`=="Non_Compliance"~1),
    R_Non_Compliance6= case_when(
      `Count_HH[6]/Reason_Not_FM`=="Non_Compliance"~1),
    R_Non_Compliance7= case_when(
      `Count_HH[7]/Reason_Not_FM`=="Non_Compliance"~1),
    R_Non_Compliance8= case_when(
      `Count_HH[8]/Reason_Not_FM`=="Non_Compliance"~1),
    R_Non_Compliance9= case_when(
      `Count_HH[9]/Reason_Not_FM`=="Non_Compliance"~1),
    R_Non_Compliance10= case_when(
      `Count_HH[10]/Reason_Not_FM`=="Non_Compliance"~1),
    R_Child_was_asleep1= case_when(
      `Count_HH[1]/Reason_Not_FM`=="Child_was_asleep"~1),
    R_Child_was_asleep2= case_when(
      `Count_HH[2]/Reason_Not_FM`=="Child_was_asleep"~1),
    R_Child_was_asleep3= case_when(
      `Count_HH[3]/Reason_Not_FM`=="Child_was_asleep"~1),
    R_Child_was_asleep4= case_when(
      `Count_HH[4]/Reason_Not_FM`=="Child_was_asleep"~1),
    R_Child_was_asleep5= case_when(
      `Count_HH[5]/Reason_Not_FM`=="Child_was_asleep"~1),
    R_Child_was_asleep6= case_when(
      `Count_HH[6]/Reason_Not_FM`=="Child_was_asleep"~1),
    R_Child_was_asleep7= case_when(
      `Count_HH[7]/Reason_Not_FM`=="Child_was_asleep"~1),
    R_Child_was_asleep8= case_when(
      `Count_HH[8]/Reason_Not_FM`=="Child_was_asleep"~1),
    R_Child_was_asleep9= case_when(
      `Count_HH[9]/Reason_Not_FM`=="Child_was_asleep"~1),
    R_Child_was_asleep10= case_when(
      `Count_HH[10]/Reason_Not_FM`=="Child_was_asleep"~1),
    R_Child_is_a_visitor1= case_when(
      `Count_HH[1]/Reason_Not_FM`=="Child_is_a_visitor"~1),
    R_Child_is_a_visitor2= case_when(
      `Count_HH[2]/Reason_Not_FM`=="Child_is_a_visitor"~1),
    R_Child_is_a_visitor3= case_when(
      `Count_HH[3]/Reason_Not_FM`=="Child_is_a_visitor"~1),
    R_Child_is_a_visitor4= case_when(
      `Count_HH[4]/Reason_Not_FM`=="Child_is_a_visitor"~1),
    R_Child_is_a_visitor5= case_when(
      `Count_HH[5]/Reason_Not_FM`=="Child_is_a_visitor"~1),
    R_Child_is_a_visitor6= case_when(
      `Count_HH[6]/Reason_Not_FM`=="Child_is_a_visitor"~1),
    R_Child_is_a_visitor7= case_when(
      `Count_HH[7]/Reason_Not_FM`=="Child_is_a_visitor"~1),
    R_Child_is_a_visitor8= case_when(
      `Count_HH[8]/Reason_Not_FM`=="Child_is_a_visitor"~1),
    R_Child_is_a_visitor9= case_when(
      `Count_HH[9]/Reason_Not_FM`=="Child_is_a_visitor"~1),
    R_Child_is_a_visitor10= case_when(
      `Count_HH[10]/Reason_Not_FM`=="Child_is_a_visitor"~1))
    
AC <- AC |> 
  select(c(3:29,60:130))
#female sampled
AE <- AC |>
  # replace(is.na("."), "0") |> 
  mutate(across((starts_with("Count_")),
                as.numeric))
AF <- AE |>
  mutate(
    female_sampled = rowSums(across(
      c("Count_HH[1]/Sex_Child":"Count_HH[10]/Sex_Child"))),
    male_sampled = Count_HH_count - female_sampled,
    total_vaccinated = rowSums(across(
      c("Count_HH[1]/FM_Child":"Count_HH[10]/FM_Child"))),
    missed_child = Count_HH_count - total_vaccinated)
AG <- AF |> 
  rowwise() |>
  mutate(FV1 =ifelse((`Count_HH[1]/Sex_Child` + `Count_HH[1]/FM_Child`) >=2, 1, 0 ),
         FV2 =ifelse((`Count_HH[2]/Sex_Child` + `Count_HH[2]/FM_Child`) >=2, 1, 0 ),
         FV3 =ifelse((`Count_HH[3]/Sex_Child` + `Count_HH[3]/FM_Child`) >=2, 1, 0 ),
         FV4 =ifelse((`Count_HH[4]/Sex_Child` + `Count_HH[4]/FM_Child`) >=2, 1, 0 ),
         FV5 =ifelse((`Count_HH[5]/Sex_Child` + `Count_HH[5]/FM_Child`) >=2, 1, 0 ),
         FV6 =ifelse((`Count_HH[6]/Sex_Child` + `Count_HH[6]/FM_Child`) >=2, 1, 0 ),
         FV7 =ifelse((`Count_HH[7]/Sex_Child` + `Count_HH[7]/FM_Child`) >=2, 1, 0 ),
         FV8 =ifelse((`Count_HH[8]/Sex_Child` + `Count_HH[8]/FM_Child`) >=2, 1, 0 ),
         FV9 =ifelse((`Count_HH[9]/Sex_Child` + `Count_HH[9]/FM_Child`) >=2, 1, 0 ),
         FV10 =ifelse((`Count_HH[10]/Sex_Child` + `Count_HH[10]/FM_Child`) >=2, 1, 0 ),
         female_vaccinated = rowSums(across(
           c("FV1":"FV10"))),
         male_vaccinated = total_vaccinated - female_vaccinated)
AN <- AG |> 
  mutate(across((starts_with("R_")),
                as.numeric))
AS <- AN |> 
  mutate(
    R_House_not_visited = sum(`R_House_not_visited1`,`R_House_not_visited2`,`R_House_not_visited3`,`R_House_not_visited4`,`R_House_not_visited5`,`R_House_not_visited6`,`R_House_not_visited7`,`R_House_not_visited8`,`R_House_not_visited9`,`R_House_not_visited10`, na.rm = TRUE), 
    R_childabsent = sum(`R_childabsent1`,`R_childabsent2`,`R_childabsent3`,`R_childabsent4`,`R_childabsent5`,`R_childabsent6`,`R_childabsent7`,`R_childabsent8`,`R_childabsent9`,`R_childabsent10`, na.rm = TRUE),
    R_Vaccinated_but_not_FM = sum(`R_Vaccinated_but_not_FM1`,`R_Vaccinated_but_not_FM2`,`R_Vaccinated_but_not_FM3`,`R_Vaccinated_but_not_FM4`,`R_Vaccinated_but_not_FM5`,`R_Vaccinated_but_not_FM6`,`R_Vaccinated_but_not_FM7`,`R_Vaccinated_but_not_FM8`,`R_Vaccinated_but_not_FM9`,`R_Vaccinated_but_not_FM10`, na.rm = TRUE),
    R_Non_Compliance = sum(`R_Non_Compliance1`,`R_Non_Compliance2`,`R_Non_Compliance3`,`R_Non_Compliance4`,`R_Non_Compliance5`,`R_Non_Compliance6`,`R_Non_Compliance7`,`R_Non_Compliance8`,`R_Non_Compliance9`,`R_Non_Compliance10`, na.rm = TRUE),
    R_Child_was_asleep = sum(`R_Child_was_asleep1`,`R_Child_was_asleep2`,`R_Child_was_asleep3`,`R_Child_was_asleep4`,`R_Child_was_asleep5`,`R_Child_was_asleep6`,`R_Child_was_asleep7`,`R_Child_was_asleep8`,`R_Child_was_asleep9`,`R_Child_was_asleep10`, na.rm = TRUE),
    R_Child_is_a_visitor = sum(`R_Child_is_a_visitor1`,`R_Child_is_a_visitor2`,`R_Child_is_a_visitor3`,`R_Child_is_a_visitor4`,`R_Child_is_a_visitor5`,`R_Child_is_a_visitor6`,`R_Child_is_a_visitor7`,`R_Child_is_a_visitor8`,`R_Child_is_a_visitor9`,`R_Child_is_a_visitor10`, na.rm = TRUE),
    Care_Giver_Informed_SIA = rowSums(across(
      c("Count_HH[1]/Care_Giver_Informed_SIA":"Count_HH[10]/Care_Giver_Informed_SIA")))) 
#summurise
AQ<-AS |> 
  select(Country,Region, District, Response, roundNumber, Date_of_LQAS, male_sampled,female_sampled,total_sampled =`Count_HH_count`, male_vaccinated, female_vaccinated, total_vaccinated, missed_child, R_Non_Compliance,R_House_not_visited,R_childabsent, R_Child_was_asleep,R_Child_is_a_visitor,R_Vaccinated_but_not_FM, Care_Giver_Informed_SIA,Cluster)
F1 <- AQ |> 
  group_by(Country, Region, District, Response, roundNumber) |>
  mutate(Date_of_LQAS = as_date(Date_of_LQAS)) |> 
  arrange(Date_of_LQAS) |> 
  mutate(Date_of_LQAS = as_date(Date_of_LQAS)) |> 
  mutate(date.diff = c(1, diff(Date_of_LQAS))) |> 
  mutate(period = cumsum(date.diff != 1)) |> 
  ungroup() |> 
  group_by(Country, Region, District, Response, roundNumber) |> 
  summarise(start_date = min(Date_of_LQAS),
            end_date = max(Date_of_LQAS),
            cluster = sum(Cluster),
            male_sampled = sum(male_sampled),
            female_sampled = sum(female_sampled),
            total_sampled = sum(total_sampled),
            male_vaccinated = sum(male_vaccinated),
            female_vaccinated = sum(female_vaccinated),
            total_vaccinated = sum(total_vaccinated),
            missed_child = sum(missed_child),
            r_Non_Compliance = sum(R_Non_Compliance),
            r_House_not_visited = sum(R_House_not_visited),
            r_childabsent = sum(R_childabsent),
            r_Child_was_asleep = sum(R_Child_was_asleep),
            r_Child_is_a_visitor = sum(R_Child_is_a_visitor),
            r_Vaccinated_but_not_FM = sum(R_Vaccinated_but_not_FM),
            Care_Giver_Informed_SIA = sum(Care_Giver_Informed_SIA),
            percent_care_Giver_Informed_SIA = Care_Giver_Informed_SIA/total_sampled)
F2 <- F1 |> 
  filter(start_date > as_date(2019-10-01)) |> 
  mutate(
    percent_care_Giver_Informed_SIA = round(percent_care_Giver_Informed_SIA, 2),
    percent_care_Giver_Informed_SIA = percent_care_Giver_Informed_SIA*100,
    total_missed = ifelse(total_sampled<60, ((60-total_sampled) + missed_child), missed_child)) |> 
  filter(cluster >= 3) |> 
  mutate(Status = case_when(
    total_missed<=3~"Pass",total_missed>3~"Fail"),
    roundNumber = case_when(
      roundNumber == "RND 1" ~ "Rnd1",
      roundNumber == "RND 2" ~ "Rnd2",
      roundNumber == "RND 3" ~ "Rnd3",
      roundNumber =="RND2" ~ "Rnd2",
      roundNumber =="RND1" ~ "Rnd1",
      roundNumber =="RND3" ~ "Rnd3",
      TRUE ~ roundNumber))|> 
  mutate(Performance = case_when(
    total_missed<4~"High",
    total_missed>=4 & total_missed<9~"Moderate",
    total_missed>=9 & total_missed<20~"Poor",
    total_missed>=20~"Very_poor"))


F3<-F2 |> 
  dplyr::mutate(Vaccine.type = case_when(
    str_detect(Response, pattern = "BITTOU") ~ "mOPV",
    str_detect(Response, pattern = "-mOPV2") ~ "mOPV",
    str_detect(Response, pattern = "-mOPV") ~ "mOPV",
    str_detect(Response, pattern = "MENAKA-mOPV2") ~ "mOPV",
    str_detect(Response, pattern = "BAMAKO-mOPV2") ~ "mOPV",
    str_detect(Response, pattern = "KANKAN-mOPV") ~ "mOPV",
    str_detect(Response, pattern = "MLI-12DS-01-2021-mOPV2") ~ "mOPV",
    str_detect(Response, pattern = "CONAKRY-mOPV") ~ "mOPV",
    str_detect(Response, pattern = "Ouagadogou") ~ "mOPV",
    str_detect(Response, pattern = "nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "bOPV") ~ "bOPV",
    str_detect(Response, pattern = "WPV1") ~ "bOPV",
    str_detect(Response, pattern = "VPOn") ~ "nOPV2",
    str_detect(Response, pattern = "TSHUAPA") ~ "nOPV2",
    str_detect(Response, pattern = "VPOb") ~ "bOPV",
    str_detect(Response, pattern = "Tanganyika") ~ "nOPV2",
    str_detect(Response, pattern = "Bangui 1") ~ "mOPV",
    str_detect(Response, pattern = "nVPO") ~ "nOPV",
    str_detect(Response, pattern = "GOTHEY") ~ "mOPV",
    str_detect(Response, pattern = "OPV") ~ "bOPV",
    str_detect(Response, pattern = "Liberia") ~ "nOPV2",
    str_detect(Response, pattern = "Mauritania") ~ "nOPV2",
    str_detect(Response, pattern = "YOPOUGON") ~ "mOPV",
    str_detect(Response, pattern = "Golfe") ~ "mOPV",
    str_detect(Response, pattern = "Kankan") ~ "nOPV2",
    str_detect(Response, pattern = "KOUIBLY") ~ "nOPV2",
    str_detect(Response, pattern = "Sierra Leone") ~ "nOPV2",
    str_detect(Response, pattern = "SEN") ~ "nOPV2",
    str_detect(Response, pattern = "CEN") ~ "nOPV2",
    str_detect(Response, pattern = "MAL") ~ "nOPV2",
    str_detect(Response, pattern = "BEN-26DS-08-2020") ~ "nOPV2",
    str_detect(Response, pattern = "BEN-39DS-01-2021") ~ "nOPV2",
    str_detect(Response, pattern = "BEN-39DS-01-2021") ~ "nOPV2",
    str_detect(Response, pattern = "BEN-xxDS-02-2020") ~ "mOPV2",
    str_detect(Response, pattern = "BERTOUA") ~ "nOPV2",
    str_detect(Response, pattern = "EBOLOWA") ~ "nOPV2",
    str_detect(Response, pattern = "EXNORD") ~ "nOPV2",
    str_detect(Response, pattern = "ExtNord2023") ~ "nOPV2",
    str_detect(Response, pattern = "NID_LID_preventive") ~ "bOPV",
    str_detect(Response, pattern = "ExtNord2023") ~ "nOPV2",
    str_detect(Response, pattern = "ADDIS ABABA") ~ "nOPV2",
    str_detect(Response, pattern = "Mekelle") ~ "nOPV2",
    str_detect(Response, pattern = "AMANSIE SOUTH") ~ "nOPV2",
    str_detect(Response, pattern = "Bangui 1") ~ "nOPV2",
    str_detect(Response, pattern = "CAF-2020-002") ~ "nOPV2",
    str_detect(Response, pattern = "CENBLOCK") ~ "nOPV2",
    str_detect(Response, pattern = "CENTRALBLK") ~ "nOPV2",
    str_detect(Response, pattern = "CHA-17DS-02-2020") ~ "nOPV2",
    str_detect(Response, pattern = "DONOMANGA") ~ "nOPV2",
    str_detect(Response, pattern = "GNBnOPV") ~ "nOPV2",
    str_detect(Response, pattern = "GOLFE") ~ "nOPV2",
    str_detect(Response, pattern = "GOTHEYE") ~ "nOPV2",
    str_detect(Response, pattern = "KEN-13DS-02-2021") ~ "nOPV2",
    str_detect(Response, pattern = "Liberia") ~ "nOPV2",
    str_detect(Response, pattern = "MopUp2022") ~ "nOPV2",
    str_detect(Response, pattern = "SSD-79DS-09-2020") ~ "nOPV2",
    str_detect(Response, pattern = "WPV1MLW") ~ "bOPV",
    str_detect(Response, pattern = "WPV1Response") ~ "bOPV",
    str_detect(Response, pattern = "Liberia") ~ "nOPV2",
    str_detect(Response, pattern = "ALG-2023-09-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "ALG-2024-01-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "nOPV2022") ~ "nOPV2",
    str_detect(Response, pattern = "BEN-2023-09-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "BFA-2023-05-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "BFA-2023-09-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "BFA-2024-02-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "BITTOU-mOPV2") ~ "mOPV",
    str_detect(Response, pattern = "Ouagadogou-mOPV2") ~ "mOPV",
    str_detect(Response, pattern = "BOT-2023-02-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "CAM-2023-05-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "CAM-2023-08-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "CAM-2024-02-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "MDG-2023-03-01_bOPV") ~ "mOPV",
    str_detect(Response, pattern = "MENAKA-mOPV2") ~ "nOPV2",
    str_detect(Response, pattern = "MLI-12DS-01-2021-mOPV2") ~ "mOPV",
    str_detect(Response, pattern = "nOPV2022") ~ "nOPV2",
    str_detect(Response, pattern = "nOPV2023") ~ "nOPV2",
    str_detect(Response, pattern = "nVPO") ~ "nOPV2",
    str_detect(Response, pattern = "nVPO_Maradi") ~ "nOPV2",
    str_detect(Response, pattern = "nVPO_Zinder") ~ "nOPV2",
    str_detect(Response, pattern = "nVPO2") ~ "nOPV2",
    str_detect(Response, pattern = "OPVb May2021") ~ "nOPV2",
    str_detect(Response, pattern = "OPVb2021") ~ "nOPV2",
    str_detect(Response, pattern = "OPVb2022") ~ "nOPV2",
    str_detect(Response, pattern = "RSSmOPV10C2021") ~ "nOPV2",
    str_detect(Response, pattern = "SEN_VPOn") ~ "nOPV2",
    str_detect(Response, pattern = "UGAnOPV") ~ "nOPV2",
    str_detect(Response, pattern = "VPOb") ~ "nOPV2",
    str_detect(Response, pattern = "nOPV2022") ~ "nOPV2",
    str_detect(Response, pattern = "nVPO2") ~ "nOPV2",
    str_detect(Response, pattern = "VPOb13ProV") ~ "nOPV2",
    str_detect(Response, pattern = "Liberia") ~ "nOPV2"))

FI<-F3 |> 
  mutate(
    Response = case_when(
      Response=="CENTRALBLK"~ "DRC-7DS-02-2022",
      Response=="nOPV2022"~ "DRC-39DS-01-2021",
      Response=="Tshuapa"~ "DRC-23DS-12-2020",
      Response=="TSHUAPA"~ "DRC-23DS-12-2020",
      Response=="VPOb13ProV"~ "DRC-39DS-01-2021",
      TRUE ~ Response
    )
  )

F4<-FI |> 
  mutate(
    start_date = as_date(start_date),
    start_date = case_when(
      Response	=="ALG-2024-01-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-01-31"),
      Response	=="Algeria Outbreak"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-12-07"),
      Response	=="Algeria Outbreak"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-01-17"),
      Response	=="Algeria Outbreak"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2023-03-15"),
      Response	=="ANG-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-09-11"),
      Response	=="ANG-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-10-16"),
      Response	=="ANGOLA-NID-2024-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-20"),
      Response	=="ANGOLA-NID-2024-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-09-09"),
      Response	=="ANGOLA-SNID-2024-07-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-07-01"),
      Response	=="BEN-2023-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-07-24"),
      Response	=="BEN-2023-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-08-28"),
      Response	=="BEN-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-02-05"),
      Response	=="BEN-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-06-10"),
      Response	=="BEN-26DS-08-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-10-12"),
      Response	=="BEN-26DS-08-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2020-10-26"),
      Response	=="BEN-39DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-05-10"),
      Response	=="BEN-39DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-05-31"),
      Response	=="BEN-39DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2021-09-06"),
      Response	=="BEN-86DS-08-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-10-17"),
      Response	=="BFA-10DS-05-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-11-16"),
      Response	=="BFA-10DS-05-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2020-12-06"),
      Response	=="BFA-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-11-28"),
      Response	=="BFA-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-07-17"),
      Response	=="BFA-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd0" ~	as_date("2023-09-25"),
      Response	=="BFA-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-10-30"),
      Response	=="BFA-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-11-27"),
      Response	=="BFA-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-13"),
      Response	=="BFA-2024-08-sNID_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-09-09"),
      Response	=="BFA-22DS-10-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-12-06"),
      Response	=="BFA-22DS-10-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2020-12-21"),
      Response	=="BOT-2023-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-02-26"),
      Response	=="BOT-2023-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-03-30"),
      Response	=="Burkina Faso 2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-11-28"),
      Response	=="Burkina Faso 2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-12-19"),
      Response	=="BUU-2023-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-06-13"),
      Response	=="BUU-2023-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-08-20"),
      Response	=="BUU-2023-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2023-10-29"),
      Response	=="CAF-2020-002"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-11-16"),
      Response	=="CAF-2020-002"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2020-12-07"),
      Response	=="CAM-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-05-29"),
      Response	=="CAM-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-06-26"),
      Response	=="CAM-2023-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-09-25"),
      Response	=="CAM-2023-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-11-06"),
      Response	=="CAM-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-04"),
      Response	=="CAR-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-06-05"),
      Response	=="CAR-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-09-10"),
      Response	=="CAR-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-09"),
      Response	=="CAR-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-04-15"),
      Response	=="CAR-2024-sNIDs-09_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-09"),
      Response	=="CAR-35DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-06-06"),
      Response	=="CAR-35DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-08-07"),
      Response	=="CHA-91DS-11-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-11-16"),
      Response	=="CHA-91DS-11-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2020-11-30"),
      Response	=="Chad_2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-01-29"),
      Response	=="CHAD-DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-05-16"),
      Response	=="CHAD-DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-06-13"),
      Response	=="CHD-2023-05-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-05-29"),
      Response	=="CHD-2023-05-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-06-19"),
      Response	=="CHD-2023-10-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-11-13"),
      Response	=="CHD-2023-10-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-12-16"),
      Response	=="CHD-2023-10-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2024-05-20"),
      Response	=="CIV-2023-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd0" ~	as_date("2023-07-17"),
      Response	=="CIV-2023-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-09-25"),
      Response	=="CIV-2023-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-10-30"),
      Response	=="CIV-2024-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-13"),
      Response	=="CIV-2024-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-09-09"),
      Response	=="CIV-72DS-05-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-06-20"),
      Response	=="CIV-72DS-05-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-09-26"),
      Response	=="CMR-16DS-01-2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd0" ~	as_date("2023-01-30"),
      Response	=="CMR-31DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-05-16"),
      Response	=="CMR-31DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-07-04"),
      Response	=="CMR-31DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2022-11-07"),
      Response	=="CNG-2023-05-bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-06-12"),
      Response	=="CNG-2023-05-bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-08-28"),
      Response	=="CNG-2023-10-nOPV_2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-12-11"),
      Response	=="CNG-2023-10-nOPV_2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-04-01"),
      Response	=="CNG-2024-08-BOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2024-09-30"),
      Response	=="CON-52DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-05-30"),
      Response	=="CON-52DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-08-09"),
      Response	=="DRC-11Prov-03-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-11-13"),
      Response	=="DRC-2023-03-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-03-19"),
      Response	=="DRC-2023-03-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-04-02"),
      Response	=="DRC-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-06-04"),
      Response	=="DRC-2023-09-KIN_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-09-25"),
      Response	=="DRC-2023-11-02_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-11-19"),
      Response	=="DRC-2024-10-01_B_nOPV"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-13"),
      Response	=="DRC-39DS-01-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-03-28"),
      Response	=="DRC-39DS-01-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-04-11"),
      Response	=="DRC-7DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-05-29"),
      Response	=="DRC-7DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-07-29"),
      Response	=="DRC-NID-03-2024-B_nOPV"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-31"),
      Response	=="DRC-NID-04-2024-bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2024-06-16"),
      Response	=="DRC-NID-07-2023-bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-07-30"),
      Response	=="DRC-NID-07-2023-bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-09-25"),
      Response	=="DRC-sNID-01-2024_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2024-02-05"),
      Response	=="DRC-sNID-08-2024-B_nOPV"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-08-11"),
      Response	=="DRC-sNID-09-2024_bOPV"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-09-22"),
      Response	=="ETH-2021-002-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-11-01"),
      Response	=="ETH-2021-002-2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-11-01"),
      Response	=="ETH-2021-002-3"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-04-04"),
      Response	=="ETH-2021-002-4"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-11-15"),
      Response	=="ETH-2024-03-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-04-29"),
      Response	=="ETH-2024-03-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-06-17"),
      Response	=="ETH-2024-08-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-13"),
      Response	=="GAM-65DS-06-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-11-23"),
      Response	=="GAM-65DS-06-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-03-22"),
      Response	=="GHA-2024-10-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-20"),
      Response	=="GHN-260-DS-07-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-09-04"),
      Response	=="GHN-260-DS-07-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-10-09"),
      Response	=="GHN-260-DS-07-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2022-12-18"),
      Response	=="GNB-117DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-04-30"),
      Response	=="GNB-117DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-06-25"),
      Response	=="GUE-20DS-02-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-03-01"),
      Response	=="GUE-20DS-02-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-06-07"),
      Response	=="GUI-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd0" ~	as_date("2023-09-11"),
      Response	=="GUI-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-10-30"),
      Response	=="GUI-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-05-13"),
      Response	=="GUI-2024-06-LIDs_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-09-09"),
      Response	=="KEN-13DS-02-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-05-25"),
      Response	=="KEN-13DS-02-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-07-20"),
      Response	=="KEN-2023-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-08-27"),
      Response	=="KEN-2023-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-10-10"),
      Response	=="KEN-2023-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2023-11-14"),
      Response	=="KEN-2024-01-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-01-30"),
      Response	=="KEN-2024-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-05"),
      Response	=="Kwara Response"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-07-05"),
      Response	=="Kwara Response"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-08-02"),
      Response	=="LBR-15DS-10-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-03-29"),
      Response	=="LBR-15DS-10-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-05-31"),
      Response	=="LBR-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-13"),
      Response	=="LBR-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-06-10"),
      Response	=="LBR-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2024-09-30"),
      Response	=="Madagascar Nationwide response cVDPV1"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-07-02"),
      Response	=="MAI-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-06-19"),
      Response	=="MAI-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-08-21"),
      Response	=="MAI-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-11-06"),
      Response	=="MAI-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-12-21"),
      Response	=="MAI-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-13"),
      Response	=="MAI-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-09-30"),
      Response	=="Malawi-WPV1"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-03-24"),
      Response	=="Malawi-WPV1"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2022-04-28"),
      Response	=="Malawi-WPV1"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2022-08-14"),
      Response	=="Malawi-WPV1"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd4" ~	as_date("2022-10-16"),
      Response	=="MAU_57DS_08-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-12-20"),
      Response	=="MAU_57DS_08-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-04-01"),
      Response	=="MAU_57DS_08-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2022-07-06"),
      Response	=="MAU-2024-01-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-31"),
      Response	=="MAU-2024-01-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-05-26"),
      Response	=="MDG-2023-03-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-05-19"),
      Response	=="MDG-2023-03-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-07-28"),
      Response	=="MDG-2023-03-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2023-09-08"),
      Response	=="MDG-2023-03-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd4" ~	as_date("2023-10-19"),
      Response	=="MDG-2024-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2024-05-17"),
      Response	=="MDG-2024-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2024-07-12"),
      Response	=="MLI-12DS-01-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-03-08"),
      Response	=="MLI-12DS-01-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-03-22"),
      Response	=="MLW-2023-04-1_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-05-18"),
      Response	=="MLW-2023-04-1_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-07-16"),
      Response	=="MLW-2023-04-1_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2023-09-16"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-03-24"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-04-24"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2022-07-10"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd4" ~	as_date("2022-08-21"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd5" ~	as_date("2022-10-16"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd6" ~	as_date("2022-12-10"),
      Response	=="MOZ-2023-03-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-04-15"),
      Response	=="MOZ-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-06-18"),
      Response	=="MOZ-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-09-18"),
      Response	=="MOZ-2024-04-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2024-05-31"),
      Response	=="MOZ-2024-04-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2024-08-02"),
      Response	=="NER-14DS-09-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-11-18"),
      Response	=="NER-14DS-09-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-12-16"),
      Response	=="NGA-2021-011-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-04-13"),
      Response	=="NGA-2021-011-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-05-25"),
      Response	=="NGA-2021-013-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-03-16"),
      Response	=="NGA-2021-013-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-04-13"),
      Response	=="NGA-2021-014-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-06-29"),
      Response	=="NGA-2021-014-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-08-03"),
      Response	=="NGA-2021-016-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-06-22"),
      Response	=="NGA-2021-016-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-07-20"),
      Response	=="NGA-2021-019"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-08-10"),
      Response	=="NGA-2021-020-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-09-14"),
      Response	=="NGA-2021-020-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-10-12"),
      Response	=="NGA-2021-020-2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-09-07"),
      Response	=="NGA-2021-020-2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-10-19"),
      Response	=="NGA-2021-020-3"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-10-12"),
      Response	=="NGA-2021-020-3"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-11-02"),
      Response	=="NGA-2021-020-4"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-09-21"),
      Response	=="NGA-2021-020-4"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-10-19"),
      Response	=="NIE-2023-04-02_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-05-14"),
      Response	=="NIE-2023-04-02_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-06-20"),
      Response	=="NIE-2023-07-03_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-07-25"),
      Response	=="NIE-2023-07-03_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-10-03"),
      Response	=="NIE-2023-07-03_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2023-11-07"),
      Response	=="NIE-2023-07-03_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd4" ~	as_date("2023-12-17"),
      Response	=="NIE-2023-10-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-10-03"),
      Response	=="NIE-2023-10-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-11-21"),
      Response	=="NIE-2024-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-05"),
      Response	=="NIE-2024-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-04-23"),
      Response	=="NIE-2024-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2024-10-01"),
      Response	=="NIG-13DS-03-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-06-14"),
      Response	=="NIG-13DS-03-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-07-14"),
      Response	=="NIG-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-05-29"),
      Response	=="NIG-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-06-22"),
      Response	=="NIG-2023-11-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-12-08"),
      Response	=="NIG-2023-11-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-01-08"),
      Response	=="NIG-2024-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-07-15"),
      Response	=="NIG-2024-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-09-30"),
      Response	=="NIG-72DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-06-05"),
      Response	=="NIG-72DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-07-02"),
      Response	=="NIG-72DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2022-10-31"),
      Response	=="NIG-9DS-06-2021_Zinder"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-08-30"),
      Response	=="NIG-9DS-06-2021_Zinder"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-09-20"),
      Response	=="Preventive campaign bOPV - Guinea"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2021-10-25"),
      Response	=="Preventive campaign bOPV- CAR"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2021-10-25"),
      Response	=="Preventive campaign bOPV -CHAD"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2021-11-01"),
      Response	=="Preventive campaign bOPV -CHAD"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2021-11-22"),
      Response	=="RWA-2023-07-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-07-27"),
      Response	=="RWA-2023-07-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-09-14"),
      Response	=="SEN-79DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-12-20"),
      Response	=="SEN-79DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-02-27"),
      Response	=="SEN-79DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2022-08-13"),
      Response	=="SLE-16DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-05-31"),
      Response	=="SLE-16DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-07-05"),
      Response	=="SLE-16DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2021-08-30"),
      Response	=="SLE-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-13"),
      Response	=="SLE-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-06-10"),
      Response	=="SLE-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2024-09-30"),
      Response	=="SSD-2024-02-01_nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-01"),
      Response	=="SSD-2024-02-01_nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-04-19"),
      Response	=="SSD-79DS-09-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-11-13"),
      Response	=="SSD-79DS-09-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-02-19"),
      Response	=="Tanganyika"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-01-29"),
      Response	=="TNZ-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-09-24"),
      Response	=="TNZ-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-11-05"),
      Response	=="TOG-39DS-06-022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-08-18"),
      Response	=="TOG-39DS-06-022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-10-16"),
      Response	=="TZA...DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-03-27"),
      Response	=="TZA...DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2022-05-21"),
      Response	=="TZA...DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2022-09-04"),
      Response	=="TZA...DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd4" ~	as_date("2022-12-04"),
      Response	=="UGA-149DS-08-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-01-17"),
      Response	=="UGA-149DS-08-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-10-31"),
      Response	=="UGA-2024-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-05"),
      Response	=="Uganda_2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-11-07"),
      Response	=="Uganda_2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-01-30"),
      Response	=="ZAM-2023-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-02-19"),
      Response	=="ZAM-2023-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-04-23"),
      Response	=="ZAM-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-06-22"),
      Response	=="ZAM-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-09-03"),
      Response	=="ZAM-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-10-29"),
      Response	=="ZAM-2024-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-07-28"),
      Response	=="ZIM-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-05-26"),
      Response	=="ZIM-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-10-13"),
      Response	=="ZIM-2023-12-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-02-23"),
      Response	=="ZIM-2023-12-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-03-22"),
      Response	=="ZMB-30DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-03-24"),
      Response	=="ZMB-30DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2022-04-24"),
      Response	=="ZMB-30DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2022-08-14"),
      Response	=="ZMB-30DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd4" ~	as_date("2022-10-30"),
      Response	=="ZWE-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-10-30"),
      TRUE ~ start_date) ,
    end_date = case_when(
      Response	=="ALG-2024-01-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-01-31"),
      Response	=="Algeria Outbreak"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-12-07"),
      Response	=="Algeria Outbreak"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-01-17"),
      Response	=="Algeria Outbreak"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2023-03-15"),
      Response	=="ANG-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-09-11"),
      Response	=="ANG-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-10-16"),
      Response	=="ANGOLA-NID-2024-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-20"),
      Response	=="ANGOLA-NID-2024-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-09-09"),
      Response	=="ANGOLA-SNID-2024-07-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-07-01"),
      Response	=="BEN-2023-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-07-24"),
      Response	=="BEN-2023-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-08-28"),
      Response	=="BEN-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-02-05"),
      Response	=="BEN-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-06-10"),
      Response	=="BEN-26DS-08-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-10-12"),
      Response	=="BEN-26DS-08-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2020-10-26"),
      Response	=="BEN-39DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-05-10"),
      Response	=="BEN-39DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-05-31"),
      Response	=="BEN-39DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2021-09-06"),
      Response	=="BEN-86DS-08-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-10-17"),
      Response	=="BFA-10DS-05-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-11-16"),
      Response	=="BFA-10DS-05-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2020-12-06"),
      Response	=="BFA-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-11-28"),
      Response	=="BFA-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-07-17"),
      Response	=="BFA-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd0" ~	as_date("2023-09-25"),
      Response	=="BFA-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-10-30"),
      Response	=="BFA-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-11-27"),
      Response	=="BFA-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-13"),
      Response	=="BFA-2024-08-sNID_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-09-09"),
      Response	=="BFA-22DS-10-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-12-06"),
      Response	=="BFA-22DS-10-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2020-12-21"),
      Response	=="BOT-2023-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-02-26"),
      Response	=="BOT-2023-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-03-30"),
      Response	=="Burkina Faso 2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-11-28"),
      Response	=="Burkina Faso 2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-12-19"),
      Response	=="BUU-2023-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-06-13"),
      Response	=="BUU-2023-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-08-20"),
      Response	=="BUU-2023-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2023-10-29"),
      Response	=="CAF-2020-002"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-11-16"),
      Response	=="CAF-2020-002"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2020-12-07"),
      Response	=="CAM-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-05-29"),
      Response	=="CAM-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-06-26"),
      Response	=="CAM-2023-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-09-25"),
      Response	=="CAM-2023-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-11-06"),
      Response	=="CAM-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-04"),
      Response	=="CAR-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-06-05"),
      Response	=="CAR-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-09-10"),
      Response	=="CAR-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-09"),
      Response	=="CAR-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-04-15"),
      Response	=="CAR-2024-sNIDs-09_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-09"),
      Response	=="CAR-35DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-06-06"),
      Response	=="CAR-35DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-08-07"),
      Response	=="CHA-91DS-11-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-11-16"),
      Response	=="CHA-91DS-11-2020"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2020-11-30"),
      Response	=="Chad_2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-01-29"),
      Response	=="CHAD-DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-05-16"),
      Response	=="CHAD-DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-06-13"),
      Response	=="CHD-2023-05-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-05-29"),
      Response	=="CHD-2023-05-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-06-19"),
      Response	=="CHD-2023-10-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-11-13"),
      Response	=="CHD-2023-10-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-12-16"),
      Response	=="CHD-2023-10-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2024-05-20"),
      Response	=="CIV-2023-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd0" ~	as_date("2023-07-17"),
      Response	=="CIV-2023-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-09-25"),
      Response	=="CIV-2023-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-10-30"),
      Response	=="CIV-2024-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-13"),
      Response	=="CIV-2024-04-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-09-09"),
      Response	=="CIV-72DS-05-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-06-20"),
      Response	=="CIV-72DS-05-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-09-26"),
      Response	=="CMR-16DS-01-2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd0" ~	as_date("2023-01-30"),
      Response	=="CMR-31DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-05-16"),
      Response	=="CMR-31DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-07-04"),
      Response	=="CMR-31DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2022-11-07"),
      Response	=="CNG-2023-05-bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-06-12"),
      Response	=="CNG-2023-05-bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-08-28"),
      Response	=="CNG-2023-10-nOPV_2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-12-11"),
      Response	=="CNG-2023-10-nOPV_2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-04-01"),
      Response	=="CNG-2024-08-BOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2024-09-30"),
      Response	=="CON-52DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-05-30"),
      Response	=="CON-52DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-08-09"),
      Response	=="DRC-11Prov-03-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-11-13"),
      Response	=="DRC-2023-03-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-03-19"),
      Response	=="DRC-2023-03-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-04-02"),
      Response	=="DRC-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-06-04"),
      Response	=="DRC-2023-09-KIN_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-09-25"),
      Response	=="DRC-2023-11-02_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-11-19"),
      Response	=="DRC-2024-10-01_B_nOPV"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-13"),
      Response	=="DRC-39DS-01-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-03-28"),
      Response	=="DRC-39DS-01-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-04-11"),
      Response	=="DRC-7DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-05-29"),
      Response	=="DRC-7DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-07-29"),
      Response	=="DRC-NID-03-2024-B_nOPV"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-31"),
      Response	=="DRC-NID-04-2024-bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2024-06-16"),
      Response	=="DRC-NID-07-2023-bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-07-30"),
      Response	=="DRC-NID-07-2023-bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-09-25"),
      Response	=="DRC-sNID-01-2024_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2024-02-05"),
      Response	=="DRC-sNID-08-2024-B_nOPV"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-08-11"),
      Response	=="DRC-sNID-09-2024_bOPV"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-09-22"),
      Response	=="ETH-2021-002-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-11-01"),
      Response	=="ETH-2021-002-2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-11-01"),
      Response	=="ETH-2021-002-3"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-04-04"),
      Response	=="ETH-2021-002-4"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-11-15"),
      Response	=="ETH-2024-03-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-04-29"),
      Response	=="ETH-2024-03-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-06-17"),
      Response	=="ETH-2024-08-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-13"),
      Response	=="GAM-65DS-06-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-11-23"),
      Response	=="GAM-65DS-06-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-03-22"),
      Response	=="GHA-2024-10-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-20"),
      Response	=="GHN-260-DS-07-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-09-04"),
      Response	=="GHN-260-DS-07-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-10-09"),
      Response	=="GHN-260-DS-07-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2022-12-18"),
      Response	=="GNB-117DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-04-30"),
      Response	=="GNB-117DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-06-25"),
      Response	=="GUE-20DS-02-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-03-01"),
      Response	=="GUE-20DS-02-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-06-07"),
      Response	=="GUI-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd0" ~	as_date("2023-09-11"),
      Response	=="GUI-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-10-30"),
      Response	=="GUI-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-05-13"),
      Response	=="GUI-2024-06-LIDs_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-09-09"),
      Response	=="KEN-13DS-02-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-05-25"),
      Response	=="KEN-13DS-02-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-07-20"),
      Response	=="KEN-2023-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-08-27"),
      Response	=="KEN-2023-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-10-10"),
      Response	=="KEN-2023-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2023-11-14"),
      Response	=="KEN-2024-01-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-01-30"),
      Response	=="KEN-2024-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-05"),
      Response	=="Kwara Response"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-07-05"),
      Response	=="Kwara Response"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-08-02"),
      Response	=="LBR-15DS-10-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-03-29"),
      Response	=="LBR-15DS-10-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-05-31"),
      Response	=="LBR-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-13"),
      Response	=="LBR-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-06-10"),
      Response	=="LBR-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2024-09-30"),
      Response	=="Madagascar Nationwide response cVDPV1"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-07-02"),
      Response	=="MAI-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-06-19"),
      Response	=="MAI-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-08-21"),
      Response	=="MAI-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-11-06"),
      Response	=="MAI-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-12-21"),
      Response	=="MAI-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-13"),
      Response	=="MAI-2024-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-09-30"),
      Response	=="Malawi-WPV1"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-03-24"),
      Response	=="Malawi-WPV1"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2022-04-28"),
      Response	=="Malawi-WPV1"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2022-08-14"),
      Response	=="Malawi-WPV1"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd4" ~	as_date("2022-10-16"),
      Response	=="MAU_57DS_08-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-12-20"),
      Response	=="MAU_57DS_08-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-04-01"),
      Response	=="MAU_57DS_08-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2022-07-06"),
      Response	=="MAU-2024-01-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-31"),
      Response	=="MAU-2024-01-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-05-26"),
      Response	=="MDG-2023-03-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-05-19"),
      Response	=="MDG-2023-03-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-07-28"),
      Response	=="MDG-2023-03-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2023-09-08"),
      Response	=="MDG-2023-03-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd4" ~	as_date("2023-10-19"),
      Response	=="MDG-2024-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2024-05-17"),
      Response	=="MDG-2024-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2024-07-12"),
      Response	=="MLI-12DS-01-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-03-08"),
      Response	=="MLI-12DS-01-2021"&	Vaccine.type	=="mOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-03-22"),
      Response	=="MLW-2023-04-1_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-05-18"),
      Response	=="MLW-2023-04-1_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-07-16"),
      Response	=="MLW-2023-04-1_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2023-09-16"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-03-24"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-04-24"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2022-07-10"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd4" ~	as_date("2022-08-21"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd5" ~	as_date("2022-10-16"),
      Response	=="MOZ-118-DS-02-2022"&	Vaccine.type	=="bOPV, nOPV2" &	roundNumber	=="Rnd6" ~	as_date("2022-12-10"),
      Response	=="MOZ-2023-03-1_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-04-15"),
      Response	=="MOZ-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-06-18"),
      Response	=="MOZ-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-09-18"),
      Response	=="MOZ-2024-04-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2024-05-31"),
      Response	=="MOZ-2024-04-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2024-08-02"),
      Response	=="NER-14DS-09-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-11-18"),
      Response	=="NER-14DS-09-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-12-16"),
      Response	=="NGA-2021-011-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-04-13"),
      Response	=="NGA-2021-011-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-05-25"),
      Response	=="NGA-2021-013-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-03-16"),
      Response	=="NGA-2021-013-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-04-13"),
      Response	=="NGA-2021-014-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-06-29"),
      Response	=="NGA-2021-014-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-08-03"),
      Response	=="NGA-2021-016-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-06-22"),
      Response	=="NGA-2021-016-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-07-20"),
      Response	=="NGA-2021-019"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-08-10"),
      Response	=="NGA-2021-020-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-09-14"),
      Response	=="NGA-2021-020-1"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-10-12"),
      Response	=="NGA-2021-020-2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-09-07"),
      Response	=="NGA-2021-020-2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-10-19"),
      Response	=="NGA-2021-020-3"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-10-12"),
      Response	=="NGA-2021-020-3"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-11-02"),
      Response	=="NGA-2021-020-4"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-09-21"),
      Response	=="NGA-2021-020-4"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-10-19"),
      Response	=="NIE-2023-04-02_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-05-14"),
      Response	=="NIE-2023-04-02_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-06-20"),
      Response	=="NIE-2023-07-03_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-07-25"),
      Response	=="NIE-2023-07-03_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-10-03"),
      Response	=="NIE-2023-07-03_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2023-11-07"),
      Response	=="NIE-2023-07-03_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd4" ~	as_date("2023-12-17"),
      Response	=="NIE-2023-10-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-10-03"),
      Response	=="NIE-2023-10-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-11-21"),
      Response	=="NIE-2024-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-05"),
      Response	=="NIE-2024-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-04-23"),
      Response	=="NIE-2024-nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2024-10-01"),
      Response	=="NIG-13DS-03-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-06-14"),
      Response	=="NIG-13DS-03-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-07-14"),
      Response	=="NIG-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-05-29"),
      Response	=="NIG-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-06-22"),
      Response	=="NIG-2023-11-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-12-08"),
      Response	=="NIG-2023-11-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-01-08"),
      Response	=="NIG-2024-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-07-15"),
      Response	=="NIG-2024-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-09-30"),
      Response	=="NIG-72DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-06-05"),
      Response	=="NIG-72DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-07-02"),
      Response	=="NIG-72DS-02-2022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2022-10-31"),
      Response	=="NIG-9DS-06-2021_Zinder"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-08-30"),
      Response	=="NIG-9DS-06-2021_Zinder"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-09-20"),
      Response	=="Preventive campaign bOPV - Guinea"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2021-10-25"),
      Response	=="Preventive campaign bOPV- CAR"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2021-10-25"),
      Response	=="Preventive campaign bOPV -CHAD"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2021-11-01"),
      Response	=="Preventive campaign bOPV -CHAD"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2021-11-22"),
      Response	=="RWA-2023-07-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-07-27"),
      Response	=="RWA-2023-07-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-09-14"),
      Response	=="SEN-79DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-12-20"),
      Response	=="SEN-79DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-02-27"),
      Response	=="SEN-79DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2022-08-13"),
      Response	=="SLE-16DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2021-05-31"),
      Response	=="SLE-16DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-07-05"),
      Response	=="SLE-16DS-01-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2021-08-30"),
      Response	=="SLE-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-05-13"),
      Response	=="SLE-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-06-10"),
      Response	=="SLE-2024-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd3" ~	as_date("2024-09-30"),
      Response	=="SSD-2024-02-01_nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-03-01"),
      Response	=="SSD-2024-02-01_nOPV2"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-04-19"),
      Response	=="SSD-79DS-09-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2020-11-13"),
      Response	=="SSD-79DS-09-2020"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2021-02-19"),
      Response	=="Tanganyika"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-01-29"),
      Response	=="TNZ-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-09-24"),
      Response	=="TNZ-2023-05-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-11-05"),
      Response	=="TOG-39DS-06-022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-08-18"),
      Response	=="TOG-39DS-06-022"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-10-16"),
      Response	=="TZA...DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-03-27"),
      Response	=="TZA...DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2022-05-21"),
      Response	=="TZA...DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2022-09-04"),
      Response	=="TZA...DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd4" ~	as_date("2022-12-04"),
      Response	=="UGA-149DS-08-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-01-17"),
      Response	=="UGA-149DS-08-2021"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2022-10-31"),
      Response	=="UGA-2024-08-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-10-05"),
      Response	=="Uganda_2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2022-11-07"),
      Response	=="Uganda_2023"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-01-30"),
      Response	=="ZAM-2023-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-02-19"),
      Response	=="ZAM-2023-02-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-04-23"),
      Response	=="ZAM-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-06-22"),
      Response	=="ZAM-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2023-09-03"),
      Response	=="ZAM-2023-09-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2023-10-29"),
      Response	=="ZAM-2024-06-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-07-28"),
      Response	=="ZIM-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2023-05-26"),
      Response	=="ZIM-2023-05-01_bOPV"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2023-10-13"),
      Response	=="ZIM-2023-12-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd1" ~	as_date("2024-02-23"),
      Response	=="ZIM-2023-12-01_nOPV"&	Vaccine.type	=="nOPV2" &	roundNumber	=="Rnd2" ~	as_date("2024-03-22"),
      Response	=="ZMB-30DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-03-24"),
      Response	=="ZMB-30DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd2" ~	as_date("2022-04-24"),
      Response	=="ZMB-30DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd3" ~	as_date("2022-08-14"),
      Response	=="ZMB-30DS-02-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd4" ~	as_date("2022-10-30"),
      Response	=="ZWE-2022"&	Vaccine.type	=="bOPV" &	roundNumber	=="Rnd1" ~	as_date("2022-10-30"),
      TRUE ~ end_date)) |>  
  filter(District != "NA")
       
F5<-F4 |>
  mutate(tot_r = (r_Non_Compliance + r_House_not_visited + r_childabsent + r_Child_was_asleep + r_Child_is_a_visitor + r_Vaccinated_but_not_FM),
         other_r = ifelse((total_missed - tot_r)< 0, 0, (total_missed - tot_r)),
         Country = case_when(
           Country == "DRC" ~ "RDC",
           Country == "Camerooun" ~ "CAE",
           Country == "CAMEROON" ~ "CAE",
           Country == "BURKINA_FASO" ~ "BFA",
           Country == "Ethiopia" ~ "ETH",
           Country == "ZAMBIA" ~ "ZMB",
           Country == "BENIN" ~ "BEN",
           Country == "CHAD" ~ "CHD",
           TRUE ~ Country))  |> 
  select(country = Country, province = Region, district = District, response = Response, vaccine.type = Vaccine.type, roundNumber, numbercluster = cluster, start_date, end_date, male_sampled, female_sampled, total_sampled, male_vaccinated, female_vaccinated , total_vaccinated, total_missed, status = Status, performance = Performance, r_Non_Compliance, r_House_not_visited, r_childabsent, r_Child_was_asleep, r_Child_is_a_visitor, r_Vaccinated_but_not_FM, other_r, percent_care_Giver_Informed_SIA) |>
  arrange(start_date)
F6$end_date <- ymd(F6$end_date)
year(F6$end_date[year(F6$end_date) == 2028]) <- 2023
year(F6$end_date[year(F6$end_date) == 2025]) <- 2024
    
  F6<-F5 |> 
  mutate(
    prct_Non_Compliance = ifelse(total_missed ==0, 0, r_Non_Compliance/total_missed),
    prct_Non_Compliance = round(prct_Non_Compliance,2),
    prct_Non_Compliance = prct_Non_Compliance*100,
    prct_House_not_visited = ifelse(total_missed ==0, 0, (r_House_not_visited/total_missed)),
    prct_House_not_visited = round(prct_House_not_visited,2),
    prct_House_not_visited = prct_House_not_visited*100,
    prct_childabsent = ifelse(total_missed ==0, 0, (r_childabsent/total_missed)),
    prct_childabsent = round(prct_childabsent,2),
    prct_childabsent = prct_childabsent*100,
    prct_Child_was_asleep = ifelse(total_missed ==0, 0, (r_Child_was_asleep/total_missed)),
    prct_Child_was_asleep = round(prct_Child_was_asleep,2),
    prct_Child_was_asleep = prct_Child_was_asleep*100,
    prct_Child_is_a_visitor = ifelse(total_missed ==0, 0, (r_Child_is_a_visitor/total_missed)),
    prct_Child_is_a_visitor = round(prct_Child_is_a_visitor,2),
    prct_Child_is_a_visitor = prct_Child_is_a_visitor*100,
    prct_Vaccinated_but_not_FM = ifelse(total_missed ==0, 0, (r_Vaccinated_but_not_FM/total_missed)),
    prct_Vaccinated_but_not_FM = round(prct_Vaccinated_but_not_FM,2),
    prct_Vaccinated_but_not_FM = prct_Vaccinated_but_not_FM*100,
    prct_other_r = ifelse(total_missed ==0, 0, (other_r/total_missed)),
    prct_other_r = round(prct_other_r,2),
    prct_other_r = prct_other_r*100
  )

write_csv(F6,"C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/LQAS_level1/ANG_LQAS.csv")


library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(Hmisc)

setwd("~/Documents/DJ/inflation-cpi-graphic-detail")
# expenditures needing revision to remove food & energy
Housing <- read.delim("source-data/cu.data.12.USHousing.txt")
Transportation <- read.delim("source-data/cu.data.14.USTransportation.txt")
Food.Beverage <- read.delim("source-data/cu.data.11.USFoodBeverage.txt")
All.Items <- read.delim("source-data/cu.data.1.AllItems.txt")

# expenditures not needing revision
Apparel <- read.delim("source-data/cu.data.13.USApparel.txt")
Medical <- read.delim("source-data/cu.data.15.USMedical.txt")
Recreation <- read.delim("source-data/cu.data.16.USRecreation.txt")
Education.Communication <- read.delim("source-data/cu.data.17.USEducationAndCommunication.txt")
Other.Goods.Services <- read.delim("source-data/cu.data.18.USOtherGoodsAndServices.txt")
Summaries <- read.delim("source-data/cu.data.2.Summaries.txt")


# Data Processing --------------------------------------------------------------
# Housing
Housing$series_id = gsub(" ", "", Housing$series_id, fixed = TRUE)
Housing %>% filter(series_id == "CUSR0000SAH" | series_id == "CUSR0000SAH21") -> Housing.New
Housing %>% filter(series_id == "CUSR0000SAH1" | series_id == "CUSR0000SAH2") -> Housing.Old

Housing.Utilities = data.frame(year = Housing.New$year[1:655],
                                       month = Housing.New$period[1:655],
                                       overall_index = Housing.New$value[1:655],
                                       utilities_index = Housing.New$value[656:1310])

write.csv(Housing.Utilities, "output-data/Housing.Utilities.csv", row.names = FALSE)


# Transportation
Transportation$series_id = gsub(" ", "", Transportation$series_id, fixed = TRUE)
Transportation %>% 
  filter(series_id == "CUSR0000SAT" | series_id == "CUSR0000SETB") %>%
  filter(year >= 1967) -> Transportation

Transportation.Motor.Fuel = data.frame(year = Transportation$year[1:655],
                                       month = Transportation$period[1:655],
                                       overall_index = Transportation$value[1:655],
                                       motor_fuel_index = Transportation$value[656:1310])

write.csv(Transportation.Motor.Fuel, "output-data/Transportation.Motor.Fuel.csv", row.names = FALSE)

# Food & Beverage
Food.Beverage$series_id = gsub(" ", "", Food.Beverage$series_id, fixed = TRUE)
Food.Beverage %>% filter(series_id == "CUSR0000SAF") -> Food.Beverage

Food.Beverage = data.frame(year = Food.Beverage$year,
                           month = Food.Beverage$period,
                           overall_index = Food.Beverage$value)

write.csv(Food.Beverage, "output-data/Food.Beverage.csv", row.names = FALSE)


# All Items
All.Items$series_id = gsub(" ", "", All.Items$series_id, fixed = TRUE)
All.Items %>% filter(series_id == "CUSR0000SA0") %>%
  filter(year >= 1967) -> All.Items

All.Items = data.frame(year = All.Items$year,
                       month = All.Items$period,
                       overall_index = All.Items$value)

write.csv(All.Items, "output-data/All.Items.csv", row.names = FALSE)
 


# Other Expenditure Categories -------------------------------------------------
# Apparel
Apparel$series_id = gsub(" ", "", Apparel$series_id, fixed = TRUE)
Apparel %>% filter(series_id == "CUSR0000SAA") %>%
  filter(year >= 1967) -> Apparel

Apparel = data.frame(year = Apparel$year,
                     month = Apparel$period,
                     apparel_index = Apparel$value)

# Medical
Medical$series_id = gsub(" ", "", Medical$series_id, fixed = TRUE)
Medical %>% filter(series_id == "CUSR0000SAM") %>%
  filter(year >= 1967) -> Medical

Medical = data.frame(year = Medical$year,
                     month = Medical$period,
                     medical_index = Medical$value)

# Recreation
Recreation$series_id = gsub(" ", "", Recreation$series_id, fixed = TRUE)
Recreation %>% filter(series_id == "CUSR0000SAR") %>%
  filter(year >= 1996) -> Recreation

Recreation = data.frame(year = Recreation$year,
                        month = Recreation$period,
                        recreation_index = Recreation$value)

# Education.Communication
Education.Communication$series_id = gsub(" ", "", Education.Communication$series_id, fixed = TRUE)
Education.Communication %>% filter(series_id == "CUSR0000SAE") %>%
  filter(year >= 1996) -> Education.Communication

Education.Communication = data.frame(year = Education.Communication$year,
                                     month = Education.Communication$period,
                                     education_index = Education.Communication$value)

# Other.Goods.Services
Other.Goods.Services$series_id = gsub(" ", "", Other.Goods.Services$series_id, fixed = TRUE)
Other.Goods.Services %>% filter(series_id == "CUSR0000SAG") %>%
  filter(year >= 1976) -> Other.Goods.Services

Other.Goods.Services = data.frame(year = Other.Goods.Services$year,
                                  month = Other.Goods.Services$period,
                                  other_goods_index = Other.Goods.Services$value)



# Data Merging -----------------------------------------------------------------
# reading in the revised expenditure categories (revised manually)
Revised <- read.csv("source-data/revised_expenditure_categories.csv")
Revised %>% left_join(Apparel) %>%
  left_join(Medical) %>%
  left_join(Recreation) %>%
  left_join(Education.Communication) %>%
  left_join(Other.Goods.Services) -> Expenditure.Data

names(Expenditure.Data)[names(Expenditure.Data) == "Entertainment"] = "entertainment_index"

# source: relative weights compiled manually from https://www.bls.gov/cpi/tables/relative-importance/home.htm
CPI.Relative.Weights <- read.csv("source-data/CPI_relative_weights.csv")
names(CPI.Relative.Weights)[names(CPI.Relative.Weights) == "Year"] = "year"

# creating vector for 2021 weights. 2021 weights will be released in December, so using 2020 as a rough proxy
year_2021_weights = unlist(c(2021, CPI.Relative.Weights[58, 2:12]))
CPI.Relative.Weights = rbind(CPI.Relative.Weights, year_2021_weights)

Expenditure.Data %>% left_join(CPI.Relative.Weights) %>%
  rename(CPI_index = CPI..minus.food...energy.,
         housing_index = Housing..minus.utilities.,
         transportation_index = Transportation..minus.motor.fuel.) -> Expenditure.Data



# 3-Month Data ------------------------------------------------------------
# every three months is M03, M06, M09, M12
Expenditure.Data %>% filter(month %in% c("M03", "M06", "M09", "M12")) -> Expenditure.Data.3

# creating inflation variable for each index
Expenditure.Data.3 %>% 
  mutate(cpi_inflation = CPI_index / lag(CPI_index, default = first(CPI_index)) - 1,
         housing_inflation = housing_index / lag(housing_index, default = first(housing_index)) - 1,
         transportation_inflation = transportation_index / lag(transportation_index, default = first(transportation_index)) - 1,
         entertainment_inflation = entertainment_index / lag(entertainment_index, default = first(entertainment_index)) - 1,
         apparel_inflation = apparel_index / lag(apparel_index, default = first(apparel_index)) - 1,
         medical_inflation = medical_index / lag(medical_index, default = first(medical_index)) - 1,
         recreation_inflation = recreation_index / lag(recreation_index, default = first(recreation_index)) - 1,
         education_inflation = education_index / lag(education_index, default = first(education_index)) - 1,
         other_inflation = other_goods_index / lag(other_goods_index, default = first(other_goods_index)) - 1) -> Expenditure.Data.3

# first doing 1977-1996 (CPI basket changes in 1996)
# 
Expenditure.Data.3 %>% 
  filter(year >= 1977 & year <= 1996) %>%
  mutate(Transportation.Minus.Fuel = Transportation - Motor.Fuel,
         Housing.Minus.Utilities = Housing - Utilities) %>%
  select(c(1, 2, 15:18, 24:28, 31:33)) -> WSD.1977.1996

# doing the weighted standard deviations for
WSD.1977.1996$WSD = rep(0,nrow(WSD.1977.1996))

for(i in 1:nrow(WSD.1977.1996)){
  WSD.1977.1996$WSD[i] = sqrt(wtd.var(c(WSD.1977.1996$housing_inflation[i], 
                                         WSD.1977.1996$transportation_inflation[i], 
                                         WSD.1977.1996$entertainment_inflation[i], 
                                         WSD.1977.1996$apparel_inflation[i], 
                                         WSD.1977.1996$medical_inflation[i], 
                                         WSD.1977.1996$other_inflation[i]),
                                       c(WSD.1977.1996$Housing.Minus.Utilities[i], 
                                         WSD.1977.1996$Transportation.Minus.Fuel[i], 
                                         WSD.1977.1996$Entertainment[i], 
                                         WSD.1977.1996$Apparel[i], 
                                         WSD.1977.1996$Medical[i], 
                                         WSD.1977.1996$Other.Goods.Services[i])))
}


Expenditure.Data.3 %>% 
  filter(year >= 1997) %>%
  mutate(Transportation.Minus.Fuel = Transportation - Motor.Fuel,
         Housing.Minus.Utilities = Housing - Utilities) %>%
  select(c(1, 2, 15, 16, 18, 19, 20, 24, 25, 27:33)) -> WSD.1997.2021

WSD.1997.2021$WSD = rep(0,nrow(WSD.1997.2021))

for(i in 1:nrow(WSD.1997.2021)){
  WSD.1997.2021$WSD[i] = sqrt(wtd.var(c(WSD.1997.2021$housing_inflation[i], 
                                        WSD.1997.2021$transportation_inflation[i], 
                                        WSD.1997.2021$recreation_inflation[i], 
                                        WSD.1997.2021$apparel_inflation[i], 
                                        WSD.1997.2021$medical_inflation[i], 
                                        WSD.1997.2021$other_inflation[i],
                                        WSD.1997.2021$education_inflation[i]),
                                      c(WSD.1997.2021$Housing.Minus.Utilities[i], 
                                        WSD.1997.2021$Transportation.Minus.Fuel[i], 
                                        WSD.1997.2021$Recreation[i], 
                                        WSD.1997.2021$Apparel[i], 
                                        WSD.1997.2021$Medical[i], 
                                        WSD.1997.2021$Other.Goods.Services[i],
                                        WSD.1997.2021$Education.Communication[i])))
}


Final.Data.3 = data.frame(year = Expenditure.Data.3$year[42:219],
                          month = Expenditure.Data.3$month[42:219],
                          cpi_inflation = Expenditure.Data.3$cpi_inflation[42:219],
                          WSD = c(unlist(WSD.1977.1996$WSD), unlist(WSD.1997.2021$WSD)))

Final.Data.3$next_period_inflation = c(Final.Data.3$cpi_inflation[2:(nrow(Final.Data.3))], NA)
Final.Data.3$current_index = as.numeric(Final.Data.3$year > 1996)


# 3-Month Modeling --------------------------------------------------------

# import Reis pure inflation
#Reis = read.csv("figure3panelA_USdataNew.csv")

# creating variable for change in inflation from Time 0 to Time 1 
# (the inflation in Time 1 is represented by cpi_inflation, 
# the inflation in Time 2 is represented by next_period_inflation)
Final.Data.3 %>% 
  mutate(inflation_change = cpi_inflation - lag(cpi_inflation, default = first(cpi_inflation))) -> Final.Data.3

# Final.Data.3$Reis_inflation = c(Reis$inflation..all..demeaned.[51:225], NA, NA, NA)

# From 1976 M12 cpi_inflation
Final.Data.3$inflation_change[1] = Final.Data.3$cpi_inflation[1] - 0.029521254

model1 = lm(next_period_inflation ~ cpi_inflation + WSD + current_index + inflation_change, 
            data = Final.Data.3)

summary(model1)


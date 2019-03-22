# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


#### 0: HEADER ####

# CLEAR ENVIRONMENT
rm(list=ls(all=TRUE))

# SET WD LAPTOP vs WORK MACHINE
# setwd("C:/Users/bevington/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Paper_2018_snow_modis")
setwd("G:/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Paper_2018_snow_modis/3_Modis_Output/Sample_allImages_Output/BC_M_D10A_RandSampls-20190201T185423Z-001/BC_M_D10A_RandSampls")

#### 1: LOAD LIBRARIES ####

# DATA MANUPULATION LIBRARIES
library(reshape2);
library(tidyverse)

df= read.csv("Rand_Samp_2010.csv")
df = df %>% select(-elevation, -.geo)
summary(df)

dfM = melt(df, id = "system.index")


dfM$date = as.Date(gsub(pattern = "X", x = dfM$variable, replacement = ""), format = "%Y%j")

sub = filter(dfM, system.index == 5 & !is.na(value))

ggplot(sub, aes(date, value)) + geom_point() + geom_smooth()

sub$date = as.numeric(format(sub$date, "%j"))

l = loess(formula = value ~ date, sub, span = 0.2)

sub$loess = predict(l)

ggplot(sub, aes(date, value)) + geom_point() + geom_line(aes(y = loess), color = "red") + theme_bw()


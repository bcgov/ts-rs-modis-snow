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
    setwd("C:/Users/bevington/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Paper_2018_snow_modis")
    # setwd("G:/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Paper_2018_snow_modis")

#### 1: LOAD LIBRARIES ####

  # DATA MANUPULATION LIBRARIES
    library(reshape2);
    library(broom);
    library(tidyverse);
    library(lubridate)
    library(lme4)
    library(Hmisc)
    library(data.table)
    library(doBy)

  # LOAD SPATIAL TOOLS
    library(sf);
    library(sp)
    library(spdep)
    library(bcmaps);
    library(viridis)

  # LOAD GGPLOT TOOLS
    library(ggrepel)
    library(ggExtra)
    library(ggpubr)
    library(GGally)
    library(ggmap);
    library(ggpmisc);
    library(RColorBrewer)
    library(ggthemes);
    library(scales);
    library(hexbin);
    library(gridExtra);

#### 2: IMPORT  ####

  # READ GOOGLE EARTH ENGINE RANDOM SAMPLE OUTPUT
    df = tibble::as_tibble(read.csv("3_Modis_Output/Loess_Output/20190102/MODIS_Samp_25_2_a_20k_final.csv", stringsAsFactors = F))

  # IMPORT TELECONNECTION DATA
    tel = as_tibble(read.csv("0_Data_ONI_PDO/original_TEL_events.csv"))

#### 3: DEFINE FUNCTIONS ####

    #### LINEAR MODEL ####

    # Run Linear Model and LOOP n times with random adjustment, summarise mean slope difference

    lm_iter = function(raw_df, groups, Y, X, textName)

    {

      # Run linear model on raw data
      lm_raw = raw_df %>%
        dplyr::group_by_(.dots = groups) %>%
        do(broom::tidy(lm(paste(Y,'~',X), data = ., na.action = na.omit))) %>%
        dplyr::filter(term != "(Intercept)")

      # Loop the same model n times for error estimation
      for(i in 1:n_iterations)

      {

        print(i)

        # Create list of offsets the length of Y
        offset = rnorm(length(raw_df[[Y]]), 0 , 5)

        # Adjust Y by offest
        raw_df$adj = raw_df[[Y]] + offset

        # Run linear model for adjusted days
        lm_adj = raw_df %>%
          dplyr::group_by_(.dots = groups) %>%
          do(broom::tidy(lm(paste("adj",'~',X), data = ., na.action = na.omit))) %>%
          dplyr::filter(term != "(Intercept)")

        # Collector: Join raw linear model to all adjusted models
        lm_raw   = full_join(
          x      = lm_raw,
          y      = lm_adj %>% dplyr::select(-statistic, -p.value, -std.error),
          by     = c(groups, "term"),
          suffix = c("",paste("_",as.character(i),sep="")))
      }


      # Summarise iterated linear models as
      lm_summary = lm_raw %>%
        gather(iteration, estimate_n, contains("estimate_")) %>%
        dplyr::group_by_(.dots = c(groups, "estimate", "p.value")) %>%
        dplyr::summarise(est_iter_min  = min(estimate_n),
                         est_iter_q25  = quantile(estimate_n, .25),
                         est_iter_mean = mean(estimate_n),
                         est_iter_q75  = quantile(estimate_n, .75),
                         est_iter_max  = max(estimate_n)) %>%
        dplyr::mutate(error_minus = abs(estimate-est_iter_q25),
                      error_plus = abs(estimate-est_iter_q75),
                      error_mean = mean(error_minus, error_plus)) %>%
        dplyr::select(
          -est_iter_min,
          -est_iter_q25,
          -est_iter_mean,
          -est_iter_q75,
          -est_iter_max,
          -error_minus,
          -error_plus)

      filename = paste("5_Draft/Figures/", textName, "_", zone_exp, "_", n, "_", n_iterations, "_", format(x = now(), format = "%Y%m%d%H%M%S"), "_", Y, "_", X,".csv", sep = "")
      write.csv(x = lm_summary, file = filename)
      return(lm_summary)
    }

    #### SPEARMAN CORRELATION ####

    # Run Spearman Correlation and LOOP n times with random adjustment, summarise mean coefficient difference

    cor_iter = function(raw_df, groups, Y, X, textName)
    {

      # Run linear model on raw data
      cor_raw = raw_df %>%
        dplyr::group_by_(.dots = groups) %>%
        dplyr::select(X, Y) %>%
        dplyr::rename(X = X, Y = Y) %>%
        dplyr::summarise(estimate = cor.test(Y, X, method = "spearman", exact=FALSE)$estimate,
                         p.value = cor.test(Y, X, method = "spearman", exact=FALSE)$p.value)

      # Loop the same model n times for error estimation
      for(i in 1:n_iterations)
      {
        print(i)

        # Create list of offsets the length of Y
        offset = rnorm(length(raw_df[[Y]]), 0 , 5)

        # Adjust Y by offest
        raw_df$adj = raw_df[[Y]] + offset


        # Run linear model for adjusted days
        cor_adj = raw_df %>%
          dplyr::group_by_(.dots = groups) %>%
          dplyr::select(X, adj) %>%
          dplyr::rename(X = X, Y = adj) %>%
          dplyr::summarise(estimate = cor.test(Y, X, method = "spearman", exact=FALSE)$estimate,
                           p.value = cor.test(Y, X, method = "spearman", exact=FALSE)$p.value)

        # Collector: Join raw linear model to all adjusted models
        cor_raw   = full_join(
          x      = cor_raw,
          y      = cor_adj %>% dplyr::select(-p.value),
          by     = c(groups),
          suffix = c("",paste("_",as.character(i),sep="")))
      }


      # Summarise iterated linear models as
      cor_summary = cor_raw %>%
        gather(iteration, estimate_n, contains("estimate_")) %>%
        dplyr::group_by_(.dots = c(groups, "estimate", "p.value")) %>%
        dplyr::summarise(est_iter_min  = min(estimate_n),
                         est_iter_q25  = quantile(estimate_n, .25),
                         est_iter_mean = mean(estimate_n),
                         est_iter_q75  = quantile(estimate_n, .75),
                         est_iter_max  = max(estimate_n)) %>%
        dplyr::mutate(error_minus = abs(estimate-est_iter_q25),
                      error_plus = abs(estimate-est_iter_q75),
                      error_mean = mean(error_minus, error_plus)) %>%
        dplyr::select(
          -est_iter_min,
          -est_iter_q25,
          -est_iter_mean,
          -est_iter_q75,
          -est_iter_max,
          -error_minus,
          -error_plus)

      filename = paste("5_Draft/Figures/", textName, "_", zone_exp, "_", n, "_", n_iterations, "_", format(x = now(), format = "%Y%m%d%H%M%S"), "_", Y, "_", X,".csv", sep = "")
      write.csv(x = cor_summary, file = filename)
      return(cor_summary)
    }

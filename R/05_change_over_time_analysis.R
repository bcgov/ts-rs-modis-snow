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

files = list.files("G:/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Paper_2018_snow_modis/5_Draft/Figures", pattern = "csv")



for(file in files){print(file)
  out = paste(strsplit(file,"_")[[1]][1],strsplit(file,"_")[[1]][2],strsplit(file,"_")[[1]][3],strsplit(file,"_")[[1]][4],strsplit(file,"_")[[1]][5], sep = "_")
  assign(out, read.csv(paste0("G:/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Paper_2018_snow_modis/5_Draft/Figures/",file)))}

# Function to test time series, and adjusted annual and seasonal time series


adjuster = function(raw_df, raw_lm, groups) {

  # raw_df = df_zone_year_mean
  # raw_lm = mod_lm_zone_year_mean %>% select(-year, -days, -index) %>% filter(!is.na(HYDROLOGICZONE_NAME))
  # groups = c(zone_name,"measurement")

# Linear regression of change over time for original data
  ts_original =  raw_df  %>%
    dplyr::group_by_(.dots = groups) %>%
    do(broom::tidy(lm(days ~ year, data = .))) %>%
    dplyr::filter(term == "year")

    print("ts_original")
    print(head(ts_original))

# Correct data using teleconnection relationships
  corrected_df = full_join(x = raw_lm, y = raw_df) %>% dplyr::mutate(days_corrected = days+(estimate*index))
  print("corrected_df")
  print(head(corrected_df))

  ts_corrected = corrected_df %>%
    dplyr::group_by_(.dots = c(groups,"tel")) %>%
    do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.exclude))) %>%
    # do(broom::glance(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
    dplyr::filter(term == "year");
  print("ts_corrected")
  print(head(ts_corrected))

  out = full_join(x = ts_original %>% mutate(tel = "RAW"), y = ts_corrected)
  print("out")
  print(head(out))

  return(out)
  }

give.n <- function(x){
  return(c(y = min(x)-0.1, label = length(x)))
  }

# Linear model for RAW and ADJUSTED data

  # All BC annual
  ts_BC_year = adjuster(raw_df = df_BC_year_mean,
                        raw_lm = mod_lm_BC_year_mean,
                        groups = c("measurement"))
  # All BC seasonal
  ts_BC_seas = adjuster(raw_df = df_BC_seas_mean,
                        raw_lm = mod_lm_BC_seas_mean,
                        groups = c("measurement","season"))
  # Hydrozones annual
  ts_HZ_year = adjuster(raw_df = df_zone_year_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                        raw_lm = mod_lm_zone_year_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                        groups = c(zone_name,"measurement"))
  # Hydrozones seasonal
  ts_HZ_seas = adjuster(raw_df = df_zone_seas_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                        raw_lm = mod_lm_zone_seas_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                        groups = c(zone_name,"measurement","season"))
  # Hydrozones / elevation annual
  ts_HZgrZ_year = adjuster(raw_df = df_zoneZ_year_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                           raw_lm = mod_lm_zoneZ_year_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                           groups = c(zone_name,"grZ","measurement"))
  # Hydrozones / elevation seasonal
  ts_HZgrZ_seas = adjuster(raw_df = df_zoneZ_seas_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                           raw_lm = mod_lm_zoneZ_seas_mean %>% dplyr::select(-year, -days, -index) %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                           groups = c(zone_name,"grZ","measurement","season"))

# Plotting


  # PLOT RAW DATA
  zone_plot(ts_HZ_year %>% filter(tel == "RAW"), -2,2,0.5) +
    facet_grid(~measurement, labeller = labeller(measurement = label_parsed))   +
    labs(title = "ts_HZ_seas_raw", x = "", y = "", fill = expression(paste("LLS (d ?",yr^-1,")", sep = "")))

  # PLOT CORRECTED ANNUAL HYDROZONE
  zone_plot(ts_HZ_year %>% filter(tel != "RAW"), -2,2,0.5) +
    facet_grid(~measurement, labeller = labeller(measurement = label_parsed))   +
    labs(title = "ts_HZ_seas_raw", x = "", y = "", fill = expression(paste("LLS (d ?",yr^-1,")", sep = "")))

  # PLOT CORRECTED SEASONAL HYDROZONE
  zone_plot(ts_HZ_seas %>% filter(tel != "RAW"), -2,2,0.5) +
    facet_grid(tel+season~measurement, labeller = labeller(measurement = label_parsed))   +
    labs(title = "mod_lm_zone_seas_mean", x = "", y = "", fill = expression(paste("LLS (d ?",yr^-1,")", sep = "")))

  # PLOT CORRECTED SEASONAL HYDROZONE AND ELEVATION
  zone_plot(ts_HZgrZ_seas %>% filter(tel == "RAW"), -2,2,0.5) +
    facet_grid(tel+measurement~grZ, labeller = labeller(measurement = label_parsed))   +
    labs(title = "mod_lm_zone_seas_mean", x = "", y = "", fill = expression(paste("LLS (d ",yr^-1,")", sep = "")))

    ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "ts_HZgrZ_raw", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), height = 8, device = "pdf")

  # PLOT CORRECTED SEASONAL HYDROZONE AND ELEVATION
  zone_plot(ts_HZgrZ_seas %>% filter(tel != "RAW"), -2,2,0.5) +
    facet_grid(tel+season~measurement+grZ, labeller = labeller(measurement = label_parsed))   +
    labs(title = "mod_lm_zone_seas_mean", x = "", y = "", fill = expression(paste("LLS (d ?",C^-1,")", sep = "")))

    ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "ts_HZgrZ_seas_detr", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), height = 8, device = "pdf")

#
#   ggplot() +
#       geom_hline(yintercept = 0) +
#       geom_point(data = ts_HZgrZ_year %>% filter(p.value > 0.05), aes(x = grZ, y = estimate), shape = 21, size = 1, alpha = 0.2) +
#
#       geom_point(data = ts_HZgrZ_year %>% filter(p.value <= 0.05), aes(x = grZ, y = estimate), shape = 21, size = 3, fill = "red") +
#       geom_smooth(data = ts_HZgrZ_year %>% filter(p.value <= 0.05), aes(x = grZ, y = estimate), method = "lm") +
#
#       facet_grid(tel~measurement, labeller = labeller(measurement = label_parsed)) +
#       theme_bw() +
#       theme(aspect.ratio = 0.4) +
#       labs(x = "Elevation (m)", y = expression(paste("LLS (d ",yr^-1,")", sep = "")))
#
#
#   ggplot() +
#     geom_hline(yintercept = 0) +
#     geom_point(data = ts_HZgrZ_seas %>% filter(p.value > 0.05 & tel != "RAW"), aes(x = grZ, y = estimate), shape = 21, size = 1, alpha = 0.2) +
#
#     geom_point(data = ts_HZgrZ_seas %>% filter(p.value <= 0.05 & tel != "RAW"), aes(x = grZ, y = estimate, fill = season), shape = 21, size = 3) +
#     geom_smooth(data = ts_HZgrZ_seas %>% filter(p.value <= 0.05 & tel != "RAW"), aes(x = grZ, y = estimate, color = season), method = "lm", se = F) +
#
#     facet_grid(tel~measurement, labeller = labeller(measurement = label_parsed)) +
#     theme_bw() +
#     theme(aspect.ratio = 1) +
#     labs(x = "Elevation (m)", y = expression(paste("LLS (d ",yr^-1,")", sep = "")))
#

#
# # HZ grZ
# names(df)
# df %>%
#   group_by(c(HYDROLOGICZONE_NAME, grZ, season)) %>%
#   select(SD_ON, SD_OFF, SD_INT)
#
# ts_HZgrZ_seas = adjuster(df_zone_seas_mean , mod_lm_zone_seas_mean%>% select(-year, -days, -index), c(zone_name,"measurement","season"))
#
# ts_HZgrZ_seas = adjuster(df_zone_seas_mean , mod_lm_zone_seas_mean%>% select(-year, -days, -index), c(zone_name,"measurement","season"))
#
#
#
#
# zone_plot(ts_HZ_year, -2, 2, 0.5) +
#   facet_wrap(tel~measurement)
#
# zone_plot(ts_HZ_seas %>% filter(tel != "RAW"), -2, 2, 0.5) +
#   facet_grid(tel+season~measurement)
#
#
# ## TIMESERIES BY ELEVATION
#
# adjuster_grZ = function(raw_df, raw_lm, groups) {
#
#   raw_df = melt(raw_df, measure.vars = c("oni","pdo"), variable.name = "tel", value.name = "index")
#   raw_df$tel = ordered(raw_df$tel, levels = tel_list, labels = tel_name)
#
#   original_lm = raw_df  %>%
#     dplyr::group_by_(.dots = groups) %>%
#     do(broom::tidy(lm(days ~ year, data = .))) %>%
#     dplyr::filter(term == "year")
#     print("original_lm")
#     print(head(original_lm))
#
#   corrected_df = full_join(x = raw_lm, y = raw_df) %>% dplyr::mutate(days_corrected = days+index*estimate)
#   print("corrected_df")
#   print(head(corrected_df))
#
#   corrected_lm = corrected_df %>%
#       filter(p.value <= 0.05) %>%
#     dplyr::group_by_(.dots = c(groups,"tel")) %>%
#     do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
#     # do(broom::glance(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
#     dplyr::filter(term == "year");
#   print("corrected_lm")
#   print(head(corrected_lm))
#   return(rbind(
#     original_lm %>% mutate(tel = "RAW"),
#     corrected_lm))
#   }
#
# adjuster_grZ_both = function(raw_df, raw_lm, groups) {
#
#   # raw_df = melt(raw_df, measure.vars = c("oni","pdo"), variable.name = "tel", value.name = "index")
#   # raw_df$tel = ordered(raw_df$tel, levels = tel_list, labels = tel_name)
#   raw_df = df_tel_year_all_m
#   raw_lm = year_grZ_lm
#   groups = c(zone_name, "measurement", "grZ")
#
#   original_lm = raw_df  %>%
#     dplyr::group_by_(.dots = groups) %>%
#     do(broom::tidy(lm(days ~ year, data = .))) %>%
#     dplyr::filter(term == "year")
#   print("original_lm")
#   print(head(original_lm))
#
#   spread(raw_lm, )
#   merge(raw_df, raw_lm %>% filter(tel == "ONI"), by = c(zone_name, "measurement","grZ"), suffixes = ".oni")
#   , no.dups = TRUE,
#         allow.cartesian=getOption("datatable.allow.cartesian"),  # default FALSE
#
#   corrected_df = full_join(x = raw_lm, y = raw_df) %>% dplyr::mutate(days_corrected = days+index*estimate)
#   print("corrected_df")
#   print(head(corrected_df))
#
#   corrected_lm = corrected_df %>%
#     filter(p.value <= 0.05) %>%
#     dplyr::group_by_(.dots = c(groups,"tel")) %>%
#     do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
#     # do(broom::glance(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
#     dplyr::filter(term == "year");
#   print("corrected_lm")
#   print(head(corrected_lm))
#   return(rbind(
#     original_lm %>% mutate(tel = "RAW"),
#     corrected_lm))
# }
#
#
#
# give.n <- function(x){
#   return(c(y = min(x)-0.05, label = length(x)))
# }
#
#
#
# # mod_lm_BC_year_mean
# # temp_annual = adjuster_grZ(raw_df = df_tel_year_all_m, raw_lm = mod_lm_BC_year_mean, groups = c(zone_name, "measurement", "grZ"))
# # temp_annual$tel = ordered(temp_annual$tel, levels = c("RAW","PDO","ONI"), labels = c("Original dataset","De-trended using PDO","De-trended using ONI"))
# #
# # ggplot(temp_annual %>% filter(p.value <= 0.05), aes(as.factor(grZ), estimate)) +
# #   geom_hline(yintercept = 0) +
# #   geom_boxplot(aes(fill = tel)) +
# #   geom_smooth(method = "lm") +
# #   facet_grid(~measurement, labeller = labeller(measurement = label_parsed)) +
# #   theme_few(base_size = 20) +
# #   theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +
# #   labs(x = "Elevation (m)", y = expression(paste("LLS (d ",yr^-1,")", sep = "")), fill = "") +
# #   scale_fill_manual(values = c("orange","light blue","white")) +
# #   stat_summary(aes(group = tel), fun.data = give.n, geom = "text", position = position_dodge(width = 1), size=3,  family="sans")
#
# # ggsave("timeseries_elevation.pdf", height = 8)
# df_tel_seas_all_m %>%
#   dplyr::group_by_(.dots = groups) %>%
#   do(broom::glance(lm(days ~ oni*pdo, data = .)))
#
# lm(days~oni*pdo, df_tel_seas_all_m)
#
# temp_seasonal = adjuster(raw_df = df_tel_seas_all_m, raw_lm = mod_lm_BC_seas_mean, groups = c(zone_name, "measurement", "grZ","season"))
#
# ggplot(temp_seasonal %>% filter(p.value <= 0.05), aes(as.factor(grZ), estimate)) +
#     geom_hline(yintercept = 0) +
#     geom_boxplot(aes(fill = season)) +
#     geom_smooth(method = "lm") +
#     facet_grid(tel~measurement, labeller = labeller(measurement = label_parsed)) +
#     theme_few() +
#     theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90, hjust = 1)) +
#     labs(x = "Elevation (m)", y = "LLS (d / yr)", fill = "Teleconnection Season") +
#     stat_summary(aes(group = tel), fun.data = give.n, geom = "text", position = position_dodge(width = 1), size=3,  family="sans")
#
# #
# #
# #
# #
# # raw_df = df_tel_year_all_m
# # raw_lm = year_grZ_lm
# #
# # raw_df = melt(raw_df, measure.vars = c("oni","pdo"), variable.name = "tel", value.name = "index")
# # raw_df$tel = ordered(raw_df$tel, levels = tel_list, labels = tel_name)
# #
# # original_lm = raw_df  %>%
# #   dplyr::group_by_(.dots = groups) %>%
# #   do(broom::tidy(lm(days ~ year, data = .))) %>%
# #   dplyr::filter(term == "year")
# # print("original_lm")
# # print(head(original_lm))
# #
# # corrected_df = full_join(x = raw_lm, y = raw_df) %>% dplyr::mutate(days_corrected = days+index*estimate)
# # print("corrected_df")
# # print(head(corrected_df))
# #
# # corrected_lm = corrected_df %>%
# #   filter(p.value <= 0.05) %>%
# #   dplyr::group_by_(.dots = c(groups,"tel")) %>%
# #   do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
# #   # do(broom::glance(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
# #   dplyr::filter(term == "year");
# # print("corrected_lm")
# # print(head(corrected_lm))
# # return(rbind(
# #   original_lm %>% mutate(tel = "RAW"),
# #   corrected_lm))
#
#
#
#
# # mod_lm_BC_year_mean
# temp_annual = adjuster(raw_df = df_tel_year_all_m, raw_lm = year_grZ_lm, groups = c(zone_name, "measurement", "grZ"))
# temp_annual$tel = ordered(temp_annual$tel, levels = c("RAW","PDO","ONI"), labels = c("Original dataset","De-trended using PDO","De-trended using ONI"))
#
# ggplot(temp_annual %>% filter(p.value <= 0.05), aes(as.factor(grZ), estimate)) +
#   geom_hline(yintercept = 0) +
#   geom_boxplot(aes(fill = tel)) +
#   geom_smooth(method = "lm") +
#   facet_grid(~measurement, labeller = labeller(measurement = label_parsed)) +
#   theme_few(base_size = 20) +
#   theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +
#   labs(x = "Elevation (m)", y = expression(paste("LLS (d ",yr^-1,")", sep = "")), fill = "") +
#   scale_fill_manual(values = c("orange","light blue","white")) +
#   stat_summary(aes(group = tel), fun.data = give.n, geom = "text", position = position_dodge(width = 1), size=3,  family="sans")
#
# # ggsave("timeseries_elevation.pdf", height = 8)
# ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "grZ_ts_tel_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), height = 10, device = "pdf")
#
#
# # #### BC ANNUAL ORIGINAL ####
# #
# # df_raw = df_BC_year_mean
# #
# # ts_BC_df_lm_original = df_raw  %>%
# #   dplyr::group_by_(.dots = c("measurement")) %>%
# #   do(broom::tidy(lm(days ~ year, data = .))) %>%
# #   dplyr::filter(term == "year")
# #
# #   # ggplot(df_BC_year_mean, aes(x = as.Date(paste(year, "-01-01", sep = "")))) +
# #   #   geom_line(aes(y = days, group = measurement)) +
# #   #   geom_point(aes(y = days, fill = measurement), shape = 21, size = 3) +
# #   #   geom_smooth(aes(y = days, color = measurement), method = "lm") +
# #   #   facet_wrap(~measurement, scales = "free", ncol = 1) +
# #   #   theme_bw() +
# #   #   theme(aspect.ratio = 0.5)
# #   # ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "raw_days_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")
# #
# # #### BC ANNUAL ADJUSTED ####
# #
# # lm_raw = mod_lm_BC_year_mean #%>% dplyr::select(-year, -days, -index)
# #
# # ts_BC_df = full_join(x = lm_raw, y = df_raw) %>% dplyr::mutate(days_corrected = days+index*estimate)
# #
# # ts_BC_df_lm_corrected = ts_BC_df %>%
# #   dplyr::group_by_(.dots = c("measurement","tel")) %>%
# #   do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
# #   dplyr::filter(term == "year");
# #
# #   # ggplot(ts_BC_df, aes(x = as.Date(paste(year, "-01-01", sep = "")))) +
# #   #   geom_line(data = df_raw, aes(x = as.Date(paste(year, "-01-01", sep = ""), color = "raw"), y = days)) +
# #   #   geom_line(aes(y = days_corrected, group = measurement)) +
# #   #   geom_point(aes(y = days_corrected, fill = measurement), shape = 21, size = 3) +
# #   #   geom_smooth(aes(y = days_corrected, color = measurement), method = "lm") +
# #   #   facet_wrap(~measurement+tel, scales = "free", ncol = 2) +
# #   #   theme_bw() +
# #   #   theme(aspect.ratio = 0.5)
# #   #  ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "annual_adjusted_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")
# #
# # #### ZONES ANNUAL ORIGINAL ####
# #
# # df_raw = df_zone_year_mean
# #
# # ts_tel_df_lm_original = df_raw %>%
# #   dplyr::group_by_(.dots = c(zone_name, "measurement")) %>%
# #   do(broom::tidy(lm(days ~ year, data = ., na.action = na.omit))) %>%
# #   dplyr::filter(term == "year")
# #
# # # zone_plot(data = ts_tel_df_lm_original, -30,30,0.5)  +
# # #   facet_grid(~measurement, labeller = labeller(measurement = label_parsed)) +
# # #   labs(title = "ts_seas_tel_df_lm_corrected", x = "", y = "", fill = expression(paste("Linear Trend (d ",yr^-1,")", sep = "")))
# # # ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "original_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 10, height = 15, device = "pdf")
# #
# #
# # # SEASON ADJUSTED
# #
# # df_raw = df_zone_seas_mean
# # lm_raw = mod_lm_zone_seas_mean %>% dplyr::select(-year, -days, -index)
# #
# # ts_tel_df = full_join(x = lm_raw, y = df_raw) %>% dplyr::mutate(days_corrected = days+(index*estimate))
# #
# # ts_tel_df_lm_corrected = ts_tel_df %>%
# #   dplyr::group_by_(.dots = c(zone_name, "measurement","tel","season")) %>%
# #   do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
# #   dplyr::filter(term == "year");
# #
# # zone_plot(ts_tel_df_lm_corrected)  +
# #   facet_grid(measurement~tel+season, labeller = labeller(measurement = label_parsed)) +
# #   labs(title = "ts_seas_tel_df_lm_corrected", x = "", y = "", fill = expression(paste("Trend (d ",yr^-1,")", sep = "")))
# #
# # ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "season_adjusted_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")
# #
# #
# #
# #
# # #### 9: CHANGE OVER TIME BY zone ####
# #
# # # YEAR
# #
# # df_raw = df_zone_year_mean
# #
# # ts_tel_df_lm_original = df_raw  %>%
# #   dplyr::group_by_(.dots = c(zone_name, "measurement")) %>%
# #   do(broom::tidy(lm(days ~ year, data = .))) %>%
# #   dplyr::filter(term == "year")
# #
# # zone_plot(ts_tel_df_lm_original)  +
# #   facet_grid(~measurement, labeller = labeller(measurement = label_parsed)) +
# #   labs(title = "ts_seas_tel_df_lm_corrected", x = "", y = "", fill = expression(paste("Trend (d ",yr^-1,")", sep = "")))
# #
# # ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "raw_days_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")
# #
# # # YEAR ADJUSTED
# #
# # lm_raw = mod_lm_zone_year_mean %>% dplyr::select(-year, -days, -index)
# #
# # ts_tel_df = full_join(x = lm_raw, y = df_raw) %>% dplyr::mutate(days_corrected = days+(index*estimate))
# #
# # ts_tel_df_lm_corrected = ts_tel_df %>%
# #   dplyr::group_by_(.dots = c(zone_name, "measurement","tel")) %>%
# #   do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
# #   dplyr::filter(term == "year");
# #
# # zone_plot(ts_tel_df_lm_corrected)  +
# #   facet_grid(tel~measurement, labeller = labeller(measurement = label_parsed)) +
# #   labs(title = "ts_seas_tel_df_lm_corrected", x = "", y = "", fill = expression(paste("Trend (d ",yr^-1,")", sep = "")))
# #
# # ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "annual_adjusted_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")
# #
# #
# # # SEASON ADJUSTED
# #
# # df_raw = df_zone_seas_mean
# # lm_raw = mod_lm_zone_seas_mean %>% dplyr::select(-year, -days, -index)
# #
# # ts_tel_df = full_join(x = lm_raw, y = df_raw) %>% dplyr::mutate(days_corrected = days+(index*estimate))
# #
# # ts_tel_df_lm_corrected = ts_tel_df %>%
# #   dplyr::group_by_(.dots = c(zone_name, "measurement","tel","season")) %>%
# #   do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
# #   dplyr::filter(term == "year");
# #
# # zone_plot(ts_tel_df_lm_corrected)  +
# #   facet_grid(measurement~tel+season, labeller = labeller(measurement = label_parsed)) +
# #   labs(title = "ts_seas_tel_df_lm_corrected", x = "", y = "", fill = expression(paste("Trend (d ",yr^-1,")", sep = "")))
# #
# # ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "season_adjusted_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# # tel$date = as.Date(paste(tel$month, " ", 1, ", ", tel$orig_year, sep = ""), format = '%B %d, %Y')
# # tel$dateLabel = format(tel$date, "%Y")
# #
# # # grid.arrange(
# # ggplot(tel, aes(date)) +
# #
# #   # geom_hline(yintercept = 0.5, linetype = 2, color = "black") +
# #   # geom_hline(yintercept = -0.5, linetype = 2, color = "black") +
# #
# #   geom_line(stat = "identity", aes(y=oni), linetype = 2, size = 0.5) +
# #   geom_point(stat = "identity", aes(y=oni_event), shape = 21, fill = "grey", size = 3) +
# #
# #   geom_line(stat = "identity", aes(y=pdo), size = 0.5) +
# #   geom_point(stat = "identity", aes(y=pdo_event), shape = 22, fill = "white", size = 3) +
# #
# #
# #
# #   geom_line(data = filter(df_BC_year_mean, measurement == "SD[ON]"), aes(x = as.Date(paste(year, "-01-01", sep = "")), y = (days-mean(days))/mean(days), group = measurement))+
# #   # geom_point(data = df_BC_year_mean, aes(x = as.Date(year), y = days, color = measurement), shape = 21, size = 3) +
# #
# #   geom_hline(yintercept = 0) +
# #
# #   scale_x_date(date_breaks = "1 year",
# #                limits = c(as.Date("2002-01-01"),as.Date("2018-01-01")),
# #                labels = date_format("%Y"))+
# #   theme_few(base_size = 20) +
# #   theme(legend.position = "none", aspect.ratio = 0.3) +
# #
# #   labs(x = "", y = "ONI (?C)")
# #
# # # ,
# #
# #
# # lm(days~year, df_BC_year_mean %>% filter(measurement == "SD[ON]"))
# # lm(days~year, df_BC_year_mean %>% filter(measurement == "SD[OFF]", tel == "ONI"))
# # lm(days~year, df_BC_year_mean %>% filter(measurement == "SD[DUR]", tel == "ONI"))
# #
# # ggplot(df_BC_year_mean, aes(x = as.Date(paste(year, "-01-01", sep = "")))) +
# #   # geom_rect(data = filter(filter(tel, !is.na(pdo_event)), pdo_event > 0), aes(xmin = as.Date(paste(year, "-01-01", sep = "")), xmax = as.Date(paste(year, "-12-31", sep = "")), ymin = 0,ymax = 20), fill = "red") +
# #   # geom_rect(data = filter(filter(tel, !is.na(pdo_event)), pdo_event < 0), aes(xmin = as.Date(paste(year, "-01-01", sep = "")), xmax = as.Date(paste(year, "-12-31", sep = "")), ymin = 0,ymax = 20), fill = "blue") +
# #   # geom_rect(data = filter(filter(tel, !is.na(oni_event)), oni_event > 0), aes(xmin = as.Date(paste(year, "-01-01", sep = "")), xmax = as.Date(paste(year, "-12-31", sep = "")), ymin = 20,ymax = 40), fill = "red") +
# #   # geom_rect(data = filter(filter(tel, !is.na(oni_event)), oni_event < 0), aes(xmin = as.Date(paste(year, "-01-01", sep = "")), xmax = as.Date(paste(year, "-12-31", sep = "")), ymin = 20,ymax = 40), fill = "blue") +
# #   geom_line(aes(y = days, group = measurement)) +
# #   geom_point(aes(y = days, fill = measurement), shape = 21, size = 3) +
# #   geom_smooth(aes(y = days, color = measurement), method = "lm") +
# #   scale_y_continuous(limits = c(0,360)) +
# #   theme_bw() +
# #   theme(aspect.ratio = 0.5)
# #
# # # ,
# # # ncol =1)
# #
# #
# #
# #
# #
# # #
# # # names(df)
# # # sum = df %>%
# # #   group_by(HYDROLOGICZONE_NAME, grZ, grLAT, grLON, typeS, year) %>%
# # #   dplyr::select(SD_ON,SD_OFF,SD_INT) %>%
# # #   summarise_all(mean)
# # #
# # #
# # # ggplot(sum, aes(grLON, grLAT)) +
# # #   geom_point(aes(color = SD_INT)) +
# # #   # geom_smooth() +
# # #   scale_color_viridis() +
# # #   theme(aspect.ratio = 1) +
# # #   facet_grid(grZ~typeS)
# # #
# # # # Run linear model on raw data
# # # sum %>%
# # #   dplyr::group_by_(.dots = c("HYDROLOGICZONE_NAME","grZ","grLAT","grLON","typeS")) %>%
# # #   dplyr::select(year, SD_INT) %>%
# # #   dplyr::rename(X = year, Y = SD_INT) %>%
# # #   dplyr::summarise(estimate = cor.test(Y, X, method = "spearman", exact=FALSE)$estimate,
# # #                    p.value = cor.test(Y, X, method = "spearman", exact=FALSE)$p.value)
# # #
# # # # Run linear model on raw data
# # # temp = sum %>%
# # #   dplyr::group_by_(.dots = c("grLON","grLAT", "grZ")) %>%
# # #   do(broom::tidy(lm(SD_OFF~year, data = ., na.action = na.omit))) %>%
# # #   dplyr::filter(term != "(Intercept)") %>%
# # #   filter(p.value <= 0.05)
# # # ggplot(temp, aes(grLON,grLAT)) + geom_point(aes(color = estimate))
# # #

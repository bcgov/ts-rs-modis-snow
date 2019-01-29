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



#### 9: CHANGE OVER TIME BY zone ####

# YEAR

df_raw = df_zone_year_mean

ts_tel_df_lm_original = df_raw  %>%
  dplyr::group_by_(.dots = c(zone_name, "measurement")) %>%
  do(broom::tidy(lm(days ~ year, data = .))) %>%
  dplyr::filter(term == "year")

zone_plot(ts_tel_df_lm_original)  +
  facet_grid(~measurement, labeller = labeller(measurement = label_parsed)) +
  labs(title = "ts_seas_tel_df_lm_corrected", x = "", y = "", fill = expression(paste("Trend (d ",yr^-1,")", sep = "")))

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "raw_days_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")

# YEAR ADJUSTED

lm_raw = mod_lm_zone_year_mean %>% dplyr::select(-year, -ELEV_MEAN, -ELEV_STDV, -days, -index)

ts_tel_df = full_join(x = lm_raw, y = df_raw) %>% dplyr::mutate(days_corrected = days+(index*estimate))

ts_tel_df_lm_corrected = ts_tel_df %>%
  dplyr::group_by_(.dots = c(zone_name, "measurement","tel")) %>%
  do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
  dplyr::filter(term == "year");

zone_plot(ts_tel_df_lm_corrected)  +
  facet_grid(tel~measurement, labeller = labeller(measurement = label_parsed)) +
  labs(title = "ts_seas_tel_df_lm_corrected", x = "", y = "", fill = expression(paste("Trend (d ",yr^-1,")", sep = "")))

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "annual_adjusted_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")


# SEASON ADJUSTED

df_raw = df_zone_seas_mean
lm_raw = mod_lm_zone_seas_mean %>% dplyr::select(-year, -ELEV_MEAN, -ELEV_STDV, -days, -index)

ts_tel_df = full_join(x = lm_raw, y = df_raw) %>% dplyr::mutate(days_corrected = days+(index*estimate))

ts_tel_df_lm_corrected = ts_tel_df %>%
  dplyr::group_by_(.dots = c(zone_name, "measurement","tel","season")) %>%
  do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
  dplyr::filter(term == "year");

zone_plot(ts_tel_df_lm_corrected)  +
  facet_grid(measurement~tel+season, labeller = labeller(measurement = label_parsed)) +
  labs(title = "ts_seas_tel_df_lm_corrected", x = "", y = "", fill = expression(paste("Trend (d ",yr^-1,")", sep = "")))

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "season_adjusted_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")










tel$date = as.Date(paste(tel$month, " ", 1, ", ", tel$orig_year, sep = ""), format = '%B %d, %Y')
tel$dateLabel = format(tel$date, "%Y")

# grid.arrange(
ggplot(tel, aes(date)) +

  # geom_hline(yintercept = 0.5, linetype = 2, color = "black") +
  # geom_hline(yintercept = -0.5, linetype = 2, color = "black") +

  geom_line(stat = "identity", aes(y=oni), linetype = 2, size = 0.5) +
  geom_point(stat = "identity", aes(y=oni_event), shape = 21, fill = "grey", size = 3) +

  geom_line(stat = "identity", aes(y=pdo), size = 0.5) +
  geom_point(stat = "identity", aes(y=pdo_event), shape = 22, fill = "white", size = 3) +



  geom_line(data = filter(df_BC_year_mean, measurement == "SD[ON]"), aes(x = as.Date(paste(year, "-01-01", sep = "")), y = (days-mean(days))/mean(days), group = measurement))+
  # geom_point(data = df_BC_year_mean, aes(x = as.Date(year), y = days, color = measurement), shape = 21, size = 3) +

  geom_hline(yintercept = 0) +

  scale_x_date(date_breaks = "1 year",
               limits = c(as.Date("2002-01-01"),as.Date("2018-01-01")),
               labels = date_format("%Y"))+
  theme_few(base_size = 20) +
  theme(legend.position = "none", aspect.ratio = 0.3) +

  labs(x = "", y = "ONI (?C)")

# ,



ggplot(df_BC_year_mean, aes(x = as.Date(paste(year, "-01-01", sep = "")))) +
  geom_rect(data = filter(filter(tel, !is.na(pdo_event)), pdo_event > 0), aes(xmin = as.Date(paste(year, "-01-01", sep = "")), xmax = as.Date(paste(year, "-12-31", sep = "")), ymin = 0,ymax = 20), fill = "red") +
  geom_rect(data = filter(filter(tel, !is.na(pdo_event)), pdo_event < 0), aes(xmin = as.Date(paste(year, "-01-01", sep = "")), xmax = as.Date(paste(year, "-12-31", sep = "")), ymin = 0,ymax = 20), fill = "blue") +
  geom_rect(data = filter(filter(tel, !is.na(oni_event)), oni_event > 0), aes(xmin = as.Date(paste(year, "-01-01", sep = "")), xmax = as.Date(paste(year, "-12-31", sep = "")), ymin = 20,ymax = 40), fill = "red") +
  geom_rect(data = filter(filter(tel, !is.na(oni_event)), oni_event < 0), aes(xmin = as.Date(paste(year, "-01-01", sep = "")), xmax = as.Date(paste(year, "-12-31", sep = "")), ymin = 20,ymax = 40), fill = "blue") +
  geom_line(aes(y = days, group = measurement)) +
  geom_point(aes(y = days, color = measurement), shape = 21, size = 3) +
  scale_y_continuous(limits = c(0,360))

# ,
# ncol =1)

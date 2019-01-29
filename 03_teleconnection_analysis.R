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

#### 5. CALC BY BC == LM AND COR (TEL/MSM) ####

# SUMMARISE MEAN VALUES PER YEAR

df_BC_year_mean  = df_tel_year_all_m %>%
  group_by(year, measurement) %>%
  select_(.dots = c("days", tel_list)) %>%
  summarise_all(mean, na.rm = T) %>%
  gather(key = tel, value = index, tel_list)

df_BC_year_mean$tel = ordered(df_BC_year_mean$tel, levels = tel_list, labels = tel_name)

# SUMMARISE MEAN VALUES PER YEAR+SEASON

df_BC_seas_mean = df_tel_seas_all_m %>%
  group_by(year, measurement, season) %>%
  select_(.dots =  c("days", tel_list)) %>%
  summarise_all(mean, na.rm = T) %>%
  gather(key = tel, value = index, tel_list)

df_BC_seas_mean$tel = ordered(df_BC_seas_mean$tel, levels = tel_list, labels = tel_name)

# ANNUAL ANALYSIS

mod_lm_BC_year_mean  = lm_iter(df_BC_year_mean, groups = c("measurement","tel"), Y = "days", X = "index", textName = "mod_lm_BC_year_mean")
mod_cor_BC_year_mean = cor_iter(df_BC_year_mean, groups = c("measurement","tel"), Y = "days", X = "index", textName = "mod_cor_BC_year_mean")

# SEASONAL ANALYSIS

mod_lm_BC_seas_mean  = lm_iter(df_BC_seas_mean, groups = c("measurement","tel","season"), Y = "days", X = "index", textName = "mod_lm_BC_seas_mean")
mod_cor_BC_seas_mean = cor_iter(df_BC_seas_mean, groups = c("measurement","tel","season"), Y = "days", X = "index", textName = "mod_cor_BC_seas_mean")

# PLOT LM COEFFICIENTS

ggplot(mod_lm_BC_seas_mean, aes(group = season)) +
  geom_vline(xintercept=c(1.5,2.5,3.5), linetype=2, color = "gray")+
  geom_errorbar(aes(x = measurement, ymin = estimate - error_mean, ymax = estimate + error_mean), position = position_dodge(width=1)) +
  geom_point(aes(x = measurement, y = estimate, fill = cut(p.value, breaks = c(-Inf, 0.05, Inf), labels = c("<=0.05", ">0.05")), shape = season, group = season), position=position_dodge(width=1), size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("dark grey","white")) +
  scale_shape_manual(values = c(21,22,23,24)) +
  scale_x_discrete(labels = parse(text = levels(mod_lm_BC_seas_mean$measurement))) +
  facet_wrap(~tel, ncol = 7) +
  theme_few(base_size = 25) +
  guides(fill = guide_legend(override.aes = list(shape=21, size = 3)), shape = guide_legend(override.aes = list(size = 3)))+
  theme(aspect.ratio = 1.6) +
  labs(x = "", y = expression(paste("Trend (d ?",C^-1,")", sep = "")), fill = "Significance", shape = "Season")

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "mod_lm_BC_seas_mean_", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 12, height = 12, device = "pdf")

# PLOT COR COEFFICIENTS
ggplot(mod_cor_BC_seas_mean, aes(group = season)) +
  geom_vline(xintercept=c(1.5,2.5,3.5), linetype=2, color = "gray")+
  geom_errorbar(aes(x = measurement, ymin = estimate - error_mean, ymax = estimate + error_mean), position = position_dodge(width=1)) +
  geom_point(aes(x = measurement, y = estimate, fill = cut(p.value, breaks = c(-Inf, 0.05, Inf), labels = c("<=0.05", ">0.05")), shape = season, group = season), position=position_dodge(width=1), size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("dark grey","white")) +
  scale_shape_manual(values = c(21,22,23,24)) +
  scale_x_discrete(labels = parse(text = levels(mod_lm_BC_seas_mean$measurement))) +
  facet_wrap(~tel, ncol = 7) +
  theme_few(base_size = 20) +
  guides(fill = guide_legend(override.aes = list(shape=21, size = 3)), shape = guide_legend(override.aes = list(size = 3)))+
  theme(aspect.ratio = 1.6) +
  labs(x = "", y = "Spearman Correlation Coefficient", fill = "Significance", shape = "Season")

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "mod_cor_BC_seas_mean_", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 12, height = 12, device = "pdf")

#### 6: CALC BY zone == LM AND COR (TEL/MSM) ####

# SUMMARISE MEAN VALUES PER YEAR

df_zone_year_mean = df_tel_year_zone_m %>%
  group_by_(.dots = c("year", "measurement", zone_name)) %>%
  summarise_all(mean, na.rm = T) %>%
  gather(key = tel, value = index, tel_list)

df_zone_year_mean$tel = ordered(df_zone_year_mean$tel, levels = tel_list, labels = tel_name)

# SUMMARISE MEAN VALUES PER YEAR + SEASON

df_zone_seas_mean = df_tel_seas_zone_m %>%
  group_by_(.dots = c("year", "season", "measurement", zone_name)) %>%
  summarise_all(mean, na.rm = T) %>%
  gather(key = tel, value = index, tel_list)

df_zone_seas_mean$tel = ordered(df_zone_seas_mean$tel, levels = tel_list, labels = tel_name)

# ANNUAL ANALYSIS

mod_lm_zone_year_mean = lm_iter(df_zone_year_mean, groups = c("measurement","tel",zone_name), Y = "days", X = "index", textName = "mod_lm_zone_year_mean")
mod_lm_zone_year_mean = merge(mod_lm_zone_year_mean, filter(df_zone_year_mean, year == 2002), by = c("measurement","tel",zone_name))

mod_cor_zone_year_mean = cor_iter(df_zone_year_mean, groups = c("measurement","tel",zone_name), Y = "days", X = "index", textName = "mod_cor_zone_year_mean")
mod_cor_zone_year_mean = merge(mod_cor_zone_year_mean, filter(df_zone_year_mean, year == 2002), by = c("measurement","tel",zone_name))

# SEASONAL ANALYSIS

mod_lm_zone_seas_mean  = lm_iter(df_zone_seas_mean, groups = c("measurement","tel","season",zone_name), Y = "days", X = "index", textName = "mod_lm_zone_seas_mean")
mod_lm_zone_seas_mean = merge(mod_lm_zone_seas_mean, filter(df_zone_seas_mean, year == 2002), by = c(zone_name, "measurement","tel","season"))

mod_cor_zone_seas_mean = cor_iter(df_zone_seas_mean, groups = c("measurement","tel","season",zone_name), Y = "days", X = "index", textName = "mod_cor_zone_seas_mean")
mod_cor_zone_seas_mean = merge(mod_cor_zone_seas_mean, filter(df_zone_seas_mean, year == 2002), by = c(zone_name, "measurement","tel","season"))


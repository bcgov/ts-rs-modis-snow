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

#### 7: PLOT BY zone == LM AND COR (TEL/MSM) ####

# Colors
colors = c("black","darkred","white","dodgerblue4","dodgerblue")

# General plotting function
zone_plot = function(data) {
  # data = mod_lm_zone_year_mean
  sig = merge(zones, filter(data, p.value <= 0.05), by = zone_name)
  not = merge(zones, filter(data, p.value > 0.05), by = zone_name)
  max = max(sig$estimate)
  min = min(sig$estimate)

  # zone_no = cbind(data.frame(coordinates(as(sig, "Spatial"))), sig$p.value)
  # names(zone_no) = c("x","y","sig")

  ggplot() +
    geom_sf(data = sig, aes(fill = estimate)) +
    geom_sf(data = not, fill = "grey") +
    # geom_point(data = zone_no, aes(x, y)) +
    # geom_text_repel(data = zone_no,aes(x = x, y = y, label = no), size = 2) +
    scale_fill_gradientn(colors=colors,
                         values=rescale(c(min,0,max)),
                         limits=c(min,max)) +
    guides(fill = guide_colourbar(barwidth = 20, barheight = 1, direction = "horizontal", title.position = "top", frame.colour = "black", ticks.colour = "black")) +
    theme_few(base_size = 15) +
    theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), aspect.ratio = 1, panel.grid = element_line(linetype = 3, color = "light gray"), legend.position = "bottom", legend.title.align = 0.5)}

# mod_lm_zone_year_mean
zone_plot(mod_lm_zone_year_mean) +
  facet_grid(tel~measurement, labeller = labeller(measurement = label_parsed))   +
  labs(title = "mod_lm_zone_year_mean", x = "", y = "", fill = expression(paste("Trend (d ?",C^-1,")", sep = "")))

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "mod_lm_zone_year_mean_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 12, height = 12, device = "pdf")

# mod_lm_zone_seas_mean
zone_plot(mod_lm_zone_seas_mean) +
  facet_grid(tel+season~measurement, labeller = labeller(measurement = label_parsed))   +
  labs(title = "mod_lm_zone_seas_mean", x = "", y = "", fill = expression(paste("Trend (d ?",C^-1,")", sep = "")))

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "mod_lm_zone_seas_mean_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 12, height = 18, device = "pdf")

# mod_cor_zone_year_mean
zone_plot(mod_cor_zone_year_mean) +
  facet_grid(tel~measurement, labeller = labeller(measurement = label_parsed))   +
  labs(title = "mod_cor_zone_year_mean", x = "", y = "", fill = "Spearman Correlation Coefficient")

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "mod_cor_zone_year_mean_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 12, height = 12, device = "pdf")

# mod_cor_zone_seas_mean
zone_plot(mod_cor_zone_seas_mean) +
  facet_grid(tel+season~measurement, labeller = labeller(measurement = label_parsed))   +
  labs(title = "mod_cor_zone_seas_mean", x = "", y = "", fill = "Spearman Correlation Coefficient")

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "mod_cor_zone_seas_mean_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 12, height = 18, device = "pdf")

#### 8: PLOT MOST IMPORTANT TEL FROM ANNUAL DATA ####

# From COR
most_imp_year_cor = mod_cor_zone_year_mean %>%
  mutate(sig = p.value <= 0.05) %>%
  group_by_(.dots = c(zone_name, "measurement")) %>%
  filter(abs(estimate) == max(abs(estimate)))

ggplot() +
  geom_sf(data = filter(merge(zones, most_imp_year_cor,by=zone_name), sig == T), aes(fill = tel)) +
  geom_sf(data = filter(merge(zones, most_imp_year_cor,by=zone_name), sig == F), fill = "grey") +
  facet_grid(~measurement, labeller = labeller(measurement = label_parsed)) +
  theme_few(base_size = 15) +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), aspect.ratio = 1, panel.grid = element_line(linetype = 3, color = "light gray")) +
  labs(fill = "Season", title = "most_imp_year_cor")

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "most_imp_year_cor_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 12, height = 12, device = "pdf")

# PLOT MOST IMPORTANT SEASON FROM SEASONAL DATA

# From COR
most_imp_seas_cor = mod_cor_zone_seas_mean %>%
  mutate(sig = p.value <= 0.05) %>%
  group_by_(.dots = c(zone_name, "measurement", "tel")) %>%
  filter(abs(estimate) == max(abs(estimate)))

ggplot() +
  geom_sf(data = filter(merge(zones,most_imp_seas_cor,by=zone_name), sig == T), aes(fill = season)) +
  geom_sf(data = filter(merge(zones,most_imp_seas_cor,by=zone_name), sig == F), fill = "grey") +
  facet_grid(tel~measurement, labeller = labeller(measurement = label_parsed)) +
  theme_few(base_size = 15) +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), aspect.ratio = 1, panel.grid = element_line(linetype = 3, color = "light gray")) +
  labs(fill = "Season", title = "most_imp_seas_cor")

ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "most_imp_seas_cor_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 12, height = 12, device = "pdf")


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



#### 10: PLOT Terrain (all data) ####

id_raw = df_tel_seas_all_m %>%
  dplyr::group_by_(.dots = c(zone_name,"id","elev","measurement","season")) %>%
  do(broom::tidy(lm(days~oni, data = ., na.action = na.omit))) %>%
  dplyr::filter(term != "(Intercept)")

id_raw = id_raw %>% filter(p.value <= 0.05) %>% filter(!is.na(p.value))

id_raw_pdo = df_tel_seas_all_m %>%
  dplyr::group_by_(.dots = c(zone_name,"id","elev","measurement","season")) %>%
  do(broom::tidy(lm(days~pdo, data = ., na.action = na.omit))) %>%
  dplyr::filter(term != "(Intercept)")

id_raw_pdo = id_raw_pdo %>% filter(p.value <= 0.05) %>% filter(!is.na(p.value))

both = grid.arrange(
  ggplot(id_raw, aes(cut(elev, seq(0, 5000, 500), labels = seq(0, 5000-500, 500)), estimate)) +
    geom_boxplot(aes(fill =measurement), show.legend = F) +
    geom_hline(yintercept = 0) +
    facet_wrap(~ season, scales = "free", ncol = 1) +
    coord_flip() +
    theme_few(base_size = 20) +
    theme(aspect.ratio = 1) +
    labs(title = "ONI", y = "LLS", x = "Elevation (m asl)")
  ,
  ggplot(id_raw_pdo, aes(cut(elev, seq(0, 5000, 500), labels = seq(0, 5000-500, 500)), estimate)) +
    geom_boxplot(aes(fill =measurement)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~ season, scales = "free", ncol = 1) +
    coord_flip() +
    theme_few(base_size = 20) +
    theme(aspect.ratio = 1)  +
    labs(title = "PDO", y = "LLS", x = "Elevation (m asl)"),
  ncol = 2)

ggsave(plot = both, filename = paste(getwd(),"/5_Draft/Figures/", "elevation_boxplot_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 40, height = 25, device = "pdf")


# ggplot(mod_lm_zone_seas_mean, aes(ELEV_MEAN, estimate)) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   geom_point(shape = 21, aes(fill = season), size = 3) +
#   scale_color_colorblind() +
#   scale_fill_colorblind() +
#   facet_wrap(tel~measurement, ncol = 3, scales = "free_y") +
#   geom_smooth(method = "lm", se = F, aes(color = season)) +
#   theme_few() + theme(aspect.ratio = 1) +
#   labs(title = "mod_lm_zone_seas_mean", x = "Elevation in metres", y = "coefficient")
#   ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "elev_mod_lm_zone_seas_mean_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")
#
# ggplot(mod_cor_zone_seas_mean, aes(ELEV_MEAN, estimate)) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   geom_point(shape = 21, aes(fill = season), size = 3) +
#   scale_color_colorblind() +
#   scale_fill_colorblind() +
#   facet_wrap(tel~measurement, ncol = 3, scales = "free_y") +
#   geom_smooth(method = "lm", se = F, aes(color = season)) +
#   theme_few() + theme(aspect.ratio = 1) +
#   labs(title = "mod_cor_zone_seas_mean", x = "Elevation in metres", y = "coefficient")
#   ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "elev_mod_cor_zone_seas_mean_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 15, device = "pdf")

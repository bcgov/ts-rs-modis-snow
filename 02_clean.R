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


#### 2: CLEAN  ####

  # Filter Elevation and Water Mask, Rename snow measurements, and group Z, asp, slp, and XY, also add unique ID
    df = df %>%
      filter(elev > 0 & watr_msk == 0) %>%
      dplyr::rename(SD_ON = int_start) %>%
      dplyr::rename(SD_OFF = int_end) %>%
      dplyr::rename(SD_INT = intp_dur) %>%
      # dplyr::rename(SD_SCI = sci) %>%
      mutate(grZ   = as.numeric(as.character(cut(elev, seq(0, 5000, 500), labels = seq(0, 5000-500, 500)))),
             grSlp = as.numeric(as.character(cut(slp, seq(0, 55, 5), labels = seq(0, 55-5, 5)))),
             grN   = as.numeric(as.character(cut(n, seq(-1, 1, 0.2), labels = seq(-1, 1-0.2, 0.2)))),
             grE   = as.numeric(as.character(cut(e, seq(-1, 1, 0.2), labels = seq(-1, 1-0.2, 0.2)))),
             grLAT = as.numeric(as.character(cut(lat, seq(45, 60, 0.2), labels = seq(45, 60-0.2, 0.2)))),
             grLON = as.numeric(as.character(cut(lon, seq(-150, -110, 0.2), labels = seq(-150, -110-0.2, 0.2)))),
             UID = abs((lat*100) * (lon*100)))
  # Optional code to change SD_ON and SD_OFF from "days since 1-Sep to Julian days
    # SD_ON  = as.numeric(format(as.Date(paste(year, "-09-01", sep = ""))+SD_ON, "%j")),
    # SD_OFF = as.numeric(format(as.Date(paste(year, "-09-01", sep = ""))+SD_OFF, "%j")))

  # Counters
    n_iterations = 10
    n = dim(df)[1]
    ns = paste(n,n_iterations, sep="_")

#### 3: BCMAPS ####

  # Load BC POLY
    bc = as(st_geometry(get_layer("bc_bound", class = "sf")), "Spatial")
    bc = transform_bc_albers(bc)

  # Transform DF to SpatialPointsDataFrame and project as ALBERS
    spdf = transform_bc_albers(SpatialPointsDataFrame(coords = df[,c("lon","lat")], data = df, proj4string = CRS("+init=epsg:4326")))

  # Clip DF to BC
    spdf = spdf[!is.na(over(spdf,bc)),]

  # Load HYDROZONES
    zones      = hydrozones()
    zone_name  = "HYDROLOGICZONE_NAME"
    zone_exp   = paste("HZ",ns, sep="_")

  # Transform zones
    zones_alb  = transform_bc_albers(as(zones, "Spatial"))

  # Extract EcoregionCode by point
    zones_sel <- over(spdf, zones_alb[,zone_name])

  # flip back to DF
    df = as.data.frame(spdf)
    df = cbind(df, zones_sel)
    df = df %>% dplyr::filter(!is.na(zone_name))

  # CLEAN ENVIRONMENT
    remove(zones_sel, spdf, zones_alb, bc)

#### 4: TELECONNECTIONS ####

  # Conversions
    tel_list = c("oni","pdo") #,"oni_plus_pdo") #,"oni_event","pdo_event","oni_event_plus_pdo_event","oni_event_plus_pdo_event_both")
    tel_name = c("ONI","PDO") #,"ONI+PDO") #,"ONIe","PDOe","ONIe+PDOe","ONIe+PDOe_b")

  # Conversions
    msm_list = c("SD_ON","SD_OFF","SD_INT")#,"SD_SCI")
    msm_name = c("SD[ON]","SD[OFF]","SD[DUR]")#,"SD[SCI]")

  # Mean TEL by year
    tel_by_year = tel %>%
      dplyr::group_by(year) %>%
      dplyr::select_(.dots = tel_list) %>%
      dplyr::filter(year >= 2002) %>%
      dplyr::summarise_all(mean, na.rm = T)

  # Mean TEL by year, season
    tel_by_seas = tel %>%
      dplyr::group_by(year, season) %>%
      dplyr::select_(.dots = tel_list) %>%
      dplyr::filter(year >= 2002) %>%
      dplyr::summarise_all(mean, na.rm = T)

  # Order Seasons
    tel_by_seas$season =
      ordered(tel_by_seas$season, levels = c("Spring","Summer","Fall","Winter"))

  # Summarise by zone
    df_zone = df %>%
      dplyr::group_by_(.dots = c(zone_name, "year")) %>%
      dplyr::summarise(
        SD_ON     = mean(SD_ON, na.rm = T),
        SD_OFF    = mean(SD_OFF, na.rm = T),
        SD_INT    = mean(SD_INT, na.rm = T),
        # SD_SCI    = mean(SD_SCI, na.rm = T),
        ELEV_MEAN = mean(elev, na.rm =T),
        ELEV_STDV = sd(elev, na.rm =T))

  # MERGE tel - AND - dfM - BY - year
    tel_by_year_all   = merge(df, tel_by_year, by="year")
    tel_by_year_zone  = merge(df_zone, tel_by_year, by="year")

  # MERGE tel_seas - AND - dfM - BY - year
    tel_by_seas_all   = merge(df, tel_by_seas, by=c("year"))
    tel_by_seas_zone  = merge(df_zone, tel_by_seas, by="year")

  # DFs to DATs
    df_all_tel_m       = data.table(melt(df,               measure.vars = msm_list, variable.name = "measurement", value.name = "days"))
    df_tel_year_all_m  = data.table(melt(tel_by_year_all,  measure.vars = msm_list, variable.name = "measurement", value.name = "days"))
    df_tel_seas_all_m  = data.table(melt(tel_by_seas_all,  measure.vars = msm_list, variable.name = "measurement", value.name = "days"))
    df_tel_year_zone_m = data.table(melt(tel_by_year_zone, measure.vars = msm_list, variable.name = "measurement", value.name = "days"))
    df_tel_seas_zone_m = data.table(melt(tel_by_seas_zone, measure.vars = msm_list, variable.name = "measurement", value.name = "days"))

  # ORDER FACTORS
    df_all_tel_m$measurement       = ordered(df_all_tel_m$measurement, levels = msm_list, labels = msm_name)
    df_tel_year_all_m$measurement  = ordered(df_tel_year_all_m$measurement, levels = msm_list, labels = msm_name)
    df_tel_seas_all_m$measurement  = ordered(df_tel_seas_all_m$measurement, levels = msm_list, labels = msm_name)
    df_tel_year_zone_m$measurement = ordered(df_tel_year_zone_m$measurement, levels = msm_list, labels = msm_name)
    df_tel_seas_zone_m$measurement = ordered(df_tel_seas_zone_m$measurement, levels = msm_list, labels = msm_name)

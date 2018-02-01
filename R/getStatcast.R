library(tidyverse)
library(baseballr)
library(lubridate)

#Download Statcast Data from Baseball Savant (do it once and save)
season2016 = c(seq(as.Date('2016-04-01'),as.Date('2016-10-01'), by = 'week'),as.Date('2016-10-03'))
statcast_data = lapply(2:length(season2016), function(i) {
    scrape_statcast_savant_batter_all(start_date = as.character(season2016[i-1]),
                                      end_date =as.character(season2016[i]-1))
})

factor_to_num = function(x) as.numeric(as.character(x))

full_statcast = do.call(rbind, statcast_data)
full_statcast = mutate_at(full_statcast, vars(release_speed,release_pos_z, zone,hit_location,
                                              pfx_x,pfx_z,plate_x,plate_z,hc_x,hc_y, pos2_person_id,vx0,vy0,vz0,ax,ay,az,sz_top,sz_bot,
                                              pos1_person_id,pos3_person_id,pos4_person_id,pos5_person_id,pos6_person_id,pos7_person_id,pos8_person_id,pos9_person_id,
                                              release_pos_y,woba_value,woba_denom,babip_value,iso_value,launch_speed_angle),
                          funs(factor_to_num))

saveRDS(full_statcast, 'data/statcast.2016.RDS')

#Download 2017
season2017 = c(seq(as.Date('2017-04-01'),as.Date('2017-10-01'), by = 'week'),as.Date('2017-10-02'))
statcast_data = lapply(2:length(season2017), function(i) {
    scrape_statcast_savant_batter_all(start_date = as.character(season2017[i-1]),
                                      end_date =as.character(season2017[i]-1))
})

full_statcast = do.call(rbind, statcast_data)
full_statcast = mutate_at(full_statcast, vars(release_speed,release_pos_z, zone,hit_location,
                                              pfx_x,pfx_z,plate_x,plate_z,hc_x,hc_y, pos2_person_id,vx0,vy0,vz0,ax,ay,az,sz_top,sz_bot,
                                              pos1_person_id,pos3_person_id,pos4_person_id,pos5_person_id,pos6_person_id,pos7_person_id,pos8_person_id,pos9_person_id,
                                              release_pos_y,woba_value,woba_denom,babip_value,iso_value,launch_speed_angle),
                          funs(factor_to_num))

saveRDS(full_statcast, 'data/statcast.2017.RDS')

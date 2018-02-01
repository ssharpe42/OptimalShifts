
#Function to 
# 1. add common variables (base states etc)
# 2. filter out home runs and balls not in play (or have no x,y or launch data)
# Some of this methodology is copied from openWAR: https://github.com/beanumber/openWAR
clean_bip_data = function(df){

    df = mutate(df, 
                #base state
                bases = paste0(as.integer(!is.na(on_1b)),
                               as.integer(!is.na(on_2b)),
                               as.integer(!is.na(on_3b))))%>%
        #rename outs
        rename(outs = outs_when_up) %>%
        #common use fields (openWAR)
        mutate(BIP = (events!='home_run'| grepl('inside-the-park', des)) &
                    grepl('hit_into_play',description) &
                    !is.na(hc_x) & !is.na(hc_y) & !is.na(launch_angle) & !is.na(launch_speed),
               HIT = events %in% c('single','double','triple','home_run'),
               OUT = !HIT) %>%
        #only keep BIP
        filter(BIP)
    
    
    # reorient coordinates for home plate to be (0,0)
    # set distance from home to 2B
    scale = sqrt(90^2 + 90^2)/51
    df = df %>%
        mutate(x = hc_x - 125,
               y = 199 - hc_y,
               r = scale * sqrt(x^2 + y^2), 
               theta = atan2(y, x), 
               x = r * cos(theta), 
               y = r * sin(theta),
               hit_dist = sqrt(x^2+y^2))
}

assign_positions = function(df){
    
    #Label the fielder (methodology from openWAR)
    
    #field outs
    df = mutate(df,
                pos = ifelse(events == 'field_out' & str_count(des, " (flies|lines|pops|grounds) out( sharply| softly)?(, | ,| to )pitcher"), 'P', NA),
                pos = ifelse(events == 'field_out' & str_count(des, " (flies|lines|pops|grounds) out( sharply| softly)?(, | ,| to )catcher"), 'C', pos),
                pos = ifelse(events == 'field_out' & str_count(des, " (flies|lines|pops|grounds) out( sharply| softly)?(, | ,| to )first baseman"), '1B', pos),
                pos = ifelse(events == 'field_out' & str_count(des, " (flies|lines|pops|grounds) out( sharply| softly)?(, | ,| to )second baseman"), '2B', pos),
                pos = ifelse(events == 'field_out' & str_count(des, " (flies|lines|pops|grounds) out( sharply| softly)?(, | ,| to )third baseman"), '3B', pos),
                pos = ifelse(events == 'field_out' & str_count(des, " (flies|lines|pops|grounds) out( sharply| softly)?(, | ,| to )shortstop"),  'SS', pos),
                pos = ifelse(events == 'field_out' & str_count(des, " (flies|lines|pops|grounds) out( sharply| softly)?(, | ,| to )left fielder"), 'LF', pos),
                pos = ifelse(events == 'field_out' & str_count(des, " (flies|lines|pops|grounds) out( sharply| softly)?(, | ,| to )center fielder"),  'CF', pos),
                pos = ifelse(events == 'field_out' & str_count(des, " (flies|lines|pops|grounds) out( sharply| softly)?(, | ,| to )right fielder"), 'RF', pos))
    
    #hits
    df = mutate(df,
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to pitcher"), 'P', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to catcher"), 'C', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to (deep |shallow )?first base"), '1B', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to (deep |shallow )?second base"), '2B', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to (deep |shallow )?third base"), '3B', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to (deep |shallow )?shortstop"),  'SS', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to (deep |shallow )?left field"), 'LF', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to (deep |shallow )?center field"),  'CF', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to (deep |shallow )?right field"), 'RF', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to left-center"), 'LF-CF', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(bunt )?(ground ball|line drive|fly ball|pop up) to right-center"), 'RF-CF', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(line drive|fly ball) down the right-field line"), 'RF', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(line drive|fly ball) down the left-field line"), 'LF', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(ground ball) down the left-field line"), 'LF-3B', pos),
                pos = ifelse(HIT & str_count(des, "on a (sharp |soft )?(ground ball) down the right-field line"), 'RF-1B', pos))
    
    
    #forceout
    df = mutate(df,
                pos = ifelse(events == 'force_out' & str_count(des, " (flies|lines|pops|grounds|bunts)( sharply| softly)? into a force out,( fielded by)? pitcher"), 'P', pos),
                pos = ifelse(events == 'force_out' & str_count(des, " (flies|lines|pops|grounds|bunts)( sharply| softly)? into a force out,( fielded by)? catcher"), 'C', pos),
                pos = ifelse(events == 'force_out' & str_count(des, " (flies|lines|pops|grounds|bunts)( sharply| softly)? into a force out,( fielded by)? first baseman"), '1B', pos),
                pos = ifelse(events == 'force_out' & str_count(des, " (flies|lines|pops|grounds|bunts)( sharply| softly)? into a force out,( fielded by)? second baseman"), '2B', pos),
                pos = ifelse(events == 'force_out' & str_count(des, " (flies|lines|pops|grounds|bunts)( sharply| softly)? into a force out,( fielded by)? third baseman"), '3B', pos),
                pos = ifelse(events == 'force_out' & str_count(des, " (flies|lines|pops|grounds|bunts)( sharply| softly)? into a force out,( fielded by)? shortstop"), 'SS', pos),
                pos = ifelse(events == 'force_out' & str_count(des, " (flies|lines|pops|grounds|bunts)( sharply| softly)? into a force out,( fielded by)? right fielder"), 'RF', pos),
                pos = ifelse(events == 'force_out' & str_count(des, " (flies|lines|pops|grounds|bunts)( sharply| softly)? into a force out,( fielded by)? left fielder"), 'LF', pos),
                pos = ifelse(events == 'force_out' & str_count(des, " (flies|lines|pops|grounds|bunts)( sharply| softly)? into a force out,( fielded by)? center fielder"), 'CF', pos))
    
    #double/triple play
    df = mutate(df,
                pos = ifelse(grepl('double_play|triple_play',events) & str_count(des, " (grounds|ground bunts|pops|lines|flies)( sharply| softly)? into a(n unassisted| sacrifice)? (double|triple) play, pitcher"), 'P', pos),
                pos = ifelse(grepl('double_play|triple_play',events)& str_count(des, " (grounds|ground bunts|pops|lines|flies)( sharply| softly)? into a(n unassisted| sacrifice)? (double|triple) play, catcher"), 'C', pos),
                pos = ifelse(grepl('double_play|triple_play',events) & str_count(des, " (grounds|ground bunts|pops|lines|flies)( sharply| softly)? into a(n unassisted| sacrifice)? (double|triple) play, first baseman"), '1B', pos),
                pos = ifelse(grepl('double_play|triple_play',events) & str_count(des, " (grounds|ground bunts|pops|lines|flies)( sharply| softly)? into a(n unassisted| sacrifice)? (double|triple) play, second baseman"), '2B', pos),
                pos = ifelse(grepl('double_play|triple_play',events) & str_count(des, " (grounds|ground bunts|pops|lines|flies)( sharply| softly)? into a(n unassisted| sacrifice)? (double|triple) play, third baseman"), '3B', pos),
                pos = ifelse(grepl('double_play|triple_play',events) & str_count(des, " (grounds|ground bunts|pops|lines|flies)( sharply| softly)? into a(n unassisted| sacrifice)? (double|triple) play, shortstop"), 'SS', pos),
                pos = ifelse(grepl('double_play|triple_play',events) & str_count(des, " (grounds|ground bunts|pops|lines|flies)( sharply| softly)? into a(n unassisted| sacrifice)? (double|triple) play, left fielder"), 'LF', pos),
                pos = ifelse(grepl('double_play|triple_play',events) & str_count(des, " (grounds|ground bunts|pops|lines|flies)( sharply| softly)? into a(n unassisted| sacrifice)? (double|triple) play, center fielder"), 'CF', pos),
                pos = ifelse(grepl('double_play|triple_play',events) & str_count(des, " (grounds|ground bunts|pops|lines|flies)( sharply| softly)? into a(n unassisted| sacrifice)? (double|triple) play, right fielder"), 'RF', pos))
    
    #sacrifice 
    df = mutate(df, 
                pos = ifelse(events %in% c('sac_bunt', 'sac_fly') & str_count(des, " on a sacrifice (fly|bunt)(,| to) pitcher"), 'P', pos),
                pos = ifelse(events %in% c('sac_bunt', 'sac_fly') & str_count(des, " on a sacrifice (fly|bunt)(,| to) catcher"), 'C', pos),
                pos = ifelse(events %in% c('sac_bunt', 'sac_fly') & str_count(des, " on a sacrifice (fly|bunt)(,| to) first baseman"), '1B', pos),
                pos = ifelse(events %in% c('sac_bunt', 'sac_fly') & str_count(des, " on a sacrifice (fly|bunt)(,| to) second baseman"), '2B', pos),
                pos = ifelse(events %in% c('sac_bunt', 'sac_fly') & str_count(des, " on a sacrifice (fly|bunt)(,| to) third baseman"), '3B', pos),
                pos = ifelse(events %in% c('sac_bunt', 'sac_fly') & str_count(des, " on a sacrifice (fly|bunt)(,| to) shortstop"), 'SS', pos),
                pos = ifelse(events %in% c('sac_bunt', 'sac_fly') & str_count(des, " on a sacrifice (fly|bunt)(,| to) left fielder"), 'LF', pos),
                pos = ifelse(events %in% c('sac_bunt', 'sac_fly') & str_count(des, " on a sacrifice (fly|bunt)(,| to) center fielder"), 'CF', pos),
                pos = ifelse(events %in% c('sac_bunt', 'sac_fly') & str_count(des, " on a sacrifice (fly|bunt)(,| to) right fielder"), 'RF', pos))
    
    #fielder choice
    df = mutate(df, 
                pos = ifelse(events  %in% c('fielders_choice_out', 'fielders_choice', 'double_play') & str_count(des, "(, fielded by|on a fielder's choice out,|a fielder's choice double play,) pitcher"), 'P', pos),
                pos = ifelse(events  %in% c('fielders_choice_out', 'fielders_choice', 'double_play') & str_count(des, "(, fielded by|on a fielder's choice out,|a fielder's choice double play,) catcher"), 'C', pos),
                pos = ifelse(events  %in% c('fielders_choice_out', 'fielders_choice', 'double_play') & str_count(des, "(, fielded by|on a fielder's choice out,|a fielder's choice double play,) first baseman"), '1B', pos),
                pos = ifelse(events  %in% c('fielders_choice_out', 'fielders_choice', 'double_play') & str_count(des, "(, fielded by|on a fielder's choice out,|a fielder's choice double play,) second baseman"), '2B', pos),
                pos = ifelse(events  %in% c('fielders_choice_out', 'fielders_choice', 'double_play') & str_count(des, "(, fielded by|on a fielder's choice out,|a fielder's choice double play,) third baseman"), '3B', pos),
                pos = ifelse(events  %in% c('fielders_choice_out', 'fielders_choice', 'double_play') & str_count(des, "(, fielded by|on a fielder's choice out,|a fielder's choice double play,) shortstop"), 'SS', pos),
                pos = ifelse(events  %in% c('fielders_choice_out', 'fielders_choice', 'double_play') & str_count(des, "(, fielded by|on a fielder's choice out,|a fielder's choice double play,) left fielder"), 'LF', pos),
                pos = ifelse(events  %in% c('fielders_choice_out', 'fielders_choice', 'double_play') & str_count(des, "(, fielded by|on a fielder's choice out,|a fielder's choice double play,) center fielder"), 'CF', pos),
                pos = ifelse(events  %in% c('fielders_choice_out', 'fielders_choice', 'double_play') & str_count(des, "(, fielded by|on a fielder's choice out,|a fielder's choice double play,) right fielder"), 'RF', pos))
    
    #Fielding errors
    df = mutate(df, 
                pos = ifelse(events == 'field_error' & str_count(des, "fielding error by pitcher"), 'P', pos),
                pos = ifelse(events == 'field_error' & str_count(des, "fielding error by catcher"), 'C', pos),
                pos = ifelse(events == 'field_error' & str_count(des, "fielding error by first baseman"), '1B', pos),
                pos = ifelse(events == 'field_error' & str_count(des, "fielding error by second baseman"), '2B', pos),
                pos = ifelse(events == 'field_error' & str_count(des, "fielding error by third baseman"), '3B', pos),
                pos = ifelse(events == 'field_error' & str_count(des, "fielding error by shortstop"), 'SS', pos),
                pos = ifelse(events == 'field_error' & str_count(des, "fielding error by left fielder"), 'LF', pos),
                pos = ifelse(events == 'field_error' & str_count(des, "fielding error by center fielder"), 'CF', pos),
                pos = ifelse(events == 'field_error' & str_count(des, "fielding error by right fielder"), 'RF', pos))
    
    #Left out following plays and label invalid since its hard to determine if the fielder was responsible:
    #   challenges
    #   out hit by ball
    #   fan interference
    #   throwing errors
    #   bunts/inside park homeruns/other hits that dont specify the location
    #   any ball where batted distance >450 feet (and in play)
    df = mutate(df, invalid = hit_dist>450 | is.na(pos))
    #Identify foul territory plays 
    df = mutate(df, foul = grepl('foul territory',des) | y-x < -10 | x+y < -10)
    
    return(df)
}


calculate_trajectories = function(df){
    #Calculate hang time, vertical velo, horizontal velo, estimated distance
    df = mutate(df,
                #velocity components
                hit_speed_v = launch_speed*sin(launch_angle*pi/180),
                hit_speed_h = launch_speed*cos(launch_angle*pi/180),
                #replace pitch height if missing
                plate_z = ifelse(is.na(plate_z) | plate_z<0, median(plate_z, na.rm = T), plate_z)
    ) 
    
    library(parallel)
    cl <- detectCores()
    group <- rep(1:cl, length.out = nrow(df))
    cluster <- create_cluster(cores = cl)%>%
        cluster_library('tidyverse')%>%
        cluster_assign_value('trajectory_df',trajectory_df)
    
    #df1 = select(df, launch_angle, launch_speed, plate_z) %>% group_by(row_number())%>% nest()
 
    by_group = select(df, launch_angle, launch_speed, plate_z, stand) %>% 
        group_by(row = row_number())%>% 
        nest() %>%
        mutate(group = group )%>% 
        partition(group, cluster = cluster)
    
    #Get hang times with trajectory_df() - takes a couple minutes
    hang_time = by_group %>% 
        mutate(hang_time = map(data, trajectory_df)) %>%
        collect() %>%
        arrange(row)
  
    #Add hang time
    df = bind_cols(df, unnest(select(ungroup(hang_time), hang_time), hang_time))
    
    return(df)
}
#test if a coordinate is foul or not (give a little wiggle room)
test_foul = function(df){
    return(mutate(df, foul =  y-x < -10 | x+y < -10 ))
}


#test if a coordinate is out of the park (400 ft wall)
test_hr= function(df){
    return(mutate(df, hr =  sqrt(x^2 +y^2) >410 ))
}

#Calculate metrics for a ball in play
#   For a specific position, or the player that actually fielded the ball calculate
#     1. Slope to fielder from home
#     2. Fielder distance from ball
#     3. Angle from fielder to ball (0 degrees means its coming straight at the fielder)
#     4. Some subjective directional metrics (don't really use these)
calc_bip_metrics = function(df, centers, position){
    
    if(position == 'closest'){
        df = left_join(df , centers) 
    }else{
        df = left_join(df %>% mutate(pos = position),
                       centers)    
    }
        
    df =mutate(df, 
               #hit distance
               hit_dist = sqrt(x^2 + y^2),
               #fielder start distance
               start_dist = sqrt(x0^2 + y0^2),
               #slope to fielder
               m0 = y0/x0,
               #distance to fielder
               fielder_dist = sqrt((x0-x)^2+(y0-y)^2),
               #angle from fielder to ball
               fielder_angle = ifelse(y<m0*x | is.infinite(m0) , 
                                 acos((-x0*(x-x0) + -y0*(y-y0))/(sqrt(x0^2 + y0^2)*fielder_dist))*180/pi, 
                                 180 + acos((x0*(x-x0) + y0*(y-y0))/(sqrt(x0^2 + y0^2)*fielder_dist))*180/pi),
               #dummy vars for 4 directions
               dir_left= between(fielder_angle, 30, 90),
               dir_right = between(fielder_angle, 270, 330),
               dir_front = (fielder_angle> 345 | fielder_angle< 15),
               dir_back = fielder_angle< 270 & fielder_angle>90,
               direction = ifelse(dir_right, 'R',
                                  ifelse(dir_left,'L',
                                         ifelse(dir_front, 'F','B'))))
    return(df)
}


#Identify n closest fielders to hits
closest_fielder = function(df, centers, top = 2){
    
    #Split up data and estimated position centers to R and L batters
    centers_R = filter(centers, stand=='R')
    df_R = filter(df, stand == 'R')
    centers_L = filter(centers, stand=='L')
    df_L = filter(df, stand =='L')
    
    #Calculate distances from ball to players
    dist_R = as.matrix(pdist::pdist(select(df_R,x,y), select(centers_R,x0,y0)))
    dist_L = as.matrix(pdist::pdist(select(df_L,x,y), select(centers_L,x0,y0)))
    
    #Get top n closest players based on initial coord
    if(nrow(df_R)>0){
        closest_R = apply(dist_R,1, sort) %>%
            t()%>% data.frame %>%
            setNames(., paste0('closest',1:ncol(.))) %>%
            select(1:top)
        which_closest_R = apply(dist_R,1,order)
    }
    if(nrow(df_L)>0){
        closest_L = apply(dist_L,1, sort) %>%
            t()%>% data.frame %>%
            setNames(., paste0('closest',1:ncol(.))) %>%
            select(1:top)
        which_closest_L = apply(dist_L,1,order)
    }
    
    #Test if ball infront of INF/OF
    inf = c('1B','2B','3B','SS')
    of= c('LF','CF','RF')
    all = c(inf, of,'P','C')
    
    if(nrow(df_R)>0){
        #Get front/back boolean for each BIP and each position
        FB_R = lapply(all, function(p) select(calc_bip_metrics(df_R, centers_R, p),dir_front, dir_back, fielder_angle)) %>%
            setNames(all)
        
        #Get front/back boolean for closest 2, and fielder angles
        closest_FB_R = do.call(rbind, lapply(1:ncol(which_closest_R), function(i) do.call(cbind, lapply(1:2, function(j) FB_R[[which_closest_R[j,i]]][i, ])))) %>%
            setNames(paste0(c('closest_front','closest_back','fielder_angle'),c(rep(1,3), rep(2,3))))
        
        #Combine directional data with original data
        df_R = bind_cols(df_R, closest_FB_R,
                         data.frame(Reduce("+",lapply(FB_R[inf], function(x) x[,1:2]))>0) %>% setNames(c('inf_front','inf_back')), 
                         data.frame(Reduce("+",lapply(FB_R[of], function(x) x[,1:2]))>0)%>% setNames(c('of_front','of_back')))
        
    }
    if(nrow(df_L)>0){
        
        #Get front/back boolean for each BIP and each position
        FB_L = lapply(all, function(p) select(calc_bip_metrics(df_L, centers_L, p),dir_front, dir_back, fielder_angle)) %>%
            setNames(all)
        
        #Get front/back boolean for closest 2, and fielder angles
        closest_FB_L = do.call(rbind, lapply(1:ncol(which_closest_L), function(i) do.call(cbind, lapply(1:2, function(j) FB_L[[which_closest_L[j,i]]][i, ])))) %>%
            setNames(paste0(c('closest_front','closest_back','fielder_angle'),c(rep(1,3), rep(2,3))))
        
        #Combine directional data with original data
        df_L = bind_cols(df_L, closest_FB_L,
                         data.frame(Reduce("+",lapply(FB_L[inf], function(x) x[,1:2]))>0) %>% setNames(c('inf_front','inf_back')), 
                         data.frame(Reduce("+",lapply(FB_L[of], function(x) x[,1:2]))>0)%>% setNames(c('of_front','of_back')))
        
    }
    
    if(nrow(df_L)>0 & nrow(df_R)>0){
        return(bind_rows(bind_cols(df_R, closest_R),
                         bind_cols(df_L, closest_L)))
    }else if(nrow(df_R)>0){
        return(bind_cols(df_R, closest_R))
    }else{
        return(bind_cols(df_L, closest_L))
    }
    
}

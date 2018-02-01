library(tidyverse)
library(baseballr)
library(lubridate)
library(openWAR)
library(lattice)
library(multidplyr)

#Load statcast data
statcast = bind_rows(readRDS('data/statcast.2016.RDS'),
                     readRDS('data/statcast.2017.RDS'))

#source useful functions
source('statcast_data_func.R')

#Id crosswalk
ids =select(read.csv('https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv'),
            key_mlbam,key_fangraphs, name_last ,name_first ) %>%
    mutate(Name = paste(name_first, name_last)) %>%
    select(-name_last, -name_first)%>%
    filter(!(is.na(key_mlbam)|key_mlbam==''|is.na(key_fangraphs)|key_fangraphs==''))


#Read in shift data by player fangraphs
# Since I dont have initial fielder positions I will need to estimate
# If we only use players who rarely have shifts, it may be a safe assumption 
NoShiftAB = read.csv('data/NoShiftAB_16-17.csv') %>% select( NSAB = AB, key_fangraphs = playerid)
TotalAB= read.csv('data/TotalAB_16-17.csv')%>% select(AB,  key_fangraphs = playerid)

#Filter to players with normal alignments >25% of the time
ShiftInfo = full_join(TotalAB, NoShiftAB) %>%
    replace_na(list(AB = 0, NSAB = 0)) %>%
    mutate(PNS = ifelse(AB==0, 1, NSAB/AB)) %>%
    left_join(ids) %>%
    filter(PNS>.25)

#Exclude all events with players that have <= 70% normal alignments
sc_no_shift = filter(statcast, batter %in% ShiftInfo$key_mlbam)

#Clean statcast data
sc_init_clean = sc_no_shift %>%
    clean_bip_data() %>%
    assign_positions()

#Filter out invalid plays
sc_subset = filter(sc_init_clean, 
                  #unlikely x,y coordinates 
                  !invalid, 
                  #For fielders to be in the normal position we should probably only use situations with no one on base or 2 outs
                  bases=='000' | outs==2)

source('trajectory.R')
#Get trajectories
sc_clean = calculate_trajectories(sc_subset)
#save
saveRDS(sc_clean, 'data/sc_clean_traj.RDS')

#Get expected starting positions for all players by R/L batter
pos_centers = bind_rows(
    filter(sc_clean, 
           #dont count foul balls
           !foul, 
           #only line outs (players won't move too much)
           grepl('lines out sharply', des),
           #play is an out
           OUT) %>% 
        group_by(pos, stand) %>% 
        #get median starting coordinates
        summarise(x0 = median(x), y0 = median(y)) %>%
        filter(!pos %in% c('C','P')),
    #manually set P and C position
    data.frame(pos = c('P','P','C','C'), stand = c('R','L','R','L'),
               x0 = c(0, 0 ,0, 0), y0 = c(55, 55, -5, -5))) %>%
    ungroup

#Assign positions to plays
sc_clean = calc_bip_metrics(sc_clean, pos_centers,'closest') %>%
    select(-x0, -y0)

#Filter out more invalid plays
# 1. Assume that a play cannot happen when the fielder is more than 300 ft away from a play
sc_clean = filter(sc_clean, fielder_dist<300|HIT )

#Lets plot centers
xyplot(y0~x0|stand, data =pos_centers, 
       pch = 16,col = 'black',
       panel = function(x, y, ...) {
    panel.baseball()
    panel.xyplot(x, y, alpha = 1, ...)
}, xlim = c(-200, 200), ylim = c(-40, 500))


###########  Modeling Out Probabilities (only applies when player in similar starting posiion )############

#Modeling data for position
#Goal: get a model for a position out prob that doesn't depend upon strict xy coordinates
# Although position specific models will break if you switch SS and CF, a shift should maintain properties
get_out_model = function(position){

    #Limit data to any play that went to CF or that was a hit (at extreme hang times might have wierd estimates)
    pos_data = calc_bip_metrics(sc_clean %>% filter(pos==position|HIT), centers = pos_centers, position = position)
    
    #glm on hang time and distance*angle
    mod = glm(I(OUT) ~ poly(hang_time,2, raw = T) + poly(hit_speed_h,3)+
                  fielder_dist*(I(cos(fielder_angle*pi/180)) + I(sin(fielder_angle*pi/180)))
                  , data =pos_data, family = 'binomial')

    return(mod)
}

inf = c('1B','2B','3B','SS')
of= c('LF','CF','RF')
all_pos = c(inf, of, 'C','P')

#List of all position models
mod_list = lapply(all_pos, get_out_model)
names(mod_list)=all_pos


out_p_grid = expand.grid(x = seq(-350,350, 10), y = seq(0, 550, 10),  
                         hang_time = c(0,2,4,6), hit_speed_h = c(50,80,100, 120), stand = 'R')%>%
    test_foul()%>%
    test_hr()%>%
    filter(!foul,!hr)

out_p_grid_list = lapply(all_pos, function(position){
    p_grid = calc_bip_metrics(out_p_grid, centers = pos_centers, position=position) %>%
        mutate(pos = position)
    
    p_grid$pred = predict(mod_list[[position]], p_grid, type = 'response')

    return(p_grid)
})

out_p_grid = do.call(rbind, out_p_grid_list)

out_grid = group_by(out_p_grid, x, y, hang_time, hit_speed_h) %>%
    summarise(pred = 1-prod(1-pred)) %>% ungroup


###########  Modeling Out Probabilities - Generalized ( Models based on starting position )############

ALL = bind_rows(filter(sc_clean, OUT), 
                do.call(rbind, lapply(all_pos, function(position) filter(sc_clean, HIT)%>%mutate(pos=position)))) %>%
    calc_bip_metrics( centers = pos_centers, position = 'closest')


mod_out = glm(I(OUT) ~ pos:(fielder_dist*(I(cos(fielder_angle*pi/180)) + 
                                              I(sin(fielder_angle*pi/180)))) +
                  (   poly(hang_time,2, raw = T):poly(hit_speed_h, 2, raw=T):poly(start_dist, 2, raw = T)+
                          poly(hang_time,2, raw = T):poly(start_dist, 2, raw = T) +
                          poly(hit_speed_h, 2, raw=T):poly(start_dist, 2, raw = T)) , data =ALL, family = 'binomial')



common_types = count(sc_clean, hang_time=floor(hang_time/.5)*.5, 
                     hit_speed_h = floor(hit_speed_h/20)*20) %>%
    filter(n>1000) %>%
    mutate(comb = paste(hang_time, hit_speed_h))


out_p_grid = expand.grid(x = seq(-350,350, 10), y = seq(0, 550, 10),  
                         hang_time = c(0,1,4,5), hit_speed_h = c(60,80), stand = 'R')%>%
    test_foul()%>%
    test_hr() %>%
    filter(!foul, !hr)


out_p_grid_list = lapply(all_pos, function(position){
    
    p_grid = calc_bip_metrics(out_p_grid, centers = pos_centers, position=position) 
    
    p_grid$pred = predict(mod_out, p_grid, type = 'response')
    
    return(p_grid)
})

out_p_grid = do.call(rbind, out_p_grid_list)

out_grid = group_by(out_p_grid, x, y, hang_time, hit_speed_h) %>%
    summarise(pred = 1-prod(1-pred)) %>% ungroup


contourplot(pred~x*y|hit_speed_h+hang_time, 
            data = out_grid %>% mutate(hang_time = factor(paste(hang_time,'sec')),  hit_speed_h = factor(hit_speed_h)),
            panel = panel.fielding, 
            region = TRUE, alpha.regions = 0.5,at = seq(0,1,.1),
            col.regions = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100), 
            contour = TRUE, pretty = T)


#############  Modeling Expected wOBA for Hits  ###############

woba_hit = closest_fielder(filter(sc_clean,HIT), pos_centers, 2) %>%
    filter(!foul)

#Look at actual distribution
group_by(test, x = floor(x/15)*15, y = floor(x/15)*15) %>% 
    summarise(woba = mean(woba_value)) %>% 
    ggplot()+geom_point(aes(x ,y, colour = woba), size = 2)


#wOBA as a function of two closest fielders and angles (beta regression better? )
# mod2d = glm(I((woba_value-.9)/(2-.9)) ~  closest1 + closest2 +
#                 I(cos(fielder_angle1*pi/180)) + I(sin(fielder_angle1*pi/180))+
#                 I(cos(fielder_angle2*pi/180)) + I(sin(fielder_angle2*pi/180)),data =woba_hit, family = 'binomial')

#wOBA as a function of two closest fielders and angles - Estimate of E[ wOBA | Hit]
mod_woba = betareg(I((woba_value-.8999)/(2.001-.8999)) ~  closest1*closest2+
                    I(cos(fielder_angle1*pi/180)) + I(sin(fielder_angle1*pi/180))+
                    I(cos(fielder_angle2*pi/180)) + I(sin(fielder_angle2*pi/180)),data =woba_hit, link ="loglog")

#Normal alignment
woba_grid = expand.grid(x = seq(-350,350, 5), y = seq(0, 550, 5),stand = 'R')%>%
    test_foul()%>%filter(!foul)%>%
    closest_fielder(. , pos_centers , 2)

#Predict by xy coord, E[woba|hit]
woba_grid = woba_grid %>% 
    mutate(woba = predict(mod_woba, woba_grid, type='response')*(2.001-.8999)+.8999) %>%
    test_foul() %>%
    mutate(woba = ifelse(foul, 0, woba))

#Contour plot woba expectancy
contourplot(woba~x*y ,
            data = p %>% filter(!duplicated(paste(x,y))) ,       
            panel = panel.fielding,
            region = TRUE, alpha.regions = 0.5, cuts = 10, 
            col.regions = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "RdBu")))(100), 
            contour = TRUE)



#Estimate E(wOBA | BIP)
woba_bip = left_join(out_grid, select(woba_grid, x, y, woba)) %>% 
    mutate(woba = (1-pred)*woba)

contourplot(woba~x*y|hit_speed_h+hang_time  ,
            data = woba_bip %>%
                mutate(hang_time = factor(paste(hang_time,'sec')), hit_speed_h = factor(paste(hit_speed_h, 'mph'))) ,         
            panel = panel.fielding,
            region = TRUE, alpha.regions = 0.5, cuts = 10, 
            col.regions = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "RdBu")))(100), 
            contour = TRUE, labels = FALSE, main = 'Normal Alignment')

centers_text = filter(pos_centers,stand=='R')

for(i in 1:2){
    trellis.focus('panel',i,3)
    ltext(x=centers_text$x0,y=centers_text$y0, labels = centers_text$pos)
    trellis.unfocus() 
}


#Simulate Shift Positions (Drastic shift )
shift_centers = mutate(pos_centers, 
                       x0 =ifelse(pos=='SS', -5, 
                                   ifelse(pos=='2B',65, ifelse(pos=='LF', -60, x0))),
                       y0 = ifelse(pos=='SS', 150, 
                                   ifelse(pos=='2B',200, y0)))

#Change out distribution
out_p_grid = expand.grid(x = seq(-350,350, 10), y = seq(0, 550, 10),  
                         hang_time = c(0,1,4,5), hit_speed_h = c(60,80), stand = 'R')%>%
    test_foul()%>%
    test_hr() %>%
    filter(!foul, !hr)
out_p_grid_list = lapply(all_pos, function(position){
    
    p_grid = calc_bip_metrics(out_p_grid, centers = shift_centers, position=position) 
    
    p_grid$pred = predict(mod2d6c, p_grid, type = 'response')
    
    return(p_grid)
})

out_p_grid = do.call(rbind, out_p_grid_list)

out_grid = group_by(out_p_grid, x, y, hang_time, hit_speed_h) %>%
    summarise(pred = 1-prod(1-pred)) %>% ungroup


woba_grid = expand.grid(x = seq(-350,350, 5), y = seq(0, 550, 5),  hang_time = c(0),  hit_speed_h = c(50),
                        stand = 'R')%>%
    test_foul()%>%filter(!foul)%>%
    #calc_bip_metrics( centers = pos_centers, position='1B') %>%
    closest_fielder(. , shift_centers, 2)

#Predict by xy coord, E[woba|hit]
woba_grid = woba_grid %>% 
    mutate(woba = predict(mod_woba, woba_grid, type='response')*(2.001-.8999)+.8999) %>%
    test_foul() %>%
    mutate(woba = ifelse(foul, 0, woba))

woba_bip = left_join(out_grid, select(woba_grid, x, y, woba)) %>% 
    mutate(woba = (1-pred)*woba)

contourplot(woba~x*y|hit_speed_h+hang_time  ,
            data = woba_bip %>%
                mutate(hang_time = factor(paste(hang_time,'sec')), hit_speed_h = factor(paste(hit_speed_h, 'mph'))) ,       
            panel = panel.fielding,
            region = TRUE, alpha.regions = 0.5, cuts = 10, 
            col.regions = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "RdBu")))(100), 
            contour = TRUE, labels = F,
            main = 'Severe Shift Alignment')

shift_centers_text = filter(shift_centers,stand=='R')

for(i in 1:2){
    trellis.focus('panel',i,3)
    ltext(x=shift_centers_text$x0,y=shift_centers_text$y0, labels = shift_centers_text$pos)
    trellis.unfocus() 
}


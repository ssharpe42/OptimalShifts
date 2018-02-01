

#Alan Nathan's 3D Trajectory Function
#Just default to normal conditions

trajectory_df= function(df){
    
    launch_angle= df$launch_angle[1]
    v0 = df$launch_speed[1] 
    z0 = df$plate_z[1]
    stand = df$stand[1]
    
    y0 = 2 
    direction = 0 
    tempF = 70
    elev = 15 
    vwind = 0 
    hwind = 0
    phiwind = 0 
    relhumid = 50
    dt = 0.01
    pressure_hg = 29.92
    
    #Set init params
    temp = (5/9)*(tempF-32)
    bar_pressure = pressure_hg*1000/39.37
    phi = direction
    x0 = 0 
    if(stand=='R'){
        sign = 1
    }else{
        sign = -1
    }
    
    #Constants
    wg = 0 
    ws = -sign*849-94*phi #sidespin
    wb = -763+120*launch_angle+21*phi*sign #backspin
    cd0	=0.3008
    cddot=0.0044
    cdspin=0.0292
    cl2 = 1.120
    cl0 = .583
    cl1 = 2.333
    w0=	2072
    theta0=7.2
    tau0=30
    circ = 9.125
    mass_oz = 5.125
    beta = .0001217
    
    #Calculate parameters for environment
    svp = 4.5841*exp((18.687-temp/234.5)*temp/(257.14+temp))
    rho = 1.2929*(273/(temp+273)*(bar_pressure*exp(-beta*elev/3.2808)-0.3783*relhumid*svp/100)/760)
    rho_lb = rho*0.06261
    c0  = 0.07182*rho_lb*(5.125/mass_oz)*(circ/9.125)^2
    const = c0
    reynold = rho*44.7*(circ/(pi*39.37))*(temp+273.16+120)/(0.000001512*(temp+273.16)^1.5)
    
    #Get velocity and other metrics
    v0_fps = v0*1.467 #fps
    cd = cd0 + cdspin*wb/1000
    v0x = v0_fps*cos(launch_angle*pi/180)*sin(phi*pi/180)
    v0y = v0_fps*cos(launch_angle*pi/180)*cos(phi*pi/180)
    v0z = v0_fps*sin(launch_angle*pi/180)
    
    wx=(wb*cos(phi*pi/180)-ws*sin(launch_angle*pi/180)*sin(phi*pi/180)+wg*v0x/v0_fps)*pi/30
    wy=(-wb*sin(phi*pi/180)-ws*sin(launch_angle*pi/180)*cos(phi*pi/180)+wg*v0y/v0_fps)*pi/30
    wz=(ws*cos(launch_angle*pi/180)+wg*v0z/v0_fps)*pi/30
    omega = sqrt(wx^2 + wy^2 + wz^2)
    romega = (circ/2/pi)*omega/12
    vxw_ = vwind*1.467*sin(phiwind*pi/180)
    vyw_ = vwind*1.467*cos(phiwind*pi/180)
    
    
    #Initialize trajectory parameters
    t=0
    x = c()
    y = c()
    z = c()
    x[1]=x0
    y[1]=y0
    z[1]=z0
    vx= v0x
    vy = v0y
    vz = v0z
    v = sqrt(v0x^2 + v0y^2 +v0z^2)
    vw = ifelse(z>hwind, sqrt((vx-vxw_)^2+(vy-vyw_)^2+vz^2),v)
    vmph = v/1.467
    S = (romega/vw)*exp(-t/(tau0*146.7/v))
    Cd = cd0 + cdspin*(wb/1000)*exp(-t/(tau0*146.7/vw))
    CI = cl2*S/(cl0+cl1*S)
    vxw = ifelse(z[1]>=hwind, vxw_, 0)
    vyw = ifelse(z[1]>=hwind, vyw_, 0)
    adragx = -const*Cd*vw*(vx-vxw)
    adragy = -const*Cd*vw*(vy-vyw)
    adragz = -const*Cd*vz*vw
    w = omega*exp(-t/tau0)*30/pi
    aMagx = const*(CI/omega)*vw*(wy*vz-wz*(vy-vyw))
    aMagy = const*(CI/omega)*vw*(wz*(vx-vxw)-wx*vz)
    aMagz = const*(CI/omega)*vw*(wx*(vy-vyw)-wy*(vx-vxw))
    ax  = adragx+aMagx
    ay = adragy+aMagy
    az = adragz+aMagz - 32.174
    i = 1
    #Loop while height is positive
    while(min(z) > -1e-5){
        t = t+dt
        i = i + 1
        x[i] = x[i-1]+ vx*dt + .5*ax*dt^2
        y[i] = y[i-1]+ vy*dt + .5*ay*dt^2
        z[i] =  z[i-1] + vz*dt + .5*az*dt^2
        vx = vx + ax*dt
        vy = vy + ay*dt
        vz =vz +az*dt
        v = sqrt(vx^2 + vy^2 +vz^2)
        vw = ifelse(z[i]>hwind, sqrt((vx-vxw_)^2+(vy-vyw_)^2+vz^2),v)
        S = (romega/vw)*exp(-t/(tau0*146.7/v))
        Cd = cd0 + cdspin*(wb/1000)*exp(-t/(tau0*146.7/vw))
        CI = cl2*S/(cl0+cl1*S)
        vxw = ifelse(z[i]>=hwind, vxw_, 0)
        vyw = ifelse(z[i]>=hwind, vyw_, 0)
        adragx = -const*Cd*vw*(vx-vxw)
        adragy = -const*Cd*vw*(vy-vyw)
        adragz = -const*Cd*vz*vw
        w = omega*exp(-t/tau0)*30/pi
        aMagx = const*(CI/omega)*vw*(wy*vz-wz*(vy-vyw))
        aMagy = const*(CI/omega)*vw*(wz*(vx-vxw)-wx*vz)
        aMagz = const*(CI/omega)*vw*(wx*(vy-vyw)-wy*(vx-vxw))
        ax  = adragx+aMagx
        ay = adragy+aMagy
        az = adragz+aMagz - 32.174
    }
    return(t-z[i]/diff(z[(i-1):i])*(dt))
}
# 
# 
# trajectory = function(launch_angle,v0,z0,
#                       stand = 'R', 
#                       y0 = 2, 
#                       direction = 0, 
#                       tempF = 70,
#                       elev = 15, 
#                       vwind = 0, 
#                       hwind = 0,
#                       phiwind = 0, 
#                       relhumid = 50,
#                       dt = 0.01,
#                       pressure_hg = 29.92){
#     
#     #Set init params
#     temp = (5/9)*(tempF-32)
#     bar_pressure = pressure_hg*1000/39.37
#     phi = direction
#     x0 = 0 
#     if(stand=='R'){
#         sign = 1
#     }else{
#         sign = -1
#     }
#     
#     
#     #Constants
#     wg = 0 
#     ws = -sign*849-94*phi #sidespin
#     wb = -763+120*launch_angle+21*phi*sign #backspin
#     cd0	=0.3008
#     cddot=0.0044
#     cdspin=0.0292
#     cl2 = 1.120
#     cl0 = .583
#     cl1 = 2.333
#     w0=	2072
#     theta0=7.2
#     tau0=30
#     circ = 9.125
#     mass_oz = 5.125
#     beta = .0001217
#     
#     #Calculate parameters for environment
#     svp = 4.5841*exp((18.687-temp/234.5)*temp/(257.14+temp))
#     rho = 1.2929*(273/(temp+273)*(bar_pressure*exp(-beta*elev/3.2808)-0.3783*relhumid*svp/100)/760)
#     rho_lb = rho*0.06261
#     c0  = 0.07182*rho_lb*(5.125/mass_oz)*(circ/9.125)^2
#     const = c0
#     reynold = rho*44.7*(circ/(pi*39.37))*(temp+273.16+120)/(0.000001512*(temp+273.16)^1.5)
#     
#     #Get velocity and other metrics
#     v0_fps = v0*1.467 #fps
#     cd = cd0 + cdspin*wb/1000
#     v0x = v0_fps*cos(launch_angle*pi/180)*sin(phi*pi/180)
#     v0y = v0_fps*cos(launch_angle*pi/180)*cos(phi*pi/180)
#     v0z = v0_fps*sin(launch_angle*pi/180)
#     
#     wx=(wb*cos(phi*pi/180)-ws*sin(launch_angle*pi/180)*sin(phi*pi/180)+wg*v0x/v0_fps)*pi/30
#     wy=(-wb*sin(phi*pi/180)-ws*sin(launch_angle*pi/180)*cos(phi*pi/180)+wg*v0y/v0_fps)*pi/30
#     wz=(ws*cos(launch_angle*pi/180)+wg*v0z/v0_fps)*pi/30
#     omega = sqrt(wx^2 + wy^2 + wz^2)
#     romega = (circ/2/pi)*omega/12
#     vxw_ = vwind*1.467*sin(phiwind*pi/180)
#     vyw_ = vwind*1.467*cos(phiwind*pi/180)
#     
#     
#     #Initialize trajectory parameters
#     t=0
#     x = c()
#     y = c()
#     z = c()
#     x[1]=x0
#     y[1]=y0
#     z[1]=z0
#     vx= v0x
#     vy = v0y
#     vz = v0z
#     v = sqrt(v0x^2 + v0y^2 +v0z^2)
#     vw = ifelse(z>hwind, sqrt((vx-vxw_)^2+(vy-vyw_)^2+vz^2),v)
#     vmph = v/1.467
#     S = (romega/vw)*exp(-t/(tau0*146.7/v))
#     Cd = cd0 + cdspin*(wb/1000)*exp(-t/(tau0*146.7/vw))
#     CI = cl2*S/(cl0+cl1*S)
#     vxw = ifelse(z[1]>=hwind, vxw_, 0)
#     vyw = ifelse(z[1]>=hwind, vyw_, 0)
#     adragx = -const*Cd*vw*(vx-vxw)
#     adragy = -const*Cd*vw*(vy-vyw)
#     adragz = -const*Cd*vz*vw
#     w = omega*exp(-t/tau0)*30/pi
#     aMagx = const*(CI/omega)*vw*(wy*vz-wz*(vy-vyw))
#     aMagy = const*(CI/omega)*vw*(wz*(vx-vxw)-wx*vz)
#     aMagz = const*(CI/omega)*vw*(wx*(vy-vyw)-wy*(vx-vxw))
#     ax  = adragx+aMagx
#     ay = adragy+aMagy
#     az = adragz+aMagz - 32.174
#     i = 1
#     while(min(z)> -.01){
#         t = t+dt
#         i = i + 1
#         x[i] = x[i-1]+ vx*dt + .5*ax*dt^2
#         y[i] = y[i-1]+ vy*dt + .5*ay*dt^2
#         z[i] =  z[i-1] + vz*dt + .5*az*dt^2
#         vx = vx + ax*dt
#         vy = vy + ay*dt
#         vz =vz +az*dt
#         v = sqrt(vx^2 + vy^2 +vz^2)
#         vw = ifelse(z[i]>hwind, sqrt((vx-vxw_)^2+(vy-vyw_)^2+vz^2),v)
#         S = (romega/vw)*exp(-t/(tau0*146.7/v))
#         Cd = cd0 + cdspin*(wb/1000)*exp(-t/(tau0*146.7/vw))
#         CI = cl2*S/(cl0+cl1*S)
#         vxw = ifelse(z[i]>=hwind, vxw_, 0)
#         vyw = ifelse(z[i]>=hwind, vyw_, 0)
#         adragx = -const*Cd*vw*(vx-vxw)
#         adragy = -const*Cd*vw*(vy-vyw)
#         adragz = -const*Cd*vz*vw
#         w = omega*exp(-t/tau0)*30/pi
#         aMagx = const*(CI/omega)*vw*(wy*vz-wz*(vy-vyw))
#         aMagy = const*(CI/omega)*vw*(wz*(vx-vxw)-wx*vz)
#         aMagz = const*(CI/omega)*vw*(wx*(vy-vyw)-wy*(vx-vxw))
#         ax  = adragx+aMagx
#         ay = adragy+aMagy
#         az = adragz+aMagz - 32.174
#     }
#     
#     return(t-z[i]/diff(z[(i-1):i])*(dt))
#     
# }
## Walsby 1997

## PI Curve

## P chlorophyll specific photosynthetic oxygen evolution 
##   mumol/mg/h
## Pm maximum photosynthetic rate
## Pc calculated net photosynthetic rate
## I photon irradiance
## R rate of respiratory oxagen production (negative value)
## alpha gradient of P/I observed at light limiting irradiances
## beta  gradient (negative) due to photoinhibition
##       at high irradiances
Pc = Pm * (1-exp(-alpha*I/Pm)) + R + beta*I

## Remarks:
## Chlorophyll in mg/m3
## R 9% of the gross photosynthesis
## parameters estimated with Excel-Solver


## Vertical distribution of light
Iz = exp(log(I0/I1)-ka*z)
## I0 nominal subsurface PAR
## ka estimated by regression analysis
## I1 = 1 mumol/m2/s


## Photon irradiance under the water surface
## Uw wind speed
## E energy irradiance
## ... making corrections for reflectance

## correction from UTC to local solar time: addition of 4min/°E
## conversion W/m2 to PAR: I/E=3.746 mumol/J
## Ia 

## Y day of the year (0: 1st Januar)
## G geographical latitude (degrees)
## t time of day (h)

## orbital angle of earth
Psi = 2 * pi * Y / 365

## angle of latitude
gamma = pi * G / 180

## time angle
tau = 2 * pi * t/24

## solar declination
delta = (0.39637 - 22.9133 * cos(Psi) + 4.02543 * sin(Psi) +
        - 0.3872 * cos(2*Psi) + 0.052*sin(2*Psi))*pi/180
        
## epsilon sun's elevation above the horizon (in radians)
##         epsilon >= 0 (above the horizon)
epsilon = asin(sin(gamma)*sin(delta) - cos(gamma)*cos(delta)*cos(tau))

## theta zenith angle of the sun
## .. set at pi/2 if epsilon < 0
theta = epsilon - pi/2

## r proportion of sunlight reflected from a smooth water surface
## can be calculated from theta with a combination of Snell's Law
## and Fresnel's Equation
r = 0.5 * ((sin(theta - asin(sin(theta/1.33))))^2) / 
          ((sin(theta + asin(sin(theta/1.33))))^2) +
    0.5 * ((tan(theta - asin(sin(theta/1.33))))^2) /
          ((tan(theta + asin(sin(theta/1.33))))^2)

## PAR under the surface
Iu = Ia * (1-r)


## wind correction of theta (surface roughening)
## derived empirical from the data of Kirk(1994)
thetaw = theta * (0.8291+0.1709*(exp(-0.235145*Uw^0.6225)))

## reflectance correction applies to direct sunlight
## and not to diffuse light under an overcast sky
## Kirk(1994) suggests a maximum loss by reflection of
## 5.2% under an overcast sky

## Ic calculated PAR for the time of day
## corrected reflection:
if (Ia < 0.5*Ic) I0=0.948*Ia

## Is approx. 2111 mumol/m2/s
Ic = Is * sin(epsilon)

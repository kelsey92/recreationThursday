# RecreationThursday #2
# Thu Jun 17 20:54:40 2021 ------------------------------

# kelsey chalmers

# Edna Andrade 
# Twilight Wave

library(tidyverse)
library(magrittr)
library(purrr)
theme_set(theme_void())

### data

sea_data <- tibble(colour = c("#e8ede7","#dbe0d9","#c8d4c9","#b5c4bd","#a8b7b0"),
                   intercept = seq(from = 10.5, by = 2, length.out = 5),
                   shift = cumsum(c(0,rep(pi/2.5,4))))

sea_data %<>%
  arrange(desc(intercept))

sky_data <- tibble(colour = rev(c("#b5b0b6","#c4c4c4","#d4d6d5","#e1e3de",
                                  "#ebece6")),
                   intercept = seq(from = 22.5, by = 2, length.out = 5),
                   shift = cumsum(c(2*pi/2.5,rep(pi/2.5,4))))

sky_data %<>%
  arrange(desc(intercept))


circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

umbrella_data <- circleFun(c(11,8),2.3,npoints = 500)

### functions

# plot sea and sky
wave_f <- function(colour,intercept,shift){
  x <- seq(from=0,to=4*pi,length.out=100)
  y <- sin(1.7*x - (pi/3) + shift) + intercept

  geom_area(aes(x = x,y=y),fill=colour)
}

# version 1
ggplot() + 
  theme_void() + 
  geom_area(aes(x=c(0,4*pi),y=c(35,35)),fill="#a49da4") + 
  pmap(sky_data,wave_f) + 
  geom_area(aes(x=c(0,4*pi),y=c(20.5,20.5)),fill="#98aea2") + 
  pmap(sea_data,wave_f) + 
  # add beach
  wave_f("#E8bd4a",7,-pi/2.5) + 
  # add sun
  geom_point(aes(x = 2*pi,y=25),colour="#e69a42",size=10) + 
  # add beach towel
  geom_line(aes(x=c(11,11.2),y=c(2,8.3)),colour="darkgrey",lwd=1.2) + 
  geom_polygon(aes(x = c(9,11,11.5,9.5),
                   y = c(1,1,2,2)),fill="#4e5ce7") + 
  # add umbrella
  geom_polygon(data = umbrella_data[1:200,],aes(x=x,y=y),fill="darkred") + 
  labs(caption = "@kelsey_chalmers #RecreationThursday
       Interpretation of Edna Andrade Twilight Wave (1973)") + 
  theme(plot.caption = element_text(size=8,face="italic"))

ggsave("version1.png")
  
# version 2
ggplot() + 
  theme_void() + 
  geom_area(aes(x=c(0,4*pi),y=c(35,35)),fill="#a49da4") + 
  pmap(sky_data,wave_f) + 
  geom_area(aes(x=c(0,4*pi),y=c(20.5,20.5)),fill="#98aea2") + 
  pmap(sea_data,wave_f) + 
  # add beach
  wave_f("#E8bd4a",7,-pi/2.5) + 
  # add sun
  geom_point(aes(x = seq(1,4*pi-1,length.out=7),
                 y = c(33,32.5,31,29,27.5,26,24)),colour="#e69a42",size=5) + 
  # add beach towel
  geom_line(aes(x=c(11,11.2),y=c(2,8.3)),colour="darkgrey",lwd=1.2) + 
  geom_polygon(aes(x = c(9,11,11.5,9.5),
                   y = c(1,1,2,2)),fill="#4e5ce7") + 
  # add umbrella
  geom_polygon(data = umbrella_data[1:200,],aes(x=x,y=y),fill="darkred") + 
  labs(caption = "@kelsey_chalmers #RecreationThursday
       Interpretation of Edna Andrade Twilight Wave (1973)") + 
  theme(plot.caption = element_text(size=8,face="italic"))

ggsave("version2.png")


## #RecreationThursday No. 4

# Odili Donald Odita's Phantom’s Shadow (2008)

# Kelsey Chalmers

library(tidyverse)
library(purrr)

inp_colour <- c('teal' = "#008A9A",
                'orange' = "#F79B00",
                'pink' = "#E5816B",
                'blue' = "#004C9C")

step_0 = data.frame(x = c(0,1/3,1),
                    ymin = c(0,0,0),
                    ymax = c(0,2/3,1))
step_1 = data.frame(x = c(0,2/3,1),
                    ymin = c(0,0,0),
                    ymax = c(1,2/3,0))
step_2 = data.frame(x = c(0,2/3,1),
                    ymin = c(0,1/3,1),
                    ymax = c(1,1,1))
step_3 = data.frame(x = c(0,1/3,1),
                    ymin = c(1,1/3,0),
                    ymax = c(1,1,1))

square_add <- function(step,x_shift,y_shift,colour){
  geom_ribbon(data=mutate(step,x=x+x_shift,
                          ymin = ymin+y_shift,
                          ymax = ymax+y_shift),
              aes(x=x,ymin=ymin,ymax=ymax),
              fill = unlist(inp_colour[colour]))
}

column_1 <- tibble(step = list(step_0,step_1,step_2,step_3,
                               step_2,step_3,step_0,step_1),
                   x_shift = rep(0,8),
                   y_shift = 0:7,
                   colour = c("teal","orange","pink","blue",
                              "pink","teal","blue","orange"))
column_2 <- tibble(step = list(step_3,step_2,step_1,step_0,
                               step_1,step_0,step_3,step_2),
                   x_shift = rep(1,8),
                   y_shift = 0:7,
                   colour = c("pink","blue","teal","orange",
                              "blue","orange","pink","teal"))
column_3 <- tibble(step = list(step_2,step_3,step_0,step_1,
                               step_0,step_1,step_2,step_3),
                   x_shift = rep(2,8),
                   y_shift = 0:7,
                   colour = c("orange","teal","orange","blue",
                              "teal","pink","teal","orange"))
column_4 <- tibble(step = list(step_1,step_0,step_3,step_2,
                               step_3,step_2,step_1,step_0),
                   x_shift = rep(3,8),
                   y_shift = 0:7,
                   colour = c("blue","pink","teal","pink",
                              "orange","blue","pink","blue"))
column_5 <- tibble(step = column_3$step,
                   x_shift = rep(4,8),
                   y_shift = 0:7,
                   colour = c("pink","teal","blue","orange",
                              "blue","pink","orange","teal"))
column_6 <- tibble(step = column_4$step,
                   x_shift = rep(5,8),
                   y_shift = 0:7,
                   colour = c("blue","orange","pink","teal",
                              "orange","teal","blue","pink"))
column_7 <- tibble(step = column_1$step,
                   x_shift = rep(6,8),
                   y_shift =0:7,
                   colour = c("teal","pink","teal","orange",
                              "pink","blue","pink","teal"))
column_8 <- tibble(step = column_2$step,
                   x_shift = rep(7,8),
                   y_shift = 0:7,
                   colour = c("orange","blue","pink","blue",
                              "teal","orange","blue","orange"))
background_square = tibble(x = c(0,8),
                           ymin = c(0,0),
                           ymax = c(8,8))

ggplot() + 
  coord_equal() + 
  geom_ribbon(data=background_square,aes(x=x,ymin=ymin,ymax=ymax),
              fill = "#969B8D") + 
  pmap(column_1,square_add) + 
  pmap(column_2,square_add) + 
  pmap(column_3,square_add) + 
  pmap(column_4,square_add) + 
  pmap(column_5,square_add) + 
  pmap(column_6,square_add) + 
  pmap(column_7,square_add) + 
  pmap(column_8,square_add) + 
  theme_void() 
  
ggsave("OditaRecreation_v1.png",width=2.86)
        
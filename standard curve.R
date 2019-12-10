#------------------------------------------
# Load packages
library(tidyverse)
library(here)


#-------------------------------------------
# Read in data

# wtf is with the here function


ggplot(co2_standards_raw, aes( x = ppm, y = area)) + 
  geom_point() +
  geom_smooth(method = "lm", formula=y~x) +
  theme_minimal()


      
                
#------------------------------------------
# Load packages
library(tidyverse)
library(here)


#-------------------------------------------
# Read in data

co2_standards <- read_csv(here::here("data",
                          "co2_standards_raw.csv"))

#-------------------------------------------
# Use lm(), linear model function, to create regression model

### Change dates in name and filter

fit20190913 <- lm(ppm ~ area, data = co2_standards %>% filter(date == "2019_09_13"))

#-------------------------------------------
#Use summary function to get summary information about linear model

### Changes dates

summary(fit20190913)

#--------------------------------------------
#Function to create standard curve
### Change date in labs

ggplot_regression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[1], y = names(fit$model)[2])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = "20190913 CO2 Standard Curve", caption = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

### Change date in filter




ggplot_regression(lm(ppm ~ area, data = co2_standards %>% 
                       filter(date == "2019_09_13")))+
  coord_flip()+
  theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        title = element_text(vjust=3))



  



      
                
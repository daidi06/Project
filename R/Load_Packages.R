# Load packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, 
               here,
               sf,
               bslib, 
               bsicons,
               rnaturalearth,
               rnaturalearthdata,
               plotly, 
               countrycode, 
               htmltools, 
               reactable,
               janitor,
               gganimate,
               transformr,
               viridis,
               gifski
)
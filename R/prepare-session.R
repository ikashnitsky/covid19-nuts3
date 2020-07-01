#===============================================================================
# 2020-07-01 -- covid19
# Prepare session
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
# Jose Manuel Aburto, jose.aburto@sociology.ox.ac.uk, @jm_aburto
#===============================================================================

library(tidyverse)
library(magrittr)
library(RColorBrewer)
library(prismatic)
library(hrbrthemes)
library(cowplot)
library(sf)
library(eurostat)
options(scipen = 9999)

library(showtext)
font_add_google("Roboto Condensed", "Roboto Condensed")
font_add_google("Roboto Slab", "Roboto Slab")
showtext::showtext_auto()


#===============================================================================
# 2020-03-18 -- twitter
# regional age differences and ncov-19
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)
library(magrittr)
library(prismatic)
library(hrbrthemes)
library(cowplot)
library(sf)
options(scipen = 9999)

library(showtext)
font_add_google("Roboto Condensed", "Roboto Condensed")
font_add_google("Roboto Slab", "Roboto Slab")
showtext::showtext_auto()



# Italy fatality data -----------------------------------------------------
# https://www.epicentro.iss.it/coronavirus/bollettino/Infografica_17marzo%20ENG.pdf
cfr <- tibble::tribble(
              ~age, ~cfr,
             "0-9",    0,
           "10-19",    0,
           "20-29",    0,
           "30-39",  0.3,
           "40-49",  0.4,
           "50-59",  1,
           "60-69",  3.5,
           "70-79", 12.3,
           "80-89", 19.6,
             "90+", 22.9
           )


# data -----------------------------------------------------------------

# eurostat data
library(eurostat)

# remote areas to remove (NUTS-2)
remote <- c(paste0('ES',c(63,64,70)),paste('FRY',1:5,sep=''),'PT20','PT30')

# geodata at NUTS-3 level    
gde3 <- eurostat_geodata_60_2016 %>% 
    filter(LEVL_CODE==3,
           !str_sub(geo, 1, 4) %in% remote) %>% 
    select(id, geometry) %>% 
    st_transform(crs = 3035)
save(gde3, file = "gde3.rda", compress = "xz")

# country borders
bord <- eurostat_geodata_60_2016 %>% 
    filter(LEVL_CODE==0,
           !str_sub(geo, 1, 4) %in% remote) %>% 
    select(id, geometry) %>% 
    st_transform(crs = 3035) %>% 
    rmapshaper::ms_innerlines()
save(gde3, file = "bord.rda", compress = "xz")

# cities to show
cities <- maps::world.cities %>%
    filter(
        pop > 6e5 | capital == 1,
        long %>% between(-20, 40),
        lat %>% between(35, 85),
        !country.etc %>% is_in(
            c("Andorra", "Faroe Islands", "Guernsey and Alderney",
              "Isle of Man", "Jersey", "Liechtenstein", "Monaco",
              "San Marino", "Svalbard and Jan Mayen", "Vatican City",
              "Algeria", "Morocco", "Gibraltar", "Syria", "Tunisia",
              "Russia", "Ukraine", "Belarus")
        )
    ) %>% 
    st_as_sf(
        coords = c("long", "lat"),
        crs = 4326
    )

# population structures
df_eu <- get_eurostat("demo_r_pjangrp3")
save(df_eu, file = "df_eu.rda", compress = "xz")



# reload back the data (start here if return to  the  code) ---------------

load("gde3.rda")
load("df_eu.rda")


# redefine age factor levels
age_levels <- c("rm", "rm", "10-19", "10-19", "20-29", "20-29", "30-39", "30-39", "40-49", "40-49", "0-9", "50-59", "50-59", "60-69", "60-69", "70-79", "70-79", "80-89", "80-89", "rm", "90+", "0-9")


# clean up the df at NUTS-3 level
dfe3 <- df_eu %>% 
    filter(
        !str_sub(geo, 1, 4) %in% remote,
        !str_sub(geo, 1, 2) %in% c("AL", "MK")
    ) %>% 
    filter(str_length(geo)==5) %>% 
    transmute(
        id = geo %>% paste,
        country = str_sub(geo, 1, 2),
        year = time %>% str_sub(1, 4) %>% as.numeric(),
        sex = sex %>% str_to_lower(), 
        # deal with age grouping
        age = age %>% lvls_revalue(age_levels) %>% paste, 
        value = values
    ) %>% 
    filter(!age=="rm") %>% 
    # summarize by 10 year age group
    group_by(id, year, sex, age) %>% 
    summarise(value = value %>% sum(na.rm = TRUE)) %>% 
    ungroup() %>% 
    drop_na() %>% 
    droplevels()


# european average proportion
eu_avg_prop <- dfe3 %>% 
    filter(year ==2019, sex=="t") %>% 
    left_join(cfr, "age") %>% 
    # assume 2/3 get infected
    mutate(death = (value*2/3) * (cfr/100)) %>% 
    summarise(value = value %>% sum(na.rm = TRUE),
              death = death %>% sum(na.rm = TRUE)) %>% 
    mutate(prop = death / value * 100) %>% 
    pull(prop)

    
# join cfr data and calculate proportion dying for total population 2019
ce3 <- dfe3 %>% 
    filter(year ==2019, sex=="t") %>% 
    left_join(cfr, "age") %>% 
    # assume 2/3 get infected
    mutate(death = (value*2/3) * (cfr/100)) %>% 
    group_by(id) %>% 
    summarise(value = value %>% sum(na.rm = TRUE),
              death = death %>% sum(na.rm = TRUE)) %>% 
    ungroup() %>% 
    drop_na() %>% 
    mutate(
        prop = death / value * 100,
        rel_prop = prop / eu_avg_prop
    ) %>% 
    # discrete
    mutate(
        rel_prop_gr = rel_prop %>% 
            cut(c(0, .5, 2/3, 4/5, .95, 100/95, 5/4, 3/2, 2, Inf)) %>% 
            lvls_revalue(
                c(
                    "Below 50% (35)", 
                    "From 50% to 67% (38)", 
                    "From 67% to 80% (64)",
                    "From 80% to 95% (174)", 
                    "European average ± 5% (234)", 
                    "From 105% to 125% (563)",
                    "From 125% to 150% (322)", 
                    "From 150% to 200% (55)", 
                    "Above 200% (1)"
                )
            )
    )
    
    
# attach geodata
me3 <- gde3 %>% left_join(ce3, "id") %>% drop_na()



pal <- RColorBrewer::brewer.pal(9, "BrBG") %>% rev 

pal.25 <- pal %>% clr_darken(shift = .25)
 
# deviation from EU level   
me3 %>% 
    # filter(!str_sub(id, 1, 2) %in% c("TR")) %>% 
    ggplot()+
    geom_sf(aes(fill = rel_prop_gr, geometry = geometry), color = NA)+
    geom_sf(data = bord, color = "#ffffff", size = .5)+
    geom_sf(data = cities, 
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_manual(values = pal.25, guide = guide_legend(ncol = 3))+
    ggthemes::theme_map(base_family = font_rc, base_size = 16)+
    theme(legend.position = "bottom",
          plot.title = element_text(family = "Roboto Slab", 
                                    face = 2, size = 24))+
    labs(title = "COVID-19 in unequally ageing European regions",
         subtitle = "NUTS-3 regions of Europe are colored according to the deviation from European pooled estimate of the proportion of population at risk of death due to COVID-19. These estimates assume age-specific case-fatality ratio the same as in Italy for the 2003 first registered COVID-19 deaths (17 March 2019) and 2/3 of the total population infected. Such an estimate for the total European population is 1.77%. Please note, this estimate is very rough and unlikely to hold true due to multiple biases of the data for the unfolding pandemic; in contrast, the population age structures data are of good quality. Thus, whatever the total infected population is and the absolute values of age-specific case-fatality ratios, the relative differences between regions would hold as long as the age-specific profile of case-fatality ratios stays proportional. This map reflects the unequal population age structures rather than the precise figures on COVID-19 fatality. It's a demographic perspective." %>% 
             str_wrap(width = 81),
         caption = "Data: Eurostat, Istituto Superiore di Sanità | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

map <- last_plot()

# plot values by countries
ce3 %>% 
    # filter(!str_sub(id, 1, 2) %in% c("TR")) %>% 
    mutate(country = id %>% str_sub(1, 2)) %>% 
    group_by(country) %>% 
    mutate(avg_prop = prop %>% weighted.mean(w = value),
           avg_rel_prop = rel_prop %>% mean) %>% 
    ungroup() %>% 
    drop_na() %>% 
    # arrange by decreasing average proprotion
    arrange(avg_prop %>% desc) %>% 
    mutate(country = country %>% fct_inorder %>% fct_rev) %>% 
    ggplot(aes(prop, country, color = rel_prop_gr, size = value/1e6))+
    geom_vline(xintercept = eu_avg_prop, size = 1, 
               color = "#B8B8B8", alpha = .5)+
    geom_point(shape = 1)+
    # add country population average marks
    geom_point(aes(x = avg_prop), shape = 124, color = "cyan", size = 4)+
    scale_color_manual(values = pal.25, guide = NULL)+
    scale_size_area(max_size = 10, breaks = c(.1, .5, 1, 5, 10))+
    scale_y_discrete(position = "right")+
    theme_minimal(base_family = font_rc)+
    theme(legend.position = c(.87, .5),
          text = element_text(lineheight = .9))+
    labs(x = "Proprotion of population\nat risk of COVID-19 death, %",
         y = NULL,
         size = "Population\nsize,\nmillion")+
    annotate("text", x = eu_avg_prop, y = 4, 
             label = "European population\npooled average, 1.77%",
             size = 4, color = "#B8B8B8", hjust = -.1, lineheight = .9,
             family = font_rc)
  
stamp <- last_plot()


# age profile of case fatality ratios
cfr %>% 
    ggplot(aes(cfr, age, fill = cfr))+
    geom_col(color = NA, width = .75)+
    scale_fill_gradientn(colours = pal.25[6:9], guide = NULL)+
    scale_x_continuous(position = "top")+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid = element_blank(),
          axis.ticks.x.top = element_line(colour = "#7F7F7F", .22),
          axis.ticks.length.x = unit(.5, "lines"))+
    labs(x = NULL, 
         y = NULL,
         subtitle = "Case Fatality\nRatio by age, %")

stamp_cfr <- last_plot()


# compose final plot
out <- ggdraw()+
    draw_plot(map)+
    draw_plot(stamp, x = .6, y = .34, width = .38, height = .33)+
    draw_plot(stamp_cfr, x = 0, y = .33, width = .2, height = .25)

# export  result as PDF
ggsave("deviations.pdf", 
       out, 
       width = 8, height = 12.5, 
       device = cairo_pdf)

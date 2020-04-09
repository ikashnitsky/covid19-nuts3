#===============================================================================
# 2020-03-19 -- covid19
# regional age differences and covid19
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================
# UPD  2020-03-26 ------------------------------
# UPD  2020-04-06 ------------------------------
# switch to Ferguson IFR



library(tidyverse)
library(magrittr)
library(paletteer)
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
# UPD  2020-04-02 ------------------------------

# CFR Italian 2 April 8592 deaths
# https://www.epicentro.iss.it/coronavirus/bollettino/Bollettino-sorveglianza-integrata-COVID-19_2-aprile-2020.pdf
cfr <-  tibble::tribble(
               ~age,   ~b,   ~f,   ~m,
              "0-9",    1e-8,    1e-8,    1e-8,
            "10-19",    1e-8,    1e-8,    1e-8,
            "20-29",  0.1,  0.1,  0.2,
            "30-39",  0.4,  0.2,  0.6,
            "40-49",  0.8,  0.4,  1.2,
            "50-59",  2.3,    1,  3.4,
            "60-69",    8,  4.8,  9.7,
            "70-79", 21.8, 15.2, 25.6,
            "80-89", 30.9, 23.1, 38.1,
              "90+", 28.7, 23.3, 40.6
            )


cfr_sr <- cfr %>% 
  mutate(mfratio=round(m/f, 1)) %>%
  select(age,mfratio) %>% 
  filter(!age=="90+") %>% 
  mutate(age = age %>% paste %>% str_replace("80-89", "80+"))

# ferguson report table 1
ifr <- tibble::tribble(
  ~age, ~hosp, ~i, ~ifr,
  "0-9", 0.01, 5.0, 0.002,
  "10-19", 0.04, 5.0, 0.006,
  "20-29", 1.1, 5.0, 0.03,
  "30-39", 3.4, 5.0, 0.08,
  "40-49", 4.3, 6.3, 0.15,
  "50-59", 8.2, 12.2, 0.60,
  "60-69", 11.8, 27.4, 2.2,
  "70-79", 16.6, 43.2, 5.1,
  "80+", 18.4, 70.9, 9.3
) %>% 
  mutate(icu=hosp*i/100) %>%
  select(-i) %>%
  pivot_longer(hosp:icu, names_to = "variable", values_to = "prop") %>% 
  left_join(cfr_sr) %>%
  mutate(
    b = prop,
    f=round(2*prop/(1+mfratio),6), #calculate male and female proportions
    m=round(2*prop*mfratio/(1+mfratio),6)
  ) %>%
  pivot_longer(b:m, names_to = "sex", values_to = "adj_prop") %>%
  rename(new_age=age) %>% 
  select(-mfratio,-prop)

# filter only ifr
adj_ifr <- ifr %>% 
  filter(variable == "ifr") %>% 
  transmute(age = new_age, sex, ifr = adj_prop)




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
save(bord, file = "bord.rda", compress = "xz")

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
save(cities, file = "cities.rda", compress = "xz")

# population structures
df_eu <- get_eurostat("demo_r_pjangrp3")
save(df_eu, file = "df_eu.rda", compress = "xz")



# reload back the data (start here if return to  the  code) ---------------

load("gde3.rda")
load("bord.rda")
load("cities.rda")
load("df_eu.rda")

# redefine age factor levels
# UPD  2020-04-06 80+ age category
age_levels <- c("rm", "rm", "10-19", "10-19", "20-29", "20-29", "30-39", "30-39", "40-49", "40-49", "0-9", "50-59", "50-59", "60-69", "60-69", "70-79", "70-79", "80+", "80+", "rm", "80+", "0-9")


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
    filter(year ==2019, !sex=="t") %>% 
    left_join(adj_ifr, c("age", "sex")) %>% 
    # assume 2/3 get infected
    mutate(death = (value*.83) * (ifr/100)) %>% 
    summarise(value = value %>% sum(na.rm = TRUE),
              death = death %>% sum(na.rm = TRUE)) %>% 
    mutate(prop = death / value * 100) %>% 
    pull(prop)

    
# join cfr data and calculate proportion dying for total population 2019
ce3 <- dfe3 %>% 
  filter(year ==2019, !sex=="t") %>% 
  left_join(adj_ifr, c("age", "sex")) %>% 
  # assume 2/3 get infected
  mutate(death = (value*.83) * (ifr/100)) %>% 
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
            cut(c(0, .5, 2/3, 4/5, .95, 100/95, 5/4, 3/2, 2, Inf))
    )

# calculate the group sizes
n_per_age_group <- ce3 %>% pull(rel_prop_gr) %>% table() %>% paste

new_levels <- paste0(
  c(
    "Below 50%", "From 50% to 67%", "From 67% to 80%",
    "From 80% to 95%", "European average ± 5%", "From 105% to 125%",
    "From 125% to 150%", "From 150% to 200%", "Above 200%"
  ),
  " (",
  n_per_age_group,
  ")"
)

ce3 <- ce3 %>% 
  mutate(rel_prop_gr = rel_prop_gr %>% lvls_revalue(new_levels))
  
    
    
# attach geodata
me3 <- gde3 %>% left_join(ce3, "id") %>% drop_na()
save(me3, file = "me3.rda", compress = "xz")

# load back the final dataset to plot
load("me3.rda")

# color palette
pal <- RColorBrewer::brewer.pal(9, "BrBG") %>% rev 
pal.25 <- pal %>% clr_darken(shift = .25)

# subtitle = "NUTS-3 regions of Europe are colored according to the deviation from European pooled estimate of the proportion of population at risk of death due to COVID-19. These estimates assume age-specific case-fatality ratio the same as in Italy for the 3047 first registered COVID-19 deaths (19 March 2020) and 2/3 of the total population infected. Such an estimate for the total European population is 2.2%. Please note, this estimate is very rough and unlikely to hold true due to multiple biases of the data for the unfolding pandemic; in contrast, the population age structures data are of good quality. Thus, whatever the total infected population is and the absolute values of age-specific case-fatality ratios, the relative differences between regions would hold as long as the age-specific profile of case-fatality ratios stays proportional. This map reflects the unequal population age structures rather than the precise figures on COVID-19 fatality. It's a demographic perspective." %>% 
#   str_wrap(width = 81),
 
# deviation from EU level   
me3 %>% 
    ggplot()+
    geom_sf(aes(fill = rel_prop_gr, geometry = geometry), color = NA)+
    geom_sf(data = bord, color = "#ffffff", size = .5)+
    geom_sf(data = cities, 
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_manual(values = pal.25, guide = guide_legend(ncol = 3))+
    ggthemes::theme_map(base_family = font_rc, base_size = 16)+
    theme(legend.position = "bottom",
          plot.title = element_text(family = "Roboto Slab", 
                                    face = 2, size = 24),
          panel.spacing = unit(0, "lines"))+
    labs(title = "COVID-19 in unequally ageing European regions",
         caption = "Data: Eurostat, Istituto Superiore di Sanità | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

map <- last_plot()

# get full country names
library(ISOcodes)

cntr <- ce3 %>% 
  mutate(country = id %>% str_sub(1, 2)) %>% 
  select(country) %>% 
  distinct() %>% 
  left_join(ISO_3166_1 %>% 
              transmute(country = Alpha_2, name = Name)) %>% 
  mutate(name = case_when(country=="UK" ~ "United Kingdom",
                          country=="EL" ~ "Greece",
                          TRUE ~ name))

# plot values by countries
ce3 %>% 
    mutate(country = id %>% str_sub(1, 2)) %>% 
    left_join(cntr) %>% 
    group_by(country) %>% 
    mutate(avg_prop = prop %>% weighted.mean(w = value),
           avg_rel_prop = rel_prop %>% mean) %>% 
    ungroup() %>% 
    drop_na() %>% 
    # arrange by decreasing average proprotion
    arrange(avg_prop %>% desc) %>% 
    mutate(name = name %>% fct_inorder %>% fct_rev) %>% 
    ggplot(aes(prop, name, color = rel_prop_gr, size = value/1e6))+
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
          text = element_text(lineheight = .9),
          axis.title.x = element_text(hjust = .7))+
    labs(x = "Proprotion of population\nat risk of COVID-19 death, %",
         y = NULL,
         size = "Population\nsize,\nmillion")+
    annotate("text", x = eu_avg_prop, y = 4, 
             label = "European population\npooled average, 1%",
             size = 3.8, fontface = 2, color = "#B8B8B8", 
             hjust = -.05, lineheight = .9,
             family = font_rc)
  
stamp <- last_plot()


# age profile of infection fatality ratios
adj_ifr %>% 
    ggplot(aes(ifr, age, color = sex))+
    geom_point(size = 3, shape = c(16, 1, 1) %>% rep(9))+
    scale_color_manual(values = c("#df356b", "#009C9C", "#eec21f"), guide = NULL)+
    scale_x_continuous(position = "top", 
                       breaks = seq(0, 10, 2.5),
                       labels = c("0", "2.5", "5", "7.5", "10"), 
                       limits = c(0, 12))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid = element_blank(),
          axis.ticks.x.top = element_line(colour = "#7F7F7F", .22),
          axis.ticks.length.x = unit(.5, "lines"))+
    labs(x = NULL, 
         y = NULL,
         subtitle = "Infection Fatality\nRatio by age and sex, %")+
  annotate("text", x = 6, y = 9, hjust = 1, size = 4, 
           label = "women", family = font_rc, color = "#009C9C")+
  annotate("text", x = 2, y = 6, hjust = 0, size = 4, 
           label = "men", family = font_rc, color = "#eec21f")

stamp_ifr <- last_plot()

# # compose final plot
# out <- ggdraw()+
#     draw_plot(map)+
#     draw_plot(stamp, x = .6, y = .34, width = .38, height = .33)+
#     draw_plot(stamp_cfr, x = 0, y = .33, width = .2, height = .25)

# compose final plot -- without lengthy subtitle
out <- ggdraw()+
  draw_plot(map)+
  draw_plot(stamp, x = .58, y = .45, width = .42, height = .47)+
  draw_plot(stamp_ifr, x = 0, y = .45, width = .2, height = .33)

  
# export  result as PDF
ggsave("deviations.pdf", 
       out, 
       width = 8, 
       # height = 12, # with lengthy caption
       height = 9,
       device = cairo_pdf)
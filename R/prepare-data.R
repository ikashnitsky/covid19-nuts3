#===============================================================================
# 2020-07-01 -- covid19
# regional age differences and covid19
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
# Jose Manuel Aburto, jose.aburto@sociology.ox.ac.uk, @jm_aburto
#===============================================================================

# This script replicates the acquisition of the data needed to reproduce the map and the inset figures.

# prepare the session -- in case you start here
source("R/prepare-session.R")


# eurostat spatial --------------------------------------------------------

# remote areas to remove (NUTS-2)
remote <- c(paste0('ES',c(63,64,70)),paste('FRY',1:5,sep=''),'PT20','PT30')

# geodata at NUTS-3 level    
gde3 <- eurostat_geodata_60_2016 %>% 
    filter(
        LEVL_CODE==3,
        !str_sub(geo, 1, 4) %in% remote
    ) %>% 
    select(id, geometry) %>% 
    st_transform(crs = 3035)

# country borders
bord <- eurostat_geodata_60_2016 %>% 
    filter(
        LEVL_CODE==0,
        !str_sub(geo, 1, 4) %in% remote
    ) %>% 
    select(id, geometry) %>% 
    st_transform(crs = 3035) %>% 
    rmapshaper::ms_innerlines()

# cities to show -- capitals and above 600k pop
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


# get full country names --------------------------------------------------

library(ISOcodes)

cntr <- gde3 %>% 
    st_drop_geometry() %>% 
    mutate(country = id %>% str_sub(1, 2)) %>% 
    select(country) %>% 
    distinct() %>% 
    left_join(ISO_3166_1 %>% 
                  transmute(country = Alpha_2, name = Name)) %>% 
    mutate(name = case_when(country=="UK" ~ "United Kingdom",
                            country=="EL" ~ "Greece",
                            TRUE ~ name))


# IFR ajusted by sex using CFR --------------------------------------------

# COVerAGE-DB for age-sex specific CFR 
# Download v 4.0 and put into the "data/raw" directory
# The version stored is from 2020-07-01
# Note: for sharing efficiency we compressed the csv file as .gz
# https://osf.io/wu5ve/download
coverage <- read_csv("data/raw/Output_10.csv.gz", skip = 3)

# calculate CFRs in the given age groups over the pooled COVerAGE-DB population
cfr_cov <- coverage %>% 
    filter(Region == "All", !Sex == "b") %>% 
    transmute(
        name = Country,
        sex = Sex,
        age = paste0(Age, "-", Age+AgeInt-1) %>% 
            str_replace_all(
                c("80-89" = "80+", "90-99" = "80+", "100-104" = "80+")
            ),
        case = Cases,
        death = Deaths
    ) %>% 
    # filter only the European countries that we will map
    right_join(cntr) %>% 
    # summarise -- pool together the whole population
    group_by(sex, age) %>% 
    summarise(
        case = case %>% sum(na.rm = T),
        death = death %>% sum(na.rm = T)
    ) %>% 
    ungroup() %>% 
    # calculate CFRs
    transmute(
        sex, age,
        cfr = death / case
    )

cfr_sr <- cfr_cov %>% 
    pivot_wider(names_from = sex, values_from = cfr) %>% 
    mutate(sr = m / f) %>%
    select(age, sr) %>% 
    drop_na()

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
    select(age, ifr)

adj_ifr <- ifr %>% 
    left_join(cfr_sr, "age") %>%
    #calculate male and female proportions
    mutate(
        b = ifr,
        f = round(2*ifr/(1+sr),6), 
        m = round(2*ifr*sr/(1+sr),6)
    ) %>%
    pivot_longer(b:m, names_to = "sex", values_to = "adj_ifr") %>%
    select(-sr,-ifr)


# eurostat population structures ------------------------------------------
df_eu <- get_eurostat("demo_r_pjangrp3")
# just in case Eurostat changes the data, a copy of the dataset downloaded on 2020-07-01 is copied to the directory "data/raw"


# redefine age factor levels -- 80+ age category
age_levels <- c("rm", "rm", "10-19", "10-19", "20-29", "20-29", "30-39", "30-39", "40-49", "40-49", "0-9", "50-59", "50-59", "60-69", "60-69", "70-79", "70-79", "80+", "80+", "rm", "80+", "0-9")


# clean up the df at NUTS-3 level
dfe3 <- df_eu %>% 
    transmute(
        id = geo %>% paste,
        country = str_sub(geo, 1, 2),
        year = time %>% str_sub(1, 4) %>% as.numeric(),
        sex = sex %>% str_to_lower(), 
        # deal with age grouping
        age = age %>% lvls_revalue(age_levels) %>% paste, 
        value = values
    ) %>% 
    # filter only needed data
    filter(
        !age=="rm",
        !str_sub(id, 1, 4) %in% remote,
        !str_sub(id, 1, 2) %in% c("AL", "MK"),
        str_length(id)==5, # only NUTS-2,
        year ==2019, 
        !sex=="t"
    ) %>% 
    # summarize by 10 year age group
    group_by(id, sex, age) %>% 
    summarise(value = value %>% sum(na.rm = TRUE)) %>% 
    ungroup() %>% 
    drop_na() %>% 
    droplevels() %>% 
    # join cfr data and calculate proportion dying for total population 2019
    left_join(adj_ifr, c("age", "sex")) %>% 
    # assume 5/6 get infected
    mutate(death = (value*5/6) * (adj_ifr/100)) %>% 
    group_by(id) %>% 
    summarise(
        value = value %>% sum(na.rm = TRUE),
        death = death %>% sum(na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    drop_na() %>% 
    mutate(
        prop = death / value * 100,
        eu_avg_prop = weighted.mean(prop, value), # average prop for Europe
        rel_prop = prop / eu_avg_prop
    ) %>% 
    # discrete
    mutate(
        rel_prop_gr = rel_prop %>% 
            cut(c(0, .5, 2/3, 4/5, .95, 100/95, 5/4, 3/2, 2, Inf))
    )

# calculate the group sizes
n_per_age_group <- dfe3 %>% pull(rel_prop_gr) %>% table() %>% paste

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

dfe3 <- dfe3 %>% 
    mutate(rel_prop_gr = rel_prop_gr %>% lvls_revalue(new_levels))


# attach geodata
me3 <- gde3 %>% left_join(dfe3, "id") %>% drop_na()


# color palette
pal <- RColorBrewer::brewer.pal(9, "BrBG") %>% rev 
pal.25 <- pal %>% clr_darken(shift = .25)


# save processed data -----------------------------------------------------
save(
    me3, bord, cities, cntr, adj_ifr, pal.25,
    file = "data/ready.rda", compress = "xz"
)


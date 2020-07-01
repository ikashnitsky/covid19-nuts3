#===============================================================================
# 2020-07-01 -- covid19
# regional age differences and covid19
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
# Jose Manuel Aburto, jose.aburto@sociology.ox.ac.uk, @jm_aburto
#===============================================================================

# This script replicates the map and the inset plots starting from the pre-processed datasets. Check "R/prepare-data.R" to replicate the data acquisition and processing steps. The 2020-07-01 versions of the datasets that might change in the future are stored in "data/raw"

# prepare the session -- in case you start here
source("R/prepare-session.R")


# load back the ready datasets to plot
load("data/ready.rda")

# deviation from EU level   
me3 %>% 
    ggplot()+
    geom_sf(aes(fill = rel_prop_gr, geometry = geometry), color = NA)+
    geom_sf(data = bord, color = "#ffffff", size = .5)+
    geom_sf(
        data = cities, 
        size = 4.5, shape = 1, stroke = .5, color = "#ffffff"
    )+
    scale_fill_manual(
        values = pal.25, 
        guide = guide_legend(ncol = 3, title.position = "top")
    )+
    ggthemes::theme_map(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = "bottom",
        plot.title = element_text(family = "Roboto Slab", 
                                  face = 2, size = 24),
        panel.spacing = unit(0, "lines")
    )+
    labs(
        fill = "How much the estimate for a specific region is below or above the European\npooled estimate of the proportion of population at risk of death due to COVID-19"
    )

map <- last_plot()


# plot values by countries
me3 %>% 
    st_drop_geometry() %>% 
    mutate(country = id %>% str_sub(1, 2)) %>% 
    left_join(cntr) %>% 
    group_by(country) %>% 
    mutate(
        avg_prop = prop %>% weighted.mean(w = value),
        avg_rel_prop = rel_prop %>% mean
    ) %>% 
    ungroup() %>% 
    drop_na() %>% 
    # arrange by decreasing average proprotion
    arrange(avg_prop %>% desc) %>% 
    mutate(name = name %>% fct_inorder %>% fct_rev) %>% 
    ggplot(aes(prop, name))+
    geom_vline(
        aes(xintercept = eu_avg_prop), size = 1, 
        color = "#B8B8B8", alpha = .5
    )+
    geom_point(aes(color = rel_prop_gr, size = value/1e6),
               shape = 1)+
    # add country population average marks
    geom_point(aes(x = avg_prop), shape = 124, color = "cyan", size = 4)+
    scale_color_manual(values = pal.25, guide = NULL)+
    scale_size_area(max_size = 10, breaks = c(.1, .5, 1, 5, 10))+
    scale_y_discrete(position = "right")+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = c(.87, .5),
        text = element_text(lineheight = .9),
        axis.title.x = element_text(hjust = .7)
    )+
    labs(
        x = "Proportion of population\nat risk of COVID-19 death, %",
        y = NULL,
        size = "Population\nsize,\nmillion"
    )+
    annotate(
        "text", x = 1.01, y = 4, 
        label = "European population\npooled estimate, 1%",
        size = 4, fontface = 2, color = "#B8B8B8", 
        hjust = -.05, lineheight = .9,
        family = font_rc
    )

stamp <- last_plot()


# age profile of infection fatality ratios
adj_ifr %>% 
    ggplot(aes(adj_ifr, age, color = sex))+
    geom_point(size = 3, shape = c(16, 1, 1) %>% rep(9))+
    scale_color_manual(
        values = c("#df356b", "#009C9C", "#eec21f"), 
        guide = NULL
    )+
    scale_x_continuous(
        position = "top", 
        breaks = seq(0, 12.5, 2.5),
        labels = c("0", "2.5", "5", "7.5", "10", "12.5"), 
        limits = c(0, 12.2)
    )+
    theme_minimal(base_family = font_rc)+
    theme(
        panel.grid = element_blank(),
        axis.ticks.x.top = element_line(colour = "#7F7F7F", .22),
        axis.ticks.length.x = unit(.5, "lines")
    )+
    labs(
        x = NULL, 
        y = NULL,
        subtitle = "Infection Fatality\nRatio by age and sex, %"
    )+
    annotate(
        "text", x = 5.5, y = 9, hjust = 1, size = 4, 
        label = "women", family = font_rc, color = "#009C9C"
    )+
    annotate(
        "text", x = 2, y = 6, hjust = 0, size = 4, 
        label = "men", family = font_rc, color = "#eec21f"
    )

stamp_ifr <- last_plot()


# compose the final plot 
out <- ggdraw()+
    draw_plot(map)+
    draw_plot(stamp, x = .58, y = .5, width = .42, height = .47)+
    draw_plot(stamp_ifr, x = .01, y = .5, width = .24, height = .33)


# export  result as PDF
ggsave(
    "figure/unequal-ageing-map.pdf", 
    out, 
    width = 9, 
    height = 9,
    device = cairo_pdf
)


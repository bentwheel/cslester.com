---
title: "The Rise and Fall of Sourdough"
date: 2020-06-17
draft: false
---

The Rise and Fall of Sourdough
==============================

Over the course of our new shelter-in-place lives in the first half of
2020, I was curious about how to best tell the story of America’s new
fascination with sourdough - and whether or not that fascination has
persisted through the year and into the summer.

The following code is a demonstration of how to web scrape Google trends
for location-specific search trends and organize this information
visually in the form of an animated choropleth (heat map) that shows
change over time.

{{< figure src="./sourdough.gif" caption="America Sours on Sourdough"  width="100%" class="img-thumbnail" link="./sourdough.gif">}}

    library(gtrendsR)

    ## Warning: package 'gtrendsR' was built under R version 3.6.2

    library(ggplot2)
    library(maps)
    library(tidyverse)

    ## ── Attaching packages ────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0
    ## ✓ purrr   0.3.3

    ## ── Conflicts ───────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()
    ## x purrr::map()    masks maps::map()

    library(ggthemes)
    library(gganimate)
    library(gifski)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    library(transformr)

    top_baking_searches <- gtrends(time = "today 3-m", gprop="web", 
                                   category = "907", geo="US")
    by_state_search <- tibble(abb = state.abb) %>% 
      distinct(abb) %>% 
      transmute(geo_search = paste0("US-",abb)) %>% 
      arrange(geo_search) %>% 
      mutate(row_index = row_number())

    map_function <- function(search_geo) {
      gtrends("sourdough", time = "today 12-m", gprop="web", 
                                  geo=search_geo, onlyInterest=T)
    }

    # This takes a while...
    by_state_trends <- as.list(by_state_search$geo_search) %>% 
      map(map_function) %>% 
      flatten() %>% 
      enframe() %>% 
      unnest()  

    ## Warning: `cols` is now required.
    ## Please use `cols = c(value)`

    state_shapes <- map_data("state") 
    state_cw <- tibble(name = tolower(state.name), abb = state.abb)

    plot.data <- by_state_trends %>% 
      separate(col = geo, into = c("c_code", "state"), sep="-") %>% 
      select(date, hits, state) %>% 
      left_join(state_cw, by=c("state"="abb")) %>% 
      mutate(hits = hits/ 100,
             date = ymd(date)) %>% 
      filter(!(state %in% c("AK","HI")))

    plot.data.final <- state_shapes %>% 
      left_join(plot.data, by=c("region"="name")) %>% 
      filter(region != "district of columbia")


    # The Rise and Fall of Sourdough Popularity

    plot <- plot.data.final %>% 
      ggplot(aes(x = long, y = lat, group = group, fill = hits)) +
      geom_polygon(color = "darkgray", size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme_map() + 
      theme(text = element_text(family = "Courier Prime"),
            legend.position = "bottom", legend.direction = "horizontal",
            legend.title = element_text(size = 12),
            plot.title = element_text(size = 20),
            plot.subtitle  = element_text(size = 18),
            plot.caption = element_text(size = 16)) + 
      scale_fill_gradient(low = "white", high = "#756bb1") +
      labs(title = "The Rise of Sourdough",
           subtitle = "Relative search popularity, week of {format(frame_time, '%Y-%m-%d')}",
           caption = "Source: Google Trends",
           fill = "Relative in-state popularity of search") +
      transition_time(date) +
      enter_fade() +
      exit_fade()
      
    plot_anim <- animate(plot, fps = 4, end_pause = 10, nframes = 52,
                         height = 625, width = 875)

    anim_save("sourdough.gif", plot_anim, path="~")

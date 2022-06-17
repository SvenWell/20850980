Seven_day_MA <- function(data, caseordeath){

    if(caseordeath == "case") {
        g <- covid %>%
            arrange(date) %>%
            group_by(location) %>%
            filter(location %in% c("Europe", "Asia", "Africa", "North America", "South America", "Oceania", "Antarctica")) %>%
            select(location, date, new_cases_smoothed, new_deaths_smoothed) %>%
            mutate(roll_mean = rollmean(new_cases_smoothed, 7, fill = NA)) %>%
            ggplot() +
            geom_line(aes(x = date, y = roll_mean, color = location), alpha = 1, size = 0.4) +
            theme_bw() +
            theme(legend.position = "bottom", legend.title=element_blank()) +
            labs(x = "",
                 y = "7 day rolling average",
                 title = "Daily new confirmed Covid-19 cases per million people",
                 subtitle = "7-day rolling average cases of Covid-19 by continent",
                 caption = "Data source: Ourworldindata.org") +
            facet_wrap(~location, scales = "free_y")
    } else if(caseordeath == "death") {
        g <- covid %>%
            arrange(date) %>%
            group_by(location) %>%
            filter(location %in% c("Europe", "Asia", "Africa", "North America", "South America", "Oceania", "Antarctica")) %>%
            select(location, date, new_cases_smoothed, new_deaths_smoothed) %>%
            mutate(roll_mean = rollmean(new_deaths_smoothed, 7, fill = NA)) %>%
            ggplot() +
            geom_line(aes(x = date, y = roll_mean, color = location), alpha = 1, size = 0.4) +
            theme_bw() +
            theme(legend.position = "bottom", legend.title=element_blank()) +
            labs(x = "",
                 y = "7 day rolling average",
                 title = "Daily new confirmed Covid-19 death per million people",
                 subtitle = "7-day rolling average deaths from Covid-19 by continent",
                 caption = "Data source: Ourworldindata.org") +
            facet_wrap(~location, scales = "free_y")
    }

    g

}







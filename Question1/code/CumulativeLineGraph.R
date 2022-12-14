CumulativeLineGraph <- function(data, caseordeath){

    if(caseordeath == "case") {
        g <- covid %>%
            filter(location %in% c("Europe", "Asia", "Africa", "North America", "South America", "Oceania", "Antarctica")) %>%
            select(location, date, total_cases_per_million, total_deaths_per_million) %>%
            ggplot() +
            geom_line(aes(x = date, y = total_cases_per_million, color = location), alpha = 1, size = 0.4) +
            theme_bw() +
            theme(legend.position = "bottom", legend.title=element_blank()) +
            labs(x = "", y = "Covid-19 Cases", title = "Cumulative cases of Covid-19 by continent", caption = "Data source: Ourworldindata.org")
    } else if(caseordeath == "death") {
        g <- covid %>%
            filter(location %in% c("Europe", "Asia", "Africa", "North America", "South America", "Oceania", "Antarctica")) %>%
            select(location, date, total_cases_per_million, total_deaths_per_million) %>%
            ggplot() +
            geom_line(aes(x = date, y = total_deaths_per_million, color = location), alpha = 1, size = 0.4) +
            theme_bw() +
            theme(legend.position = "bottom", legend.title=element_blank()) +
            labs(x = "", y = "Deaths from Covid-19", title = "Cumulative Deaths from Covid-19 by continent", caption = "Data source: Ourworldindata.org")
    }

    g

}

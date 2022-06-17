Case_Fatality_Ratio <- function(data, populationorpoverty){

    if(populationorpoverty == "population") {
        g <- covid %>%
            group_by(location) %>%
            mutate(Cases = max(total_cases, na.rm=T),
                   Deaths = max(total_deaths, na.rm=T),
                   CaseFatalityRatio = Deaths/Cases*100) %>%
            select(location, population_density, Cases, Deaths, CaseFatalityRatio) %>%
            unique() %>%
            drop_na() %>%
            arrange(desc(Cases)) %>%
            head(20) %>%
            ggplot() +
            geom_point(aes(x = population_density, y = CaseFatalityRatio, colour = location)) +
            theme_bw() +
            theme(legend.position = "bottom", legend.title=element_blank()) +
            labs(x = "Population density",
                 y = "Case Fatality Ratio",
                 title = "Effect of population density on the case fatality ratio",
                 caption = "Data source: Ourworldindata.org")

    } else if (populationorpoverty == "poverty") {
        g <- covid %>%
            group_by(location) %>%
            mutate(Cases = max(total_cases, na.rm=T),
                   Deaths = max(total_deaths, na.rm=T),
                   CaseFatalityRatio = Deaths/Cases*100) %>%
            select(location, extreme_poverty, Cases, Deaths, CaseFatalityRatio) %>%
            unique() %>%
            drop_na() %>%
            arrange(desc(Deaths)) %>%
            head(20) %>%
            ggplot() +
            geom_point(aes(x = extreme_poverty, y = CaseFatalityRatio, colour = location)) +
            theme_bw() +
            theme(legend.position = "bottom", legend.title=element_blank()) +
            labs(x = "Percentage Extreme Poverty",
                 y = "Case Fatality Ratio",
                 title = "Effect of poverty density on the case fatality ratio",
                 caption = "Data source: Ourworldindata.org")
    }

    g

}

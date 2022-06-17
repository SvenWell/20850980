Year_plot <- function(data, year){

    startyear <- ymd(paste(year,"0101", sep = ""))
    endyear <- ymd(paste(year+1,"0101", sep = ""))

    data_year <- data %>% filter(date >= startyear & date < endyear)

    g <- data_year %>%
        ggplot(aes(x = date, y = mean_temp)) +
        geom_point( aes(colour = mean_temp)) +
        scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 16) +
        geom_smooth(color = "red",size = 1) +
        scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
        ggtitle ("Daily average temperature") +
        xlab("Date") +  ylab ("Average Temperature ( ºC )")

    g

}


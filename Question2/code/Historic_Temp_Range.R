Historic_Temp_Range <- function(data, colour){

    data_months <- data %>%
        mutate( months = format(date, "%B"))

    g <- data_months %>%
        ggplot(aes(x = months, y = mean_temp)) +
        geom_violin(fill = colour) +
        scale_x_discrete(limits = month.name) +
        ggtitle ("Temperature distribution by month") +
        xlab("Month") +  ylab ("Average temperature ( ºC )")

    g
}

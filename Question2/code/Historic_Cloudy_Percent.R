Historic_Cloudy_Percent <- function(data){

    data_months_cloudy <- data %>%
        mutate(months = format(date, "%B")) %>%
        group_by(months) %>%
        summarize(clouds = mean(cloud_cover, na.rm = TRUE),
                  sunny = mean(sunshine, na.rm = TRUE)) %>%
        mutate(pctcloudy = clouds/(clouds+sunny)*100)

    g <- data_months_cloudy %>%
        ggplot() +
        geom_bar(aes(x = months, y = pctcloudy, fill = pctcloudy), stat = "identity", colour = "blue") +
        scale_x_discrete(limits = month.name) +
        xlab("Months") +  ylab ("Percentage Cloudy to Sunny") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        scale_color_hue(direction = -1, h.start=90)

    g
}

portfolio_return_function_q7 <- function(data = data, weights = weights){
    stocks <- data

    J200_RetPort <- rmsfuns::Safe_Return.portfolio(data,
                                                   weights = weights, lag_weights = TRUE,
                                                   verbose = TRUE, contribution = TRUE,
                                                   value = 1, geometric = TRUE)

    # Need to clean and save the data

    # Clean and save portfolio returns and weights:
    J200_Contribution <- J200_RetPort$"contribution" %>% xts_tbl()

    J200_BPWeight <- J200_RetPort$"BOP.Weight" %>% xts_tbl()

    J200_BPValue <- J200_RetPort$"BOP.Value" %>% xts_tbl()

    names(J200_Contribution) <- c("date", names(J200_RetPort$"contribution"))
    names(J200_BPWeight) <- c("date", names(J200_RetPort$"BOP.Weight"))
    names(J200_BPValue) <- c("date", names(J200_RetPort$"BOP.Value"))

    # Let's bind all of these together now:
    df_port_return_J200 <-
        left_join(stocks %>% select(date, Tickers, Return),
                  J200_BPWeight %>% gather(Tickers, weight, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(., J200_BPValue %>% gather(Tickers, value_held, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(., J200_Contribution %>% gather(Tickers, Contribution, -date),
                  by = c("date", "Tickers"))


    # Calculate Portfolio Returns:
    df_Portf_J200 <- df_port_return_J200 %>%
        group_by(date) %>%
        summarise(PortfolioReturn = sum(Return*weight, na.rm = TRUE)) %>%
        filter(PortfolioReturn != 0)


    output <- df_Portf_J200 %>% rename(J200 = PortfolioReturn) %>%
        pivot_longer(c("J200"), names_to = "Portfolio", values_to = "Returns")
    output
}

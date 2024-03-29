impute_missing_returns <- function(return_mat, impute_returns_method = "NONE"){
    # Make sure we have a date column called date:
    if( !"date" %in% colnames(return_mat) ) stop("No 'date' column provided in return_mat. Try again please.")

    # Note my use of 'any' below...
    # Also note that I 'return' return_mat - which stops the function and returns return_mat.
    if( impute_returns_method %in% c("NONE", "None", "none") ) {
        if( any(is.na(return_mat)) ) warning("There are missing values in the return matrix..
                                         Consider maybe using impute_returns_method = 'Drawn_Distribution_Own' / 'Drawn_Distribution_Collective'")
        return(return_mat)
    }

    if( impute_returns_method  == "Average") {

        return_mat <-
            return_mat %>% gather(Stocks, Returns, -date) %>%
            group_by(date) %>%
            mutate(Avg = mean(Returns, na.rm=T)) %>%
            mutate(Avg = coalesce(Avg, 0)) %>% # date with no returns - set avg to zero
            ungroup() %>%
            mutate(Returns = coalesce(Returns, Avg)) %>% select(-Avg) %>% spread(Stocks, Returns)

        # That is just so much easier when tidy right? See how I gathered and spread again to give back a wide df?
        return(return_mat)
    } else

        if( impute_returns_method  == "Drawn_Distribution_Own") {

            N <- nrow(return_mat)
            return_mat <-
                # DIY: see what density function does
                left_join(return_mat %>% gather(Stocks, Returns, -date),
                          return_mat %>% gather(Stocks, Returns, -date) %>% group_by(Stocks) %>%
                              mutate(Dens = list(density(Returns, na.rm=T))) %>%
                              summarise(set.seed(42), Random_Draws = list(sample(Dens[[1]]$x, N, replace = TRUE, prob=.$Dens[[1]]$y))),
                          by = "Stocks"
                ) %>%  group_by(Stocks) %>%
                # Random draw from sample:
                mutate(Returns = coalesce(Returns, Random_Draws[[1]][row_number()])) %>%
                select(-Random_Draws) %>% ungroup() %>% spread(Stocks, Returns)
            return(return_mat)
        } else

            if( impute_returns_method  == "Drawn_Distribution_Collective") {
                NAll <- nrow(return_mat %>% gather(Stocks, Returns, -date))
                # DIY: see what density function does
                return_mat <-
                    bind_cols(
                        return_mat %>% gather(Stocks, Returns, -date),
                        return_mat %>% gather(Stocks, Returns, -date) %>%
                            mutate(Dens = list(density(Returns, na.rm=T))) %>%
                            summarise(set.seed(42), Random_Draws = list(sample(Dens[[1]]$x, NAll, replace = TRUE, prob=.$Dens[[1]]$y))) %>%
                            unnest(Random_Draws)
                    ) %>%
                    mutate(Returns = coalesce(Returns, Random_Draws)) %>% select(-Random_Draws) %>% spread(Stocks, Returns)
                return(return_mat)
            } else

                if( impute_returns_method  == "Zero") {
                    warning("This is probably not the best idea but who am I to judge....")
                    return_mat[is.na(return_mat)] <- 0
                    return(return_mat)
                } else
                    stop("Please provide a valid impute_returns_method method. Options include:\n'Average', 'Drawn_Distribution_Own', 'Drawn_Distribution_Collective' and 'Zero'.")

    return_mat

}
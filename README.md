# Purpose

This is the README for the Financial Econometrics Practical Exam. This
folder was created using by running:

``` r
fmxdat::make_project(Mac = TRUE)
```

With the folder structure for all of the questions made with the
following code: \* Noting the difference between creating an html output
and a pdf output

``` r
CHOSEN_LOCATION <- "/Users/svenwellmann/Desktop/Masters Semester 2/Financial Econometrics/Exam/"

Texevier::create_template_html(directory = glue::glue("{CHOSEN_LOCATION}20850980/Questions/"), template_name = "Question1")

Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}20850980/Questions/"), template_name = "Question2")

Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}20850980/Questions/"), template_name = "Question3")

Texevier::create_template_html(directory = glue::glue("{CHOSEN_LOCATION}20850980/Questions/"), template_name = "Question4")

Texevier::create_template_html(directory = glue::glue("{CHOSEN_LOCATION}20850980/Questions/"), template_name = "Question5")

Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}20850980/Questions/"), template_name = "Question6")

Texevier::create_template_html(directory = glue::glue("{CHOSEN_LOCATION}20850980/Questions/"), template_name = "Question7")
```

Cleaning the environment, loading packages and sourcing all functions:

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection 
```

    ##          used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
    ## Ncells 480318 25.7    1038882 55.5         NA   666908 35.7
    ## Vcells 892018  6.9    8388608 64.0      16384  1824468 14.0

``` r
pacman::p_load("xts", "tidyverse", "tbl2xts", "PerformanceAnalytics", "lubridate", "glue", "rmsfuns", "fmxdat", "tidyr", "devtools", "readr", "TTR",
               "DEoptimR", "robustbase", "rportfolios", "RiskPortfolios", "fitHeavyTail", "quadprog", "cowplot")
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Question 1

## Fund Performance

## Code used for Figures and Tables

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection 
```

    ##           used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells 2441641 130.4    4618865 246.7         NA  3499542 186.9
    ## Vcells 4078857  31.2    8388608  64.0      16384  6751771  51.6

``` r
pacman::p_load("tidyverse", "lubridate")
```

``` r
list.files('Question1/code/', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))
```

## Data

The raw data files were placed in their respective question data folders

# Question 2

## Yield Spreads

We must load in the data from the data folder within the question
folder.

``` r
loc2 <- "Questions/Question2/data/"

SA_bonds <- read_rds(glue::glue("{loc2}SA_Bonds.rds"))
BE_Infl <- read_rds(glue::glue("{loc2}BE_Infl.rds")) # 10yr Break even inflation estimate
bonds_2y <- read_rds(glue::glue("{loc2}bonds_2y.rds")) # International 2yr spreads
bonds_10y <- read_rds(glue::glue("{loc2}bonds_10y.rds")) # International 10yr spreads
usdzar <- read_rds(glue::glue("{loc2}usdzar.rds"))
ZA_Infl <- read_rds(glue::glue("{loc2}ZA_Infl.rds"))
IV <- read_rds(glue::glue("{loc2}IV.rds")) # Volatility Index
```

Then with the data we must clean all the data we want to use and
creature new features we can graph. For the SA_Bonds we need to gather
the data into tidy format to make plotting easier. We will also clean
the inflation data and exchange rate data.

``` r
pacman::p_load("tidyverse", "lubridate", "fmxdat")

# Transforming the data into tidy format
SA_bonds_clean <- SA_bonds %>%  
    arrange(date) %>% 
    gather(Ticker, Yield, -date)

# Creating yield spread features 
SA_Spreads <- SA_bonds %>% 
    arrange(date) %>% 
    group_by(date) %>% 
    mutate(S_2Yr = ZA_2Yr-SA_3M, S_10Yr = ZA_10Yr-SA_3M) %>% 
    ungroup() %>% 
    pivot_longer(c("S_2Yr", "S_10Yr"), names_to = "Spreads", values_to = "Rates") 

# SA Bonds correct format to left_join with Inflation
# Keeping the data wide
SA_bonds_inf <- SA_bonds %>% 
    arrange(date) %>% 
    group_by(date) %>% 
    mutate(S_2Yr = ZA_2Yr-SA_3M, S_10Yr = ZA_10Yr-SA_3M) %>% 
    filter(date >= lubridate::ymd(20200101)) %>% 
    mutate(YM = format(date, "%Y_%B")) %>% 
    group_by(YM) %>% 
    filter(date == last(date)) %>% 
    ungroup()

# Cleaning Break-even Inflation into monthly values
BE_Infl_clean <- BE_Infl %>% 
    select(-Name) %>%  
    arrange(date) %>% 
    rename(BE_Infl = Price) %>% 
    filter(date >= lubridate::ymd(20200101)) %>% 
    mutate(YM = format(date, "%Y_%B")) %>% 
    group_by(YM) %>% 
    filter(date == last(date)) %>% 
    select(-date)

# Cleaning RSA Inflation into monthly values
ZA_Infl_clean <- ZA_Infl %>% 
        arrange(date) %>% 
        filter(date >= lubridate::ymd(20200101)) %>% 
        rename(Inflation = Price) %>% 
        select(-Name) %>% 
        mutate(YM = format(date, "%Y_%B")) %>% 
        select(-date)

# Left_join Bonds spreads, Break even inflation and ZA inflation and then put make long
SA_spreads_inf <- left_join(SA_bonds_inf, left_join(BE_Infl_clean, ZA_Infl_clean, by = "YM"), by = "YM") %>% 
    select(-YM) %>% 
    pivot_longer(c("S_2Yr", "S_10Yr", "BE_Infl", "Inflation"), names_to = "Spreads", values_to = "Rates")

# Cleaning USD-ZAR 
usdzar_clean <- usdzar %>% 
    arrange(date) %>% 
    filter(date >= lubridate::ymd(20200101)) %>% 
    rename(EX = Price) %>% 
    select(-Name)

# Left_join daily bond yields and the USD-ZAR exchange rate

usd_zar_since2020 <- left_join(SA_bonds %>% 
    arrange(date) %>% 
    group_by(date) %>% 
    mutate(S_2Yr = ZA_2Yr-SA_3M, S_10Yr = ZA_10Yr-SA_3M) %>% 
    filter(date >= lubridate::ymd(20200101)), usdzar_clean, by = "date") %>% 
    pivot_longer(c("S_2Yr", "S_10Yr", "EX"), names_to = "Spreads", values_to = "Rates")
```

## First graph

This will look at bond returns historically and how they have moved.
This will then be filtered to look at bond yields since 2020 and assess
their movements.

``` r
SA_Bond_All <- SA_bonds_clean %>% 
    ggplot() +
    geom_line(aes(date, Yield, color = Ticker)) +
    fmxdat::theme_fmx(title.size = ggpts(30),
                      subtitle.size = ggpts(28),
                      caption.size = ggpts(25),
                      CustomCaption = T) + 
  fmxdat::fmx_cols() + 
  labs(x = "", 
       y = "Bond Yields", 
       caption = "Note:\nCalculation own",
       title = "Bond Yields in South Africa across the 3 month, 2 year and 10 year maturities") + 
    guides(color = F)
```

    ## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    ## of ggplot2 3.3.4.

``` r
fmxdat::finplot(SA_Bond_All, x.vert = T, x.date.type = "%Y", x.date.dist = "2 years")
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

This graph shows that over the last 20+ years bond yields have actually
decreased relatively. With the largest decline that of the shortest
maturity bond, the 3-month bond. What we can see in this graph is that
the bond yields before 2020 were moving with one another. Since 2020 you
can see a divergence of the Bond Yields, taking us to our next analysis:
Yield Spreads.

## Second graph

Bond spreads across the same 20+ year range.

``` r
bond_spreads <- SA_Spreads %>% 
    ggplot() +
    geom_line(aes(date, Rates, colour = Spreads), alpha = 0.8) +
    labs(title = "Mid and Long Term SA Spreads", y = "Spreads (Yields)", x ="") +
    fmxdat::theme_fmx() + 
    fmxdat::fmx_cols()

fmxdat::finplot(bond_spreads, x.date.type = "%Y", x.vert = TRUE, x.date.dist = "2 years")
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png) This
plot confirms that the spreads do face a significant spike in 2020, this
can be seen especially for the spread of the long-term (10 year) bond
yield. Now we look further into the data from 2020 until now.

## Third graph

``` r
bond_spreads_since2020 <- SA_Spreads %>% 
    filter(date >= as.Date("2020/01/01")) %>% 
    ggplot() +
    geom_line(aes(date, Rates, colour = Spreads), alpha = 0.8) +
    labs(title = "Mid and Long Term SA Spreads", y = "Spreads (Yields)", x ="") +
    fmxdat::theme_fmx() + 
    fmxdat::fmx_cols()

fmxdat::finplot(bond_spreads_since2020, x.date.type = "%Y%m", x.vert = TRUE )
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

## Fourth Graph

Below we graph Break-even inflation, inflation and mid-term and
long-term bond yields.

``` r
SA_spreads_inf_since2020 <- SA_spreads_inf %>% 
    ggplot() +
    geom_line(aes(date, Rates, colour = Spreads), alpha = 0.8) +
    labs(title = "Mid and Long Term SA Spreads", 
         y = "Spreads (Yields)", 
         x ="", 
         subtitle = "Including Inflation and Break-Even 10 Year Yield (End of Month)") +
    fmxdat::theme_fmx(title.size = ggpts(25), subtitle.size = ggpts(20)) + 
    fmxdat::fmx_cols()

fmxdat::finplot(SA_spreads_inf_since2020, x.date.type = "%Y%m", x.vert = TRUE)
```

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)

What we witness is that Break-even Inflation and the long-term bond
spread tracks very closely since 2020. We see inflation rising since
2021 and the 2 year yield spread remains somewhat stationary while
inflation increases.

# Can look at this across the entirre dataframe except in % Changes

## Fifth Graph - Exchange rate and bonds

The below graph shows how the USD-ZAR exchange rate and the medium and
long-term bond yields moved in sync at the beginning of 2020. The
exchange rate lowered from its peak in 2020 while the long-term bond
yields remained at their high level.

``` r
usd_zar_plot <- usd_zar_since2020 %>% 
    ggplot() +
    geom_line(aes(date, Rates, colour = Spreads), alpha = 0.8) +
    labs(title = "Mid and Long Term SA Spreads", y = "Spreads (Yields)", x ="", subtitle = "Including USD/ZAR Exchange Rate") +
    fmxdat::theme_fmx(title.size = ggpts(25), subtitle.size = ggpts(20)) + 
    fmxdat::fmx_cols()

fmxdat::finplot(usd_zar_plot, x.date.type = "%Y%m", x.vert = TRUE)
```

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)

## Graph 6

Compare ZAR yields to other countries

## Graph 7

Do a correlation through time of the exchange rate on yield spreads.

## Graph 8

Volatility Index Graph

# Question 3

## Portfolio Construction

First we have to load in the data

``` r
loc3 <- "Questions/Question3/data/"

usdzar <- read_rds(glue::glue("{loc2}usdzar.rds"))
T40 <- read_rds(glue::glue("{loc3}T40.rds"))
RebDays <- read_rds(glue::glue("{loc3}Rebalance_days.rds"))
```

What do we want to do?

ALSI(J200) and the SWIX(J400) to write a report to find differences in
the return profiles.

We must first look at different: \* sized indexes \* sector exposures \*
stock concentration over time.

Can use stratification of usdzar in order to look into return profiles
in different periods of currency volatility.

## Firstly a Sector and Index analysis

Compare the performances of the different sectors for the ALSI and SWIX.
Both the ALSI and SWIX indices track each other closely. Clearly
industrials were the best performing sector having grow by 1400%.
Recently resources has begun to outperform financials.

## What do we have

T40 has the Tickers, Return for the Tickers per day, Sector, Index_Name,
and the weights for both ALSI and SWIX RebDays has the dates of
rebalance Effective Date and Trade Day

## What we need:

Portfolio return per sector and per Index_Name.

For Portfolio Returns we are going to use Safe_Return.portfolio function
from PerformanceAnalytics and for that we need the data wide and in xts
format

``` r
# # First we get the weights of the ALSI and SWIX into xts format
# j200_weights <- T40 %>% select(date, Tickers, J200) %>% spread(Tickers, J200) %>% tbl_xts()
# j400_weights <- T40 %>% select(date, Tickers, J400) %>% spread(Tickers, J400) %>% tbl_xts()
# 
# # Then we get the returns for all the Tickers into xts format
# T40_returns <- T40 %>% select(date, Tickers, Return) %>% spread(Tickers, Return) %>% tbl_xts()
# 
# # Forcing the NA values to 0
# j200_weights[is.na(j200_weights)] <- 0
# j400_weights[is.na(j400_weights)] <- 0
# T40_returns[is.na(T40_returns)] <- 0
```

Now we use the Safe_Return.portfolio function to get portfolio returns
for each portfolio based on their seperate weightings

``` r
portfolio_return_function <- function(data = data, sector = "", index = ""){
    stocks <- data
    
    if(!sector == ""){
        stocks <- data %>% filter(Sector %in% sector) %>% 
            group_by(date) %>% 
            mutate(J400 = J400/sum(J400, na.rm = TRUE), J200 = J200/sum(J200, na.rm = TRUE)) %>% 
            ungroup()
    } 
    if(!index == ""){
        stocks <- data %>% filter(Index_Name %in% index) %>% 
            group_by(date) %>% 
            mutate(J400 = J400/sum(J400, na.rm = TRUE), J200 = J200/sum(J200, na.rm = TRUE)) %>% 
            ungroup()
    } 
    if(!sector == "" & !index == ""){
        stocks <- data %>%
            group_by(date) %>% 
            mutate(J400 = J400/sum(J400, na.rm = TRUE), J200 = J200/sum(J200, na.rm = TRUE)) %>% 
            ungroup()
    }
    
    # First we get the weights of the ALSI and SWIX into xts format
    j200_weights <- stocks %>% select(date, Tickers, J200) %>% spread(Tickers, J200) %>% tbl_xts()
    j400_weights <- stocks %>% select(date, Tickers, J400) %>% spread(Tickers, J400) %>% tbl_xts()
    
    # Then we get the returns for all the Tickers into xts format
    stock_returns <- stocks %>% select(date, Tickers, Return) %>% spread(Tickers, Return) %>% tbl_xts()
    
    # Forcing the NA values to 0
    j200_weights[is.na(j200_weights)] <- 0
    j400_weights[is.na(j400_weights)] <- 0
    stock_returns[is.na(stock_returns)] <- 0
    
    
    J200_RetPort <- rmsfuns::Safe_Return.portfolio(stock_returns, 
                                                   weights = j200_weights, lag_weights = TRUE,
                                                   verbose = TRUE, contribution = TRUE, 
                                                   value = 1, geometric = TRUE) 
    
    J400_RetPort <- rmsfuns::Safe_Return.portfolio(stock_returns, 
                                                 weights = j400_weights, 
                                                 lag_weights = TRUE,
                                                 verbose = TRUE, contribution = TRUE, 
                                                 value = 1, geometric = TRUE) 
    
    # Need to clean and save the data
    
    # Clean and save portfolio returns and weights:
    J200_Contribution <- J200_RetPort$"contribution" %>% xts_tbl() #%>% mutate(date = lag(date), date = coalesce(date, index(j200_weights)[1]) )
    
    J200_BPWeight <- J200_RetPort$"BOP.Weight" %>% xts_tbl() #%>%
        #mutate(date = lag(date), date = coalesce(date, index(j200_weights)[1]) )
        
    J200_BPValue <- J200_RetPort$"BOP.Value" %>% xts_tbl()# %>% 
        #mutate(date = lag(date), date = coalesce(date, index(j200_weights)[1]) )
        
    # Clean and save portfolio returns and weights:
    J400_Contribution <- J400_RetPort$"contribution" %>% xts_tbl() #%>%
        #mutate(date = lag(date), date = coalesce(date, index(j400_weights)[1]) )
        
    J400_BPWeight <- J400_RetPort$"BOP.Weight" %>% xts_tbl()# %>%
        #mutate(date = lag(date), date = coalesce(date, index(j400_weights)[1]) )
        
    J400_BPValue <- J400_RetPort$"BOP.Value" %>% xts_tbl() #%>% 
        #mutate(date = lag(date), date = coalesce(date, index(j400_weights)[1]) )
    
    names(J200_Contribution) <- c("date", names(J200_RetPort$"contribution"))
    names(J200_BPWeight) <- c("date", names(J200_RetPort$"BOP.Weight"))
    names(J200_BPValue) <- c("date", names(J200_RetPort$"BOP.Value"))
    names(J400_Contribution) <- c("date", names(J400_RetPort$"contribution"))
    names(J400_BPWeight) <- c("date", names(J400_RetPort$"BOP.Weight"))
    names(J400_BPValue) <- c("date", names(J400_RetPort$"BOP.Value"))

    
    # Let's bind all of these together now:
    df_port_return_J200 <- 
        left_join(stocks %>% select(date, Tickers, Return), 
                  J200_BPWeight %>% gather(Tickers, weight, -date), 
                  by = c("date", "Tickers") ) %>% 
        left_join(., J200_BPValue %>% gather(Tickers, value_held, -date), 
                  by = c("date", "Tickers") ) %>% 
        left_join(., J200_Contribution %>% gather(Tickers, Contribution, -date), 
                  by = c("date", "Tickers"))
    
    df_port_return_J400 <- 
        left_join(stocks %>% select(date, Tickers, Return),
                  J400_BPWeight %>% gather(Tickers, weight, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(., J400_BPValue %>% gather(Tickers, value_held, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(., J400_Contribution %>% gather(Tickers, Contribution, -date),
                  by = c("date", "Tickers"))
    
    # Calculate Portfolio Returns:
    df_Portf_J200 <- df_port_return_J200 %>% 
        group_by(date) %>% 
        summarise(PortfolioReturn = sum(Return*weight, na.rm = TRUE)) %>% 
        filter(PortfolioReturn != 0)
    
    # Calculate Portfolio Returns:
    df_Portf_J400 <- df_port_return_J400 %>% 
        group_by(date) %>% 
        summarise(PortfolioReturn = sum(Return*weight, na.rm = TRUE)) %>% 
        filter(PortfolioReturn != 0)
        
    output <- left_join(df_Portf_J200 %>% rename(J200 = PortfolioReturn), 
                     df_Portf_J400 %>%  rename(J400 = PortfolioReturn), 
                     by = "date") %>% 
        pivot_longer(c("J200", "J400"), names_to = "Portfolio", values_to = "Returns")
    output
}
```

``` r
# Compare Sectors for ALSI and SWIX

sectors <- T40 %>% pull(Sector) %>% unique()
sector_return <- list()

for(i in 1:length(sectors)){
    # Loop through sectors and calculate returns and cumulative returns
    sector_return[[i]] <- portfolio_return_function(T40, sector = sectors[i]) %>% group_by(Portfolio) %>% 
    mutate(cumreturn_Rand = (cumprod(1 + Returns))) %>% # Start at 1
        mutate(cumreturn_Rand = cumreturn_Rand/first(cumreturn_Rand)) %>% 
        mutate(Sector = sectors[i])
}

# Rename tibbles
names(sector_return) <- sectors
sector_return
```

    ## $Resources
    ## # A tibble: 6,916 × 5
    ## # Groups:   Portfolio [2]
    ##    date       Portfolio   Returns cumreturn_Rand Sector   
    ##    <date>     <chr>         <dbl>          <dbl> <chr>    
    ##  1 2008-01-02 J200       0.0148            1     Resources
    ##  2 2008-01-02 J400       0.0164            1     Resources
    ##  3 2008-01-03 J200       0.00699           1.01  Resources
    ##  4 2008-01-03 J400       0.0131            1.01  Resources
    ##  5 2008-01-04 J200      -0.000828          1.01  Resources
    ##  6 2008-01-04 J400      -0.00315           1.01  Resources
    ##  7 2008-01-07 J200      -0.0184            0.988 Resources
    ##  8 2008-01-07 J400      -0.0153            0.994 Resources
    ##  9 2008-01-08 J200       0.0151            1.00  Resources
    ## 10 2008-01-08 J400       0.0213            1.02  Resources
    ## # … with 6,906 more rows
    ## 
    ## $Industrials
    ## # A tibble: 6,916 × 5
    ## # Groups:   Portfolio [2]
    ##    date       Portfolio  Returns cumreturn_Rand Sector     
    ##    <date>     <chr>        <dbl>          <dbl> <chr>      
    ##  1 2008-01-02 J200       0.0123           1     Industrials
    ##  2 2008-01-02 J400       0.0130           1     Industrials
    ##  3 2008-01-03 J200      -0.0179           0.982 Industrials
    ##  4 2008-01-03 J400      -0.0194           0.981 Industrials
    ##  5 2008-01-04 J200      -0.00681          0.975 Industrials
    ##  6 2008-01-04 J400      -0.00380          0.977 Industrials
    ##  7 2008-01-07 J200      -0.00122          0.974 Industrials
    ##  8 2008-01-07 J400      -0.00368          0.973 Industrials
    ##  9 2008-01-08 J200       0.00680          0.981 Industrials
    ## 10 2008-01-08 J400       0.00481          0.978 Industrials
    ## # … with 6,906 more rows
    ## 
    ## $Financials
    ## # A tibble: 6,916 × 5
    ## # Groups:   Portfolio [2]
    ##    date       Portfolio  Returns cumreturn_Rand Sector    
    ##    <date>     <chr>        <dbl>          <dbl> <chr>     
    ##  1 2008-01-02 J200       0.00808          1     Financials
    ##  2 2008-01-02 J400       0.0101           1     Financials
    ##  3 2008-01-03 J200      -0.0189           0.981 Financials
    ##  4 2008-01-03 J400      -0.0193           0.981 Financials
    ##  5 2008-01-04 J200      -0.0158           0.966 Financials
    ##  6 2008-01-04 J400      -0.0132           0.968 Financials
    ##  7 2008-01-07 J200      -0.00957          0.956 Financials
    ##  8 2008-01-07 J400      -0.00907          0.959 Financials
    ##  9 2008-01-08 J200       0.00818          0.964 Financials
    ## 10 2008-01-08 J400       0.0102           0.969 Financials
    ## # … with 6,906 more rows

``` r
# Combine Dataframes
sectors_cum_ret <- rbind(sector_return[[1]], sector_return[[2]], sector_return[[3]]) %>% arrange(date)
    
q2_p1 <- sectors_cum_ret %>% 
    ggplot() +
    geom_line(aes(date, cumreturn_Rand, colour = Portfolio), alpha = 0.8) + facet_wrap(~Sector) + fmxdat::fmx_cols() + 
    labs(title = "Cumulative Returns per Sector for ALSI and SWIX", y = "Cumulative Returns", x = "") + 
    fmxdat::theme_fmx(title.size = ggpts(25))

finplot(q2_p1)
```

![](README_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
# Compare indices for ALSI and SWIX

indices <- T40 %>%  pull(Index_Name) %>% na.omit(.) %>%  unique()
indices_return <- list()

for(i in 1:length(indices)){
    # Loop through sectors and calculate returns and cumulative returns
    indices_return[[i]] <- portfolio_return_function(T40,index = indices[i]) %>% group_by(Portfolio) %>% 
    mutate(cumreturn_Rand = (cumprod(1 + Returns))) %>% # Start at 1
mutate(cumreturn_Rand = cumreturn_Rand/first(cumreturn_Rand)) %>% 
    mutate(Index = indices[i])
}

# Rename tibbles
names(indices_return) <- indices

# Combine Dataframes
indices_return_cum <- rbind(indices_return[[1]], indices_return[[2]], indices_return[[3]]) %>% arrange(date)
    
q2_p2 <- indices_return_cum %>% 
        ggplot() +
        geom_line(aes(date, cumreturn_Rand, colour = Portfolio), alpha = 0.8) + facet_wrap(~Index) + fmxdat::fmx_cols() + 
        labs(title = "Cumulative Returns per Index for ALSI and SWIX", y = "Cumulative Returns", x = "") +
        fmxdat::theme_fmx(title.size = ggpts(25))

finplot(q2_p2)
```

![](README_files/figure-markdown_github/unnamed-chunk-18-1.png)

We need to create a function that can filter the dataframe into sector
and Index_Name and then calculate portfolio returns on the subsets.

Problems to face: the weights will not sum to 1 so we must normalise the
weights in the subset. This will also cause more 0’s and NA values,
which can be pushed to 0.

So each weight per day must be normalised to the sum of weights for that
subset.

## Stratify the returns by high and low volatility

``` r
zar <-  usdzar  %>% 
    filter(date > ymd(20080101)) %>% 
    mutate(Return = Price/lag(Price) - 1) %>% 
    filter(date > first(date)) %>% 
    select(-c(Price, Name))

ZARSD <- zar %>% 
    mutate(YearMonth = format(date, "%Y%B")) %>% 
    group_by(YearMonth) %>% 
    summarise(SD = sd(Return)*sqrt(52)) %>% 
    # Top Decile Quantile overall (highly volatile month for ZAR:
    mutate(TopQtile = quantile(SD, 0.8), BotQtile = quantile(SD, 0.2))

Hi_Vol <- ZARSD %>% filter(SD > TopQtile) %>% pull(YearMonth)

Low_Vol <- ZARSD %>% filter(SD < BotQtile) %>% pull(YearMonth)

# Create generic function to compare performance:

Perf_comparisons <- function(Idxs, YMs, Alias){
  # For stepping through uncomment:
  # YMs <- Hi_Vol
  Unconditional_SD <- Idxs %>% 
      group_by(Tickers) %>% 
      mutate(Full_SD = sd(Return) * sqrt(252)) %>% 
      filter(YearMonth %in% YMs) %>% 
      summarise(SD = sd(Return) * sqrt(252), across(.cols = starts_with("Full"), .fns = max)) %>% 
      arrange(desc(SD)) %>% mutate(Period = Alias) %>% 
      group_by(Tickers) %>% 
      mutate(Ratio = SD / Full_SD)
  
  Unconditional_SD
}

ALSI_SWIX <- portfolio_return_function(T40)

# Prepare and Winzorise Returns
ALSI_SWIX <- ALSI_SWIX %>% 
    group_by(Portfolio) %>% 
    mutate(YearMonth = format(date, "%Y%B")) %>% 
    rename(Tickers = Portfolio, Return = Returns) %>% 
    group_by(Tickers) %>% 
    mutate(Top = quantile(Return, 0.99), Bot = quantile(Return, 0.01)) %>% 
    mutate(Return = ifelse(Return > Top, Top, ifelse(Return < Bot, Bot, Return))) %>% ungroup()

perf_hi <- Perf_comparisons(ALSI_SWIX, YMs = Hi_Vol, Alias = "High_Vol")

perf_lo <- Perf_comparisons(ALSI_SWIX, YMs = Low_Vol, Alias = "Low_Vol")

# Print High Vol
perf_hi
```

    ## # A tibble: 2 × 5
    ## # Groups:   Tickers [2]
    ##   Tickers    SD Full_SD Period   Ratio
    ##   <chr>   <dbl>   <dbl> <chr>    <dbl>
    ## 1 J200    0.286   0.193 High_Vol  1.48
    ## 2 J400    0.284   0.193 High_Vol  1.47

``` r
perf_lo
```

    ## # A tibble: 2 × 5
    ## # Groups:   Tickers [2]
    ##   Tickers    SD Full_SD Period  Ratio
    ##   <chr>   <dbl>   <dbl> <chr>   <dbl>
    ## 1 J400    0.154   0.193 Low_Vol 0.797
    ## 2 J200    0.151   0.193 Low_Vol 0.783

## Construct a capped portfolio

``` r
# first filter out the Effective rebalance days 
effective_rebDays <- RebDays %>% filter(Date_Type %in% "Effective Date")

rebalance_ALSI <- T40 %>% filter(date %in% effective_rebDays$date) %>% 
    mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    select(date, Tickers, Return, J200, RebalanceTime) %>% 
    rename(weight = J200) %>% 
    # There are 27 NA's in this so we coalesce them to 0
    mutate(weight = coalesce(weight , 0))

# Checking if there are any NA's left
sum(is.na(rebalance_ALSI$weight))
```

    ## [1] 0

``` r
# Construct Capped Portfolio and Determine Performance for ALSI

Proportional_Cap_Foo <- function(df_Cons, W_Cap = 0.08){
  
  # Let's require a specific form from the user... Alerting when it does not adhere this form
  if( !"weight" %in% names(df_Cons)) stop("... for Calc capping to work, provide weight column called 'weight'")
  
  if( !"date" %in% names(df_Cons)) stop("... for Calc capping to work, provide date column called 'date'")
  
  if( !"Tickers" %in% names(df_Cons)) stop("... for Calc capping to work, provide id column called 'Tickers'")

  # First identify the cap breachers...
  Breachers <- df_Cons %>% filter(weight > W_Cap) %>% pull(Tickers)
  
  # Now keep track of breachers, and add to it to ensure they remain at 10%:
  if(length(Breachers) > 0) {
    
    while( df_Cons %>% filter(weight > W_Cap) %>% nrow() > 0 ) {
      df_Cons <- 
          bind_rows(
              df_Cons %>% filter(Tickers %in% Breachers) %>% mutate(weight = W_Cap),
              df_Cons %>% filter(!Tickers %in% Breachers) %>% mutate(weight = (weight / sum(weight, na.rm=T)) * (1-length(Breachers)*W_Cap) )
              )
      
      Breachers <- c(Breachers, df_Cons %>% filter(weight > W_Cap) %>% pull(Tickers))
    }

    if( sum(df_Cons$weight, na.rm=T) > 1.1 | sum(df_Cons$weight, na.rm=T) < 0.9 | max(df_Cons$weight, na.rm = T) > W_Cap) {
    stop( glue::glue("For the Generic weight trimming function used: 
    the weight trimming causes non unit summation of weights for date: {unique(df_Cons$date)}...
    \nThe restriction could be too low or some dates have extreme concentrations...") )
        }
  } else {
          
      }
  df_Cons
}
  
# Apply Proportional_Cap_Foo to ALSI to get capped return for cap of 10%
Capped_df <- rebalance_ALSI %>% 
    group_split(RebalanceTime) %>% 
    map_df(~Proportional_Cap_Foo(., W_Cap = 0.1) ) %>% 
    select(-RebalanceTime)
 
ALSI_wts <- Capped_df %>% 
    tbl_xts(cols_to_xts = weight, spread_by = Tickers)

ALSI_rts <- T40 %>% 
    filter(Tickers %in% unique(Capped_df$Tickers)) %>% 
    tbl_xts(cols_to_xts = Return, spread_by = Tickers)

ALSI_wts[is.na(ALSI_wts)] <- 0

ALSI_rts[is.na(ALSI_rts)] <- 0

ALSI_capped <- rmsfuns::Safe_Return.portfolio(R = ALSI_rts, weights = ALSI_wts, lag_weights = T) %>% 
    xts_tbl() %>% 
    rename(ALSI = portfolio.returns)

# Construct Capped Portfolio and Determine Performance for SWIX

reb_SWIX <- T40 %>% 
    filter(date %in% effective_rebDays$date) %>% 
    mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    select(date, Tickers, Return, J400, RebalanceTime) %>% 
    rename(weight = J400) %>% 
    mutate(weight = coalesce(weight , 0))
  
# Apply Proportional_Cap_Foo to ALSI to get capped return for cap of 6%
Capped_df <- 
    reb_SWIX %>% 
    group_split(RebalanceTime) %>% 
    map_df(~Proportional_Cap_Foo(., W_Cap = 0.06) ) %>% 
    select(-RebalanceTime)
 
SWIX_wts <- Capped_df %>% tbl_xts(cols_to_xts = weight, spread_by = Tickers)

SWIX_rts <- T40 %>% 
    filter(Tickers %in% unique(Capped_df$Tickers)) %>% 
    tbl_xts(cols_to_xts = Return, spread_by = Tickers)

SWIX_wts[is.na(SWIX_wts)] <- 0

SWIX_rts[is.na(SWIX_rts)] <- 0

SWIX_capped <- rmsfuns::Safe_Return.portfolio(R = SWIX_rts, weights = SWIX_wts, lag_weights = T) %>% 
    xts_tbl() %>% 
    rename(SWIX = portfolio.returns)


# Combine and Plot Performance

capped_indices <- left_join(ALSI_capped, SWIX_capped, by = "date") %>% 
    pivot_longer(c("ALSI", "SWIX"), names_to = "Portfolio", values_to = "returns")

# Calculate Uncapped Return for ALSI
ALSI_wts <- T40 %>% 
filter(date %in% effective_rebDays$date) %>% 
mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    rename(weight = J200) %>% 
    mutate(weight = coalesce(weight , 0)) %>% 
    select(date, Tickers, Return, weight, RebalanceTime) %>% 
     tbl_xts(cols_to_xts = weight, spread_by = Tickers)

ALSI_wts[is.na(ALSI_wts)] <- 0

ALSI_rts[is.na(ALSI_rts)] <- 0

ALSI_capped <- rmsfuns::Safe_Return.portfolio(R = ALSI_rts, weights = ALSI_wts, lag_weights = T) %>% 
    xts_tbl() %>% 
    rename(ALSI = portfolio.returns)

# Calculate Uncapped Return for SWIX
 
SWIX_wts <- T40 %>% 
filter(date %in% effective_rebDays$date) %>% 
mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    rename(weight = J400) %>% 
    mutate(weight = coalesce(weight , 0)) %>% 
    select(date, Tickers, Return, weight, RebalanceTime) %>% 
     tbl_xts(cols_to_xts = weight, spread_by = Tickers)

SWIX_wts[is.na(SWIX_wts)] <- 0

SWIX_rts[is.na(SWIX_rts)] <- 0

SWIX_capped <- rmsfuns::Safe_Return.portfolio(R = SWIX_rts, weights = SWIX_wts, 
    lag_weights = T) %>% 
xts_tbl() %>% 
rename(SWIX = portfolio.returns)

# Combine and Plot

ALSI_SWIX <- left_join(ALSI_capped, SWIX_capped, by = "date") %>% 
    pivot_longer(c("ALSI", "SWIX"), names_to = "Portfolio", values_to = "Returns")

q2_p3 <- capped_indices %>% 
    group_by(Portfolio) %>%
    mutate(Idx = cumprod(1 + returns)) %>% 
    ggplot() + 
    geom_line(aes(date, Idx, colour = Portfolio), alpha = 0.8) + 
    labs(subtitle = "ALSI capped at 10% and SWIX at 6%", x = "", y = "Cumulative Return") + 
    fmx_cols() + 
    fmxdat::theme_fmx(subtitle.size = ggpts(20))

q2_p4 <- ALSI_SWIX %>% 
    group_by(Portfolio) %>%
    mutate(Idx = cumprod(1 + Returns)) %>% 
    ggplot() + 
    geom_line(aes(date, Idx, colour = Portfolio), alpha = 0.8) + 
    labs(subtitle = "Uncapped Index Calculation for ALSI and SWIX", x = "", y = "Cumulative Return") + 
    fmx_cols() + 
    fmxdat::theme_fmx(subtitle.size = ggpts(20))

plot_grid(finplot(q2_p3), finplot(q2_p4), labels = list(title = "Comparing Capped and Uncapped returns of ALSI and SWIX"), label_size = ggpts(30), align = "h")
```

![](README_files/figure-markdown_github/unnamed-chunk-22-1.png)

# Question 4

## What it is

# Question 5

## What it is

# Question 6

## What it is

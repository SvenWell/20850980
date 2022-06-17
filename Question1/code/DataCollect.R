DataCollect <- function(datapath){

    library(tidyverse)

    # silent read function
    silentread <- function(x){
        hushread <- purrr::quietly(read_csv)
        df <- hushread(x)
        df$result
    }

    datalist <-
        list.files(path = datapath, pattern = "*.csv", full.names = TRUE) %>%
        as.list() %>%
        map(~silentread(.))

    datalist

}

DataCollectQ3 <- function(datapath, findtype, csvlen = 0){

    library(tidyverse)

    # silent read function
    silentread <- function(x){
        hushread <- purrr::quietly(read_csv)
        df <- hushread(x)
        df$result
    }

    datalist <-
        list.files(path = datapath, pattern = "*.csv", full.names = TRUE) %>%
        as.list()
    findtype <- paste("(",findtype, ")", sep = "")

    if(csvlen != 0) {
        data <- keep(datalist, grepl(datalist, pattern = findtype) & nchar(listnames) == csvlen) %>%
            map(~silentread(.)) %>%
            plyr::rbind.fill()
    } else {
        data <- keep(datalist, grepl(datalist, pattern = findtype)) %>%
            map(~silentread(.)) %>%
            plyr::rbind.fill()
    }

    data

}

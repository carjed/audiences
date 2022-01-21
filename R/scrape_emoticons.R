# modified from https://raw.githubusercontent.com/today-is-a-good-day/emojis/master/scrapeEmoticons.R

library(rvest)
library(magrittr)
library(dplyr)

# reference website
# url <- "http://apps.timwhitlock.info/emoji/tables/unicode"

# using a local copy due to connection issues with the above url
<<<<<<< HEAD
url <- paste0("~/projects/audiences", "/Emoji unicode characters for use on the web.html")
=======
url <- paste0(datadir, "/Emoji unicode characters for use on the web.html")
>>>>>>> 64cbec55ab53574be1346db0d0b7ae9cce410194

# get emoticons
emoticons <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[1]') %>%
    html_table()
<<<<<<< HEAD
# emoticons <- data.frame(emoticons[[1]]$Native, emoticons[[1]]$Unicode, emoticons[[1]]$Bytes,
=======
# emoticons <- data.frame(emoticons[[1]]$Native, emoticons[[1]]$Unicode, emoticons[[1]]$Bytes, 
>>>>>>> 64cbec55ab53574be1346db0d0b7ae9cce410194
#                            emoticons[[1]]$Description, stringsAsFactors = FALSE)
# names(emoticons) <- c("Native", "Unicode", "Bytes", "Description")

# get additional emoticons
addemoticons <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[6]') %>%
    html_table()
<<<<<<< HEAD
# addemoticons <- data.frame(addemoticons[[1]]$Native, addemoticons[[1]]$Bytes,
=======
# addemoticons <- data.frame(addemoticons[[1]]$Native, addemoticons[[1]]$Bytes, 
>>>>>>> 64cbec55ab53574be1346db0d0b7ae9cce410194
#                               addemoticons[[1]]$Description, stringsAsFactors = FALSE)
# names(addemoticons) <- c("Native", "Bytes", "Description")

# get dingbats
dingbats <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[2]') %>%
    html_table()
<<<<<<< HEAD
# dingbats <- data.frame(dingbats[[1]]$Native, dingbats[[1]]$Bytes,
=======
# dingbats <- data.frame(dingbats[[1]]$Native, dingbats[[1]]$Bytes, 
>>>>>>> 64cbec55ab53574be1346db0d0b7ae9cce410194
#                           dingbats[[1]]$Description, stringsAsFactors = FALSE)
# names(dingbats) <- c("Native", "Bytes", "Description")

# get transports
transport <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[3]') %>%
    html_table()
# transport <- data.frame(transport[[1]]$Native, transport[[1]]$Bytes, 
#                            transport[[1]]$Description, stringsAsFactors = FALSE)
# names(transport) <- c("Native", "Bytes", "Description")

# get additional transports
addtransport <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[7]') %>%
    html_table()
# addtransport <- data.frame(addtransport[[1]]$Native, addtransport[[1]]$Bytes, 
#                               addtransport[[1]]$Description, stringsAsFactors = FALSE)
# names(addtransport) <- c("Native", "Bytes", "Description")

# get enclosed emoticons
enclosed <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[4]') %>%
    html_table()
# enclosed <- data.frame(enclosed[[1]]$Native, enclosed[[1]]$Bytes, 
#                           enclosed[[1]]$Description, stringsAsFactors = FALSE)
# names(enclosed) <- c("Native", "Bytes", "Description")

# get uncategorized emoticons
uncategorized <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[5]') %>%
    html_table()
# uncategorized <- data.frame(uncategorized[[1]]$Native, uncategorized[[1]]$Bytes, 
#                                uncategorized[[1]]$Description, stringsAsFactors = FALSE)
# names(uncategorized) <- c("Native", "Bytes", "Description")

# get additional other emoticons
addothers <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div[2]/div/div/table[8]') %>%
    html_table()
# addothers <- data.frame(addothers[[1]]$Native, addothers[[1]]$Bytes, 
#                            addothers[[1]]$Description, stringsAsFactors = FALSE)
# names(addothers) <- c("Native", "Bytes", "Description")

# combine all dataframes to overall dataframe
alltogether <- bind_rows(list(emoticons[[1]], addemoticons[[1]], dingbats[[1]], transport[[1]], 
                              addtransport[[1]], enclosed[[1]], uncategorized[[1]], addothers[[1]]))

<<<<<<< HEAD
names(alltogether) <- c("Native", "Apple", "Android", "Android2", "Symbola", "Twitter", "Unicode", "Bytes", "Description")
=======
names(alltogether) <- c("Native", "Apple", "Android", "Symbola", "Twitter", "Unicode", "Bytes", "Description")
>>>>>>> 64cbec55ab53574be1346db0d0b7ae9cce410194

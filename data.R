library(tidyverse)

library(xml2)
library(rvest)
library(purrr)

library(jsonlite)

library(httr)

auth <- str_c("Bearer ", read_file("key.private"))

# https://developer.srgssr.ch/apis/tpc-polis/docs

# locations ----
url <- "https://api.srgssr.ch/polis-api/v2/locations"

response <- GET(paste0(url, "?lang=de"), 
                add_headers(accept = "application/json",
                            Authorization = auth))


read1 <- read_xml(response) # xml2

read1 %>% class()

nodes1 <- xml_find_all(read1, "Location")

map_df(nodes1, function(x) {
  kids <- xml_children(x)
  setNames(as.list(type.convert(xml_text(kids))), 
           xml_name(kids))
}) -> tbl_data

tbl_data

attrs1 <- nodes1 %>% 
  xml_attrs()

attrnames <- names(attrs1[[1]])

attrs_tbl <- attrs1 %>% 
  unlist() %>% 
  matrix(ncol = 3, nyrow = T) %>% 
  as_tibble()

colnames(attrs_tbl) <- attrnames

locations <- bind_cols(attrs_tbl, tbl_data)

locations %>% saveRDS("data/locations.RDS")


# locationById (requires Id)----
baseurl <- "https://api.srgssr.ch/polis-api/v2/locations/"
locationid <- 7

url <- paste0(baseurl, locationid)

response <- GET(paste0(url, "?lang=de"), 
                add_headers(accept = "application/json",
                            Authorization = auth))


read1 <- read_xml(response) # xml2

# read1 %>% xml_structure()

nodes1 <- xml_find_all(read1, "Location")

map_df(nodes1, function(x) {
  kids <- xml_children(x)
  setNames(as.list(type.convert(xml_text(kids))), 
           xml_name(kids))
}) -> tbl_data

tbl_data

attrs1 <- nodes1 %>% 
  xml_attrs()

attrnames <- names(attrs1[[1]])

attrs_tbl <- attrs1 %>% 
  unlist() %>% 
  matrix(ncol = 3, byrow = T) %>% 
  as_tibble()

colnames(attrs_tbl) <- attrnames

locationById <- bind_cols(attrs_tbl, tbl_data)

# locationById %>% saveRDS("data/locationById.RDS")


# parties ----
url <- "https://api.srgssr.ch/polis-api/v2/parties"

response <- GET(paste0(url, "?lang=de"), 
                add_headers(accept = "application/json",
                            Authorization = auth))


read1 <- read_xml(response) # xml2

nodes1 <- xml_find_all(read1, "Party")

map_df(nodes1, function(x) {
  kids <- xml_children(x)
  setNames(as.list(type.convert(xml_text(kids))), 
           xml_name(kids))
}) -> tbl_data

attrs1 <- nodes1 %>% 
  xml_attrs()

attrnames <- names(attrs1[[1]])

attrs_tbl <- attrs1 %>% 
  unlist() %>% 
  matrix(ncol = 3, byrow = T) %>% 
  as_tibble()

colnames(attrs_tbl) <- attrnames

parties <- bind_cols(attrs_tbl, tbl_data)

parties %>% saveRDS("data/parties.RDS")



# OTHER ----



GET("https://api.srgssr.ch/polis-api/v2/elections?lang=de", 
    add_headers(accept = "application/json",
                Authorization = auth)) -> response

response

GET("https://api.srgssr.ch/polis-api/v2/parties?lang=de", 
    add_headers(accept = "application/xml",
                Authorization = auth)) -> response

response %>% 
  content() %>%
  html_nodes("Party") %>% 
  html_attrs() %>% 
  map(2) %>% 
  unlist()

read1 <- read_xml(response) # xml2

xml_attrs(read1) <- c()
  
nodes1 <- xml_find_all(read1, "Party")

nodes1 %>% 
  xml_attrs() %>% 
  unlist() %>% 
  matrix(ncol = 3, byrow = T) %>% 
  as_tibble() %>% 
  rename("id" = 1, "smartVoteID" = 2, "xmlns" = 3)
  
  # .[[1]] %>% 
  names()


  lapply() %>% 
  lapply(bind_rows)

  # as.data.frame() %>%


map_df(nodes1, function(x) {
  kids <- xml_children(x)
  setNames(as.list(type.convert(xml_text(kids))), 
           xml_name(kids))
}) -> tbl_data

tbl_data

  content() %>% 
  html_nodes("ShortName") %>% 
  map() %>% 
  map_df(~as.list(.))
  


  html_structure(indent = 10)

#' Get Hits
#'
#' @description Returns all number one hits for a given year (1959+)
#' @param year The year about which you want to know the jams
#' @import dplyr
#' @import purrr
#' @import RCurl
#' @import rvest
#' @import lubridate
#' @import janitor
#' @import zoo
#' @import xml2
#' @return A tibble of number one hits
#' @export get_hits
#'
#' @examples
#' get_hits(1981)
#'
get_hits <- function(year){
  year <- as.numeric(year)
  if(year<1959){
    stop("time machine broke: year must greater than or equal to 1959")
  }
  url <- paste0("https://en.wikipedia.org/wiki/List_of_Billboard_Hot_100_number-one_singles_of_",year)
  webpage <- read_html(url)
  l_item <- which.max(map_dbl(webpage %>%
                                html_table(), length))
  tbl <- webpage %>%
    html_table() %>%
    `[[`(l_item) %>%
    as_tibble() %>%
    clean_names() %>%
    select(issue_date, song, artist = artist_s) %>%
    mutate(issue_date = str_replace(issue_date, ' ', '-'),
           date = mdy(str_c(issue_date, ' - ', year)),
           song = str_remove_all(song, '\"')) %>%
    select(date, song, artist)

  tbl %>%
    mutate(art_dt = mdy(str_c(artist, '-',year)),
           art_dt = ifelse((year(date) != year(art_dt) | is.na(art_dt)), NA, art_dt),
           art_dt = as.Date(art_dt),
           new_date = coalesce(art_dt, date)) %>%
    mutate(artist = ifelse(new_date > date, NA, artist),
           artist = na.locf(artist)) %>%
    select(date = new_date, song, artist) %>%
    mutate(song = str_trim(str_remove_all(song, "\\[([0-9]|[a-z])\\]")),
           artist = str_trim(str_remove_all(artist, "\\[([0-9]|[a-z])\\]")))
}


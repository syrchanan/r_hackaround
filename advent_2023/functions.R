import_advent <- function(day, year, session) {
  
  if (session == "") {
    stop("Need session info")
  }
  
  url <- paste0("https://adventofcode.com/", year, "/day/", day, "/input")
  
  request <- httr::GET(url,
                       httr::set_cookies(session = session))
  httr::stop_for_status(request)
  
  raw_data <- httr::content(request, encoding = "UTF-8")
  
  lines_data <- stringr::str_split(raw_data, "\n")[[1]]
  
  
  return(
    list(
      raw_data = lines_data, 
      parsed_data = tibble::tibble(
        x = readr::parse_guess(
          lines_data, locale = readr::locale(
            "en", grouping_mark = ""
          )
        )
      )
    )
  )
  
}


adjacent_cross <- function(tbl, tbl_2 = tbl, incl_diagonal = F) {
  
  if (incl_diagonal) {
    cells <- expand.grid(row_chg = c(-1:1), 
                         col_chg = c(-1:1))
  } else {
    cells <- data.frame(row_chg = c(-1, 1, 0, 0),
                        col_chg = c(0, 0, -1, 1))
  }
  
  tbl %>%
    tidyr::crossing(cells) %>%
    mutate(row_2 = row + row_chg,
           col_2 = col + col_chg) %>%
    inner_join(tbl_2, by = c(row_2 = "row", col_2 = "col"), suffix = c("", "_2")) %>%
    filter(row != row_2 | col != col_2) %>%
    select(-row_chg, -col_chg)
  
}

parse_grid <- function(tbl, variable, sep_char = "") {
  
  tbl %>%
    mutate(row = row_number()) %>%
    mutate(value = stringr::str_split({{ variable }}, sep_char)) %>%
    select(-{{ variable }}) %>%
    tidyr::unnest(value) %>%
    group_by(row) %>%
    mutate(col = row_number()) %>%
    ungroup() -> tbl_out
  
  return(
    list(
      raw_tbl = tbl_out, 
      parsed_data = {
        tbl_out %>% 
          mutate(
            value = readr::parse_guess(
              value, 
              locale = readr::locale("en", grouping_mark = "")
            )
          )
      }
    )
  )

}

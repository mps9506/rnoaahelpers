
#' create_dates
#' Creates a tibble with a column of start dates and column of end dates by year
#' @param start_date Date
#' @param end_date Date
#' @import dplyr
#' @return tibble
#' @export
#' @keywords internal

create_dates <- function(start_date, end_date) {
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)

  start_date1 <- as.Date(paste0(start_year, "-01-01"))
  start_date2 <- as.Date(paste0(end_year, "-01-01"))

  start_dates <- seq.Date(start_date1, start_date2, by = "year")

  end_date1 <- as.Date(paste0(start_year, "-12-31"))
  end_date2 <- as.Date(paste0(end_year, "-12-31"))

  end_dates <- seq.Date(end_date1, end_date2, by = "year")

  df <- dplyr::tibble(start_dates = start_dates,
               end_dates = end_dates)
  return(df)
}


#' download_ncdc
#'
#' Functionally downloads specified data using rnoaa's ncdc function over the specified date range. Use this when you have to download multiple years worth of data
#'
#' @param start_date Date
#' @param end_date Date
#' @param station_id character
#' @param data_type_id character
#' @param token character
#' @param progress logical
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom progress progress_bar
#'
#' @return tibble
#' @export

download_ncdc <- function(start_date, end_date, station_id, data_type_id, token, progress = TRUE) {

  ## create dataframe with column of start dates and column of end dates
  ## this defaults to year/01/01 and year/12/31 for now.
  df <- create_dates(start_date, end_date)

  ## load a progress bar
  if (progress)  pb <- progress::progress_bar$new(total = length(df$start_dates),
                                                  format = "  downloading [:bar] :percent eta: :eta")

  df <- df %>%
    mutate(data = purrr::map2(.data$start_dates, .data$end_dates, ~{
      x <- get_data(as.character(.x), as.character(.y),
                    station_id = station_id,
                    data_type_id = data_type_id,
                    token = token,
                    add_units = TRUE)

      if (progress) pb$tick()

      return(x)
      })) %>%
    dplyr::select(.data$data) %>%
    tidyr::unnest(cols = .data$data)

  return(df)
}


#' get_data
#'
#' @param start_date Date
#' @param end_date Date
#' @param data_type_id character
#' @param station_id character
#' @param token character
#' @param add_units logical
#' @importFrom rnoaa ncdc
#' @return dataframe
#' @export
#' @keywords internal

get_data <- function(start_date, end_date, data_type_id, station_id, token, add_units = TRUE) {

  x <- ncdc(datasetid = "GHCND", stationid = station_id,
            datatypeid = data_type_id, startdate = start_date,
            enddate = end_date, token = token, limit = 500,
            add_units = add_units)


  Sys.sleep(5)
  return(x$data)
}

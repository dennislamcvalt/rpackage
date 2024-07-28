
#' Read CSV file to tibble
#'
#' @description
#' The `fars_read()` function checks if the specified file exists, reads the CSV
#' file into a data frame using `read_csv()`, suppresses any messages, and
#' converts the data frame into a tibble before returning it as the output
#'
#' @section External Functions Not Imported:
#' * [readr::read_csv()] used to read the CSV file.
#' * [dplyr::tbl_df()] used to convert the data frame to a tibble.
#'
#' @param filename The name of a CSV file to be read; if the filename is not
#'   valid, an error message "file filename does not exist" is generated.
#'
#' @return A tibble is returned as the output
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2014.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a file name from year
#'
#' @description
#' The `make_filename()` function generates a filename string for a specific
#' year, following the specific format used by the Fatality Analysis Reporting
#' System (FARS) using base R functions.
#'
#' @param year Represents the year for which the filename needs to be generated;
#'   if year is not a numeric, an NA value will be introduced by coercion thereby
#'   generating an improper filename, "accident_NA.csv.bz2".
#'
#' @return A string is returned in the format "accident_%d.csv.bz2" but with
#'   the "%d" replaced with the year parameter supplied by the user.
#'
#' @examples
#' make_filename(2023)
#' make_filename(1985)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Create list of data frames
#'
#' @description
#' The `fars_read_years()` function reads and processes data files in the
#' working directory for multiple years; it takes a vector of numeric years as
#' input and returns a list of processed data frames.
#'
#' @section External Functions Not Imported:
#' * [dplyr::mutate()] used to add a year column to the data frame.
#' * [dplyr::select()] used to select the `Month` and `year` columns.
#'
#' @importFrom magrittr '%>%'
#'
#' @param years A vector of years for which the data files in the working
#'   directory need to be read and processed. If a corresponding CSV file
#'   for a year in the vector of numeric years is not present in the working
#'   directory, an error message indicating "invalid year" is generated and the
#'   data frame for that year will be set to `NULL`
#'
#' @return A list of data frames; each data frame contains the month values from
#'   the processed data files in the working directory and the corresponding year
#'   specified in the `years` parameter.
#'
#' @seealso [make_filename()] to create a file name from a year
#'
#' @examples
#' fars_read_years(c(2013,2014,2015))
#' fars_read_years(2013:2015)
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Count records per month for each year
#'
#' @description
#' The `fars_summarize_years()` function summarizes the data obtained from
#' multiple years by counting the number of records for each month in each year.
#' The function takes a vector of years as input and returns a data frame with
#' summarized information.
#'
#' @section External Functions Not Imported:
#' * [dplyr::bind_rows()] used to combine all data frames into one data frame.
#' * [dplyr::group_by()] used to group rows of data frame by `year` and `MONTH`.
#' * [dplyr::summarize()] used to calculate a summary statistic for each group.
#' * [tidyr::spread()] used to convert data frame from long to wide format.
#'
#' @importFrom dplyr n
#'
#' @inheritParams fars_read_years
#'
#' @return A data frame with a `MONTH` column and a column of record counts
#'   for each year in the `years` parameter.
#'
#' @seealso [fars_read_years()] to create a list of data frames
#'
#' @examples
#' fars_summarize_years(c(2013, 2014, 2015))
#' fars_summarize_years(2013:2015)
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot accidents on state-level map
#'
#' @description
#' The `fars_map_state()` function plots the locations of accidents on a map for
#' a specific state and year using data from the Fatality Analysis Reporting
#' System (FARS). The function takes two parameters: `state.num`, which represents
#' the state number and `year`, which represents the year of the data.
#'
#' @section External Functions Not Imported:
#' * [dplyr::filter()] used to create a subset of the data.
#' * [maps::map()] used to plot a state map.
#' * [graphics::points()] used to plot points on a map.
#'
#' @param state.num A numeric which represents the state number. If no accidents
#'   occurred in the state during the specified year, than a "no accidents to
#'   plot" message appears. If the state number is invalid, an error message,
#'   "Invalid state number" is generated.
#'
#' @param year A numeric which represents the year of the data. The value must
#'   correspond to a year included in the filename of a CSV file in the working
#'   directory such as "accidents_2013.csv.bz2" where the 2013 in the filename
#'   represents the year of the data. No corresponding file generates and error
#'   message.
#'
#' @return A plot of points representing accidents in a given year within a
#'   state-level map.
#'
#' @seealso
#' * [make_filename()] to create a file name from a year
#' * [fars_read()] to read CSV file to tibble
#'
#' @examples
#' fars_map_state(10, 2013)
#' fars_map_state(5, 2015)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

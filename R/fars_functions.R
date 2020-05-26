#' @title Read in FARS data file
#'
#' @description Read in a Facility Analysis Reporting System data file. Not exported.
#'
#' @details Throws an error if file cannot be found.
#'
#' @param filename A character string of the data file path.
#'
#' @return This function returns a data frame table of the FARS data.
#'
#' @examples
#' \dontrun{fars_read("accident_2015.csv.bz2")}
#'
#' @seealso \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}



#' @title Make a file name for data in a specific year
#'
#' @description Creates a filename to be read in with specified FARS data. Not exported.
#'
#' @param year Character String of the year that the data file is being made for.
#'
#' @return Returns a character string of the filename for the specified year.
#'
#' @examples
#' \dontrun{make_filename(2015)}
#'
#' @seealso \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'
make_filename <- function(year) {
  year <- as.integer(year)
  filename <- sprintf("accident_%d.csv.bz2", year)
  print(filename)
  full_filename <- system.file("inst", "extdata", filename, package = "FARSFunctions", mustWork=TRUE)
  full_filename
}
make_filename(2015)


#' @title Read in Facility Analysis Reporting System data for selected years
#'
#' @description Read in FARS data for specified years.
#'
#' @details Throws an error if specified years are invalid.
#'
#' @param years Integer of the years to be read.
#'
#' @return Returns a list of tibbles of the FARS data for each month of the selected years.
#'
#' @examples
#' \dontrun{fars_read_years(c(2013, 2014, 2015))}
#'
#' @seealso \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'
#' @importFrom dplyr select
#'
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



#' @title Count number of motor vehicle accidents by year for selected years
#'
#' @description Counts the number of fatal motor vehicle accidents for each month of the specified years in the FARS data set.
#'
#' @details Throws an error if specified years are invalid.
#'
#' @param years Numeric vector of years to summarize.
#'
#' @return Returns a tbl_df of the number of motor vehicle accidents for each selected year.
#'
#' @examples
#' \dontrun{fars_summarize_years(2013,2014,2015)}
#'
#' @seealso \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#' @title Plot a map of all fatal motor vehicle accidents for a specific year in a specified state
#'
#' @description makes a map of all fatal motor vehicle accidents for a specified year and specified state.
#'
#' @details Throws an error if specified state ID numbers and years are invalid.
#'  Gives a warning if there are no fatal motor vehicle accidents in a given state and year.
#'
#' @param state.num Integer of state ID number to plot a map for.
#' @param year Integer of the year to plot motor vehicle accidents from.
#'
#' @return A map of a state with points of GPS coordinates of motor vehicle accidents from a specific year.
#'
#' @examples
#' \dontrun{fars_map_state(15, 2013)}
#'
#' @seealso \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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

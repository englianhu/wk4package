#' @title Read a FARS query into a tibble
#'
#' @description fars_read() takes in a *.csv file from the NHTSA's FARS #' archives  and returns a tibble.
#'
#' @details This function takes in a *.csv file and using the readr package will load the file as a tibble then will return it as a tbl_df (subclass of a data.frame) through dplyr. This function is ideally to be used with the FARS (Fatality Analysis Reporting System) archives for accident data in *.csv format. If no file exists an error will occur before attempting to read the file. 
#'
#' @seealso \url{https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/FARS/} for other .csv files from FARS (Fatality Analysis Reporting System)
#' to process. Explore the NHTSA's website for more information on the FARS program.
#' @examples
#' # this will stop
#'	\dontrun{fars_read("this_file_does_not_exist.csv")} 
#' #returns data as tbl_df
#'  #setwd(system.file("data-raw", package = "wk4package"))
#'  #getwd()
#'  \dontrun{fars_read("accident_2013.csv.bz2") }
#' @importFrom readr read_csv
#' @param filename Relative filepath to the current working directory. This must be an existing comma separated values file (*.csv) or it will throw an error.
#' @return A tibble from the inputted *.csv file.
#' @family FARS functions
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' @title Generate filename for FARS data.
#'
#' @description Input a year that you want to receive the FARS filename of.
#'
#' @details Input a numeric or character object representing an integer which will then be coerced into an integer object. This integer is finally formatted into a character object of the form "accident_1234.csv.bz2" where "1234" is placeholder for the integer. 
#'
#' @param year A "character" or "numeric" input that can be interpreted as an integer by the `as.integer()` function.
#' @return A "character" object where the number 
#'
#' @examples
#' #setwd(system.file("data-raw", package = "wk4package"))
#' #returns "accident_2009.csv.bz2"
#'	\dontrun{make_filename(2009)}
#'  #returns "accident_2012.csv.bz2"
#'	\dontrun{make_filename("2012")}
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' @title Search and print out a dataset by month and year of accident.
#'
#' @description Provide a vector of years and receive all accident data for those given years from FARS accident archival data.
#' @details Provide a list/vector of years you would like to query in the form of XXXX in numerical, integer, or character form. This will return a list
#' of each years accident info containing the month and year the accident occured.
#' @param years A character/numerical vector containing the years that would like to be query.
#' @return A list of tibbles with the column arrangement of month/year. Years type is dependent on the parameter's type.
#' @family FARS functions
#' @examples
#'  #read in one year, will return a tibble containing the month as an integer
#' #setwd(system.file("data-raw", package = "wk4package"))
#'  \dontrun{fars_read_years(2015)}
#'  # read in multiple years, will return a list of tibbles 
#'  # containing the month as a double and year as a <dbl>.
#'  \dontrun{fars_read_years(c(2013,2014))}
#'  # You can mix and match object types
#'  \dontrun{fars_read_years(list("2013",2014))}
#' 
#' @importFrom purrr `%>%`
#' @import dplyr
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

#' @title Summarize FARS accident data by month and year;
#'
#' @description Summarize the Fatality Analysis Reporting System data, counting the number of accidents per month over the stated set of years.
#' @details
#' Do note that the following packages are needed: dplyr, tidyr, readr, magrittr, and purrr.
#' @param years Character/numeric vector containing the years to summarize.
#' @return A tibble containing a summary with the years as columns and month as rows. The values are the number of accidents for that year's month.
#' @examples 
#' #setwd(system.file("data-raw", package = "wk4package"))
#' # Returns a table with the number of accidents for every month in 2013 and 2014.
#'	\dontrun{fars_summarize_years(c(2013, 2014))}
#' # Character objects work too
#'	\dontrun{fars_summarize_years(c("2013", "2014"))}
#' @import dplyr
#' @import tidyr
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' @title Map accidents on American state maps.
#' @description Display crashes for a state according to their MIPS code for a specified year.
#' @details Note that the year specified should be available on the current working directory, e.g. for year = "2013" the file "accidents_2013.csv.bz2" should appear when calling `dir()` in your instance.
#' Do note that the following packages are needed: dplyr, tidyr, readr, magrittr, purrr, and maps.
#' @param state.num The FIPS state code that corresponds to a state. The FIPS state code of American territories such as Puerto Rico or American Samoa are not guaranteed to work, especially if the dataset doesn't contain information on those territories.
#' @param year "character" or "integer" representing the year in XXXX format (e.g. 2013).
#' @examples
#'  # setwd(system.file("data-raw", package = "wk4package")) to try this out
#'  # Call the function to show Oregonian accidents for 2013 on a map.
#'  # This will throw an error if "accident_2013.csv.bz2" is not in the current working directory.
#'  \dontrun{fars_map_state(41, 2013)}
#'  # Running a FIPS code for a territory like American Samoa (60) or a year where the Republic didn't exist (e.g. 900AD) will throw an error.
#'  \dontrun{fars_map_state(60, 900)}
#' @return NULL
#' @references \href{https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code#FIPS_state_codes}{Wikipedia Article on FIPS codes} for a key between state.num and the region/state you want to input. 
#' \url{https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code#FIPS_state_codes}.
#' @importFrom purrr `%>%`
#' @import dplyr
#' @import maps
#' @family FARS functions
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

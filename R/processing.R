
#' Format Date Columns
#'
#' Formats date columns in CDC community level dataset, outoutting one column as date_format (in Month day, Year format) and one as date_updated (in Month-day format)
#'
#' @importFrom magrittr %>%
#'
#' @param df CDC community-level dataset
#' @param datecol Name of datecolumn formatting; default date_updated
#'
#' @return Dataframe with formatted date_updated and date_format columns
#' @export
#'
#' @examples{
#' df <- cdccovidplotting::read_county_commlevels("Maryland")
#'
#' format_date(df)
#'
#' }
format_date <- function(df, datecol = date_updated){
  df %>%
    dplyr::mutate(date_updated := as.Date({{datecol}}),
           date_format = format(date_updated, "%B %d, %Y"),
           date_updated = format(date_updated, "%m-%d"))

}


#' Calculate Change variables
#'
#' Calculates change in a given variable or variables from previous week to next week, as well as percentage changes.
#'
#' @param df CDC community level dataframe processed by format_date
#' @param colname Name or vector of names of columns to calculate change in previous week for as strings. Default c("covid_hospital_admissions_per_100k", "covid_inpatient_bed_utilization", "covid_cases_per_100k")
#' @param chngstr Name or vector of names of new column as string. Default c("chng_hospadmit", "chng_inpatient", "chng_covid")
#' @param .countycol Data-masked name of county column; default count
#' @param .dateformat Data-masked name of date column; default date_updated
#'
#' @return Dataframe with change in
#' @export
#'
#' @examples{
#' df <- cdccovidplotting::read_county_commlevels("Maryland")
#' df <- cdccovidplotting::format_date(df)
#'
#' dfone <- cdccovidplotting::chng_vals(df, colname = 'covid_cases_per_100k', "cases")
#'
#' dftwo <- cdccovidplotting::chng_vals(df, colname = c("covid_cases_per_100k", "covid_hospital_admissions_per_100k"), c("cases", "hospadmit"))
#'
#' dfthree <- cdccovidplotting::chng_vals(df)
#'
#' }
chng_vals <- function(df, colname = c("covid_hospital_admissions_per_100k", "covid_inpatient_bed_utilization", "covid_cases_per_100k"), chngstr = c("chng_hospadmit", "chng_inpatient", "chng_covid"), .countycol = county, .dateformat = date_updated){

  sortdf <- df %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{.countycol}}) %>%
    dplyr::arrange({{.dateformat}})

  if (length(chngstr) == 1){
    pct_chng <- paste0(chngstr, "_pct")

    outdf <- sortdf %>%
      dplyr::mutate(!!dplyr::sym(chngstr) := !!dplyr::sym(colname) - stats::lag(!!dplyr::sym(colname)),
                    !!dplyr::sym(pct_chng) := round(!!dplyr::sym(chngstr) * 100 / lag(!!dplyr::sym(colname)), 2))
  }

  else if(length(chngstr) > 1){

    outdf <- sortdf

    purrr::walk2(colname, chngstr, ~{

      pct_chng <- paste0(.y, "_pct")

      outdf <<- outdf  %>%
        dplyr::mutate(!!dplyr::sym(.y) := !!dplyr::sym(.x) - lag(!!dplyr::sym(.x)),
                      !!dplyr::sym(pct_chng) := round(!!dplyr::sym(.y) * 100 / lag(!!dplyr::sym(.x)), 2))

    })

  }

  outdf

}



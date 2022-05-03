#' Read-in CDC Community-level Data
#'
#' Read in county-level CDC community-level data from CDC API, filtering to provided statename
#'
#' @param statename Name of state to read county data for
#'
#' @return Dataset on CDC community-levels, filtered to supplied state
#' @export
#'
#' @examples{
#' read_county_commlevels("Maryland")
#'
#' read_county_commlevels("Massachusetts")
#'
#' }
read_county_commlevels <- function(statename){
  utils::read.csv(paste0("https://data.cdc.gov/resource/3nnm-4jni.csv?state=", statename))
}

#' Read and process CDC Community-level Data
#'
#' Reads and processes CDC community-level data for counties, formatting date variables and calculating changes in variables from each week
#'
#' @param statename Name of state to process county-level information on
#'
#' @return Dataframe with formatted date columns and change in values from prior week to current week
#' @export
#'
#' @examples{
#' read_process_commlevels("Maryland")
#'
#' read_process_commlevels("Alabama")
#'
#' }
read_process_commlevels <- function(statename){
  cdccovidplotting::read_county_commlevels(statename = statename) %>%
    cdccovidplotting::format_date() %>%
    cdccovidplotting::chng_vals()
}

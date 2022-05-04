#' Plot Community-level Components
#'
#' Creates plotly plot of metrics that compose CDC community-level
#'
#' @param vareval Expression of variable plotting in y-axis. E.g., ~ covid_cases_per_100k.
#' @param subtitle Title of plot
#' @param basechng String-name of variable measuring change from previous week associated with vareval. E.g., "chng_covid" would be associated with the evaluated variable ~ covid_cases_per_100k. Assumes that the percentage-change variable shares the same base-name
#' @param countydf CDC community-level dataframe filtered to one county
#'
#' @return
#' @export
#'
#' @examples{
#'
#'df <- cdccovidplotting::read_process_commlevels("Maryland")
#'
#'df_county <- dplyr::filter(df, county == "Allegany County")
#'
#'plot_commlevels(vareval = ~ covid_hospital_admissions_per_100k, subtitle = "Hospital admits", df_county, basechng = "chng_hospadmit")
#'
#' }
plot_commlevels <- function(countydf, vareval, subtitle, basechng){

  # extract string version of variable from vareval
  varstring <- gsub("~", "", dplyr::as_label(dplyr::quo(!!vareval)))

  basepct <- paste0(basechng, "_pct")

  returnplot <- plotly::plot_ly(countydf %>%
                                  dplyr::arrange(date_updated),
                                x = ~ date_updated,
                                y = vareval,
                                text = ~ glue::glue(paste0("Change since last week: {", basechng, "}
                         Percent change since last week: {", basepct, " %>% round(., 1)}%")),
                                showlegend = F,
                                type = "scatter",
                                mode = "line+marker")


  maxval <- dplyr::case_when(varstring == "covid_cases_per_100k" ~ 250,
                             varstring == "covid_hospital_admissions_per_100k" ~ 25,
                             varstring == "covid_inpatient_bed_utilization" ~ 20)

  if (max(countydf[[varstring]]) < maxval){
    returnplot <- returnplot %>%
      plotly::layout(xaxis = list(title = "Date"),
                     yaxis = list(title = "",
                                  tickvals = seq(0, maxval, maxval / 5),
                                  range = c(0, maxval)),
                     title = "")
  }

  else {
    returnplot <- returnplot %>%
      plotly::layout(xaxis = list(title = "Date"),
                     yaxis = list(title = ""),
                     title = "")
  }

  returnplot %>%
    plotlywrappers::subplot_title(subtitle)

}


#' Add CDC Community Level Threshold Lines
#'
#' Adds dotted lines for thresholds at which CDC community level increases
#'
#' @param baseplot plotly plot of CDC community level data
#' @param varstring Name of variable reflected in plot as string; determines at which levels to draw thresholds for
#' @param countydf CDC community-level dataframe filtered to one county
#' @param .covidvar String name of variable associated with covid-cases per 100,000 residents; default covid_cases_per_100k
#' @param .countycol String name of variable associated with filtered county; default county
#'
#' @return plotly plot with threshold dotted lines added to it for supplied variable
#' @export
#'
#' @examples{
#'
#'df <- cdccovidplotting::read_process_commlevels("Maryland")
#'
#'df_county <- dplyr::filter(df, county == "Allegany County")
#'
#'plot <- cdccovidplotting::plot_commlevels(vareval = ~ covid_hospital_admissions_per_100k, subtitle = "Hospital admits", df_county, basechng = "chng_hospadmit")
#'lines_approp(plot, "covid_hospital_admissions_per_100k", df_county)
#'
#' }
lines_approp <- function(baseplot, varstring, countydf, .covidvar = "covid_cases_per_100k", .countycol = "county"){

  # pull most recent data/case level
  curr_level_cases <- countydf %>%
    dplyr::group_by(!!dplyr::sym(.countycol)) %>%
    dplyr::arrange(desc(date_updated)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(!!dplyr::sym(.covidvar))

  if (length(curr_level_cases) > 1){
    stop("Error; multiple counties present in dataframe")
  }

  if (varstring == "covid_cases_per_100k"){

    returnplot <- baseplot %>%
      plotlywrappers::dotted_line(df = countydf, x_col = "date_updated", line_val = 200, gluetext = "", .showarrow = F)

  }

  else{
    if (curr_level_cases < 200){

      var_med <- dplyr::case_when(varstring == "covid_hospital_admissions_per_100k" ~ 10,
                           varstring == "covid_inpatient_bed_utilization" ~ 10)

      var_high <- dplyr::case_when(varstring == "covid_hospital_admissions_per_100k" ~ 20,
                            varstring == "covid_inpatient_bed_utilization" ~ 15)

      returnplot <- baseplot %>%
        plotlywrappers::multi_line(df = countydf, x_col = "date_updated", yvec = c(var_med, var_high), c("Medium", "High"))

    }

    if (curr_level_cases >= 200){
      var_high <- dplyr::case_when(varstring == "covid_hospital_admissions_per_100k" ~ 10,
                            varstring == "covid_inpatient_bed_utilization" ~ 10)

      returnplot <- baseplot %>%
        plotlywrappers::dotted_line(df = countydf, x_col = "date_updated", line_val = var_high, gluetext = "High", .showarrow = T)

    }

  }

  return(returnplot)

}

# test_pull <- function(pullvar){
#   mtcars %>%
#     dplyr::pull({{pullvar}})
#
# }

# test_pull(pullvar = mpg)

#' Plot Community-level data and draw threshold lines
#'
#' Generates plot of community-level COVID-19 data for given county and draws threshold lines at which point risk threshold increases. Wrapper for cdccovidplotting::plot_commlevels and cdccovidplotting::lines_approp functions.
#'
#' @param countydf CDC community-level dataframe filtered to one county. Alternatively, can provide unfiltered dataframe with countyfilt set to county want to filter data to
#' @param vareval Expression of variable plotting in y-axis. E.g., ~ covid_cases_per_100k.
#' @param subtitle Title of plot
#' @param basechng String-name of variable measuring change from previous week associated with vareval. E.g., "chng_covid" would be associated with the evaluated variable ~ covid_cases_per_100k. Assumes that the percentage-change variable shares the same base-name
#' @param countyfilt String name of county to filter countydf to; default NULL, which assumes countydf is already filtered to one county. Performs grepl match with countyfilt so partial matches acceptable
#' @param .covidvar String name of variable associated with covid-cases per 100,000 residents; default covid_cases_per_100k
#' @param .countycol String name of variable associated with filtered county; default county
#'
#' @return Plot of CDC community-level COVID-19 data for one county with risk-threshold lines drawn
#' @export
#'
#' @examples{
#'
#'df <- cdccovidplotting::read_process_commlevels("Maryland")
#'
#'df_county <- dplyr::filter( df, county == "Allegany County")
#'
#'cdccovidplotting::plot_commlevels_lines(vareval = ~ covid_hospital_admissions_per_100k, subtitle = "Hospital admits", countydf = df_county, basechng = "chng_hospadmit")
#'
#'cdccovidplotting::plot_commlevels_lines(vareval = ~ covid_hospital_admissions_per_100k, subtitle = "Hospital admits", countydf = df_county, basechng = "chng_hospadmit", countyfilt = "Allegany")
#'
#'
#'
#' }
plot_commlevels_lines <- function(countydf, vareval, subtitle, basechng, countyfilt = NULL, .covidvar = "covid_cases_per_100k", .countycol = "county"){

  if (!is.null(countyfilt)){
    countydf <- countydf %>%
      dplyr::filter(grepl(countyfilt, !!dplyr::sym(.countycol), ignore.case = T))
  }

  if (nrow(countydf) == 0 | length(dplyr::pull(countydf, !!dplyr::sym(.countycol)) %>% unique()) != 1){
    stop("More or less than 1 county column present in countydf after countyfilt applied")
  }

  cdccovidplotting::plot_commlevels(countydf = countydf, vareval = vareval, subtitle = subtitle, basechng = basechng) %>%
    cdccovidplotting::lines_approp(varstring = gsub("~", "", dplyr::as_label(dplyr::quo(!!vareval))), countydf = countydf, .covidvar = .covidvar, .countycol = .countycol)

}

#' Generate subplots of Community-level components
#'
#' Generates subplot of plotly plots of component-metrics that make up CDC community level for one county.
#'
#' @param countydf CDC community-level dataframe filtered to one county. Alternatively, can provide unfiltered dataframe with countyfilt set to county want to filter data to
#' @param varevals List2 of fomulas of variables plotting in y-axis. E.g., ~ covid_cases_per_100k. Default rlang::list2(~ covid_cases_per_100k, ~ covid_hospital_admissions_per_100k, ~ covid_hospital_admissions_per_100k)
#' @param subtitles Titles of subplots. Default c("COVID cases\ per 100k", "Percent of beds filled by COVID patients", "COVID hospital admits per 100k")
#' @param basechngs String-names of variables measuring change from previous week associated with vareval. E.g., "chng_covid" would be associated with the evaluated variable ~ covid_cases_per_100k. Assumes that the percentage-change variable shares the same base-name. Default c("chng_covid", "chng_inpatient", "chng_hospadmit")
#' @param countyfilt String name of county to filter countydf to; default NULL, which assumes countydf is already filtered to one county. Performs grepl match with countyfilt so partial matches acceptable
#' @param .covidvar String name of variable associated with covid-cases per 100,000 residents; default covid_cases_per_100k
#' @param .countycol String name of variable associated with filtered county; default county
#' @param subrows Number of rows in subplot; default 2
#'
#' @return Subplots of COVID-19 variables
#' @export
#'
#' @examples
subplots_keyvars <- function(countydf, varevals = rlang::list2(~ covid_cases_per_100k, ~ covid_hospital_admissions_per_100k, ~ covid_hospital_admissions_per_100k), subtitles = c("COVID cases\nper 100k", "Percent of beds\nfilled by\nCOVID patients", "COVID hospital\nadmits per\n100k"), basechngs = c("chng_covid", "chng_inpatient", "chng_hospadmit"), countyfilt= NULL, subrows = 2, .covidvar = "covid_cases_per_100k", .countycol = "county"){

  plots <- purrr::map(seq_along(subtitles), function(pos){

    cdccovidplotting::plot_commlevels_lines(countydf = countydf, vareval = varevals[[pos]], subtitle = subtitles[pos], basechng = basechngs[pos], countyfilt = countyfilt, .covidvar = .covidvar, .countycol = .countycol)

  })

  plotly::subplot(plots, nrows = subrows, shareX = T, shareY = F)

}


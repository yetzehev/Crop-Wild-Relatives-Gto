
#' Calculate Summary Stats for Groups
#'
#' Calculates summary stats (e.g., n, mean, median, SD, and range) for a single dependent variable for each combination of independent variables.
#'
#' @param df is a data frame.
#' @param dv a column name that contains a single dependent variable to calculate summary stats for.
#' @param ... names of cols corresponding to IVs to group by (supports n > 1 cols). IVs must be factors.
#'
#' @return
#' @export
#'
#' @examples
#' data("CO2")
#' df <- CO2
#' head(df)
#' group_by_summary_stats(df, uptake, Type, Plant)
group_by_summary_stats <- function(df, dv, ...){

  # Written by Parker Tichko, May, 2020
  # Contact: my first name DOT last name @ gmail.com

  require(dplyr)
  require(stringr)
  require(tibble)
  

    # drops NAs
    na.rm = TRUE

    # quote input args to use with dplyr functions
    dv <- dplyr::enquo(dv)
    group_vars <- dplyr::quos(...)

    # convert to tibble, if not already tibble
    if(tibble::is_tibble(df) == FALSE){
    df <- tibble::as_tibble(df)}

    # group by df variables
    df <- dplyr::group_by(df,!!! group_vars)

    #df <- group_by_at(df,!! group_vars)

    # calculate summary stats
    # !!s unquote input args, use !!! for multiply input args
   dplyr::summarise(df,
              N = sum(complete.cases(!! dv)),
              Mean = round(mean(!! dv, na.rm = na.rm), digits = 2),
              Median = round(median(!! dv, na.rm = na.rm), digits = 2),
              SD = round(sd(!! dv, na.rm = na.rm), digits = 2),
              SE = sd(!! dv, na.rm=na.rm)/sqrt(length(na.omit(!! dv))),
              Range = stringr::str_c(round(min(!! dv, na.rm = na.rm), digits = 2),
                            round(max(!! dv, na.rm = na.rm), digits = 2), sep = "-"))
}


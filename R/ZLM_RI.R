#' takes an analyte vector from read_INLAB_data function and calculates the reference interval using the refineR package.
#'
#' @param data A data.table containing the imported data, together with calculated age an single date values
#' @param analyte A string with the name of the analyte to calculate the reference interval for, must be contained within the data.table from read_INLAB_data
#' @param NBootstrap Number of bootstrap samples to use for the calculation
#' @param seed Seed for the random number generator
#'
#' @return list of reference interval values
#' @export
#'
#' @examples ZLM_RI(data = read_INLAB_data("BlutDxI", dir.path = "/home/olli/R_local/labStat/"), analyte = "Kreatinin")
ZLM_RI <- function(data, analyte, NBootstrap = 0, seed = 8173){
  df.filt <- data[data$Bezeichnung == analyte,]
  RI.data <- dplyr::pull(df.filt, Werte) |> na.omit()
  RI <- refineR::findRI(RI.data, NBootstrap = NBootstrap, seed = seed)
  refineR::print.RWDRI(RI, RIperc = c(0.025, 0.975), CIprop = 0.95, pointEst = "fullDataEst")
  refineR::plot.RWDRI(RI, showPathol = TRUE, showValue = TRUE, CIprop = 0.95, pointEst = "fullDataEst", showCI = TRUE)
  assign(paste(analyte, "RI_data", sep = "_"), RI, envir = .GlobalEnv)
}

#' Visualise risk scores in a table.
#'
#' @importFrom DT datatable
#' @importFrom scales percent
#'
#' @author Finlay Campbell
#'
#' @param risk The updated risk table access via \code{values$}
#' @param weightings The updated weightings access via \code{values$}
#'
vis_risk_table <- function(risk, weightings) {

  if(is.null(risk) | is.null(weightings)) return(NULL)

  risk %>%
    mutate(across(-1, ~ sprintf("%.2f", .x))) %>%
    ## add percent weighting to the table
    rename_with(
      \(nms) map_chr(nms, function(nm) {
        mtch <- match(nm, names(weightings))
        if(!is.na(mtch)) paste0(nm, " (", percent(weightings[mtch]), ")")
        else nm
      }))

}

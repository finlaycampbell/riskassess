#' Calculate risks from groupings, scores and overall weightings
#'
#' @author Finlay Campbell
#'
#' @param groupings Named list of named numeric vectors grouping the different
#'   scores into pillars and indicating their relative weighting within-pillar,
#'   as returned by \code{read_data}.
#'
#' @param scores data.frame of risk scores for each geographic region, as
#'   returned by \code{read_data}.
#'
#' @param weightings Numeric vector of the same length as \code{groupings}
#'   indicating the relative weightings of the different pillars.
#'
get_risks <- function(groupings, scores,
                      weightings = rep(1/length(groupings), length(groupings))) {

  weightings <- weightings/sum(weightings)

  tibble(
    scores[,1],
    map_dfc(
      groupings,
      \(grouping) {
        score <- scores[,names(grouping)]
        weightings <- map2_dfc(score, grouping, ~ as.numeric(!is.na(.x)) * .y) %>%
          as.matrix() %>% apply(1, \(x) x/sum(x)) %>% t()
        if(length(grouping) == 1) weightings <- t(weightings)
        apply(
          as.matrix(score) * weightings, 1,
          \(x) if(all(is.na(x))) return(NA) else return(sum(x, na.rm = TRUE))
        )
      }
    )
  ) %>%
    mutate(Total = apply(select(., -1), 1, \(x) sum(x*weightings)))

}

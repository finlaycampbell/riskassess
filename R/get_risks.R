#' Calculate risks from groupings, scores and overall weightings
#'
#' @importFrom utils write.csv
#'
#' @author Finlay Campbell
#'
get_risks <- function(groupings, scores, weightings = rep(1/length(groupings), length(groupings))) {

  weightings <- weightings/sum(weightings)

  df <- tibble(
    scores[,1],
    map_dfc(
      groupings,
      function(grouping) {
        score <- scores[,names(grouping)]
        weightings <- map2_dfc(score, grouping, ~ as.numeric(!is.na(.x)) * .y) %>%
          as.matrix() %>% apply(1, \(x) x/sum(x)) %>% t()
        if(length(grouping) == 1) weightings <- t(weightings)
        apply(
          as.matrix(score) * weightings, 1,
          function(x) if(all(is.na(x))) return(NA) else return(sum(x, na.rm = TRUE))
        )
      }
    )
  ) %>%
    mutate(Total = apply(select(., -1), 1, function(x) sum(x*weightings)))

}

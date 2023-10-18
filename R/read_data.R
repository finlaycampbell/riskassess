#' Read scores and groupings from data file provided
#'
#' @importFrom readxl read_excel
#'
#' @author Finlay Campbell
#'
read_data <- function(file) {

  ## read headers and scores themselves
  headers <- names(suppressMessages(read_excel(file))[1,-1])
  scores <- read_excel(file, skip = 1) %>%
    mutate(across(-1, as.numeric))

  ## get column indices of different groups
  header_start <- which(!grepl("\\.\\.\\.", headers))
  header_end <- lead(header_start) - 1
  header_end[is.na(header_end)] <- length(headers)

  groupings <- map2(
    setNames(header_start, headers[header_start]), header_end,
    function(start, end) {
      setNames(
        rep(1/length(start:end), length(start:end)),
        names(scores)[-1][start:end]
      )
    }
  )

  return(list(groupings = groupings, scores = scores))

}

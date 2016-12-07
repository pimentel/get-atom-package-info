# required packages:
#
# dplyr
# httr
# jsonlite
# parallel
# urltools

library('parallel')

atom_url <- 'https://atom.io/api/packages'

# get the total number of pages
header <- httr::HEAD(atom_url)
links <- strsplit(header$headers$link, ', ')[[1]]
links <- sub('<', '', links)
links <- sub('>.*$', '', links)

last_page <- as.integer(max(urltools::param_get(links, 'page')$page))

message(paste0('extracting ', last_page, ' pages'))

get_page <- function(page_number, how_to_sort = 'desc') {
  response <- httr::GET(atom_url,
    query = list(sort = how_to_sort, page = page_number))
  jsonlite::fromJSON(httr::content(response, as = 'text'),
    simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}

page_results <- mclapply(1:last_page, get_page)
all_results <- do.call('c', page_results)

get_column <- function(results, column_name, simplify = TRUE) {
  sapply(results, `[[`, column_name, simplify = simplify)
}

get_data_frame <- function(results) {
  result <- list()
  result$name <- get_column(results, 'name')
  result$version <- unname(unlist(get_column(results, 'releases')))
  result$downloads <- get_column(results, 'downloads')

  as.data.frame(result)
}

package_df <- get_data_frame(all_results)

stopifnot(all.equal(clean, dplyr::arrange(clean, desc(downloads))))

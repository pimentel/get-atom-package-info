# required packages:
#
# dplyr
# httr
# jsonlite
# parallel
# snow
# urltools

n_cpu <- 3
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop('Usage: RScript read_json.R N_CPU')
}

n_cpu <- as.integer(args[1])

library('snow')

cl <- makeCluster(n_cpu, type = 'SOCK')

atom_url <- 'https://atom.io/api/packages'

# get the total number of pages
header <- httr::HEAD(atom_url)
links <- strsplit(header$headers$link, ', ')[[1]]
links <- sub('<', '', links)
links <- sub('>.*$', '', links)

last_page <- as.integer(max(urltools::param_get(links, 'page')$page))

message(paste0('extracting ', last_page, ' pages'))

get_page <- function(page_number, how_to_sort = 'desc') {
  atom_url <- 'https://atom.io/api/packages'
  response <- httr::GET(atom_url,
    query = list(sort = how_to_sort, page = page_number))
  jsonlite::fromJSON(httr::content(response, as = 'text'),
    simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}

# get request time
request_time <- as.POSIXlt(Sys.time(), tz = 'GMT', origin = Sys.timezone())
request_time <- paste(strsplit(as.character(request_time), ' ')[[1]], collapse = '_')

# request every page
page_results <- snow::parLapply(cl, 1:last_page, get_page)
all_results <- do.call('c', page_results)

stopCluster(cl)

get_column <- function(results, column_name, simplify = TRUE) {
  sapply(results, `[[`, column_name, simplify = simplify)
}

get_data_frame <- function(results) {
  result <- list()
  result$name <- get_column(results, 'name')
  version <- get_column(results, 'releases')
  version[which(sapply(version, length) == 0)] <- list(latest = NA)
  result$version <- unname(unlist(version))
  result$downloads <- get_column(results, 'downloads')
  result$stargazers <- get_column(results, 'stargazers_count')

  as.data.frame(result)
}

package_df <- get_data_frame(all_results)
package_df <- dplyr::distinct(package_df)

# sanity check that nothing is going weird with api.
# we requested in descending order, so, sorting into sending should give us the same thing
stopifnot(all.equal(package_df, dplyr::arrange(package_df, desc(downloads))))

saveRDS(package_df, file.path('..', 'results', paste0(request_time, '_GMT.rds')))

#' Retrieve path from cache or download file and cache
#'
#' This function is described in the BiocFileCache vignette.
#'
#' @param id character(1) BEDbase id
#' @param url character(1) remote resource
#' @param bfc BiocFileCache(1) object
#' @param quietly logical(1) (default \code{TRUE}) display message
#'
#' @return character(1) filepath
#'
#' @examples
#' id <- "233479aab145cffe46221475d5af5fae"
#' url <- paste0(
#'     "https://data2.bedbase.org/files/2/6/",
#'     "26a57da7c732a8e63a1dda7ea18af021.bed.gz"
#' )
#' .download_to_cache(id, url, BiocFileCache::BiocFileCache(tempdir()))
#'
#' @noRd
.download_to_cache <- function(id, url, bfc, quietly = TRUE) {
    rid <- BiocFileCache::bfcquery(bfc, id, "rname")$rid
    if (!length(rid)) {
        if (!quietly) {
            rlang::inform(paste("Downloading", url, "..."))
        }
        rid <- names(BiocFileCache::bfcadd(bfc,
            rname = id, fpath = url, rtype = "web",
            download = TRUE, verbose = !quietly
        ))
    }
    BiocFileCache::bfcrpath(bfc, rids = rid)
}

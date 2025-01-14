#' Create nested path
#'
#' @description Create directory structure following the BEDbase url structure
#' by creating nested directories in the cache.
#'
#' @param file_name character(1) BEDbase file name
#' @param bfc BiocFileCache(1) object
#'
#' @return character(1) file path
#'
#' @examples
#' bedbase_url <- paste0(
#'     "https://data2.bedbase.org/files/2/6/",
#'     "26a57da7c732a8e63a1dda7ea18af021.bed.gz"
#' )
#' .create_nested_path(bedbase_url, bfc)
#' # [1] "2/6/26a57da7c732a8e63a1dda7ea18af021.bed.gz"
#'
#' @noRd
.create_nested_path <- function(bedbase_url, bfc) {
    file_name <- .get_file_name(bedbase_url)
    bfc_path <- BiocFileCache::bfccache(bfc)
    nested_path <- file.path(bfc_path, substr(file_name, 1, 1))
    if (!dir.exists(nested_path)) {
        dir.create(nested_path)
    }
    nested_path <- file.path(nested_path, substr(file_name, 2, 2))
    if (!dir.exists(nested_path)) {
        dir.create(nested_path)
    }
    file.path(nested_path, file_name)
}

#' Retrieve path from cache or download file and cache
#'
#' This function is described in the BiocFileCache vignette.
#'
#' @param id character(1) BEDbase id
#' @param bedbase_url character(1) remote resource
#' @param bfc BiocFileCache(1) object
#' @param quietly logical(1) (default \code{TRUE}) display message
#'
#' @return character(1) filepath
#'
#' @examples
#' id <- "233479aab145cffe46221475d5af5fae"
#' bedbase_url <- paste0(
#'     "https://data2.bedbase.org/files/2/6/",
#'     "26a57da7c732a8e63a1dda7ea18af021.bed.gz"
#' )
#' .download_to_cache(id, bedbase_url, BiocFileCache::BiocFileCache(tempdir()))
#'
#' @noRd
.download_to_cache <- function(id, bedbase_url, bfc, quietly = TRUE) {
    rid <- BiocFileCache::bfcquery(bfc, id, "rname")$rid
    if (!length(rid)) {
        if (!quietly) {
            rlang::inform(paste("Downloading", bedbase_url, "..."))
        }
        rpath <- .create_nested_path(bedbase_url, bfc)
        download.file(bedbase_url, rpath, quiet = quietly)
        rid <- names(BiocFileCache::bfcadd(bfc,
            rname = id,
            fpath = rpath, rtype = "local", rpath = rpath,
            download = FALSE, action = "asis", verbose = !quietly
        ))
    }
    BiocFileCache::bfcrpath(bfc, rids = rid)
}

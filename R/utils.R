#' Format BED file metadata
#'
#' @param records list(1) metadata
#'
#' @return tibble(1) file metadata
#'
#' @examples
#' api <- BEDbase()
#' ex_bed <- bb_example(api, "bed")
#' ex_metadata <- bb_metadata(api, ex_bed$id, TRUE)
#' .format_metadata_files(ex_bed$files)
#'
#' @noRd
.format_metadata_files <- function(metadata) {
    dplyr::bind_rows(metadata) |>
        tidyr::unnest_wider(access_methods) |>
        tidyr::unnest_wider(access_url)
}

#' Save a file from BEDbase to the cache or a path
#'
#' Will create directories that do not exist when saving
#'
#' @param metadata list(1) full metadata
#' @param cache_or_path BiocFileCache(1) or character(1) cache or save path
#' @param file_type character(1) bed or bigbed
#' @param access_type character(1) s3 or http
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return character(1) file path
#'
#' @examples
#' api <- BEDbase()
#' ex_bed <- bb_example(api, "bed")
#' md <- bb_metadata(api, ex_bed$id, TRUE)
#' .get_file(md, tempdir(), "bed", "http")
#'
#' @noRd
.get_file <- function(
        metadata, cache_or_path, file_type = c("bed", "bigbed"),
        access_type = c("s3", "http"), quietly = TRUE) {
    file_details <- .format_metadata_files(metadata$files) |>
        dplyr::filter(
            name == paste(file_type, "file", sep = "_"),
            access_id == access_type
        )
    if (is(cache_or_path, "BiocFileCache")) {
        cached_file <- .download_to_cache(
            metadata$id, file_details$url,
            cache_or_path, quietly
        )
        bedbase_file <- tryCatch(
            R.utils::gunzip(cached_file, remove = FALSE),
            error = function(e) {
                gsub(".gz", "", cached_file)
            }
        )
    } else {
        if (!dir.exists(cache_or_path)) {
            dir.create(cache_or_path, recursive = TRUE)
        }
        url_parts <- unlist(strsplit(file_details$url, "/"))
        bedbase_file <- file.path(cache_or_path, url_parts[length(url_parts)])
        utils::download.file(file_details$url, bedbase_file, quiet = quietly)
    }
    bedbase_file
}

#' Get extra_cols
#'
#' @param file_path character(1) path to BED
#' @param x double(1) the x in BEDX+Y
#' @param y double(1) the y in BEDX+Y
#'
#' @return vector(1) representing extraCols for rtracklayer
#'
#' @examples
#' id <- "608827efc82fcaa4b0bfc65f590ffef8"
#' api <- BEDbase()
#' md <- bb_metadata(api, id, TRUE)
#' file_path <- .get_file(md, getCache(api), "bed", "http")
#' .get_extra_cols(file_path, 3, 9)
#'
#' @noRd
.get_extra_cols <- function(file_path, x, y) {
    t <- utils::read.table(file_path, sep = "\t")
    extra_cols <- c()
    stopifnot(x + y == dim(t)[2])
    t_seq <- seq(from = x + 1, to = x + y)
    for (i in t[t_seq]) {
        if (typeof(i) == "integer") {
            col_type <- "numeric"
        } else {
            col_type <- typeof(i)
        }
        extra_cols <- c(extra_cols, col_type)
    }
    stats::setNames(extra_cols, names(t[t_seq]))
}

#' Import with genome
#'
#' @param args list(1) arguments to create a GRanges object
#'
#' @return GRanges(1) object representing BED
#'
#' @examples
#' api <- BEDbase()
#' ex_bed <- bb_example(api, "bed")
#' md <- bb_metadata(api, ex_bed$id, TRUE)
#' file_path <- .get_file(md, getCache(api), "bed", "http")
#' args <- list(
#'     con = file_path,
#'     format = gsub("peak", "Peak", metadata$bed_format),
#'     genome = md$genome_alias
#' )
#' .import_with_genome(args)
#'
#' @noRd
.import_with_genome <- function(args) {
    tryCatch(
        do.call(rtracklayer::import, args),
        error = function(e) {
            genome <- args["genome"]
            gro <- do.call(rtracklayer::import, within(args, rm("genome")))
            GenomeInfoDb::genome(gro) <- genome
            gro
        }
    )
}

#' Create GRanges object from a BED file
#'
#' If the BED format is known, `extra_cols` may be used to set the column name
#' and type. For example, \code{extra_cols = c(signalValue = "numeric",
#' pValue = "numeric", qValue = "numeric")}.
#'
#' Aborts if the length of `extra_cols` is not equal to Y in BEDX+Y.
#'
#' @param file_path character(1) path to BED file
#' @param metadata list(1) full metadata
#' @param extra_cols character(1) (default \code{NULL}) extra column names to
#' construct  GRanges objects
#' @param quietly boolean(1) (default \code{TRUE}) Display information messages
#'
#' @return GRanges(1) object representing BED
#'
#' @examples
#' api <- BEDbase()
#' ex_bed <- bb_example(api, "bed")
#' md <- bb_metadata(api, ex_bed$id, TRUE)
#' file_path <- .get_file(md, getCache(api), "bed", "http")
#' .bed_file_to_granges(file_path, md)
#'
#' @noRd
.bed_file_to_granges <- function(
        file_path, metadata, extra_cols = NULL,
        quietly = TRUE) {
    args <- list(con = file_path)
    args["format"] <- gsub("peak", "Peak", metadata$bed_format)
    nums <- stringr::str_replace(metadata$bed_type, "bed", "") |>
        stringr::str_split_1("\\+") |>
        as.double()

    if (!is.null(extra_cols) && (nums[2] != length(extra_cols))) {
        rlang::abort("`extra_cols` length must match Y value in `bed_type`.")
    }

    if (!grepl("Peak", args["format"]) && nums[2] != 0 && is.null(extra_cols)) {
        if (!quietly) {
            rlang::inform("Assigning column names and types.")
        }
        extra_cols <- .get_extra_cols(file_path, nums[1], nums[2])
    }

    if (!is.null(extra_cols)) {
        args[["extraCols"]] <- extra_cols
    }

    args["genome"] <- metadata$genome_alias

    .import_with_genome(args)
}

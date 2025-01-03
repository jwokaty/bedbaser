#' BEDbase class
#'
#' @importFrom methods new
#'
#' @return BEDbase class instance
.BEDbase <- setClass(
    "BEDbase",
    slots = c("cache"),
    contains = "Service"
)

.BEDBASE_API_REFERENCE_VERSION <- "0.8.0"

#' @rdname BEDbase
#'
#' @title An R client for BEDbase
#'
#' @description bedbaser exposes the BEDbase API and includes convenience
#' functions for common tasks, such as to import a BED file by `id` into a
#' `GRanges` object and a BEDset by its `id` into a `GRangesList`.
#'
#' The convenience functions are as follows
#' * `bedbaser::BEDbase()`: API service constructor
#' * `bedbaser::getCache()`: Retrieve cache
#' * `bedbaser::setCache()`: Set path to cache
#' * `bedbaser::bb_example()`: Retrieve an example BED file or BEDset
#' * `bedbaser::bb_metadata()`: Retrieve metadata for a BED file or BEDset
#' * `bedbaser::bb_list_beds()`: List all BED files
#' * `bedbaser::bb_list_bedsets()`: List all BEDsets
#' * `bedbaser::bb_beds_in_bedset()`: List BED files in BEDset
#' * `bedbaser::bb_bed_text_search()`: Search BED files by text
#' * `bedbaser::bb_to_granges()`: Create a `GRanges` object from a BED id
#' * `bedbaser::bb_to_grangeslist()`: Create a `GrangesList` from a BEDset id
#' * `bedbaser::bb_save()`: Save a BED file to a path.
#'
#' @param cache_path string(1) cache
#' @param quietly logical(1) (default \code{FALSE}) display messages
#'
#' @return BEDbase object
#'
#' @examples
#' api <- BEDbase(cache_path = tempdir())
#' ex_bed <- bb_example(api, "bed")
#' ex_bed_md <- bb_metadata(api, "bed")
#'
#' @export
BEDbase <- function(cache_path, quietly = FALSE) {
    if (missing(cache_path)) {
        cache_path <- tools::R_user_dir("bedbaser", which = "cache")
    }
    api <- suppressWarnings(
        .BEDbase(
            cache = BiocFileCache::BiocFileCache(cache_path),
            AnVIL::Service(
                service = "bedbase",
                host = "api.bedbase.org",
                api_reference_version = .BEDBASE_API_REFERENCE_VERSION,
                authenticate = FALSE,
                package = "bedbaser",
                api_url = character(),
                api_reference_url = "https://api.bedbase.org/openapi.json",
            )
        )
    )
    info <- httr::content(api$list_beds_v1_bed_list_get(limit = 0, offset = 0))
    if (!quietly) {
        message(info$count, "BED files available.")
    }
    api
}

#' @rdname BEDbase
#'
#' @param x BEDbase(1) object
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @export
setGeneric("getCache", function(x, quietly = TRUE) standardGeneric("getCache"))

#' Return cache path
#'
#' @param x BEDbase(1) object
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return BiocFileCache(1) object of BED files
#'
#' @examples
#' api <- BEDbase(tempdir())
#' getCache(api)
#'
#' @export
setMethod(
    "getCache", "BEDbase",
    function(x, quietly = TRUE) {
        if (quietly) {
            BiocFileCache::bfcinfo(x@cache)
        }
        x@cache
    }
)

#' @rdname BEDbase
#'
#' @param x BEDbase(1) object
#' @param cache_path character(1)
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @export
setGeneric(
    "setCache",
    function(x, cache_path, quietly = TRUE) standardGeneric("setCache")
)

#' Set cache path
#'
#' @param x BEDbase(1) object
#' @param cache_path character(1)
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return BiocFileCache(1) object of BED files
#'
#' @examples
#' api <- BEDbase(tempdir())
#' api <- setCache(api, "/tmp")
#'
#' @export
setMethod(
    "setCache", "BEDbase",
    function(x, cache_path, quietly = TRUE) {
        x@cache <- BiocFileCache::BiocFileCache(cache_path)
        if (quietly) {
            BiocFileCache::bfcinfo(x@cache)
        }
        x
    }
)

#' Display API
#'
#' @param x BEDbase(1) object
#' @param ... other options
#' @param .deprecated (default \code{FALSE}) if deprecated
#'
#' @importFrom AnVIL operations
#'
#' @return list(1) API end points
#'
#' @examples
#' api <- BEDbase()
#' operations(api)
#'
#' @export
setMethod(
    "operations", "BEDbase",
    function(x, ..., .deprecated = FALSE) {
        methods::callNextMethod(x, ..., .deprecated = .deprecated)
    }
)

#' Get the example BED file or BEDset with metadata
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param rec_type character(1) bed or bedset
#'
#' @return list(1) bed files or bedsets
#'
#' @examples
#' api <- BEDbase()
#' bb_example(api, "bed")
#' bb_example(api, "bedset")
#'
#' @export
bb_example <- function(api, rec_type = c("bed", "bedset")) {
    rec_type <- match.arg(rec_type)
    if (rec_type == "bed") {
        rsp <- api$get_example_bed_record_v1_bed_example_get()
    } else {
        rsp <- api$get_example_bedset_record_v1_bedset_example_get()
    }
    httr::content(rsp)
}

#' Get metadata for a BED file or BEDset
#'
#' @description Get metadata for a BED file or BEDset. Abort
#' if not found or id is not not 32 characters
#'
#' @rdname bb_metadata
#'
#' @param api API object created from \code{BEDbase()}
#' @param id integer1() record or object identifier
#' @param full logical(1) (default \code{FALSE}) include full record with
#' stats, files, and metadata
#'
#' @return list(1) metadata
#'
#' @examples
#' api <- BEDbase()
#'
#' ex_bed <- bb_example(api, "bed")
#' bb_metadata(api, ex_bed$id)
#'
#' ex_bedset <- bb_example(api, "bedset")
#' bb_metadata(api, ex_bedset$id)
#'
#' @export
bb_metadata <- function(api, id, full = FALSE) {
    rsp <- api$get_bed_metadata_v1_bed__bed_id__metadata_get(
        bed_id = id,
        full = full
    )
    if (rsp$status_code != 200) {
        rsp <- api$get_bedset_metadata_v1_bedset__bedset_id__metadata_get(
            bedset_id = id,
            full = full
        )
    }
    result <- httr::content(rsp)
    if (rsp$status_code == 404) {
        rlang::abort(result$detail)
    } else if (rsp$status != 200) {
        rlang::abort("{result$type}: input {result$input} {result$msg}")
    } else {
        result
    }
}

#' List BEDs
#'
#' @rdname bb_list_beds
#'
#' @param api API object created from \code{BEDbase()}
#' @param genome character(1) (default \code{NULL}) genome keyword
#' @param bed_type character(1) (default \code{NULL}) bed file type
#' @param limit integer(1) (default \code{1000}) maximum records
#' @param offset integer(1) (default \code{0}) page token of records
#'
#' @return tibble(1) of BED records
#'
#' @examples
#' api <- BEDbase()
#' bb_list_beds(api)
#'
#' @export
bb_list_beds <- function(
        api, genome = NULL, bed_type = NULL, limit = 1000,
        offset = 0) {
    rsp <- api$list_beds_v1_bed_list_get(
        genome = genome, bed_type = bed_type,
        limit = limit, offset = offset
    )
    recs <- httr::content(rsp)
    results <- tibble::tibble()
    if (recs$count) {
        results <- do.call(
            dplyr::bind_rows,
            lapply(recs$results, function(x) {
                unlist(x)
            })
        )
    }
    results
}

#' List BEDsets
#'
#' @rdname bb_list_bedsets
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param query character() (default \code{NULL}) keyword
#' @param limit integer(1) (default \code{1000}) maximum records
#' @param offset integer(1) (default \code{0}) page token of records
#'
#' @return tibble(1) of BEDset records
#'
#' @examples
#' api <- BEDbase()
#' bb_list_bedsets(api)
#'
#' @export
bb_list_bedsets <- function(api, query = NULL, limit = 1000, offset = 0) {
    rsp <- api$list_bedsets_v1_bedset_list_get(
        query = query,
        limit = limit,
        offset = offset
    )
    recs <- httr::content(rsp)
    results <- tibble::tibble()
    if (recs$count) {
        results <- dplyr::bind_rows(recs$results) |>
            tidyr::unnest(cols = c(bed_ids))
    }
    results
}

#' Get BEDs associated with BEDset
#'
#' @rdname bb_beds_in_bedset
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param bedset_id integer(1) BEDset record identifier
#'
#' @return tibble(1) information of BED files in BEDset
#'
#' @examples
#' api <- BEDbase()
#' ex_bedset <- bb_example(api, "bedset")
#' bb_beds_in_bedset(api, ex_bedset$id)
#'
#' @export
bb_beds_in_bedset <- function(api, bedset_id) {
    rsp <- api$get_bedfiles_in_bedset_v1_bedset__bedset_id__bedfiles_get(
        bedset_id = bedset_id
    )
    recs <- httr::content(rsp)
    results <- tibble::tibble()
    if (recs$count) {
        results <- do.call(
            dplyr::bind_rows,
            lapply(recs$results, function(x) {
                unlist(x)
            })
        )
    }
    results
}

#' Search BED files by text
#'
#' Returns all available results scored.
#'
#' @rdname bb_bed_text_search
#'
#' @param api API object of BEDbase created from \code{BEDbase()}
#' @param query character(1) keywords to search
#' @param limit integer(1) (default \code{10}) maximum number of results
#' @param offset integer(1) (default \code{0}) page offset of results
#'
#' @return tibble(1) of results
#'
#' @examples
#' api <- BEDbase()
#' bb_bed_text_search(api, "hg38")
#'
#' @export
bb_bed_text_search <- function(api, query, limit = 10, offset = 0) {
    encoded_query <- utils::URLencode(query, reserved = TRUE)
    rsp <- api$text_to_bed_search_v1_bed_search_text_post(
        query = encoded_query,
        limit = limit,
        offset = offset
    )
    recs <- httr::content(rsp)
    results <- tibble::tibble()
    if (recs$count) {
        results <- purrr::map_depth(.x = recs$results, 1, \(y) unlist(y)) |>
            dplyr::bind_rows()
    }
    results
}

#' Create a GRanges object given a BED id
#'
#' Generates the column and types for broad and narrow peak files.
#'
#' @rdname bb_to_granges
#'
#' @param api API object of BEDbase created from \code{BEDbase()}
#' @param bed_id integer(1) BED record identifier
#' @param file_type character(1) bed or bigbed
#' @param extra_cols character() (default \code{NULL}) extra column names to
#' construct GRanges objects
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return GRanges(1) object
#'
#' @examples
#' api <- BEDbase()
#' ex_bed <- bb_example(api, "bed")
#' bb_to_granges(api, ex_bed$id)
#'
#' @export
bb_to_granges <- function(
        api, bed_id, file_type = "bed", extra_cols = NULL,
        quietly = TRUE) {
    stopifnot(file_type %in% c("bed", "bigbed"))
    metadata <- bb_metadata(api, bed_id, TRUE)
    file_path <- .get_file(metadata, getCache(api), file_type, "http", quietly)

    if (file_type == "bed") {
        .bed_file_to_granges(file_path, metadata, extra_cols, quietly)
    } else if (file_type == "bigbed") {
        if (.Platform$OS.type == "windows") {
            rlang::warn("This feature does not work on Windows.")
        } else {
            args <- list(
                con = file_path,
                genome = metadata$genome_alias,
                format = "bigBed"
            )
            .import_with_genome(args)
        }
    }
}

#' Create a GRangesList object given a BEDset id
#'
#' @rdname bb_to_grangeslist
#'
#' @param api API object of BEDbase created from \code{BEDbase()}
#' @param bedset_id integer(1) BEDset record identifier
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return GRangesList(1) object
#'
#' @examples
#' api <- BEDbase()
#' id <- "lola_hg38_ucsc_features"
#' bb_to_grangeslist(api, id)
#'
#' @export
bb_to_grangeslist <- function(api, bedset_id, quietly = TRUE) {
    beds <- bb_beds_in_bedset(api, bedset_id)
    gros <- list()
    for (bed_id in beds$id) {
        gro <- bb_to_granges(api, bed_id, quietly = quietly)
        gros[[length(gros) + 1]] <- gro
    }
    GenomicRanges::GRangesList(gros)
}

#' Save a BED or BEDset files to a path given an id
#'
#' @rdname bb_save
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param bed_or_bedset_id integer(1) BED or BEDset record identifier
#' @param path character(1) directory to save file
#' @param file_type character(1) (default \code{"bed"}) bed, bigbed, etc.
#' @param access_type character(1) (default \code{"http"})  s3 or http
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return An invisible \code{NULL}
#'
#' @examples
#' api <- BEDbase()
#' ex <- bb_example(api, "bed")
#' bb_save(api, ex$id, tempdir())
#'
#' @export
bb_save <- function(
        api, bed_or_bedset_id, path, file_type = "bed",
        access_type = "http", quietly = TRUE) {
    if (!dir.exists(path)) {
        rlang::abort(paste(path, "doesn't exist.", sep = " "))
    }
    metadata <- bb_metadata(api, bed_or_bedset_id, TRUE)
    if ("bedsets" %in% names(metadata)) {
        ids <- list(metadata$id)
    } else {
        ids <- metadata$bed_ids
    }
    for (id in ids) {
        metadata <- bb_metadata(api, id, TRUE)
        .get_file(metadata, path, file_type, access_type, quietly)
    }
}

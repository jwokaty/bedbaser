#' BEDbase class
#'
#' @importFrom methods new
#'
#' @return BEDbase class instance
.BEDbase <- setClass(
    "BEDbase",
    slots = c("bedfiles", "bedsets"),
    contains = "Service"
)

.BEDBASE_API_REFERENCE_VERSION <- "0.8.0"

#' @rdname BEDbase
#'
#' @title An R client for BEDbase
#'
#' @description bedbaser exposes the [BEDbase API](https://api.bedbase.org)
#' and includes convenience functions for common tasks, such as to import a
#' BED file by `id` into a `GRanges` object and a BEDset by its `id` into a
#' `GRangesList`.
#'
#' \code{BEDbase()} creates a cache similar to that of the
#' [Geniml BBClient's cache](https://docs.bedbase.org/geniml):
#'     cache_path
#'       bedfiles
#'         a/f/afile.bed.gz
#'       bedsets
#'         a/s/aset.txt
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
#' bedbase <- BEDbase(cache_path = tempdir())
#' ex_bed <- bb_example(bedbase, "bed")
#' bb_metadata(bedbase, ex_bed$id)
#'
#' @export
BEDbase <- function(cache_path, quietly = FALSE) {
    if (missing(cache_path)) {
        cache_path <- tools::R_user_dir("bedbaser", which = "cache")
    } else if (!dir.exists(cache_path)) {
        dir.create(file.path(cache_path, "bedfiles"), recursive = TRUE)
        dir.create(file.path(cache_path, "bedsets"), recursive = TRUE)
    }
    bedbase <- suppressWarnings(
        .BEDbase(
            bedfiles = BiocFileCache::BiocFileCache(
                file.path(cache_path, "bedfiles")
            ),
            bedsets = BiocFileCache::BiocFileCache(
                file.path(cache_path, "bedsets")
            ),
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
    info <- httr::content(
        bedbase$list_beds_v1_bed_list_get(limit = 0, offset = 0)
    )
    if (!quietly) {
        message(info$count, " BED files available.")
    }
    bedbase
}

#' @rdname BEDbase
#'
#' @param x BEDbase(1) object
#' @param type character(1) bedfiles or bedsets
#'
#' @export
setGeneric(
    "getCache",
    function(x, cache_type = c("bedfiles", "bedsets")) standardGeneric("getCache")
)

#' Return cache path
#'
#' @param x BEDbase(1) object
#' @param cache_type character(1) bedfiles or bedsets
#'
#' @return BiocFileCache(1) object of BED files
#'
#' @examples
#' bedbase <- BEDbase(tempdir())
#' getCache(bedbase, "bedfiles")
#'
#' @export
setMethod(
    "getCache", "BEDbase",
    function(x, cache_type = c("bedfiles", "bedsets")) {
        cache_type <- match.arg(cache_type)
        if (cache_type == "bedsets") {
            x@bedsets
        } else {
            x@bedfiles
        }
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

#' Set cache along path
#'
#' @param x BEDbase(1) object
#' @param cache_path character(1)
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return BiocFileCache(1) object of BED files
#'
#' @examples
#' bedbase <- BEDbase(tempdir())
#' bedbase <- setCache(bedbase, "/tmp")
#'
#' @export
setMethod(
    "setCache", "BEDbase",
    function(x, cache_path, quietly = TRUE) {
        x@bedfiles <- BiocFileCache::BiocFileCache(
            file.path(cache_path, "bedfiles"),
            !quietly
        )
        x@bedsets <- BiocFileCache::BiocFileCache(
            file.path(cache_path, "bedsets"),
            !quietly
        )
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
#' bedbase <- BEDbase()
#' operations(bedbase)
#'
#' @export
setMethod(
    "operations", "BEDbase",
    function(x, ..., .deprecated = FALSE) {
        methods::callNextMethod(x, ..., .deprecated = .deprecated)
    }
)

#' Display schemas
#'
#' @param x BEDbase(1) object
#'
#' @importFrom AnVIL schemas
#'
#' @return list(1) API end points
#'
#' @examples
#' bedbase <- BEDbase()
#' schemas(bedbase)
#'
#' @export
setMethod(
    "schemas", "BEDbase",
    function(x) {
        methods::callNextMethod(x)
    }
)

#' Display tags
#'
#' @param x BEDbase(1) object
#' @param .tags character() tags for filtering operations
#' @param .deprecated (default \code{FALSE}) if deprecated
#'
#' @importFrom AnVIL tags
#'
#' @return list(1) API end points
#'
#' @examples
#' bedbase <- BEDbase()
#' tags(bedbase)
#'
#' @export
setMethod(
    "tags", "BEDbase",
    function(x, .tags, .deprecated = FALSE) {
        methods::callNextMethod(x, .tags, .deprecated = .deprecated)
    }
)

#' Get the example BED file or BEDset with metadata
#'
#' @param bedbase API object of BEDbase created from BEDbase()
#' @param rec_type character(1) bed or bedset
#'
#' @return list(1) bed files or bedsets
#'
#' @examples
#' bedbase <- BEDbase()
#' bb_example(bedbase, "bed")
#' bb_example(bedbase, "bedset")
#'
#' @export
bb_example <- function(bedbase, rec_type = c("bed", "bedset")) {
    rec_type <- match.arg(rec_type)
    if (rec_type == "bed") {
        rsp <- bedbase$get_example_bed_record_v1_bed_example_get()
    } else {
        rsp <- bedbase$get_example_bedset_record_v1_bedset_example_get()
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
#' @param bedbase API object created from \code{BEDbase()}
#' @param id integer1() record or object identifier
#' @param full logical(1) (default \code{FALSE}) include full record with
#' stats, files, and metadata
#'
#' @return list(1) metadata
#'
#' @examples
#' bedbase <- BEDbase()
#'
#' ex_bed <- bb_example(bedbase, "bed")
#' bb_metadata(bedbase, ex_bed$id)
#'
#' ex_bedset <- bb_example(bedbase, "bedset")
#' bb_metadata(bedbase, ex_bedset$id)
#'
#' @export
bb_metadata <- function(bedbase, id, full = FALSE) {
    rsp <- bedbase$get_bed_metadata_v1_bed__bed_id__metadata_get(
        bed_id = id,
        full = full
    )
    if (rsp$status_code != 200) {
        rsp <- bedbase$get_bedset_metadata_v1_bedset__bedset_id__metadata_get(
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
#' @param bedbase API object created from \code{BEDbase()}
#' @param genome character(1) (default \code{NULL}) genome keyword
#' @param bed_type character(1) (default \code{NULL}) bed file type
#' @param limit integer(1) (default \code{1000}) maximum records
#' @param offset integer(1) (default \code{0}) page token of records
#'
#' @return tibble(1) of BED records
#'
#' @examples
#' bedbase <- BEDbase()
#' bb_list_beds(bedbase)
#'
#' @export
bb_list_beds <- function(
        bedbase, genome = NULL, bed_type = NULL, limit = 1000,
        offset = 0) {
    rsp <- bedbase$list_beds_v1_bed_list_get(
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
#' @param bedbase API object of BEDbase created from BEDbase()
#' @param query character() (default \code{NULL}) keyword
#' @param limit integer(1) (default \code{1000}) maximum records
#' @param offset integer(1) (default \code{0}) page token of records
#'
#' @return tibble(1) of BEDset records
#'
#' @examples
#' bedbase <- BEDbase()
#' bb_list_bedsets(bedbase)
#'
#' @export
bb_list_bedsets <- function(bedbase, query = NULL, limit = 1000, offset = 0) {
    rsp <- bedbase$list_bedsets_v1_bedset_list_get(
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
#' @param bedbase API object of BEDbase created from BEDbase()
#' @param bedset_id integer(1) BEDset record identifier
#'
#' @return tibble(1) information of BED files in BEDset
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bedset <- bb_example(bedbase, "bedset")
#' bb_beds_in_bedset(bedbase, ex_bedset$id)
#'
#' @export
bb_beds_in_bedset <- function(bedbase, bedset_id) {
    rsp <- bedbase$get_bedfiles_in_bedset_v1_bedset__bedset_id__bedfiles_get(
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
#' @param bedbase API object of BEDbase created from \code{BEDbase()}
#' @param query character(1) keywords to search
#' @param limit integer(1) (default \code{10}) maximum number of results
#' @param offset integer(1) (default \code{0}) page offset of results
#'
#' @return tibble(1) of results
#'
#' @examples
#' bedbase <- BEDbase()
#' bb_bed_text_search(bedbase, "hg38")
#'
#' @export
bb_bed_text_search <- function(bedbase, query, limit = 10, offset = 0) {
    encoded_query <- utils::URLencode(query, reserved = TRUE)
    rsp <- bedbase$text_to_bed_search_v1_bed_search_text_post(
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
#' @param bedbase API object of BEDbase created from \code{BEDbase()}
#' @param bed_id integer(1) BED record identifier
#' @param file_type character(1) bed or bigbed
#' @param extra_cols character() (default \code{NULL}) extra column names to
#' construct GRanges objects
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return GRanges(1) object
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bed <- bb_example(bedbase, "bed")
#' bb_to_granges(bedbase, ex_bed$id)
#'
#' @export
bb_to_granges <- function(
        bedbase, bed_id, file_type = "bed", extra_cols = NULL,
        quietly = TRUE) {
    stopifnot(file_type %in% c("bed", "bigbed"))
    metadata <- bb_metadata(bedbase, bed_id, TRUE)
    file_path <- .get_file(
        metadata, getCache(bedbase, "bedfiles"), file_type,
        "http", quietly
    )

    bed_file <- tryCatch(
        R.utils::gunzip(file_path, remove = FALSE),
        error = function(e) {
            gsub(".gz", "", file_path)
        }
    )

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
#' @param bedbase API object of BEDbase created from \code{BEDbase()}
#' @param bedset_id integer(1) BEDset record identifier
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return GRangesList(1) object
#'
#' @examples
#' bedbase <- BEDbase()
#' bb_to_grangeslist(bedbase, "lola_hg38_ucsc_features")
#'
#' @export
bb_to_grangeslist <- function(bedbase, bedset_id, quietly = TRUE) {
    beds <- bb_beds_in_bedset(bedbase, bedset_id)
    gros <- list()
    for (bed_id in beds$id) {
        gro <- bb_to_granges(bedbase, bed_id, quietly = quietly)
        gros[[length(gros) + 1]] <- gro
    }
    GenomicRanges::GRangesList(gros)
}

#' Save a BED or BEDset files to a path given an id
#'
#' @rdname bb_save
#'
#' @param bedbase API object of BEDbase created from BEDbase()
#' @param bed_or_bedset_id integer(1) BED or BEDset record identifier
#' @param path character(1) directory to save file
#' @param file_type character(1) (default \code{"bed"}) bed, bigbed, etc.
#' @param access_type character(1) (default \code{"http"})  s3 or http
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return An invisible \code{NULL}
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bed <- bb_example(bedbase, "bed")
#' bb_save(bedbase, ex_bed$id, tempdir())
#'
#' @export
bb_save <- function(
        bedbase, bed_or_bedset_id, path, file_type = "bed",
        access_type = "http", quietly = TRUE) {
    if (!dir.exists(path)) {
        rlang::abort(paste(path, "doesn't exist.", sep = " "))
    }
    metadata <- bb_metadata(bedbase, bed_or_bedset_id, TRUE)
    if ("bedsets" %in% names(metadata)) {
        ids <- list(metadata$id)
    } else {
        ids <- metadata$bed_ids
    }
    for (id in ids) {
        metadata <- bb_metadata(bedbase, id, TRUE)
        .get_file(metadata, path, file_type, access_type, quietly)
    }
}

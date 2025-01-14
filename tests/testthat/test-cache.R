test_that("nested path is created", {
    bedbase <- BEDbase(quietly = TRUE)
    bfc <- getCache(bedbase, "bedfiles")
    bfc_path <- BiocFileCache::bfccache(bfc)
    bedbase_url <- "http://an_example_url/2468.bed.gz"
    expect_equal(
        .create_nested_path(bedbase_url, bfc),
        file.path(bfc_path, "2/4/2468.bed.gz")
    )
})

test_that("default cache location is used", {
    path <- tools::R_user_dir("bedbaser", which = "cache")
    bedbase <- BEDbase(quietly = TRUE)
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    gro <- bb_to_granges(bedbase, id, "bed")
    bfc <- BiocFileCache::BiocFileCache(file.path(path, "bedfiles"))
    expect_equal(BiocFileCache::bfccache(bfc), file.path(path, "bedfiles"))
    md <- bb_metadata(bedbase, id, TRUE)
    file_path <- .get_file(md, getCache(bedbase, "bedfiles"), "bed", "http")
    expect_true(file_path %in% BiocFileCache::bfcinfo(bfc)$rpath)
})

test_that("path is used if set when calling constructor", {
    path <- tempdir()
    bedbase <- BEDbase(path, TRUE)
    bfc <- getCache(bedbase, "bedfiles")
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    gro <- bb_to_granges(bedbase, id, "bed")
    expect_equal(BiocFileCache::bfccache(bfc), file.path(path, "bedfiles"))
})

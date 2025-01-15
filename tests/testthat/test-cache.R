test_that("nested path is created", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    cache <- getCache(bedbase, "bedfiles")
    cache_path <- BiocFileCache::bfccache(cache)
    bedbase_url <- "http://an_example_url/2468.bed.gz"
    expect_equal(
        .create_nested_path(bedbase_url, cache),
        file.path(cache_path, "2/4/2468.bed.gz")
    )
})

test_that("default cache location is used", {
    path <- tools::R_user_dir("bedbaser", which = "cache")
    bedbase <- BEDbase(quietly = TRUE)
    expect_true(dir.exists(file.path(path, "bedfiles")))
    expect_true(dir.exists(file.path(path, "bedsets")))
})

test_that("path is used if set when calling constructor", {
    path <- tempdir()
    bedbase <- BEDbase(path, TRUE)
    cache <- getCache(bedbase, "bedfiles")
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    gro <- bb_to_granges(bedbase, id, "bed")
    expect_equal(BiocFileCache::bfccache(cache), file.path(path, "bedfiles"))
})

test_that("bedset txt is cached", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    id <- "test_bedset"
    beds <- bb_beds_in_bedset(bedbase, id)
    cache <- getCache(bedbase, "bedsets")
    .cache_bedset_txt(id, beds$id, cache)
    rpath <- .create_nested_path(id, cache)
    expect_equal(readLines(rpath), beds$id)
    expect_equal(BiocFileCache::bfcquery(cache, id, "rname")$rpath, rpath)
})

test_that("bed files are cached", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    id <- bb_example(bedbase, "bed")$id
    cache <- getCache(bedbase, "bedfiles")
    rid <- BiocFileCache::bfcquery(cache, id, "rname")$rid
    expect_length(rid, 0)
    bedbase_url <- .get_url(bb_metadata(bedbase, id, TRUE), "bed", "http")
    rpath <- .cache_bedfile(id, bedbase_url, cache)
    expect_true(file.exists(rpath))
    rid <- BiocFileCache::bfcquery(cache, id, "rname")$rid
    expect_length(length(rid), 1)
})

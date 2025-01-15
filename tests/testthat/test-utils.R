test_that(".get_file_name returns file name", {
    expect_equal(
        .get_file_name("https://this/is/an/example"),
        "example"
    )
})

test_that(".get_url returns a url", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    ex_bed <- bb_example(bedbase, "bed")
    ex_md <- bb_metadata(bedbase, ex_bed$id, TRUE)
    file_url <- .get_url(ex_md, "bed", "http")
    expect_true(stringr::str_detect(file_url, "(https?|ftp|s3)://"))
})

test_that(".get_file returns a valid file path", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    ex_bed <- bb_example(bedbase, "bed")
    md <- bb_metadata(bedbase, ex_bed$id, TRUE)
    file_path <- .get_file(md, tempdir(), "bed", "http")
    expect_true(file.exists(file_path))
    file_path <- .get_file(md, getCache(bedbase, "bedfiles"), "bed", "http")
    expect_true(file.exists(file_path))
})

test_that(".get_extra_cols returns a named vector", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    id <- "608827efc82fcaa4b0bfc65f590ffef8"
    md <- bb_metadata(bedbase, id, TRUE)
    file_path <- .get_file(md, getCache(bedbase, "bedfiles"), "bed", "http")
    extra_cols <- .get_extra_cols(file_path, 3, 9)
    expect_equal(9, length(extra_cols))
})

test_that(".format_metadata_files returns a tibble with a url column", {
    bedbase <- BEDbase(quietly = TRUE)
    ex_bed <- bb_example(bedbase, "bed")
    ex_md <- bb_metadata(bedbase, ex_bed$id, TRUE)
    mdf <- .format_metadata_files(ex_md$files)
    expect_true(methods::is((mdf)[1], "tbl_df"))
    expect_true("url" %in% names(mdf))
})

test_that(".get_file returns a valid file path", {
    bedbase <- BEDbase(quietly = TRUE)
    ex_bed <- bb_example(bedbase, "bed")
    md <- bb_metadata(bedbase, ex_bed$id, TRUE)
    temp_path <- tempdir()
    file_path <- .get_file(md, temp_path, "bed", "http")
    expect_true(file.exists(file_path))
    file_path <- .get_file(md, getCache(bedbase), "bed", "http")
    expect_true(file.exists(file_path))
})

test_that(".get_extra_cols returns a named vector", {
    bedbase <- BEDbase(quietly = TRUE)
    id <- "608827efc82fcaa4b0bfc65f590ffef8"
    md <- bb_metadata(bedbase, id, TRUE)
    file_path <- .get_file(md, getCache(bedbase), "bed", "http")
    extra_cols <- .get_extra_cols(file_path, 3, 9)
    expect_equal(9, length(extra_cols))
})

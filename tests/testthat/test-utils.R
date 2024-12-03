test_that(".format_metadata_files returns a tibble with a url column", {
    api <- BEDbase()
    ex_bed <- bb_example(api, "bed")
    ex_md <- bb_metadata(api, ex_bed$id, TRUE)
    mdf <- .format_metadata_files(ex_md$files)
    expect_true(is((mdf)[1], "tbl_df"))
    expect_true("url" %in% names(mdf))
})

test_that(".get_file returns a valid file path", {
    api <- BEDbase()
    ex_bed <- bb_example(api, "bed")
    md <- bb_metadata(api, ex_bed$id, TRUE)
    temp_path <- tempdir()
    file_path <- .get_file(md, temp_path, "bed", "http")
    expect_true(file.exists(file_path))
    file_path <- .get_file(md, getCache(api), "bed", "http")
    expect_true(file.exists(file_path))
})

test_that(".get_extra_cols returns a named vector", {
    api <- BEDbase()
    id <- "608827efc82fcaa4b0bfc65f590ffef8"
    md <- bb_metadata(api, id, TRUE)
    file_path <- .get_file(md, getCache(api), "bed", "http")
    extra_cols <- .get_extra_cols(file_path, 3, 9)
    expect_equal(9, length(extra_cols))
})

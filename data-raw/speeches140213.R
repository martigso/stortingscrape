## Build the example dataset `speeches140213`: the speeches in the transcript of
## the Storting's meeting on 13 February 2014, used in the vignette.
##
## Run from the package root.

pkgload::load_all()

speeches140213 <- get_speeches("s140213")

usethis::use_data(speeches140213, overwrite = TRUE)

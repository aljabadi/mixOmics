devtools::check(vignettes = FALSE, check_dir = "../check")
rcmdcheck::rcmdcheck(args = c("--ignore-vignettes"), build_args = c("--no-build-vignettes"), error_on = "never", check_dir = "../check")

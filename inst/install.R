file.remove("NAMESPACE") |> suppressWarnings()
usethis::use_gpl_license(version = 3, include_future = TRUE)
devtools::document()
devtools::check()
remove.packages("smartReports") |> suppressWarnings()
devtools::install()


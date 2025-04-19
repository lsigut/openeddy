# original file: LNZ23_GF_essentials_2024-12-13 (Lanzhot station CZ-Lnz)
# subset: 2023-07-23 - 2023-08-05 (14 days)
# units were reformatted to be consistent
# order of QC columns was shifted to start with _SSITC and follow with _for_GF
# using of read_eddy() for data loading attaches varnames and units attributes
# strptime_eddy() converts timestamp column to POSIXct and shifts it by -900 s

eddy_data <- read_eddy("data-raw/eddy_data.csv")

# convert timestamp to POSIXct and shift the date-time information to represent
# the center of averaging period which is required for reliable processing
eddy_data$timestamp <- strptime_eddy(eddy_data$timestamp, shift.by = -900)

# compression optimization automated by: tools::resaveRdaFiles("data/")
# recognition of optimal compression: tools::checkRdaFiles("data/")
# the smallest file size was obtained using "xz" compression
usethis::use_data(eddy_data, overwrite = TRUE, compress = "xz")

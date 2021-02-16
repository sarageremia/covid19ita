load_data <- function(start, end, main_url, data_url) {
  date <- rep(NA, as.numeric(end - start))
  end_filename <- rep(NA, length(date))

  i <- start
  v <- 1

  while (i <= end & v <= length(date)) {
    date[v] <- i

    v <- v + 1
    i <- i + days(1)
  }
  date <- as.Date(date, origin = "1970-01-01")

  for (i in 1:length(date)) {
    end_filename[i] <- paste0(
      format.Date(date[i], "%Y"),
      format.Date(date[i], "%m"),
      format.Date(date[i], "%d")
    )
  }

  data <- tibble()

  for (i in end_filename) {
    url <- paste0(main_url, data_url, i, ".csv")
    data_from_each_file <- read.csv(url, sep = ",")
    data <- bind_rows(data, data_from_each_file)
  }
  return(data)
}

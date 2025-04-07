tempfile_path <- tempfile()
download.file("http://davidcard.berkeley.edu/data_sets/njmin.zip", destfile = tempfile_path)
tempdir_path <- tempdir()
unzip(tempfile_path, exdir = tempdir_path)
codebook <- read_lines(file = paste0(tempdir_path, "/codebook"))

variable_names <- codebook %>%
  `[`(8:59) %>%
  `[`(-c(5, 6, 13, 14, 32, 33)) %>%
  str_sub(1, 13) %>%
  str_squish() %>%
  str_to_lower()

dataset <- read_table2(paste0(tempdir_path, "/public.dat"),
                       col_names = FALSE)

dataset <- dataset %>%
  select(-X47) %>%
  `colnames<-`(., variable_names) %>%
  mutate_all(as.numeric) %>%
  mutate(sheet = as.character(sheet)) %>%
  mutate(
    state = ifelse(state == 1, "nj", "pa")
  )


write.csv(dataset,file="fast-food-data.csv")
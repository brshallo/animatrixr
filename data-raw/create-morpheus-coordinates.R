library(magick)
# link to photo source (was cropped manually):
# https://i.pinimg.com/originals/b1/22/ba/b122ba1de60f1b04b7a93e1597f688a7.jpg
matrix_spoon <- magick::image_read(here::here("inst", "extdata", "morpheus3-2.jpg"))

# Code for this is mostly copied from:
# https://github.com/ropensci/magick/issues/101#issuecomment-353191334
bitmap <- matrix_spoon %>%
  image_edge(radius = 1.5) %>%
  image_convert(type = 'grayscale') %>%
  image_quantize(2, colorspace = 'gray') %>%
  image_negate() %>%
  image_reducenoise(radius = 1.5) %>%
  image_data(channels = 'gray')

morpheus <- which(bitmap < 100, arr.ind = TRUE) %>%
  as_tibble() %>%
  select(-dim1) %>%
  rename(x = dim2, y = dim3) %>%
  mutate(across(c(x, y), ~-(.x - mean(.x))))

usethis::use_data(morpheus, overwrite = TRUE)

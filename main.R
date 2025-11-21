install.packages("magick")
library(magick)
dir <- ""
setwd(dir)
out_dir <- ""


resize_images <- function(filepath_in, filepath_out, idx_start, idx_end) {
  n <- idx_end - idx_start
  resized <- list()

  for (i in idx_start:idx_end) {
    img <- image_read(sprintf("%s/normal-%s.jpg", filepath_in, i))
    img_resized <- image_resize(img, "512x512!")
    resized[[i - idx_start + 1]] <- img_resized
    image_write(img_resized, path = paste0(filepath_out, "/test-", i, ".jpg"), format = "jpg")
  }
  return(resized)
}
resized_images <- resize_images(dir, out_dir, 2000, 2010)

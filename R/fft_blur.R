create_kernel <- function(width, height, sd) {
  i <- do.call(expand.grid, lapply(c(height, width), function(d) c(seq(0, d/2), seq(-floor((d - 1)/2), -1))))
  v <- exp(-rowSums(i^2) / (2 * sd^2)) / (sd * sqrt(2 * pi))^2
  matrix(v, nrow = height, ncol = width)
}

image_fft_blur <- function(x, sigma) {
  padding <- max(ceiling(3 * sigma), 1)
  dim <- image_info(x)
  pad_rows <- c(rep_len(1, padding), seq_len(dim$width), rep_len(dim$width, padding))
  pad_cols <- c(rep_len(1, padding), seq_len(dim$height), rep_len(dim$height, padding))
  unpad_rows <- seq_len(dim$width) + padding
  unpad_cols <- seq_len(dim$height) + padding
  kernel <- fft(create_kernel(dim$height + padding * 2, dim$width + padding * 2, sigma))
  x <- x[[1]]
  blurry <- lapply(seq_len(dim(x)[1]), function(i) {
    future::future({
        channel <- matrix(as.integer(x), nrow = dim$width, ncol = dim$height)
        channel <- channel[pad_rows, pad_cols]
        channel <- Re(fft(fft(channel) * kernel, inverse = TRUE) / length(channel))
        as.raw(as.integer(round(channel[unpad_rows, unpad_cols])))
      },
      globals = list(
        x = x[i, ,], kernel = kernel, dim = dim, pad_rows = pad_rows, pad_cols = pad_cols,
        unpad_rows = unpad_rows, unpad_cols = unpad_cols
      )
    )
  })
  for (i in seq_len(dim(x)[1])) {
    x[i, , ] <- future::value(blurry[[i]])
  }
  image_read(x)
}

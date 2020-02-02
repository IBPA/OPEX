#' Add white noise to the output column given a dataset stored in a data.frame.
#' It is only used when testing the method on a synthetic dataset since a 
#' real dataset already has noise in it.

add_noise <- function(noise_level, adddata) {
  noises <- rep(0, nrow(adddata))
  for (dup in 1:4) {
    noise <- rnorm(nrow(adddata), 0, adddata[, 3] * noise_level * 0.01)
    noises <- noises + noise
  }
  noises <- noises/4
  adddata[, 3] <- adddata[, 3] + noises
  
  return(adddata)
}

#' Return angle of rotation of matrix.
#'
#' @param m 2x2 matrix.
#'
#' @return Double of length 1.
#' @export
find_angle <- function(m) {
  atan2(m[2,1], m[1,1])
}

#' Create Rotation Matrix
#'
#' @param theta Angle in radians.
#'
#' @return 2x2 matrix.
#' @export
rotation_matrix <- function(theta){
  tribble(~ x, ~ y,
          cos(theta), -sin(theta),
          sin(theta), cos(theta)) %>%
    as.matrix()
}

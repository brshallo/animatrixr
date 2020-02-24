#' Sequence of Matrices, Linear
#'
#' Make sequence of matrices, changing coordinates linearly
#'
#' @param from Matrix
#' @param to Matrix
#' @param ... Passed into `seq()` function, generally should only input `length.out`.
#'
#' @return List of matrices.
#' @export
#'
seq_matrix_l <- function(from, to, ...){
  start_dim <- dim(from)

  map2(from, to, seq, ...) %>%
    transpose() %>%
    map(unlist) %>%
    map(structure, dim = start_dim)
}

#' Sequence of Matrices, Linear and Polar
#'
#' Make sequence of matrices, changing rotation of a unit vector on a polar axis
#' and remaining component of transformation changes linearly. Both happen
#' simultaneously so a rotation will only look like a rotation if done on a unit
#' vector.
#'
#' @param from Matrix
#' @param to Matrix
#' @param ... Passed into `seq()` function, generally should only input `length.out`.
#'
#' @return List of matrices.
#' @export
#'
seq_matrix_lp <- function(from, to, ...){

  angle_start <- find_angle(from)
  angle_end <- find_angle(to)

  angle_seq_vec <- seq(from = angle_start, to = angle_end, ...)

  angle_seq <- map(angle_seq_vec, rotation_matrix)

  remaining_start <- from %*% solve(first(angle_seq))
  remaining_end <- to %*% solve(last(angle_seq))
  remaining_seq <- seq_matrix_l(remaining_start, remaining_end, ...)

  map2(remaining_seq, angle_seq, `%*%`)
}



#' Sequence of Matrices, polar followed by linear
#'
#' Generally should not use. Given an individual transformation matrix, return a
#' combination of polar and linear components. Rotation though happens on a unit
#' circle.
#'
#' @param from Matrix
#' @param to Matrix
#' @param ... Passed into `seq()` function, generally should only input `length.out`.
#'
#' @return List of matrices.
#' @export
#'
seq_matrix_rotate_first <- function(from, to, ...){

  warning(
    "Probably should not use. E.g.:\nShould NOT be combined with multiple transformation matrices.\nNumber of frames can  be double w/e is passed into `...`"
  )

  angle_start <- find_angle(from)
  angle_end <- find_angle(to)

  angle_seq_vec <- seq(from = angle_start, to = angle_end, ...)
  angle_seq <- map(angle_seq_vec, rotation_matrix)

  # remaining_start <- from %*% solve(first(angle_seq))
  remaining_end <- to %*% solve(last(angle_seq))

  remaining_zero <- (from - remaining_end) %>%
    sum() %>%
    near(0, tol = 3^-10)

  if(remaining_zero) return(angle_seq)

  other_seq <- seq_matrix_l(last(angle_seq), to, ...)[-1]

  c(angle_seq, other_seq)
}

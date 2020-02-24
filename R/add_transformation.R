#' Add Matrix Transformation
#'
#' Creates list of matrices. If list is inputted then concatenates onto list of matrices.
#'
#' @param from Matrix or list of matrices.
#' @param m Matrix transformation.
#' @param seq_fun The function type for the matrix transformation, `animatrixr::seq_matrix_l`
#' @param n_frames Number of frames to create explicitly, defaults to 20.
#'
#' @return List of matrices.
#' @export
#'
add_transformation <- function(from = matrix(c(1,0,0,1), nrow = 2),
                               m,
                               seq_fun = animatrixr::seq_matrix_l,
                               n_frames = 20){

  if (is.list(from)){
    from_tail  <-  last(from)
    to <- m %*% from_tail

    output <- seq_fun(from_tail, to, length.out = n_frames + 1)

    return(c(from, output[-1]))

  }else{
    to <- m %*% from

    output <- seq_fun(from, to, length.out = n_frames + 1)

    return(output)
  }
}

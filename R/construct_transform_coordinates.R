#' Create coordinates for a grid.
#'
#'given vectors of x and y intercepts, return a dataframe with columns x, y,
#'xend, yend (meant for input into geom_segment()).
#'
#' @param xintercepts Numeric vector, default is -5:5.
#' @param yintercepts Numeric vector, default is -5:5.
#'
#' @return Dataframe.
#' @export
construct_grid <- function(xintercepts = -5:5, yintercepts = -5:5){
  bind_rows(
    crossing(x = xintercepts,
             y = min(yintercepts),
             yend = max(yintercepts)) %>%
      mutate(xend = x),
    crossing(y = yintercepts,
             x = min(xintercepts),
             xend = max(xintercepts)) %>%
      mutate(yend = y)
  ) %>%
    select(x, y, xend, yend)
}

#' Transform coordinates
#'
#' Given dataframe, column names of coordinates, and a transformation matrix,
#' return dataframe with transformed coordinates.
#'
#' @param df Dataframe with coordinates.
#' @param ... Column names where coordinates are (Will be treated as x, y, ...).
#'   Column names not supplied will be unchanged.
#' @param m Matrix transformation to apply.
#'
#' @return Dataframe with transformed coordinates.
#' @export
#'
transform_df_coords <- function(df, ..., m = diag(length(df))){

  df_names <- names(df)

  df_coords <- df %>%
    select(...)

  df_coords_names <- names(df_coords)

  df_matrix <- df_coords %>%
    as.matrix() %>%
    t()

  df_coords_new <- (m %*% df_matrix) %>%
    t() %>%
    as_tibble() %>%
    set_names(df_coords_names)

  df_other <- df %>%
    select(-one_of(df_coords_names))

  bind_cols(df_coords_new, df_other) %>%
    select(df_names)
}

#' Transform Coordinates of Segment
#'
#' Helper function to apply `transform_df_coords()` to the dataframe constructed
#' by `construct_grid()` or similar.
#'
#' @param df Dataframe with column names `x`, `y`, `xend`, `yend`.
#' @param m 2x2 matrix.
#'
#' @return Dataframe with same column names but coordinates transformed according to `m`.
#' @export
#'
transform_segment <- function(df, m){
  df %>%
    transform_df_coords(x, y, m = m) %>%
    transform_df_coords(xend, yend, m = m)
}

#' Scale Data Frame
#'
#' Helper function for adding coordinates onto animation of transformation.
#'
#' @param data Dataframe containing at least one numeric column.
#' @param limit Maximum magnitude value in range of coordinates after scaling, defaults to 5.
#'
#' @return A dataframe that has been scaled so that the largest value in a numeric column is 5.
#' @export
#'
#' @examples scale_data(tibble(x = 1:10, y = -1:-10))
scale_data <- function(data, limit = 5){
  max_magnitude <- max(abs(select(data, where(is.numeric))))
  scale_factor <- limit / max_magnitude
  mutate(data, across(where(is.numeric), ~.x * scale_factor))
}

#' Animate matrix
#'
#' @param transformations List of matrix transformations to apply (length will
#'   determine total number of explicit frames in animation.)
#' @param points_start Dataframe containing x and y columns of points that will
#'   be plotted (if any coordinate is greater than magnitude 5, coordinates will
#'   be automatically scaled down).
#' @param return_graph_objects Logical indicating whether to return the
#'   dataframe containing the objects that would have gone into the animation.
#' @param return_static Logical indicating whether to return the static ggplot
#'   graph.
#' @param datasaurus Logical, where `TRUE` is equivalent to is equivalent to
#'   setting `points_start = filter(datasauRus::datasaurus_dozen, dataset ==
#'   "dino")`
#'
#' @return An animation created by `gganimate`.
#' @export
#'
animate_matrix <-function(transformations,
                          points_start = NULL,
                          return_graph_objects = FALSE,
                          return_static = FALSE,
                          datasaurus = FALSE){

  grid_start <- construct_grid() %>%
    mutate(id = row_number())

  basis_start <- tibble(
    x = c(0, 0),
    y = c(0, 0),
    xend = c(1, 0),
    yend = c(0, 1),
    # `vec` is unnecessary, will just use to differentiate colors
    vec = c("i", "j")
  ) %>%
    mutate(id = max(grid_start$id) + row_number())

  # store objects in list-col with transformations
  graph_objects <- tibble(time = seq_along(transformations),
                          transform = transformations) %>%
    mutate(grids = map(transform, transform_segment, df = grid_start),
           basis = map(transform, transform_segment, df = basis_start))

  grid_all <- graph_objects %>%
    select(time, grids) %>%
    unnest(grids)

  basis_all <- graph_objects %>%
    select(time, basis) %>%
    unnest(basis)

  if(datasaurus){
    points_start <- filter(datasauRus::datasaurus_dozen, dataset == "dino")
  }

  if(!is.null(points_start)){

    if(!all(c("x", "y") %in% colnames(points_start))) stop("'x' and 'y' columns must be in `points_start`")

    if(5 <= max(abs(select(points_start, where(is.numeric))))){
      points_start <- scale_data(points_start)
      message("x, y coordinates of `points_start` scaled so that maximum magnitude is 5.")
    }

    points_start <- points_start %>%
      mutate(id = max(basis_start$id) + row_number())

    graph_objects <- graph_objects %>%
      mutate(points = map(transform, ~transform_df_coords(points_start, x, y, m = .x)))

    points_all <- graph_objects %>%
      select(time, points) %>%
      unnest(points)
  }

  if(return_graph_objects) return(graph_objects)

  x_breaks <- unique(grid_start$x)
  y_breaks <- unique(grid_start$y)

  p <- ggplot(aes(x = x, y = y, group = id), data = grid_all)+
    geom_segment(aes(xend = xend, yend = yend))+
    geom_segment(aes(xend = xend, yend = yend, colour = vec), arrow = arrow(length = unit(0.02, "npc")), size = 1.2, data = basis_all)+
    scale_x_continuous(breaks = x_breaks, minor_breaks = NULL)+
    scale_y_continuous(breaks = y_breaks, minor_breaks = NULL)+
    coord_fixed()+
    theme_minimal()+
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")

  if(!is.null(points_start)) p <- p + geom_point(colour = "darkgreen", size = 0.5, data = points_all)

  if(return_static) return(p)

  p_anim <- p + gganimate::transition_states(time, state_length = 0, wrap = FALSE)

  gganimate::animate(p_anim, duration = 5, start_pause = 10, end_pause = 10)
}

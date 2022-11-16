#' Custom Dodge Enough
#'
#' Supply a numeric vector and size of points as supplied to geom_point.
#' This function returns an appropriate x/y axis (whatever you did NOT supply in vec) to avoid points overlapping
#'
#' @param vec numeric vector used to determine closeness of points
#' @param pointsize the size of the points.
#'
#' @return vec dodged to avoid overlap
#' @export
#'
ggdodge_custom_dodge_amount <- function(vec, pointsize, spacing = 1) {

  dist_from_prev_point <- c(Inf, tail(vec, n=-1) - head(vec, n=-1))
  #browser()
  y_add = ifelse(
    dist_from_prev_point < ggplot2:::.pt * pointsize,
    yes = spacing,
    no = 0
  )

  # If points are consecutively close together keep adding larger amounts to y at one time
  new_y_add <- numeric(length(y_add))
  for (i in seq_along(y_add)){
    #browser()
    if(i ==1 ){
      new_y_add[[i]] <- y_add[[i]]
    }
    else{
      prev_y_add <- new_y_add[[i-1]]

      if(y_add[[i]] == 0)
        new_y_add[[i]] <- 0
      else {
        new_y_add[[i]] <- prev_y_add + y_add[[i]]
      }
    }

  }
  return(new_y_add)
}

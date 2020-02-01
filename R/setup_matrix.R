#-------------------------------------------------------------------------------
# This function sets the position of the 2 provided colours (x, y) in each
# small grid-square
#-------------------------------------------------------------------------------

setup_matrix <- function(x, y, pattern, position){

  # Set rotate function
  rotate <- function(x) t(apply(x, 2, rev))

  # Set matrix pattern
  if(is.null(pattern)){
    stop("You must provide: \n- a pattern string to use one of the preset patterns: \n   stripe; corner; circle; diagonal_cross; diagonal_diamond; checkered")
  } else {
    if(pattern=="stripe"){
      m <- matrix(c(x,x,x,x,x,x,x,x,x,x,
                    x,y,y,y,y,y,y,y,y,y,
                    x,y,x,x,x,x,x,x,x,x,
                    x,y,x,y,y,y,y,y,y,y,
                    x,y,x,y,x,x,x,x,x,x,
                    x,y,x,y,x,y,y,y,y,y,
                    x,y,x,y,x,y,x,x,x,x,
                    x,y,x,y,x,y,x,y,y,y,
                    x,y,x,y,x,y,x,y,x,x,
                    x,y,x,y,x,y,x,y,x,y),
                  nrow=10, ncol=10)
    } else if(pattern=="corner"){
      m <- matrix(c(y,y,x,y,y,x,x,x,x,x,
                    y,y,x,y,y,x,x,x,x,x,
                    x,x,x,y,y,x,x,x,x,x,
                    y,y,y,y,y,x,x,x,x,x,
                    y,y,y,y,y,x,x,x,x,x,
                    x,x,x,x,x,y,y,y,y,y,
                    x,x,x,x,x,y,y,y,y,y,
                    x,x,x,x,x,y,y,x,x,x,
                    x,x,x,x,x,y,y,x,y,y,
                    x,x,x,x,x,y,y,x,y,y),
                  nrow=10, ncol=10)
    } else if(pattern=="circle"){
      m <- matrix(c(x,x,x,x,x,x,x,x,x,x,
                    x,y,y,y,y,y,y,y,y,x,
                    x,y,x,x,x,x,x,x,y,x,
                    x,y,x,y,y,y,y,x,y,x,
                    x,y,x,y,x,x,y,x,y,x,
                    x,y,x,y,x,x,y,x,y,x,
                    x,y,x,y,y,y,y,x,y,x,
                    x,y,x,x,x,x,x,x,y,x,
                    x,y,y,y,y,y,y,y,y,x,
                    x,x,x,x,x,x,x,x,x,x),
                  nrow=10, ncol=10)
    } else if(pattern=="diagonal_cross"){
      m <- matrix(c(x,x,y,y,y,x,x,x,y,y,
                    x,x,x,y,y,y,x,x,x,y,
                    y,x,x,x,y,y,y,x,x,x,
                    y,y,x,x,x,y,y,y,x,x,
                    y,y,y,x,x,x,y,y,y,x,
                    x,y,y,y,x,x,x,y,y,y,
                    x,x,y,y,y,x,x,x,y,y,
                    x,x,x,y,y,y,x,x,x,y,
                    y,x,x,x,y,y,y,x,x,x,
                    y,y,x,x,x,y,y,y,x,x),
                  nrow=10, ncol=10)
    } else if(pattern=="diagonal_diamond"){
      m <- matrix(c(y,y,x,x,x,y,y,y,x,x,
                    y,x,x,x,y,y,y,x,x,x,
                    x,x,x,y,y,y,x,x,x,y,
                    x,x,y,y,y,x,x,x,y,y,
                    x,y,y,y,x,x,x,y,y,y,
                    y,y,y,x,x,x,y,y,y,x,
                    y,y,x,x,x,y,y,y,x,x,
                    y,x,x,x,y,y,y,x,x,x,
                    x,x,x,y,y,y,x,x,x,y,
                    x,x,y,y,y,x,x,x,y,y),
                  nrow=10, ncol=10)
    } else if(pattern=="checkered"){
      m <- matrix(c(x,y,x,y,x,y,x,y,x,y,
                    y,x,y,x,y,x,y,x,y,x,
                    x,y,x,y,x,y,x,y,x,y,
                    y,x,y,x,y,x,y,x,y,x,
                    x,y,x,y,x,y,x,y,x,y,
                    y,x,y,x,y,x,y,x,y,x,
                    x,y,x,y,x,y,x,y,x,y,
                    y,x,y,x,y,x,y,x,y,x,
                    x,y,x,y,x,y,x,y,x,y,
                    y,x,y,x,y,x,y,x,y,x),
                  nrow=10, ncol=10)
    }
  }

  # Set rotational pattern
  if(position=="tl"){
    m = m
  } else if(position=="tr"){
    m = rotate(m)
  } else if(position=="br"){
    m = rotate(rotate(m))
  } else if(position=="bl"){
    m = rotate(rotate(rotate(m)))
  }

  return(m)
}
#-------------------------------------------------------------------------------
# This function sets the combination of the colours (1-10) in each quarter of
# the pattern
#-------------------------------------------------------------------------------

setup_guide_df <- function(position){

  # Create guide df
  if(is.null(position)){

    stop("You must provide: \n- a quarter position for the matrix (tl; tr; bl; br)")

    } else {

    if(position=="tl"){
      guide_df <- data.frame(x = c(1,1,1,1,1,
                                   2,2,2,2,2,
                                   3,3,3,3,3,
                                   4,4,4,4,4,
                                   5,5,5,5,5),
                             y = c(1,2,3,4,5,
                                   1,2,3,4,5,
                                   1,2,3,4,5,
                                   1,2,3,4,5,
                                   1,2,3,4,5))
    } else if(position=="tr"){
      guide_df <- data.frame(x = c(10,10,10,10,10,
                                   9,9,9,9,9,
                                   8,8,8,8,8,
                                   7,7,7,7,7,
                                   6,6,6,6,6),
                             y = c(5,4,3,2,1,
                                   5,4,3,2,1,
                                   5,4,3,2,1,
                                   5,4,3,2,1,
                                   5,4,3,2,1))
    } else if(position=="br"){
      guide_df <- data.frame(x = c(6,6,6,6,6,
                                   7,7,7,7,7,
                                   8,8,8,8,8,
                                   9,9,9,9,9,
                                   10,10,10,10,10),
                             y = c(6,7,8,9,10,
                                   6,7,8,9,10,
                                   6,7,8,9,10,
                                   6,7,8,9,10,
                                   6,7,8,9,10))
    } else if(position=="bl"){
      guide_df <- data.frame(x = c(10,9,8,7,6,
                                   10,9,8,7,6,
                                   10,9,8,7,6,
                                   10,9,8,7,6,
                                   10,9,8,7,6),
                             y = c(5,5,5,5,5,
                                   4,4,4,4,4,
                                   3,3,3,3,3,
                                   2,2,2,2,2,
                                   1,1,1,1,1))
    }
  }

  return(guide_df)
}
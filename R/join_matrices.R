#-------------------------------------------------------------------------------
# This function joins the matrices into a larger square grif=d
#-------------------------------------------------------------------------------

join_matrices <- function(pattern, position){

  # Set row/column starts
  row_n=1
  col_n=1

  guide_df <- setup_guide_df(position)

  # Run loop
  for(i in 1:nrow(guide_df)){

    l <- setup_matrix(x=guide_df$x[i], y=guide_df$y[i], pattern, position=position)

    if(row_n == 1 & col_n == 1){
      row_1 <- l
    } else if(row_n == 1){
      row_1 <- cbind(row_1, l)
    } else if(row_n == 2 & col_n == 1){
      row_2 <- l
    } else if(row_n == 2){
      row_2 <- cbind(row_2, l)
    } else if(row_n == 3 & col_n == 1){
      row_3 <- l
    } else if(row_n == 3){
      row_3 <- cbind(row_3, l)
    } else if(row_n == 4 & col_n == 1){
      row_4 <- l
    } else if(row_n == 4){
      row_4 <- cbind(row_4, l)
    } else if(row_n == 5 & col_n == 1){
      row_5 <- l
    } else if(row_n == 5){
      row_5 <- cbind(row_5, l)
    }

    if(col_n %in% 1:4){
      col_n = col_n + 1
    } else {
      row_n = row_n + 1
      col_n = 1
    }

  }

  # Join matrices together into one large grid
  combination <- rbind(row_1, row_2)
  combination <- rbind(combination, row_3)
  combination <- rbind(combination, row_4)
  combination <- rbind(combination, row_5)

  # Transform to dataframe
  melt_combination <- melt(combination)

  # Set bins for colouring the plot
  melt_combination$bin <- cut(as.numeric(melt_combination$value),
                              breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5))

  return(melt_combination)
}
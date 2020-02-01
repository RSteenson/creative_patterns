#-------------------------------------------------------------------------------
# This function takes the larger grid matrix output and plots with the colour
# palette provided
#-------------------------------------------------------------------------------

plot_mitred_square_pattern <- function(pattern, colour_palette){

  # Setup matrices
  tl <- join_matrices(pattern, position="tl")
  tr <- join_matrices(pattern, position="tr")
  br <- join_matrices(pattern, position="br")
  bl <- join_matrices(pattern, position="bl")

  # Build each plot
  tl_plot <- ggplot(data=tl, aes(x=Var1, y=Var2, fill=bin)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_manual(values=colour_palette) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_reverse(lim=c(50,0), expand = c(0, 0)) +
    theme_collapse() +
    coord_equal()
  tr_plot <- ggplot(data=tr, aes(x=Var1, y=Var2, fill=bin)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_manual(values=colour_palette) +
    scale_x_reverse(lim=c(50,0), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_collapse() +
    coord_equal()
  br_plot <- ggplot(data=br, aes(x=Var1, y=Var2, fill=bin)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_manual(values=colour_palette[6:10]) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_reverse(lim=c(50,0), expand = c(0, 0)) +
    theme_collapse() +
    coord_equal()
  bl_plot <- ggplot(data=bl, aes(x=Var1, y=Var2, fill=bin)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_manual(values=colour_palette) +
    scale_x_reverse(lim=c(50,0), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_collapse() +
    coord_equal()

  # Prepare the plots for arrangement
  g_tl_plot <- ggplot_gtable(ggplot_build(tl_plot))
  g_tr_plot <- ggplot_gtable(ggplot_build(tr_plot))
  g_br_plot <- ggplot_gtable(ggplot_build(br_plot))
  g_bl_plot <- ggplot_gtable(ggplot_build(bl_plot))

  # Arrange the 4 plots in a square grid
  maxWidth = grid::unit.pmax(g_tl_plot$widths[2:3], g_tr_plot$widths[2:3], g_br_plot$widths[2:3], g_bl_plot$widths[2:3])
  g_tl_plot$widths[2:3] <- as.list(maxWidth)
  g_tr_plot$widths[2:3] <- as.list(maxWidth)
  g_br_plot$widths[2:3] <- as.list(maxWidth)
  g_bl_plot$widths[2:3] <- as.list(maxWidth)
  grid.arrange(g_tl_plot, g_tr_plot, g_bl_plot, g_br_plot, ncol=2)

}

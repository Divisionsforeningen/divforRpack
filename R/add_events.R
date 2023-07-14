add_events <- function(df = NA, x = NA, y = NA, xend = NA, yend = NA,
                       heatmap = FALSE, shot = FALSE, lines = FALSE,
                       eventArgs = list(),
                       shotArgs = list(),
                       heatmapArgs = list(),
                       lineArgs = list()) {
  # TODO Add heatmap functionality - start, end or all as argument
  # TODO Make shot map usable for tracking dimensions

  # Define standard values for events, shots and heatmaps

  event_args <- list(color = div_col(type = "highlight"), border = "black")
  shot_args <- list(color = div_col(type = "highlight"), border = "black")
  heatmap_args <- list(alpha = 0.05, color="turbo", type="start", fill="red")
  line_args <- list(linetype = "solid", color = "black")

  # Replace standards with user inputs if any
  shot_args <- modifyList(shot_args, shotArgs[intersect(names(shotArgs), names(shot_args))])
  event_args <- modifyList(event_args, eventArgs[intersect(names(eventArgs), names(event_args))])
  heatmap_args <- modifyList(heatmap_args, heatmapArgs[intersect(names(heatmapArgs), names(heatmap_args))])
  line_args <- modifyList(line_args, lineArgs[intersect(names(lineArgs), names(line_args))])

  # Create empty geom_point to serve as base
  # p=list(geom_point())
  p <- list(theme_pitch())

  if (heatmap == TRUE) {
    # Throw an error if cmap is not usable
    if (!length(heatmap_args) == 0) {
      if (!is.numeric(heatmap_args[["alpha"]]) || (heatmap_args[["alpha"]] > 1 || heatmap_args[["alpha"]] < 0)) {
        stop("Heatmap alpha is unusable")
      }
    }

    # Creates list of geoms for heatmap
    h <- list(
      stat_density_2d(data = df, aes(x=x,y=y, fill = after_stat(level)),
                      na.rm = T, n=10,
                      geom = "polygon", alpha = heatmap_args[["alpha"]], fill=heatmap_args[["fill"]]),
      theme(legend.position = "none"),
      scale_x_continuous(limits = c(-400,500)),
      scale_y_continuous(limits = c(-400,500))
    )


    # Append heatmap list to output
    p <- append(p, h)
  }

  if (lines == TRUE) {
    # Throw an error if color is not usable
    tryCatch(div_col(color = line_args[["color"]]), error = function(e) stop("Line color not usable"))


    # Creates list with geoms for lines
    l <- list(geom_segment(
      data = df, aes(x = x, y = y, xend = xend, yend = yend),
      color = line_args[["color"]], linetype = line_args[["linetype"]],
      arrow = arrow(length = unit(.25, "cm"))
    ))

    # Append line list to output
    p <- append(p, l)
  }

  if (shot == F) {
    # Throw an error if color is not usable
    tryCatch(div_col(color = event_args[["color"]]), error = function(e) stop("Point color not usable"))

    # Throw an error if border is not usable
    tryCatch(div_col(color = event_args[["border"]]), error = function(e) stop("Border color not usable"))

    # Adds events via geom_point
    e <- list(
      geom_point(
        data = df, aes(x = x, y = y), fill = event_args[["color"]],
        color = event_args[["border"]], pch = 21, size = 5)
    )

    # Append events list to output
    p <- append(p, e)

    # Returns list of ggplot layers
    return(p)
  } else if (shot == TRUE) {
    # Throw an error if color is not usable
    tryCatch(div_col(color = shot_args[["color"]]), error = function(e) stop("Point color not usable"))

    # Throw an error if border is not usable
    tryCatch(div_col(color = shot_args[["border"]]), error = function(e) stop("Border color not usable"))

    # Create list of geoms for shot map
    s <- list(
      geom_point(
        data = df, aes(x = x, y = y), fill = shot_args[["color"]],
        color = shot_args[["border"]], pch = 21, size = 5
      ),
      coord_cartesian(xlim=c(0,100), ylim=c(0,100))
    )

    # Append shot map list to output
    p <- append(p, s)

    # Returns list of ggplot layers
    return(p)
  } else {
    stop("Something went wrong...")
  }
}



# Test


ggplot2::ggplot() +
  annotate_pitch(alpha=0, goals = goals_strip) +
  add_events(
    df = a, x = x, y = y, xend = xend, yend = yend,
    shot = T, heatmap = T, lines = T,
    eventArgs = list(),
    shotArgs = list(color="orange", border=div_col(type="highlight")),
    heatmapArgs = list(fill="blue"),
    lineArgs = list(linetype="dashed")
  )+
  add_events(goals,shotArgs = list(color="red"), heatmap = T, shot=T, lines = T)+
  geom_rect(aes(xmin=100,xmax=105, ymin=0, ymax=100),fill="white")+
  geom_rect(aes(xmin=0,xmax=105, ymin=-50, ymax=0),fill="white")+
  geom_rect(aes(xmin=0,xmax=105, ymin=100, ymax=150),fill="white")+
  geom_rect(aes(xmin=-5,xmax=0, ymin=-50, ymax=150),fill="white")+
  geom_segment(aes(x=100,xend=100,y=0,yend=100))+
  geom_segment(aes(x=0,xend=100.1,y=0,yend=0))+
  geom_segment(aes(x=0,xend=0,y=0,yend=100))

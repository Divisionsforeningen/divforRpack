#' add_events
#'
#' Add events to pitch plot - does not plot the pitch itself
#'
#' @param df Data frame
#' @param x X coordinate
#' @param y Y coordinate
#' @param xend X end coordinate
#' @param yend Y end coordinate
#' @param heatmap Set to "TRUE" to add heatmap based on x,y
#' @param shot Set to "TRUE" to switch to shot plot
#' @param lines Set to "TRUE" to add lines from x,y to xend,yend
#' @param bgCol Background color - set to shiny theme
#' @param shirt Column with shirt numbers - will be added to events
#' @param eventArgs Event arguments, usable: color and outcome (0 or 1)
#' @param shotArgs Shot arguments, usable: color and outcome ("goal" and "other")
#' @param heatmapArgs Heat map arguments: usable alpha, color and fill
#' @param lineArgs Lines arguments: usable linetype and color
#'
#' @return Returns set of ggplot layers to add to a ggplot
#' @export
#'
add_events <- function(df = NA, x = NA, y = NA, xend = NA, yend = NA,
                       heatmap = FALSE, shot = FALSE, lines = FALSE,
                       bgCol = "white",
                       shirt = NA,
                       eventArgs = list(),
                       shotArgs = list(),
                       heatmapArgs = list(),
                       lineArgs = list()) {
  # TODO Add heatmap functionality - start, end or all as argument
  # TODO Make shot map usable for tracking dimensions

  # Define standard values for events, shots and heatmaps

  event_args <- list(color = c(div_col(type = "highlight"), div_col("goal")), border = "black", outcome = NA)
  shot_args <- list(color = c(div_col(type = "goal"), div_col(type = "fill")), border = "black", outcome = NA)
  heatmap_args <- list(alpha = 0.1, fill = "red")
  line_args <- list(linetype = "solid", color = "black")

  # Replace standards with user inputs if any
  shot_args <- modifyList(shot_args, shotArgs[intersect(names(shotArgs), names(shot_args))])
  event_args <- modifyList(event_args, eventArgs[intersect(names(eventArgs), names(event_args))])
  heatmap_args <- modifyList(heatmap_args, heatmapArgs[intersect(names(heatmapArgs), names(heatmap_args))])
  line_args <- modifyList(line_args, lineArgs[intersect(names(lineArgs), names(line_args))])

  if (!is.na(shot_args[["outcome"]]) && length(shot_args[["color"]]) < 2) {
    h <- list(div_col(type = "fill"))
    shot_args[["color"]] <- append(shot_args[["color"]], h)
  }

  if (!is.na(event_args[["outcome"]]) && length(event_args[["color"]]) < 2) {
    h <- list(event_col(type = "fill"))
    event_args[["color"]] <- append(event_args[["color"]], h)
  }

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
      stat_density_2d(
        data = df, aes(x = .data[[x]], y = .data[[y]], fill = after_stat(level)),
        na.rm = T,
        geom = "polygon", alpha = heatmap_args[["alpha"]], fill = heatmap_args[["fill"]]
      ),
      theme(legend.position = "none"),
      scale_x_continuous(limits = c(-400, 500)),
      scale_y_continuous(limits = c(-400, 500)),
      geom_rect(aes(xmin = 100, xmax = 105, ymin = 0, ymax = 100), fill = bgCol),
      geom_rect(aes(xmin = 0, xmax = 105, ymin = -50, ymax = 0), fill = bgCol),
      geom_rect(aes(xmin = 0, xmax = 105, ymin = 100, ymax = 150), fill = bgCol),
      geom_rect(aes(xmin = -5, xmax = 0, ymin = -50, ymax = 150), fill = bgCol),
      geom_segment(aes(x = 100, xend = 100, y = 0, yend = 100)),
      geom_segment(aes(x = 0, xend = 100.01, y = 0, yend = 0)),
      geom_segment(aes(x = 0, xend = 0, y = 0, yend = 100)),
      geom_segment(aes(x = 0, xend = 100.01, y = 100, yend = 100))
    )

    # Append heatmap list to output
    p <- append(p, h)

    dim <- list(
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
    )

    p <- append(p, dim)
  }

  if (lines == TRUE) {
    # Throw an error if color is not usable
    tryCatch(div_col(color = line_args[["color"]]), error = function(e) stop("Line color not usable"))


    # Creates list with geoms for lines
    l <- list(geom_segment(
      data = df, aes(x = .data[[x]], y = .data[[y]], xend = .data[[xend]], yend = .data[[yend]]),
      color = line_args[["color"]], linetype = line_args[["linetype"]],
      arrow = arrow(length = unit(.25, "cm"))
    ))

    # Append line list to output
    p <- append(p, l)
  }

  if (shot == F) {
    if (length(shot_args[["color"]]) > 1) {
      # Throw an error if color is not usable
      for (i in length(event_args[["color"]])) {
        tryCatch(div_col(color = event_args[["color"]][i]), error = function(e) stop("Point color not usable"))
      }
    }
    if (length(event_args[["border"]]) > 1) {
      # Throw an error if border is not usable
      for (i in length(event_args[["border"]])) {
        tryCatch(div_col(color = event_args[["border"]][i]), error = function(e) stop("Border color not usable"))
      }
    }

    if (is.na(event_args[["outcome"]])) {
      # Adds events via geom_point
      e <- list(
        geom_point(
          data = df, aes(x = .data[[x]], y = .data[[y]]), color = event_args[["border"]][1], shape = 19, size = 6
        ),
        geom_point(
          data = df, aes(x = .data[[x]], y = .data[[y]]), color = event_args[["color"]][1], shape = 19, size = 4
        ),
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
      )
    } else {
      # Adds events via geom_point
      e <- list(
        geom_point(
          data = df %>% filter(.data[[event_args[["outcome"]]]] == 1),
          aes(x = .data[[x]], y = .data[[y]]), color = event_args[["border"]][1], pch = 19, size = 6
        ),
        geom_point(
          data = df %>% filter(.data[[event_args[["outcome"]]]] == 1),
          aes(x = .data[[x]], y = .data[[y]]), color = event_args[["color"]][1], pch = 19, size = 4
        ),
        geom_point(
          data = df %>% filter(.data[[event_args[["outcome"]]]] == 0),
          aes(x = .data[[x]], y = .data[[y]]), color = event_args[["border"]][1], pch = 19, size = 6
        ),
        geom_point(
          data = df %>% filter(.data[[event_args[["outcome"]]]] == 0),
          aes(x = .data[[x]], y = .data[[y]]), color = event_args[["color"]][2], pch = 19, size = 4
        )
      )
    }



    # Append events list to output
    p <- append(p, e)

    if (!is.na(shirt)) {
      # Create list of geoms for shirt numbers
      j <- list(
        geom_text(data = df, aes(x = .data[[x]], y = .data[[y]], label = shirt), color = "white", size = 3)
      )

      # Append shirt numbers to output

      p <- append(p, j)
    }

    # Returns list of ggplot layers
    return(p)
  } else if (shot == TRUE) {
    if (length(shot_args[["color"]]) > 1) {
      # Throw an error if color is not usable
      for (i in length(shot_args[["color"]])) {
        tryCatch(div_col(color = shot_args[["color"]][i]), error = function(e) stop("Point color not usable"))
      }
    }
    if (length(shot_args[["border"]]) > 1) {
      # Throw an error if border is not usable
      for (i in length(shot_args[["border"]])) {
        tryCatch(div_col(color = shot_args[["border"]][i]), error = function(e) stop("Border color not usable"))
      }
    }


    if (is.na(shot_args[["outcome"]])) {
      # Create list of geoms for shot map
      s <- list(
        geom_point(
          data = df, aes(x = .data[[x]], y = .data[[y]]), color = shot_args[["border"]][1], pch = 19, size = 6
        ),
        geom_point(
          data = df, aes(x = .data[[x]], y = .data[[y]]), color = shot_args[["color"]][1], pch = 19, size = 4
        )
      )
    } else {
      # Create list of geoms for shot map
      s <- list(
        geom_point(
          data = df %>% filter(tolower(.data[[shot_args[["outcome"]]]]) == "goal"),
          aes(x = .data[[x]], y = .data[[y]]), color = shot_args[["border"]][1], pch = 19, size = 6
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[shot_args[["outcome"]]]]) == "goal"),
          aes(x = .data[[x]], y = .data[[y]]), color = shot_args[["color"]][1], pch = 19, size = 4
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[shot_args[["outcome"]]]]) != "goal"),
          aes(x = .data[[x]], y = .data[[y]]), color = shot_args[["border"]][1], pch = 19, size = 6
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[shot_args[["outcome"]]]]) != "goal"),
          aes(x = .data[[x]], y = .data[[y]]), color = shot_args[["color"]][2], pch = 19, size = 4
        )
      )
    }

    # Create rest of needed shot geoms
    sh <- list(
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)),
      coord_flip(xlim = c(49, 101)),
      scale_y_reverse()
    )

    # Append to shot map
    s <- append(s, sh)

    # Append shot map list to output
    p <- append(p, s)

    if (!is.na(shirt)) {
      # Create list of geoms for shirt numbers
      j <- list(
        geom_text(data = df, aes(x = .data[[x]], y = .data[[y]], label = shirt), color = "white", size = 3)
      )

      # Append shirt numbers to output

      p <- append(p, j)
    }

    # Returns list of ggplot layers
    return(p)
  } else {
    stop("Something went wrong...")
  }
}

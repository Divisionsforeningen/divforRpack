#' Add Events to Pitch Plot
#'
#' Adds events to a pitch plot without plotting the pitch itself.
#'
#' @param df A data frame containing event data.
#' @param x Column with X coordinates.
#' @param y Column with Y coordinates.
#' @param xend Column with X end coordinates.
#' @param yend Column with Y end coordinates.
#' @param heatmap Set to TRUE to add a heatmap based on x, y.
#' @param shot Set to TRUE to switch to a shot plot.
#' @param corners Set to TRUE to switch to a corner plot.
#' @param lines Set to TRUE to add lines from x, y to xend, yend.
#' @param passZones Set to TRUE to add counts for passes to each zone.
#' @param bgCol Background color - set according to the Shiny theme.
#' @param shirt Column with shirt numbers - will be added to events.
#' @param eventArgs Event arguments, including color and outcome column (0 or 1).
#' @param shotArgs Shot arguments, including color and outcome column ("goal" or "other").
#' @param heatmapArgs Heat map arguments, including alpha, color, fill, and type ("start" or "end").
#' @param lineArgs Line arguments, including linetype and color.
#' @param cornerArgs Use for Opta tags - arguments: colors and type column name.
#' @param size Point size - defaults to 4, minimum is 3.
#' @param provider Name of the data provider.
#' @param pmCol Pitch marking colors for heatmap use.
#' @param textCol Text color.
#' @param title Title of the plot.
#' @param explanation Explanation of the chart.
#'
#' @return A set of ggplot layers to add to a ggplot.
#'
#' @export
#'
#' @examples
#' x <- 100
#' df <- data.frame(
#'   x = runif(x, 20, 75),
#'   y = runif(x, 10, 90),
#'   xend = runif(x, 40, 99),
#'   yend = runif(x, 10, 90),
#'   outcome = round(runif(x), 0)
#' )
#'
#'
#' p <- ggplot() +
#'   annotate_pitch() +
#'   add_events(
#'     df = df, x = "x", y = "y", xend = "xend", yend = "yend",
#'     provider = "Wyscout",
#'     lines = TRUE, passZones = FALSE, textCol = "black", bgCol = "white",
#'     corners = FALSE, cornerArgs = list(),
#'     eventArgs = list(outcome = "outcome"),
#'     shot = FALSE, shotArgs = list(),
#'     heatmap = TRUE, heatmapArgs = list(type = "end", outcome = "outcome")
#'   ) +
#'   theme_pitch()
#'
#' p
#'
add_events <- function(df = NA, x = NA, y = NA, xend = NA, yend = NA,
                       provider = NA, size = 4,
                       heatmap = FALSE, shot = FALSE, corners = FALSE,
                       lines = FALSE, passZones = FALSE, pmCol = "black",
                       bgCol = "white", textCol = "black",
                       title = "Put title here", explanation = "Put explanation here",
                       shirt = NA,
                       eventArgs = list(),
                       shotArgs = list(),
                       cornerArgs = list(),
                       heatmapArgs = list(),
                       lineArgs = list()) {}
  # Comments ----------------------------------------------------------------


  # TODO Add heatmap functionality - start, end or all as argument
  # TODO Make shot map usable for tracking dimensions

  # Define standard values for events, shots and heatmaps

  # Wyscout adjustment ------------------------------------------------------


  if (tolower(provider) == "wyscout" && !is.na(y)) {
    df <- df %>% mutate(yp = abs(.data[[y]] - 100))
  }

  if (tolower(provider) == "wyscout" && !is.na(yend)) {
    df <- df %>% mutate(ypend = abs(.data[[yend]] - 100))
  }

  if (tolower(provider) != "wyscout" && !is.na(y)) {
    df <- df %>% mutate(yp = .data[[y]])
  }

  if (tolower(provider) != "wyscout" && !is.na(yend)) {
    df <- df %>% mutate(ypend = .data[[yend]])
  }

  # Setup -------------------------------------------------------------------

  # Sizes for points
  if (!is.numeric(size)) {
    stop("Size is not numeric")
  }
  ss <- max(size, 1)
  ls <- ifelse(ss > 2, ss + 2, ss)


  event_args <- list(color = c(div_col(type = "highlight"), div_col("goal")), border = "black", outcome = NA)
  shot_args <- list(color = c(div_col(type = "goal"), div_col(type = "fill")), border = "black", outcome = NA)
  corner_args <- list(color = c(div_col(color = "orange"), div_col(color = "forestgreen"), div_col(color = "red"), div_col(color = "lightgrey")), border = "black", type = NA)
  heatmap_args <- list(alpha = 0.1, fill = "red", type = "start", outcome = NA)
  line_args <- list(linetype = c("solid", "dashed"), color = "black", direction = "Last", alpha = 0.5)

  # Replace standards with user inputs if any
  shot_args <- modifyList(shot_args, shotArgs[intersect(names(shotArgs), names(shot_args))])
  event_args <- modifyList(event_args, eventArgs[intersect(names(eventArgs), names(event_args))])
  corner_args <- modifyList(corner_args, cornerArgs[intersect(names(cornerArgs), names(corner_args))])
  heatmap_args <- modifyList(heatmap_args, heatmapArgs[intersect(names(heatmapArgs), names(heatmap_args))])
  line_args <- modifyList(line_args, lineArgs[intersect(names(lineArgs), names(line_args))])

  if (!is.na(shot_args[["outcome"]]) && length(shot_args[["color"]]) < 2) {
    h <- list(div_col(type = "fill"))
    shot_args[["color"]] <- append(shot_args[["color"]], h)
  }

  if (!is.na(event_args[["outcome"]]) && length(event_args[["color"]]) < 2) {
    h <- list(div_col(type = "fill"))
    event_args[["color"]] <- append(event_args[["color"]], h)
  }

  if (!is.na(event_args[["outcome"]]) && lines == T && length(line_args[["linetype"]]) < 2) {
    h <- list("dashed", "dotted")
    line_args[["linetype"]] <- append(line_args[["linetype"]], h)
  }

  if (!is.na(corner_args[["type"]]) && length(corner_args[["color"]]) < 3) {
    h <- list(div_col(color = "purple"), div_col(color = "yellow"))
    coner_args[["color"]] <- append(corner_args[["color"]], h)
  }


  # Checks on input ---------------------------------------------------------

  # TODO Implement test on [["border"]] input

  # If corners=T then set heatmap type to end
  if (corners == T) {
    heatmap_args[["type"]] <- "end"
  }

  # If pmCol is not a color throw an error
  tryCatch(
    expr = {
      div_col(color = pmCol)
    },
    error = function(e) {
      stop("pmCol is not a usable color!")
    }
  )

  # Init plot ---------------------------------------------------------------

  # Create empty geom_point to serve as base
  p <- list(theme_pitch())


  # Heatmap -----------------------------------------------------------------

  if (heatmap == TRUE) {
    # Throw an error if cmap is not usable
    if (!length(heatmap_args) == 0) {
      if (!is.numeric(heatmap_args[["alpha"]]) || (heatmap_args[["alpha"]] > 1 || heatmap_args[["alpha"]] < 0)) {
        stop("Heatmap alpha is unusable")
      }
    }

    if (heatmap_args[["type"]] == "end") {
      if (is.na(heatmap_args[["outcome"]])) {
        h <- list(
          stat_density_2d(
            data = df, aes(x = .data[[xend]], y = ypend, fill = after_stat(nlevel)),
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
          geom_segment(aes(x = 100, xend = 100, y = 0, yend = 100), color = pmCol),
          geom_segment(aes(x = 0, xend = 100.01, y = 0, yend = 0), color = pmCol),
          geom_segment(aes(x = 0, xend = 0, y = 0, yend = 100), color = pmCol),
          geom_segment(aes(x = 0, xend = 100.01, y = 100, yend = 100), color = pmCol)
        )
      } else {
        # Creates list of geoms for heatmap
        h <- list(
          stat_density_2d(
            data = df %>% filter(.data[[heatmap_args[["outcome"]]]] == 0), aes(x = .data[[xend]], y = ypend, fill = after_stat(level)),
            na.rm = T,
            geom = "polygon", alpha = heatmap_args[["alpha"]], fill = event_args[["color"]][2]
          ),
          stat_density_2d(
            data = df %>% filter(.data[[heatmap_args[["outcome"]]]] == 1), aes(x = .data[[xend]], y = ypend, fill = after_stat(level)),
            na.rm = T,
            geom = "polygon", alpha = heatmap_args[["alpha"]], fill = event_args[["color"]][1]
          ),
          theme(legend.position = "none"),
          scale_x_continuous(limits = c(-400, 500)),
          scale_y_continuous(limits = c(-400, 500)),
          geom_rect(aes(xmin = 100, xmax = 105, ymin = 0, ymax = 100), fill = bgCol),
          geom_rect(aes(xmin = 0, xmax = 105, ymin = -50, ymax = 0), fill = bgCol),
          geom_rect(aes(xmin = 0, xmax = 105, ymin = 100, ymax = 150), fill = bgCol),
          geom_rect(aes(xmin = -5, xmax = 0, ymin = -50, ymax = 150), fill = bgCol),
          geom_segment(aes(x = 100, xend = 100, y = 0, yend = 100), color = pmCol),
          geom_segment(aes(x = 0, xend = 100.01, y = 0, yend = 0), color = pmCol),
          geom_segment(aes(x = 0, xend = 0, y = 0, yend = 100), color = pmCol),
          geom_segment(aes(x = 0, xend = 100.01, y = 100, yend = 100), color = pmCol)
        )
      }

      # Append heatmap list to output
      p <- append(p, h)

      dim <- list(
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
      )
    } else {
      # Creates list of geoms for heatmap
      if (is.na(heatmap_args[["outcome"]])) {
        h <- list(
          stat_density_2d(
            data = df, aes(x = .data[[x]], y = yp, fill = after_stat(level)),
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
          geom_segment(aes(x = 100, xend = 100, y = 0, yend = 100), color = pmCol),
          geom_segment(aes(x = 0, xend = 100.01, y = 0, yend = 0), color = pmCol),
          geom_segment(aes(x = 0, xend = 0, y = 0, yend = 100), color = pmCol),
          geom_segment(aes(x = 0, xend = 100.01, y = 100, yend = 100), color = pmCol)
        )
      } else {
        # Creates list of geoms for heatmap
        h <- list(
          stat_density_2d(
            data = df %>% filter(.data[[heatmap_args[["outcome"]]]] == 0), aes(x = .data[[x]], y = yp, fill = after_stat(level)),
            na.rm = T,
            geom = "polygon", alpha = heatmap_args[["alpha"]], fill = event_args[["color"]][2]
          ),
          stat_density_2d(
            data = df %>% filter(.data[[heatmap_args[["outcome"]]]] == 1), aes(x = .data[[x]], y = yp, fill = after_stat(level)),
            na.rm = T,
            geom = "polygon", alpha = heatmap_args[["alpha"]], fill = event_args[["color"]][1]
          ),
          theme(legend.position = "none"),
          scale_x_continuous(limits = c(-400, 500)),
          scale_y_continuous(limits = c(-400, 500)),
          geom_rect(aes(xmin = 100, xmax = 105, ymin = 0, ymax = 100), fill = bgCol),
          geom_rect(aes(xmin = 0, xmax = 105, ymin = -50, ymax = 0), fill = bgCol),
          geom_rect(aes(xmin = 0, xmax = 105, ymin = 100, ymax = 150), fill = bgCol),
          geom_rect(aes(xmin = -5, xmax = 0, ymin = -50, ymax = 150), fill = bgCol),
          geom_segment(aes(x = 100, xend = 100, y = 0, yend = 100), color = pmCol),
          geom_segment(aes(x = 0, xend = 100.01, y = 0, yend = 0), color = pmCol),
          geom_segment(aes(x = 0, xend = 0, y = 0, yend = 100), color = pmCol),
          geom_segment(aes(x = 0, xend = 100.01, y = 100, yend = 100), color = pmCol)
        )
      }

      # Append heatmap list to output
      p <- append(p, h)

      dim <- list(
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
      )
    }

    p <- append(p, dim)
  }

  # Lines -------------------------------------------------------------------

  if (lines == TRUE) {
    # Throw an error if color is not usable
    tryCatch(div_col(color = line_args[["color"]]), error = function(e) stop("Line color not usable"))

    if (!is.na(event_args[["outcome"]])) {
      # Creates list with geoms for lines
      l <- list(
        geom_segment(
          data = df %>% filter(.data[[event_args[["outcome"]]]] == 1),
          aes(x = .data[[x]], y = yp, xend = .data[[xend]], yend = ypend),
          color = line_args[["color"]][1], linetype = line_args[["linetype"]][1],
          arrow = arrow(length = unit(.25, "cm"))
        ),
        geom_segment(
          data = df %>% filter(.data[[event_args[["outcome"]]]] == 0),
          aes(x = .data[[x]], y = yp, xend = .data[[xend]], yend = ypend),
          color = line_args[["color"]][1], linetype = line_args[["linetype"]][2],
          arrow = arrow(length = unit(.25, "cm"))
        )
      )

      # Append line list to output
      p <- append(p, l)
    } else if (corners == TRUE && !is.na(corner_args[["type"]])) {
      # Creates list with geoms for lines
      l <- list(
        #
        geom_segment(
          data = df %>% filter(tolower(.data[[corner_args[["type"]]]]) == "in-swinger"),
          aes(x = .data[[x]], y = yp, xend = .data[[xend]], yend = ypend), linewidth = 1,
          color = corner_args[["color"]][1], linetype = line_args[["linetype"]][1],
          arrow = arrow(length = unit(.25, "cm"), type = "open")
        ),
        geom_segment(
          data = df %>% filter(tolower(.data[[corner_args[["type"]]]]) == "out-swinger"),
          aes(x = .data[[x]], y = yp, xend = .data[[xend]], yend = ypend), linewidth = 1,
          color = corner_args[["color"]][2], linetype = line_args[["linetype"]][1],
          arrow = arrow(length = unit(.25, "cm"), type = "open")
        ),
        geom_segment(
          data = df %>% filter(tolower(.data[[corner_args[["type"]]]]) %nin% c("in-swinger", "out-swinger")),
          aes(x = .data[[x]], y = yp, xend = .data[[xend]], yend = ypend), linewidth = 1,
          color = corner_args[["color"]][3], linetype = line_args[["linetype"]][1],
          arrow = arrow(length = unit(.25, "cm"), type = "open")
        ),
        annotate("label",
          x = 55, y = c(25, 50, 75), label = c("in-swinger", "other", "out-swinger"),
          fill = c(corner_args[["color"]][1], corner_args[["color"]][3], corner_args[["color"]][2]),
          size = 4
        )
      )

      # Append line list to output
      p <- append(p, l)
    } else {
      # Creates list with geoms for lines
      l <- list(geom_segment(
        data = df, aes(x = .data[[x]], y = yp, xend = .data[[xend]], yend = ypend),
        color = line_args[["color"]], linetype = line_args[["linetype"]][1],
        arrow = arrow(length = unit(.25, "cm"))
      ))

      # Append line list to output
      p <- append(p, l)
    }
  }


  # Events ------------------------------------------------------------------

  if (shot == F && corners == F) {
    if (length(event_args[["color"]]) > 1) {
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
          data = df, aes(x = .data[[x]], y = yp), color = event_args[["border"]][1], shape = 19, size = ls
        ),
        geom_point(
          data = df, aes(x = .data[[x]], y = yp), color = event_args[["color"]][1], shape = 19, size = ss
        )
      )
    } else {
      # Adds events via geom_point
      e <- list(
        geom_point(
          data = df %>% filter(.data[[event_args[["outcome"]]]] == 1),
          aes(x = .data[[x]], y = yp), color = event_args[["border"]][1], pch = 19, size = ls
        ),
        geom_point(
          data = df %>% filter(.data[[event_args[["outcome"]]]] == 1),
          aes(x = .data[[x]], y = yp), color = event_args[["color"]][1], pch = 19, size = ss
        ),
        geom_point(
          data = df %>% filter(.data[[event_args[["outcome"]]]] == 0),
          aes(x = .data[[x]], y = yp), color = event_args[["border"]][1], pch = 19, size = ls
        ),
        geom_point(
          data = df %>% filter(.data[[event_args[["outcome"]]]] == 0),
          aes(x = .data[[x]], y = yp), color = event_args[["color"]][2], pch = 19, size = ss
        )
      )
    }

    # Append events list to output
    p <- append(p, e)

    if (!is.na(shirt)) {
      # Create list of geoms for shirt numbers
      j <- list(
        geom_text(data = df, aes(x = .data[[x]], y = yp, label = .data[[shirt]]), color = ifelse(is.na(textCol), div_col(type = "b_text"), div_col(color = textCol)), size = 3)
      )

      # Append shirt numbers to output

      p <- append(p, j)
    }

    # Pass zones --------------------------------------------------------------
    if (passZones == T) {
      # Calculated zone values
      leftlow <- df %>%
        filter(ypend > 63.2 & .data[[xend]] < 50) %>%
        summarise(sum(n()))

      centerlow <- df %>%
        filter(ypend <= 63.2 & ypend >= 36.8 & .data[[xend]] < 50) %>%
        summarise(sum(n()))

      rightlow <- df %>%
        filter(ypend < 36.8 & .data[[xend]] < 50) %>%
        summarise(sum(n()))

      lefthigh <- df %>%
        filter(ypend > 63.2 & .data[[xend]] >= 50) %>%
        summarise(sum(n()))

      centerhigh <- df %>%
        filter(ypend <= 63.2 & ypend >= 36.8 & .data[[xend]] >= 50) %>%
        summarise(sum(n()))

      righthigh <- df %>%
        filter(ypend < 36.8 & .data[[xend]] >= 50) %>%
        summarise(sum(n()))

      # Create pass zone text
      t <- list(
        annotate("text", x = 25, y = 80, label = leftlow, size = 7, color = ifelse(is.na(textCol), div_col(type = "b_text"), div_col(color = textCol))),
        annotate("text", x = 25, y = 50, label = centerlow, size = 7, color = ifelse(is.na(textCol), div_col(type = "b_text"), div_col(color = textCol))),
        annotate("text", x = 25, y = 20, label = rightlow, size = 7, color = ifelse(is.na(textCol), div_col(type = "b_text"), div_col(color = textCol))),
        annotate("text", x = 75, y = 80, label = lefthigh, size = 7, color = ifelse(is.na(textCol), div_col(type = "b_text"), div_col(color = textCol))),
        annotate("text", x = 75, y = 50, label = centerhigh, size = 7, color = ifelse(is.na(textCol), div_col(type = "b_text"), div_col(color = textCol))),
        annotate("text", x = 75, y = 20, label = righthigh, size = 7, color = ifelse(is.na(textCol), div_col(type = "b_text"), div_col(color = textCol))),
        theme(text = element_text(family = "sans"))
      )

      # Append corner list to output
      p <- append(p, t)
    }

    # Returns list of ggplot layers
  }

  # Shots -------------------------------------------------------------------

  else if (shot == TRUE) {
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
          data = df, aes(x = .data[[x]], y = yp), color = shot_args[["border"]][1], pch = 19, size = ls
        ),
        geom_point(
          data = df, aes(x = .data[[x]], y = yp), color = shot_args[["color"]][1], pch = 19, size = ss
        )
      )
    } else {
      # Create list of geoms for shot map
      s <- list(
        geom_point(
          data = df %>% filter(tolower(.data[[shot_args[["outcome"]]]]) != "goal"),
          aes(x = .data[[x]], y = yp), color = shot_args[["border"]][1], pch = 19, size = ls
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[shot_args[["outcome"]]]]) != "goal"),
          aes(x = .data[[x]], y = yp), color = shot_args[["color"]][2], pch = 19, size = ss
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[shot_args[["outcome"]]]]) == "goal"),
          aes(x = .data[[x]], y = yp), color = shot_args[["border"]][1], pch = 19, size = ls
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[shot_args[["outcome"]]]]) == "goal"),
          aes(x = .data[[x]], y = yp), color = shot_args[["color"]][1], pch = 19, size = ss
        )
      )
    }

    # Create rest of needed shot geoms
    sh <- list(
      coord_flip(xlim = c(50, 100), ylim = c(100, 0))
    )

    # Append to shot map
    s <- append(s, sh)

    # Append shot map list to output
    p <- append(p, s)

    if (!is.na(shirt)) {
      # Create list of geoms for shirt numbers
      j <- list(
        geom_text(data = df, aes(x = .data[[x]], y = yp, label = .data[[shirt]]), color = ifelse(is.na(textCol), div_col(type = "b_text"), div_col(color = textCol)), size = 3)
      )

      # Append shirt numbers to output

      p <- append(p, j)
    }
  }

  # Corners -----------------------------------------------------------------

  else if (corners == TRUE) {
    if (is.na(corner_args[["type"]])) {
      # Create list of geoms for corner map
      c <- list(
        geom_point(
          data = df, aes(x = .data[[xend]], y = ypend), color = corner_args[["border"]][1], pch = 19, size = ls
        ),
        geom_point(
          data = df, aes(x = .data[[xend]], y = ypend), color = corner_args[["color"]][4], pch = 19, size = ss
        )
      )
    } else {
      # Create list of geoms for corner map
      c <- list(
        geom_point(
          data = df %>% filter(tolower(.data[[corner_args[["type"]]]]) == "in-swinger"),
          aes(x = .data[[xend]], y = ypend), color = corner_args[["border"]][1], pch = 19, size = ls
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[corner_args[["type"]]]]) == "in-swinger"),
          aes(x = .data[[xend]], y = ypend), color = corner_args[["color"]][1], pch = 19, size = ss
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[corner_args[["type"]]]]) == "out-swinger"),
          aes(x = .data[[xend]], y = ypend), color = corner_args[["border"]][1], pch = 19, size = ls
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[corner_args[["type"]]]]) == "out-swinger"),
          aes(x = .data[[xend]], y = ypend), color = corner_args[["color"]][2], pch = 19, size = ss
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[corner_args[["type"]]]]) %nin% c("in-swinger", "out-swinger")),
          aes(x = .data[[xend]], y = ypend), color = corner_args[["border"]][1], pch = 19, size = ls
        ),
        geom_point(
          data = df %>% filter(tolower(.data[[corner_args[["type"]]]]) %nin% c("in-swinger", "out-swinger")),
          aes(x = .data[[xend]], y = ypend), color = corner_args[["color"]][3], pch = 19, size = ss
        )
      )
    }

    # Create rest of needed corner geoms

    # if(tolower(provider)=="wyscout"){
    #   ch <- list(
    #     coord_flip(xlim = c(50,100), ylim=c(0,100))
    #   )
    # }
    #
    # if(tolower(provider)!="wyscout"){
    #   ch <- list(
    #     coord_flip(xlim = c(50,100), ylim=c(100,0))
    #   )
    # }
    ch <- list(
      coord_flip(xlim = c(50, 100), ylim = c(100, 0))
    )


    # Append to corner
    c <- append(c, ch)

    # Append corner list to output
    p <- append(p, c)
  } else {
    stop("Something went wrong...")
  }

  cap <- list(
    labs(
      title = title,
      subtitle = explanation,
      caption = paste0("Data from ", provider)
    )
  )

  p <- append(p, cap)

  return(p)
}

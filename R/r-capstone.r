#
# "Mastering Software Development in R" Capstone project
# Sergey Kolchin, 06.2018
#
#
library(readr)
library(dplyr)
library(ggplot2)
library(grid)
library(leaflet)

#' Data loading function
#' The function loads the raw NOAA data from a given file to a dataframe

#' @importFrom readr read_tsv
#'
#' @param filename  A name of the source file
#'
#' @return Raw dataset
#'
#' @examples
#' \dontrun{
#'   data <- eq_load_data("signif.txt")
#' }
#'
#' @export
eq_load_data <- function (filename) {
  readr::read_tsv(filename)
}

#' Data cleaning function
#' The function  takes raw NOAA data frame and returns a clean data frame.
#'
#' @param raw_data The raw NOAA dataset (data frame)
#'
#' @importFrom dplyr mutate
#'
#' @return Clean dataset
#'
#' @examples
#' \dontrun{
#'   clean_data <- eq_clean_data(raw_data)
#' }
#'
#' @export
eq_clean_data <- function(raw_data) {

  # Merge year, month, day to a Date-class DATE variable
  # DAY and MONTH might contain NAs, set them to 01 jan
  # A DATE_C column is added presenting whether the YEAR is less than 0 (BC) or greater than 0 (AD)
  # Latitude and longitude are converted to numeric
  raw_data %>%
    dplyr::mutate(
      DAY = ifelse(is.na(DAY), 1, DAY),
      MONTH = ifelse(is.na(MONTH), 1, MONTH),
      DATE = as.Date(
        paste0(
          sprintf("%02d", MONTH), "/",
          sprintf("%02d", DAY), "/",
          sprintf("%04d", YEAR)),
        format = ifelse(YEAR < 0, "%m/%d/-%Y", "%m/%d/%Y")),
      DATE_C = ifelse(YEAR < 0, "BC", "AD"),
      LATITUDE = as.numeric(LATITUDE),
      LONGITUDE = as.numeric(LONGITUDE)
    )
}

#' Location cleaning function
#' The function takes NOAA data and cleans the LOCATION_NAME column
#' by stripping out the country name (including the colon) and
#' converting names to title case (as opposed to all caps)
#'
#' @importFrom dplyr mutate
#'
#' @param raw_data The raw NOAA dataset (as data frame)
#'
#' @return Dataset with cleaned LOCATION_NAME column
#'
#' @examples
#' \dontrun{
#'   clean_data <- eq_location_clean(raw_data)
#' }
#'
#' @export
eq_location_clean = function(raw_data) {

  # Internal function to make title case
  title_case <- function(x){
    if(length(x) > 0) {
      paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
    }
  }

  raw_data %>%
    dplyr::mutate(
      LOCATION_NAME = title_case(
        trimws(
          gsub("^.*?:", "", LOCATION_NAME)
          )
      )
    )
}


#' The GeomTimeline prototype object
#' This function will plot a timeline  of earthquakes ranging from xmin to xmax
#' with a point for each earthquake
#
#' @importFrom ggplot2 ggproto alpha aes
#' @importFrom grid circleGrob gpar unit gList linesGrob
#'
#' @examples
#' \dontrun{
#'   geom_timeline(data = data, aes(x = DATE, colour = DEATHS))
#' }
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
                                  required_aes = c("x"),
                                  default_aes = ggplot2::aes(y = 0.1, colour = 1, alpha = 1, fill = 1, size = 1),
                                  draw_key = function(data, params,size) {
                                    # Custom function to draw the legend
                                    grid::circleGrob(
                                      x = 0.5,
                                      y = 0.5,
                                      r = grid::unit(data$size * 0.1, "char"),
                                      gp = grid::gpar(
                                        col = ggplot2::alpha(data$colour, data$alpha),
                                        fill = ggplot2::alpha(data$fill, data$alpha)
                                      )
                                    )
                                  },
                                  draw_group = function(data, panel_scales, coord) {

                                    # Transform coords
                                    coords <- coord$transform(data, panel_scales)

                                    # Make a list of grobs
                                    grid::gList(
                                      # A line crossing through the points
                                      grid::linesGrob(
                                        x = coords$x,
                                        y = coords$y,
                                        gp = grid::gpar(
                                          col = "gray80",
                                          alpha = 0.8
                                        )
                                      ),
                                      # Actual point
                                      grid::circleGrob(
                                        x = coords$x,
                                        y = coords$y,
                                        r = grid::unit(coords$size * 0.1, "char"),
                                        gp = grid::gpar(
                                          col = ggplot2::alpha(coords$colour, coords$alpha),
                                          fill = ggplot2::alpha(coords$colour, coords$alpha)
                                        )
                                      )
                                    )
                                  }
)

#' Function to build a layer for the geom_timeline proto function
#'
#' @importFrom ggplot2 layer
#'
#' @param mapping Set of aesthetic mappings created by aes or aes_
#' @param data The data to be displayed in this layer
#' @param stat The statistical transformation to use on the data for this layer, as a string
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#' @param ... Any other parameters
#'
#' @examples
#' \dontrun{
#'   geom_timeline(data = data, aes(x = DATE, colour = DEATHS))
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' The GeomTimelineLabel prototype object
#' This function will plot a timeline  of earthquakes ranging from xmin to xmax
#' with a point for each earthquake
#
#' @importFrom ggplot2 ggproto draw_key_blank aes
#' @importFrom dplyr arrange mutate
#' @importFrom utils head
#' @importFrom grid circleGrob gpar unit gList linesGrob
#'
#' @examples
#' \dontrun{
#'   geom_timeline_label(data = data, aes(x = DATE, label = LOCATION))
#' }
#'
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", Geom,
                                 required_aes = c("x", "label"),
                                 draw_key = ggplot2::draw_key_blank,
                                 default_aes = ggplot2::aes(y = 0.1, n_max = NA, magnitude = NA),
                                 draw_group = function(data, panel_scales, coord) {

                                   # Subset data to n_max largest (by magnitude) earthquakes
                                   # Magnitude has to be provided too
                                   n_max <- data[["n_max"]]
                                   if (!is.na(n_max[1])) {
                                     data <- data %>%
                                       dplyr::arrange(desc(magnitude)) %>%
                                       head(n_max[1]) %>%
                                       dplyr::arrange(x)
                                   }

                                   # Calculate a small delta for target line y value based on viewport limits
                                   dy <- (panel_scales$y.range[2] - panel_scales$y.range[1]) / 20

                                   # Make a dataset with source data and ID column
                                   # specifying a group of lines to draw
                                   df_from <- data %>% dplyr::mutate(id = 1:n())

                                   # Make a dataset with target (x,y) values and ID column as well
                                   df_to <- data %>% dplyr::mutate(y = y + dy, id = 1:n())

                                   # Combine the datasets
                                   # polygonGrob uses ID to select pairs of coordinates to draw
                                   df_all <- rbind(df_from, df_to)

                                   # Transform coords
                                   coords_to <- coord$transform(df_to, panel_scales)
                                   coords_all <- coord$transform(df_all, panel_scales)

                                   # If magnitude provided, display it in a label
                                   if (any(!is.na(data$magnitude))) {
                                     coords_to$label <- paste0(
                                       coords_to$label, " (",
                                       coords_to$magnitude,")"
                                       )
                                   }

                                   # Make a list of grobs
                                    grid::gList(
                                      # Text labels
                                      grid::textGrob(
                                        x = coords_to$x,
                                        y = coords_to$y,
                                        label = coords_to$label,
                                        just = "left",
                                        rot = 45,
                                        check.overlap = TRUE
                                      ),

                                      # Lines
                                      grid::polylineGrob(
                                        x = coords_all$x,
                                        y = coords_all$y,
                                        id = coords_all$id,
                                        gp = grid::gpar(
                                          col = "gray80",
                                          alpha = 0.8
                                        )
                                      )
                                   )
                                 }
)

#' Function to build a layer for the geom_timeline_label proto function
#'
#' @importFrom ggplot2 layer
#'
#' @param mapping Set of aesthetic mappings created by aes or aes_
#' @param data The data to be displayed in this layer
#' @param stat The statistical transformation to use on the data for this layer, as a string
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#' @param ... Any other parameters
#'
#' @examples
#' \dontrun{
#'   geom_timeline_label(data = data, aes(x = DATE, label = LOCATION))
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Timeline theme function
#' Derived from ordinary theme to make a timeline look properly on the plot
#' Has to be used with geom_timelime
#'
#' @importFrom ggplot2 theme
#'
#' @param ... Parameters to be passed to the ordinary theme() function
#'
#' @export
theme_timeline <- function(...) {
  ggplot2::theme(...)  %+replace%
  ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(colour = "black"),
    axis.ticks.length = grid::unit(0.2,"cm"),
    legend.position = "bottom",
    legend.key = ggplot2::element_rect(fill = "transparent")
  )
}

#' Eartquake visualisation function
#' The function displays earthquakes on a map with annotation displayed in a popup
#'
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers
#'
#' @param data  Clean NOAA dataset (data.frame)
#' @param annot_col Name of a column to take annotation text from. Default is DATE
#'
#' @examples
#' \dontrun{
#'   eq_map(data = data, annot_col = "DATE")
#' }
#'
#' @export
eq_map <- function(data, annot_col = "DATE") {
  # Make a leaflet
  leaflet::leaflet(data = data) %>%
    leaflet::addProviderTiles("OpenStreetMap.Mapnik") %>%
    leaflet::addCircleMarkers(
      lng = ~ LONGITUDE,
      lat = ~ LATITUDE,
      radius = ~ EQ_PRIMARY,
      popup = ~ data[[annot_col]]
    )
}

#' Make up a label for visualization popup
#' To be used with eq_map() function to make more informative popup labe;
#'
#' @param data  The NOAA dataset
#'
#' @examples
#' \dontrun{
#'   data %>% mutate(popup_text = eq_create_label(.)) %>% eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data) {
  paste0(
    "<b>Location:</b> ", data$LOCATION_NAME, "<br>",
    "<b>Magnitude:</b> ", data$EQ_PRIMARY, "<br>",
    "<b>Total deaths:</b> ", ifelse(is.na(data$TOTAL_DEATHS), "Unknown", data$TOTAL_DEATHS)
  )
}

library(sf)
library(dplyr)


#' Performs a full union
#'
#' @param x An sf object.
#' @param y An sf object.
#' @returns An sf object.
#' @examples
#'library(VHRscope)
#'library(sf)
#'library(dplyr)
#'library(ggplot2)
#'
#'pol1 = st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0))))
#'pol2 = st_polygon(list(rbind(c(2,0), c(4,0), c(4,2), c(2,2), c(2,0))))
#'
#'A <- st_as_sf(st_sfc(pol1,pol2)) %>% mutate(cat = "A")
#'
#'pol3 = st_polygon(list(rbind(c(1,1), c(3,1), c(3,3), c(1,3), c(1,1))))
#'pol4 = st_polygon(list(rbind(c(3,1), c(5,1), c(5,3), c(3,3), c(3,1))))
#'
#'B <- st_as_sf(st_sfc(pol3,pol4)) %>% mutate(cat = "B")
#'
#'ggplot(data = A)+
#'  geom_sf(fill = 'red', alpha = .5)+
#'  geom_sf(data = B, fill = 'blue', alpha = .5)
#'
#'C <- st_full_union(x = A, y = B)
#'
#'ggplot(data = C)+
#'  geom_sf(aes(fill = cat.x, color = cat.y), alpha = .5)
#'
#'
st_full_union <- function(x,y) {
  #
  # function doing a real GIS union operation such as in QGIS or ArcGIS
  #
  # x- the first sf
  # y - the second sf
  #

  x2 <- x
  y2 <- y

  st_agr(x2) = "constant"
  st_agr(y2) = "constant"

  ## make sure no duplicate names in x and y
  colnames_x <- colnames(x2)
  colnames_x <- colnames_x[colnames_x != attr(x2, "sf_column")]
  colnames_y <- colnames(y2)
  colnames_y <- colnames_y[colnames_y != attr(y2, "sf_column")]

  colnames_x2 <- ifelse(colnames_x %in% colnames_y, paste0(colnames_x, '.x'))
  colnames_y2 <- ifelse(colnames_y %in% colnames_x, paste0(colnames_y, '.y'))


  colnames(x2)[colnames(x2) != attr(x2, "sf_column")] <- colnames_x2
  colnames(y2)[colnames(y2) != attr(y2, "sf_column")] <- colnames_y2


  set1 <- st_difference(y2,st_union(x2))
  set2 <- st_difference(x2, st_union(y2))

  set3 <- st_intersection(x2, y2)
  union <- bind_rows(set1, set2, set3)
  return(st_as_sf(union))
}

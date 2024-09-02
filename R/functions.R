

#' Performs a full union as in i.e. QGIS
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
#' @export
st_full_union <- function(x,y) {
  #
  # function doing a real GIS union operation such as in QGIS or ArcGIS
  #
  # x- the first sf
  # y - the second sf
  #

  x2 <- x
  y2 <- y

  sf::st_agr(x2) = "constant"
  sf::st_agr(y2) = "constant"

  ## make sure no duplicate names in x and y
  colnames_x <- colnames(x2)
  colnames_x <- colnames_x[colnames_x != attr(x2, "sf_column")]
  colnames_y <- colnames(y2)
  colnames_y <- colnames_y[colnames_y != attr(y2, "sf_column")]

  colnames_x2 <- ifelse(colnames_x %in% colnames_y, paste0(colnames_x, '.x'),colnames_x)
  colnames_y2 <- ifelse(colnames_y %in% colnames_x, paste0(colnames_y, '.y'),colnames_y)


  colnames(x2)[colnames(x2) != attr(x2, "sf_column")] <- colnames_x2
  colnames(y2)[colnames(y2) != attr(y2, "sf_column")] <- colnames_y2


  set1 <- sf::st_difference(y2,sf::st_union(x2))
  set2 <- sf::st_difference(x2, sf::st_union(y2))

  set3 <- sf::st_intersection(x2, y2)
  union <- dplyr::bind_rows(set1, set2, set3)
  return(st_as_sf(union))
}

#' Create discrete scale for Natura 2000 habitat types
#' @description
#' ggplot2 helper function to create a discrete scale for Natura 2000 habitat types
#'
#' @param palette character to indicate color palette: default 'PZ'.
#' @param legend character to indicate the legend appearance: 'code', 'name' or 'name_code'
#' @export


scale_fill_habitats <- function(palette = 'PZ',legend = 'code', ...){

  habitattypes <- read.csv(file = file.path(system.file(package = "VHRscope"), "extdata", "habitattypes_colors.csv"), sep = ';')

  habitattypes$color[habitattypes$habitattype == 'H0000'] <- rgb(1,1,1, .5)

  if(legend == 'code'){
    ggplot2::scale_fill_manual(values = habitattypes$color,
                      breaks = habitattypes$habitattype,
                      ...)
  } else if (legend == 'name'){
    ggplot2::scale_fill_manual(values = habitattypes$color,
                      breaks = habitattypes$habitattype,
                      labels = habitattypes$description,
                      ...)
  } else if (legend == 'name_code'){
    ggplot2::scale_fill_manual(values = habitattypes$color,
                      breaks = habitattypes$habitattype,
                      labels = paste0(habitattypes$description, ' (',
                                      habitattypes$habitattype, ')'),
                      ...)
  } else {
    stop("For legend argument, choose either 'name', 'code' or 'name_code'")
  }


}


#' Create discrete scale for SNL-beheertype codes
#' @description
#' ggplot2 helper function to create a discrete scale for SNL-beheertype codes
#'
#' @param palette character to indicate color palette: default 'PZ'.
#' @param legend character to indicate the legend appearance: 'code', 'name' or 'name_code'
#' @export

scale_fill_beheertypes <- function(palette = 'PZ',legend = 'code', ...){

  beheertypes <- read.csv(file = file.path(system.file(package = "VHRscope"), "extdata", "snl_beheertypen_colors.csv"), sep = ';')


  if(legend == 'code'){
    ggplot2::scale_fill_manual(values = beheertypes$color,
                      breaks = beheertypes$beheertype_code,
                      ...)
  } else if (legend == 'name'){
    ggplot2::scale_fill_manual(values = beheertypes$color,
                      breaks = beheertypes$beheertype_code,
                      labels = beheertypes$description,
                      ...)
  } else if (legend == 'name_code'){
    ggplot2::scale_fill_manual(values = beheertypes$color,
                      breaks = beheertypes$habitattype,
                      labels = paste0(beheertypes$description, ' (',
                                      beheertypes$beheertype_code, ')'),
                      ...)
  } else {
    stop("For legend argument, choose either 'name', 'code' or 'name_code'")
  }


}

#' Returns 'hoofdtype' van habitattypecode
#'
#' @param habitattype character vector with habittattype codes
#' @examples
#' example code
#'
#'
#' library(VHRscope)
#'
#' habitathoofdtype('H2180A')
#' @export

habitathoofdtype <- function(habitattype){
  gsub('[ABCDEFGboem]+$','',habitattype)
}

#' Returns 'habitattype' van habitatsubtypecode
#'
#' @param habitattype character vector with habittattype codes
#' @examples
#' library(VHRscope)
#'
#' habitattype(habitatsubtype = 'H2190Aom')
#'
#'@export
habitattype <- function(habitatsubtype){
  gsub('[boem]+$','',habitatsubtype)
}

#' Returns a dataframe with monitoring species requirements per SNL-beheertype
#'
#' @param beheertype character with SNL-beheertype code,'all' returns all beheertypen. See https://www.bij12.nl/onderwerp/natuursubsidies/index-natuur-en-landschap/natuurtypen/ for details.
#' @examples
#' library(VHRscope)
#'
#' get_SNL_monitoring_details()
#'
#'@export
get_SNL_monitoring_details <- function(beheertype = 'all'){
  df <- read.csv(file = file.path(system.file(package = "VHRscope"), "extdata", "snl_monitoring_details.csv"), sep = ';')
  if(beheertype != 'all'){
    return(df[df$beheercode == beheertype,])
  } else {
    return(df)
  }
}

#' Returns a dataframe with main monitoring requirements per SNL-beheertype
#'
#' @param beheertype character with SNL-beheertype code,'all' returns all beheertypen. See https://www.bij12.nl/onderwerp/natuursubsidies/index-natuur-en-landschap/natuurtypen/ for details.
#' @examples
#' library(VHRscope)
#'
#' get_SNL_monitoring_overview()
#'
#'@export
get_SNL_monitoring_overview <- function(beheertype = 'all'){
  df <- read.csv(file = file.path(system.file(package = "VHRscope"), "extdata", "snl_monitoring_overview.csv"), sep = ';')
  if(beheertype != 'all'){
    return(df[df$beheercode == beheertype,])
  } else {
    return(df)
  }
}

#' Returns a dataframe with monitoring requirements per Natura2000 habitattype
#'
#' @param habitattype character with habitattype code,'all' returns all beheertypen. See https://www.natura2000.nl/beschermde-natuur/habitattypen for details.
#' @examples
#' library(VHRscope)
#'
#' get_SNL_monitoring_overview()
#'
#'@export
get_N2000_monitoring <- function(habitattype = 'all'){
  df <- read.csv(file = file.path(system.file(package = "VHRscope"), "extdata", "N2000_typische_soorten.csv"), sep = ';')
  if(habitattype != 'all'){
    return(df[df$Habitattype == habitattype,])
  } else {
    return(df)
  }
}

#' Returns an sf object with Dutch N2000 areas
#'
#' @param gebiedNummer character with official number of the area,'all' returns all areas. See https://www.natura2000.nl/gebieden for details.
#' @examples
#' library(VHRscope)
#'
#' sf_N2000 <- st_read_N2000NL()
#'
#' plot((sf_N2000 %>% filter(gebiedNummer %in% 116:126))[,1])
#'
#'@export
st_read_N2000NL <- function(gebiedNummer = 'all'){

  sf_N2000 <- sf::st_read(dsn = file.path(system.file(package = "VHRscope"), "extdata", "N2000_gebieden.gpkg"))

  if(gebiedNummer != 'all'){
    return(sf_N2000[sf_N2000$gebiedNummer == gebiedNummer,])
  } else {
    return(sf_N2000)
  }
}


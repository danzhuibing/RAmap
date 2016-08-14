#' Add a layer of road map to a Leaflet map widget
#'
#' @param map a leaflet map widget
#' @param center a numeric vector defineing the center point of the map widget,
#'   default set to Beijing Tiananmen. Notice that the longitude and latitude
#'   should follow the Chinese standard coordinate GCJ-02
#' @param zoom zoom level of the 10, default set to 10
#' @param layerId layerId in leaflet
#' @param group group in leaflet
#' @return a leaflet map widget
#' @export
#' @examples
#' leaflet() %>% addRoadMap()
addRoadMap <- function(map, center=c(116.40, 39.90), zoom=10, layerId=NULL, group=NULL) {
  map <- map %>% addTiles(
    'http://webrd02.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
    tileOptions(tileSize=256, minZoom=3, maxZoom=18),
    attribution = '&copy; <a href="http://ditu.amap.com/">高德地图</a>',
    layerId = layerId,
    group = group
    ) %>%
    setView(center[1], center[2], zoom=zoom)
  return(map)
}

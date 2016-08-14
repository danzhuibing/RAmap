#' Add a layer of driving route to a Leaflet map widget
#'
#' This function adds a layer of driving route to a Leaflet map widget using \pkg{leaflet}.
#'
#' @param map a leaflet map widget
#' @param origin a numeric vector defining the origin location, format c(longitude, latitude)
#' @param destination a numeric vector defining the destination, format c(longitude, latitude)
#' @param waypoints a data frame consists of two columns, where the first column is longitude
#'   and the second latitude, at most 16 rows.
#' @param strategy refer to the original api http://lbs.amap.com/api/webservice/reference/direction/#t7
#' @param key access key of amap api, please register at the website if you do not have one.
#' @param layerId layerId in leaflet
#' @param group group in leaflet
#' @param weight weight in leaflet
#' @param opacity opacity in leaflet
#' @param popup popup in leaflet, available attributes include status
#' @return a leaflet map widget
#' @export
#' @examples
#' \dontrun{
#' leaflet() %>% addDrivingRoute(key="your key from amap website")
#' }
addDrivingRoute <- function(map, origin=c(116.379028, 39.865042),
                            destination=c(116.427281, 39.903719),
                            waypoints=NULL,
                            strategy=0,
                            key,
                            layerId=NULL, group=NULL, weight=5, opacity = 1,
                            popup = NULL
) {
  driving_json <- getDrivingJSON(origin, destination, waypoints, strategy, key)
  if(!is.null(driving_json)) {
    trafficpal <- colorFactor(c("#984EA3", "#E41A1C", "#FFFF33", "#4DAF4A", "#999999"),
                              domain=c("极度拥堵", "拥堵", "缓行", "畅通", "未知"), ordered=TRUE)
    driving_df <- fromDrivingJSONToDataFrame(driving_json)
    driving_spdf <- fromDrivngDataFrameToSpatialPolylines(driving_df)
    map <- map %>%
      addPolylines(data=driving_spdf, color=~trafficpal(status), layerId=layerId, group=group, weight=weight, opacity=opacity, popup=popup)  %>%
      addLegend("bottomleft", pal = trafficpal, values = driving_spdf@data$status,
                title = "交通状态",
                opacity = 1
      ) %>%
      fitBounds(driving_spdf@bbox["x", "min"] - 0.001, driving_spdf@bbox["y", "min"] - 0.001,
                driving_spdf@bbox["x", "max"] + 0.001, driving_spdf@bbox["y", "max"] + 0.001
      )
    return(map)
  } else {
    return(map)
  }
}

util_get_path <- function(path, i) {
  df <- ldply(path$steps,
              function(x){
                x$tmcs %>%
                  list.select(status, polyline) %>%
                  list.stack()
              }
  )
  df$lc_code <- rownames(df)
  df$path_id <- i
  return(df)
}

util_points_to_spline <- function(df, x_field, y_field, id_field){
  df <- as.data.frame(df)
  data <- as.matrix(df[,c(x_field, y_field)])
  id = df[1, id_field]
  Lines(list(Line(data)), ID=id)
}

util_sp_to_spdf <- function(splines,  data, id_field)  {
  ids <- data.frame(names(splines), stringsAsFactors = F)
  colnames(ids) <- id_field
  join_name <- dplyr::inner_join(ids, data)
  row.names(join_name ) <- join_name[, id_field]
  splinesdf <- SpatialLinesDataFrame(splines, data=join_name)
  proj4string(splinesdf ) <- CRS("+init=epsg:4326") # 设置投影坐标系，leaflet可以不用设置
  return(splinesdf)
}

getDrivingJSON <- function(origin=c(116.379028, 39.865042),
                           destination=c(116.427281, 39.903719),
                           waypoints=NULL,
                           strategy=0,
                           key
) {
  driving_url <- 'http://restapi.amap.com/v3/direction/driving'
  origin_str <- str_c(round(origin, 6), collapse = ",")
  destin_str <- str_c(round(destination, 6), collapse = ",")
  if(!is.null(waypoints)) {s
    waypoi_str <- str_c(round(waypoints[, 1], 6), round(waypoints[, 2], 6), sep=",", collapse=";")
    params <- list(origin=origin_str, destination=destin_str, waypoints=waypoi_str,
                   strategy=strategy, key=key, extensions='all')
  } else {
    params <- list(origin=origin_str, destination=destin_str, strategy=strategy,
                   key=key, extensions='all')
  }
  r <- GET(driving_url, query=params)
  if(r$status_code==200) {
    return(content(r, 'text', encoding='utf-8'))
  }
  else {
    return(NULL)
  }
}

fromDrivingJSONToDataFrame <- function(json_str) {
  if(!is.null(json_str)) {
    json_list <- fromJSON(json_str, simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F)
    path_df <- json_list$route$paths %>%
      list.map(f(x,i) ~ util_get_path(x, i)) %>%
      list.stack()
    return(path_df)
  }
  else {
    return(NULL)
  }
}

fromDrivngDataFrameToSpatialPolylines <- function(df) {
  if(!is.null(df)) {
    df <- df %>%
      unite(Line_id, path_id, lc_code)
    dt <- df %>%
      cSplit(c("polyline"), sep=";", direction="long") %>%
      separate(polyline, c("Lng", "Lat"), sep=",", convert=TRUE)
    df.long <- as.data.frame(dt)
    lst <- split(df.long, df.long[, c("Line_id")])
    names(lst) <- NULL
    sl <- SpatialLines(llply(lst, util_points_to_spline, "Lng", "Lat", "Line_id"))
    sldf <- util_sp_to_spdf(sl, df, "Line_id")
    return(sldf)
  } else {
    return(NULL)
  }
}



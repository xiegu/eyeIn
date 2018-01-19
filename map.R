haierMap <- function(level = 'province'){
  tmpData <- switch(level,
                    province = haierProvDensity,
                    city = haierCityDensity)
  tmpData <- as.data.frame(tmpData)
  map <- switch(level,
                province = leafletGeo('china', tmpData),
                city = leafletGeo('city', tmpData))
  pal <- colorNumeric(
    palette = "Blues",
    domain = map$value)
  leaflet(map) %>% amap(group = "高德")%>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "黑底") %>%   
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                fillColor = ~pal(value),
                color = 'white',
                highlight = highlightOptions(
                  weight = 3,
                  color = 'steelblue',
                  fillOpacity = 0.7,
                  bringToFront = TRUE
                ),
                label = sprintf("<strong style = 'color:black'>区域: </strong> <span style = 'color:black'>%s</span><br/><strong style = 'color:black'>门店数</strong>
                                <span style = 'color:black'>%g</span>",
                                map$name,map$value) %>%lapply(HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = '15px'
                )
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "门店数",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底"),                     
      options = layersControlOptions(collapsed = FALSE)
    )
}


greeCompMap <- function(level = 'province'){
  tmpData <- switch(level,
                    province = greeProvDensity,
                    city = greeCityDensity)
  tmpData <- as.data.frame(tmpData)
  map <- switch(level,
                province = leafletGeo('china', tmpData),
                city = leafletGeo('city', tmpData))
  pal <- colorNumeric(
    palette = "Greens",
    domain = map$value)
  leaflet(map) %>% amap(group = "高德")%>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "黑底") %>%   
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                fillColor = ~pal(value),
                color = 'white',
                highlight = highlightOptions(
                  weight = 3,
                  color = 'green',
                  fillOpacity = 0.7,
                  bringToFront = TRUE
                ),
                label = sprintf("<strong style = 'color:black'>区域: </strong> <span style = 'color:black'>%s</span><br/><strong style = 'color:black'>门店数</strong>
                                <span style = 'color:black'>%g</span>",
                                map$name,map$value) %>%lapply(HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = '15px'
                )
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "门店数",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底"),                     
      options = layersControlOptions(collapsed = FALSE)
    )
}

mideaCompMap <- function(level = 'province'){
  tmpData <- switch(level,
                    province = mideaProvDensity,
                    city = mideaCityDensity)
  tmpData <- as.data.frame(tmpData)
  map <- switch(level,
                province = leafletGeo('china', tmpData),
                city = leafletGeo('city', tmpData))
  pal <- colorNumeric(
    palette = "Reds",
    domain = map$value)
  leaflet(map) %>% amap(group = "高德")%>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "黑底") %>%   
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                fillColor = ~pal(value),
                color = 'white',
                highlight = highlightOptions(
                  weight = 3,
                  color = 'red',
                  fillOpacity = 0.7,
                  bringToFront = TRUE
                ),
                label = sprintf("<strong style = 'color:black'>区域: </strong> <span style = 'color:black'>%s</span><br/><strong style = 'color:black'>门店数</strong>
                                <span style = 'color:black'>%g</span>",
                                map$name,map$value) %>%lapply(HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = '15px'
                )
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "门店数",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底"),                     
      options = layersControlOptions(collapsed = FALSE)
    )
  
}

compRateMap <- function(level = 'province', competitor = 'gree'){
  tmpData <- switch(level,
                    province = list(midea = mideaProvDensity, gree = greeProvDensity),
                    city = list(midea = mideaCityDensity, gree = greeCityDensity)
                    )
  compData <- switch(competitor,
                    midea = tmpData$midea,
                    gree = tmpData$gree
                    )
  haierData <- switch(level,
                      province = haierProvDensity,
                      city = haierCityDensity
                      )
  fullData <- switch(level,
                     province = full_join(haierData, compData, by = c('province' = 'province')),
                     city = full_join(haierData, compData, by = c('city' = 'city'))
  )
  colnames(fullData) <- switch(level,
                               province = c('province', 'haier_no', 'comp_no'),
                               city = c('city', 'haier_no', 'comp_no'))
  fullData <- mutate(fullData, rate = ifelse(is.na(haier_no),NA, ifelse(is.na(comp_no), 99, round(haier_no/comp_no, 2))))%>%
    select(-ends_with('no')) %>%
    as.data.frame
  map <- switch(level,
                province = leafletGeo('china', fullData),
                city = leafletGeo('city', fullData))
  pal <- colorNumeric(
    palette = "viridis",
    domain = map$value)
  leaflet(map) %>% amap(group = "高德")%>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "黑底") %>%   
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                fillColor = ~pal(value),
                color = 'white',
                highlight = highlightOptions(
                  weight = 3,
                  color = 'gold',
                  fillOpacity = 0.7,
                  bringToFront = TRUE
                ),
                label = sprintf("<strong style = 'color:black'>区域: </strong> <span style = 'color:black'>%s</span><br/><strong style = 'color:black'>优势系数</strong>
                                <span style = 'color:black'>%g</span>",
                                map$name,map$value) %>%lapply(HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = '15px'
                )
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "优势系数",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底"),                     
      options = layersControlOptions(collapsed = FALSE)
    )
}

cityMap <- function(chengshi){
  cityTable <- filter(haierTable, city == chengshi)
  cityGree <- filter(gree, city == chengshi)
  cityMidea <- filter(midea, city == chengshi)
  city_pop_area <- filter(china_pop_area, city == chengshi)
  districtDF <- data.frame(city_pop_area$division, city_pop_area$popoverarea)
  districtMap <- leafletGeo(chengshi, districtDF)
pal <- colorNumeric(
  palette = "Blues",
  domain = districtMap$value)

pop1 <- paste0("<strong>区域: </strong>", 
               districtMap$name, 
               "<br><strong>", 
               "人口密度(人数/平方千米)", 
               ": </strong>", 
               districtMap$value
)

leaflet(districtMap)%>%amap(group = '高德')%>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "黑底") %>%   
  addPolygons(stroke = TRUE,
              smoothFactor = 1,
              fillOpacity = 0.7,
              weight = 1,
              color = ~pal(value),
              popup = pop1,
              group = '行政区',
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)
  )%>%
  addMarkers(data = cityTable, ~longitude, ~latitude,
             clusterOptions = markerClusterOptions(
               iconCreateFunction = JS("function (cluster) {
                                         var childCount = cluster.getChildCount();
                                         var c = ' marker-cluster-small';
                                         return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40)})
                 }")
             ),
             popup = paste0("<strong>名称: </strong>", 
                            cityTable$name, 
                            "<br><strong>", 
                            "地址", 
                            ": </strong>",
                            cityTable$address
             ),
             icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png", iconWidth = 20, iconHeight =32),
             group = '海尔专卖店'
             )%>%
  addMarkers(data = cityGree, ~longitude, ~latitude,
             clusterOptions = markerClusterOptions(
               iconCreateFunction = JS("function (cluster) {
                                         var childCount = cluster.getChildCount();
                                         var c = ' marker-cluster-medium';
                                         return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40)})
                 }")
             ),
             popup = paste0("<strong>名称: </strong>", 
                            cityGree$name, 
                            "<br><strong>", 
                            "地址", 
                            ": </strong>",
                            cityGree$address
             ),
             icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png", iconWidth = 20, iconHeight =32),
             group = '格力专卖店'
             )%>%
  addMarkers(data = cityMidea, ~longitude, ~latitude,
             clusterOptions = markerClusterOptions(
               iconCreateFunction = JS("function (cluster) {
                                         var childCount = cluster.getChildCount();
                                         var c = ' marker-cluster-large';
                                         return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40)})
                 }")
             ),
             popup = paste0("<strong>名称: </strong>", 
                            cityMidea$name, 
                            "<br><strong>", 
                            "地址", 
                            ": </strong>",
                            cityMidea$address
             ),
             icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", iconWidth = 20, iconHeight =32),
             group = '美的专卖店'
             )%>%
  addLayersControl( 
    baseGroups = c("高德", "黑底"),                     
    overlayGroups = c("行政区","海尔专卖店", "格力专卖店", "美的专卖店"),
    options = layersControlOptions(collapsed = FALSE)
  )
}

placeMap <- function(chengshi, shop){
    placeTable <- filter(haierTable, (city == chengshi) & (name %in% shop))
  
  leaflet()%>%amap(group = "高德")%>%
    #setView(data = placeTable, ~longitude, ~latitude, zoom = 10)%>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "黑底") %>%   
    addMarkers(data = placeTable, ~longitude, ~latitude, 
               popup = paste0("<span style=\"color:black\"><strong>名称: </strong>", 
                              placeTable$name, 
                              "<br><strong>", 
                              "地址", 
                              ": </strong>",
                              placeTable$address,
                              "</span>"
               ),
               icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png", iconWidth = 20, iconHeight =32)
    )%>%
    addCircles(data = placeTable, ~longitude, ~latitude, weight = 1,
               radius = 500, group = '500米')%>%
    addCircles(data = placeTable, ~longitude, ~latitude, weight = 1,
               radius = 1000, group = '1000米')%>%
    addCircles(data = placeTable, ~longitude, ~latitude, weight = 1,
               radius = 3000, group = '3000米')%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底"),
      overlayGroups = c('500米', '1000米', '3000米'),
      options = layersControlOptions(collapsed = FALSE)
    )%>%
    hideGroup(c('1000米','3000米'))#%>%
    #setView(placeTable$longitude, placeTable$latitude, zoom = 12)
}
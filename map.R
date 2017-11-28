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
    addProviderTiles(providers$Stamen.TonerLite, group = "白底") %>%
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                color = ~pal(value),
                popup = paste0("<strong>区域: </strong>", 
                               map$name,
                               "<br><strong>", 
                               "门店数", 
                               ": </strong>", 
                               map$value
                )
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "门店数",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底", "白底"),                     
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
    addProviderTiles(providers$Stamen.TonerLite, group = "白底") %>%
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                color = ~pal(value),
                popup = paste0("<strong>区域: </strong>", 
                               map$name,
                               "<br><strong>", 
                               "门店数", 
                               ": </strong>", 
                               map$value
                )
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "门店数",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底", "白底"),                     
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
    addProviderTiles(providers$Stamen.TonerLite, group = "白底") %>%
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                color = ~pal(value),
                popup = paste0("<strong>区域: </strong>", 
                               map$name,
                               "<br><strong>", 
                               "门店数", 
                               ": </strong>", 
                               map$value
                )
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "门店数",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底", "白底"),                     
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
    addProviderTiles(providers$Stamen.TonerLite, group = "白底") %>%
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                color = ~pal(value),
                popup = paste0("<strong>区域: </strong>", 
                               map$name,
                               "<br><strong>", 
                               "优势系数", 
                               ": </strong>", 
                               map$value
                )
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "优势系数",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底", "白底"),                     
      options = layersControlOptions(collapsed = FALSE)
    )
}



zzMap <- function(){
pal <- colorNumeric(
  palette = "Blues",
  domain = zzDistrictMap$value)

pop1 <- paste0("<strong>区域: </strong>", 
               zzDistrictMap$name, 
               "<br><strong>", 
               "人口密度(人数/平方千米)", 
               ": </strong>", 
               zzDistrictMap$value
)

leaflet(zzDistrictMap)%>%amap(group = '高德')%>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "黑底") %>%   
  addProviderTiles(providers$Stamen.TonerLite, group = "白底") %>%
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
  addMarkers(data = haierZhengzhouTable, ~longitude, ~latitude, 
             popup = paste0("<strong>名称: </strong>", 
                            haierZhengzhouTable$name, 
                            "<br><strong>", 
                            "电话", 
                            ": </strong>", 
                            haierZhengzhouTable$telephone,
                            "<br><strong>", 
                            "地址", 
                            ": </strong>",
                            haierZhengzhouTable$address
             ),
             icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png", iconWidth = 20, iconHeight =32),
             group = '海尔专卖店'
             )%>%
  addMarkers(data = greeZhengzhou, ~longitude, ~latitude, 
             popup = paste0("<strong>名称: </strong>", 
                            greeZhengzhou$name, 
                            "<br><strong>", 
                            "电话", 
                            ": </strong>", 
                            greeZhengzhou$telephone,
                            "<br><strong>", 
                            "地址", 
                            ": </strong>",
                            greeZhengzhou$address
             ),
             icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png", iconWidth = 20, iconHeight =32),
             group = '格力专卖店'
             )%>%
  addMarkers(data = mideaZhengzhou, ~longitude, ~latitude,
             popup = paste0("<strong>名称: </strong>", 
                            mideaZhengzhou$name, 
                            "<br><strong>", 
                            "电话", 
                            ": </strong>", 
                            mideaZhengzhou$telephone,
                            "<br><strong>", 
                            "地址", 
                            ": </strong>",
                            mideaZhengzhou$address
             ),
             icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png", iconWidth = 20, iconHeight =32),
             group = '美的专卖店'
             )%>%
  addLayersControl( 
    baseGroups = c("高德", "黑底", "白底"),                     
    overlayGroups = c("行政区","海尔专卖店", "格力专卖店", "美的专卖店"),
    options = layersControlOptions(collapsed = FALSE)
  )
}

shMap <- function(){
  pal <- colorNumeric(
    palette = "Blues",
    domain = shDistrictMap$value)
  
  pop1 <- paste0("<strong>区域: </strong>", 
                 shDistrictMap$name, 
                 "<br><strong>", 
                 "人口密度(人数/平方千米)", 
                 ": </strong>", 
                 shDistrictMap$value
  )
  
  leaflet(shDistrictMap)%>%amap(group = '高德')%>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "黑底") %>%   
    addProviderTiles(providers$Stamen.TonerLite, group = "白底") %>%
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
    addMarkers(data = haierShanghaiTable, ~longitude, ~latitude, 
               popup = paste0("<strong>名称: </strong>", 
                              haierShanghaiTable$name, 
                              "<br><strong>", 
                              "电话", 
                              ": </strong>", 
                              haierShanghaiTable$telephone,
                              "<br><strong>", 
                              "地址", 
                              ": </strong>",
                              haierShanghaiTable$address
               ),
               icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png", iconWidth = 20, iconHeight =32),
               group = '海尔专卖店'
    )%>%
    addMarkers(data = greeShanghai, ~longitude, ~latitude, 
               popup = paste0("<strong>名称: </strong>", 
                              greeShanghai$name, 
                              "<br><strong>", 
                              "电话", 
                              ": </strong>", 
                              greeShanghai$telephone,
                              "<br><strong>", 
                              "地址", 
                              ": </strong>",
                              greeShanghai$address
               ),
               icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png", iconWidth = 20, iconHeight =32),
               group = '格力专卖店'
    )%>%
    addMarkers(data = mideaShanghai, ~longitude, ~latitude,
               popup = paste0("<strong>名称: </strong>", 
                              mideaShanghai$name, 
                              "<br><strong>", 
                              "电话", 
                              ": </strong>", 
                              mideaShanghai$telephone,
                              "<br><strong>", 
                              "地址", 
                              ": </strong>",
                              mideaShanghai$address
               ),
               icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png", iconWidth = 20, iconHeight =32),
               group = '美的专卖店'
    )%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底", "白底"),                     
      overlayGroups = c("行政区","海尔专卖店", "格力专卖店", "美的专卖店"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

placeMap <- function(city, shop){
  if(city == '上海'){
    placeTable <- subset(haierShanghaiTable, name %in% shop)
  }else{
    placeTable <- subset(haierZhengzhouTable, name %in% shop)
  }
  leaflet()%>%amap(group = "高德")%>%
    #setView(data = placeTable, ~longitude, ~latitude, zoom = 10)%>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "黑底") %>%   
    addProviderTiles(providers$Stamen.TonerLite, group = "白底") %>%
    addMarkers(data = placeTable, ~longitude, ~latitude, 
               popup = paste0("<span style=\"color:black\"><strong>名称: </strong>", 
                              placeTable$name, 
                              "<br><strong>", 
                              "电话", 
                              ": </strong>", 
                              placeTable$telephone,
                              "<br><strong>", 
                              "地址", 
                              ": </strong>",
                              placeTable$address,
                              "</span>"
               ),
               icon = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png", iconWidth = 20, iconHeight =32)
    )%>%
    addCircles(data = placeTable, ~longitude, ~latitude, weight = 1,
               radius = 500, group = '500 M')%>%
    addCircles(data = placeTable, ~longitude, ~latitude, weight = 1,
               radius = 1000, group = '1000 M')%>%
    addCircles(data = placeTable, ~longitude, ~latitude, weight = 1,
               radius = 3000, group = '3000 M')%>%
    addLayersControl( 
      baseGroups = c("高德", "黑底", "白底"),
      overlayGroups = c('500 M', '1000 M', '3000 M'),
      options = layersControlOptions(collapsed = FALSE)
    )#%>%
    #setView(placeTable$longitude, placeTable$latitude, zoom = 12)
}
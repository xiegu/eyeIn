#library(scales)
#library(dplyr)

#load("data/haier_china.RData")

#chengshi = '上海市'

score_calculator <- function(chengshi){
  haierCityTable <- filter(haierTable, city == chengshi)
  
  # --------------------------------------------------------------------------------------------------------------
  # Find a,b,c such that the distribution of mart a*mart_500 + b*mart_1000 + c*mart_5000 tends closely to Gaussian
  #---------------------------------------------------------------------------------------------------------------
  
  # List all the cmbinations of a,b,c 
  par <- data.frame(a = NA, b = NA, c = NA)
  for (i in 1:11) {
    a <- ((i-1)/10)
    rest <- 12 - i
    for (j in 1:rest) {
      b = ((j-1)/10)
      c = 1 - a - b
      par <- rbind(par, c(a, b, round(c,2)))
    }
  }
  par <- par[-c(1),]
  
  # For mart
  m <- numeric()
  for (i in 1:nrow(par)) {
    a <- par[i,1]
    b <- par[i,2]
    c <- par[i,3]
    mart <- a * haierCityTable$mart_5000 + b * haierCityTable$mart_1000 + c * haierCityTable$mart_500
    if(min(mart) == max(mart)){
      m[i] = 0
    }else{
      m[i] = shapiro.test(mart)$p.value
    }
  }
  
  pm <- as.numeric(par[which.max(m), ])
  p1 <- as.numeric(pm[1])
  p2 <- as.numeric(pm[2])
  p3 <- as.numeric(pm[3])
  mart <- p1 * haierCityTable$mart_5000 + p2 * haierCityTable$mart_1000 +  p3 * haierCityTable$mart_500
  
  # For shop
  s <- numeric()
  for (i in 1:nrow(par)) {
    a <- par[i,1]
    b <- par[i,2]
    c <- par[i,3]
    shop <- a * haierCityTable$shop_5000 + b * haierCityTable$shop_1000 + c * haierCityTable$shop_500
    if(min(shop) == max(shop)){
      s[i] = 0
    }else{
      s[i] = shapiro.test(shop)$p.value
    }
  }
  
  ps <- as.numeric(par[which.max(s), ])
  p1 <- as.numeric(ps[1])
  p2 <- as.numeric(ps[2])
  p3 <- as.numeric(ps[3])
  shop <- p1 * haierCityTable$shop_5000 + p2 * haierCityTable$shop_1000 + p3 * haierCityTable$shop_500
  
  # For bank
  b <- numeric()
  for (i in 1:nrow(par)) {
    a <- par[i,1]
    b <- par[i,2]
    c <- par[i,3]
    bank <- a * haierCityTable$bank_5000 + b * haierCityTable$bank_1000 + c * haierCityTable$bank_500
    if(min(bank) == max(bank)){
      b[i] = 0
    }else{
      b[i] = shapiro.test(bank)$p.value
    }
  }
  
  pb <- as.numeric(par[which.max(b), ])
  p1 <- as.numeric(pb[1])
  p2 <- as.numeric(pb[2])
  p3 <- as.numeric(pb[3])
  bank <- p1 * haierCityTable$bank_5000 + p2 * haierCityTable$bank_1000 + p3 * haierCityTable$bank_500
  
  # For resid
  r <- numeric()
  for (i in 1:nrow(par)) {
    a <- par[i,1]
    b <- par[i,2]
    c <- par[i,3]
    resid <- a * haierCityTable$resid_5000 + b * haierCityTable$resid_1000 + c * haierCityTable$resid_500
    if(min(resid) == max(resid)){
      r[i] = 0
    }else{
      r[i] = shapiro.test(resid)$p.value
    }
  }
  
  pr <- as.numeric(par[which.max(r), ])
  p1 <- as.numeric(pr[1])
  p2 <- as.numeric(pr[2])
  p3 <- as.numeric(pr[3])
  resid <- p1 * haierCityTable$resid_5000 + p2 * haierCityTable$resid_1000 + p3 * haierCityTable$resid_500
  
  # For bus
  vec <- seq(0,1,0.1)
  bu <- numeric()
  for (i in 1:11) {
    a <- vec[i]
    bus <- a * haierCityTable$bus_1000 + (1-a) * haierCityTable$bus_500
    if(min(bus) == max(bus)){
      bu[i] = 0
    }else{
      bu[i] = shapiro.test(resid)$p.value
    }
  }
  
  pbu <- c(vec[which.max(bu)], 1- vec[which.max(bu)])
  p1 <- pbu[1]
  p2 <- pbu[2]
  bus <- p1 * haierCityTable$bus_1000 + p2 * haierCityTable$bus_500
  
  # For parking
  pp <- 1
  parking <- pp * haierCityTable$parking_500
  
  # For competition
  competition <- ((haierCityTable$gome_1000 + haierCityTable$gome_500 + haierCityTable$suning_500 + haierCityTable$suning_1000)*3 + (haierCityTable$gree_500 + haierCityTable$gree_1000)*2)/16
  
  # Rescale to 60-100
  mart <- rescale(mart, to = c(60, 100))
  bus <- rescale(bus, to = c(60, 100))
  shop <- rescale(shop, to = c(60, 100))
  bank <- rescale(bank, to = c(60, 100))
  resid <- rescale(resid, to = c(60, 100))
  parking <- rescale(parking, to = c(60, 100))
  competition <- rescale(competition, to = c(60, 100))
  
  # Score calculation
  life <- 0.6 * mart + 0.1 * bank + 0.3 * resid
  traffic <- 0.5 * bus + 0.5 * parking
  business <- 0.8 * mart + 0.2 * shop
  development <- 0 * bank + 0.9 * mart + 0.1 * resid
  competition <- competition
  final.score <- 0.25 * life + 0.25 * traffic + 0.25 * business + 0.15 * development + 0.1 * competition
  df <- data.frame(life, traffic, business, development, competition, final.score)
  
  haierCityTable <- data.frame(haierCityTable, round(data.frame(score.life = life, score.traffic = traffic, score.business = business, score.development = development, score.competition = competition, score.final = final.score),0))
  cityPopDensity <- filter(china_pop_area, division == chengshi) %>% mutate(., popDesity = population/area*10000)
  haierCityTable <- mutate(haierCityTable, score.final.adj1 = round(score.final*cityPopDensity$popDesity/1000))
  return(haierCityTable)
}


cityList <- unique(haierTable$city)

tableList <- list()
for(i in cityList){
  tableList[[i]] <- score_calculator(i)
  cat(i, '\n')
}
haierTable <- do.call(rbind, tableList)

rm(list = c('tableList', 'i'))
#save(gree, midea, haierTable, haier_d_sales, haier_m_sales, china_pop_area, file = 'data/haier_china.RData')

library(readr)
library(dplyr)
library(readxl)
library(scales)
library(dplyr)

# Load competitor & haier data and haier store poi data
gree <- read_delim("gree_data_table.txt",
                   " ", escape_double = FALSE, col_types = cols(latitude = col_number(),
                                                                longitude = col_number()), trim_ws = TRUE)

midea <- read_delim("midea_data_table.txt",
                    " ", escape_double = FALSE, col_types = cols(latitude = col_number(),
                                                                 longitude = col_number()), trim_ws = TRUE)
haier <- read_delim("haier_data_table.txt", 
                    " ", escape_double = FALSE, trim_ws = TRUE)
haier <- distinct(haier)%>%subset(., province != 'province')

gree <- filter(gree, !is.na(telephone)&!is.na(longitude)&!is.na(latitude))
midea <- filter(midea, !is.na(telephone)&!is.na(longitude)&!is.na(latitude))
haier$shopAddress <- unlist(lapply(haier$shopAddress, function(x) gsub("/", "", x)))
haier$shopCode <- as.character(haier$shopCode)

haier_china_poi <- read_csv("haier_china_data_table.csv")

haier_china_poi <- filter(haier_china_poi, !is.na(longitude)&!is.na(latitude))
#haier_china_poi$shopCode <- as.character(haierShanghai$shopCode)

# Load Haier data
shanghai <- read_csv("data/中心/上海.csv", col_types = cols(销售日期 = col_date(format = '%m/%d/%Y'), 销售月份 = col_date(format = '%Y%m')))
beijing <- read_csv("data/中心/北京.csv", col_types = cols(销售日期 = col_date(format = '%m/%d/%Y'), 销售月份 = col_date(format = '%Y%m')))
hefei <- read_csv("data/中心/合肥.csv", col_types = cols(销售日期 = col_date(format = '%m/%d/%Y'), 销售月份 = col_date(format = '%Y%m')))
jinan <- read_csv("data/中心/济南.csv", col_types = cols(销售日期 = col_date(format = '%m/%d/%Y'), 销售月份 = col_date(format = '%Y%m')))
jining <- read_csv("data/中心/济宁.csv", col_types = cols(销售日期 = col_date(format = '%m/%d/%Y'), 销售月份 = col_date(format = '%Y%m')))
yantai <- read_csv("data/中心/烟台.csv", col_types = cols(销售日期 = col_date(format = '%m/%d/%Y'), 销售月份 = col_date(format = '%Y%m')))
zhengzhou <- read_csv('data/中心/郑州.csv', col_types = cols(销售日期 = col_date(format = '%m/%d/%Y'), 销售月份 = col_date(format = '%Y%m')))
qingdao <- read_csv("data/中心/青岛.csv", col_types = cols(销售日期 = col_date(format = '%m/%d/%Y'), 销售月份 = col_date(format = '%Y%m')))


data <- bind_rows(shanghai, beijing, hefei, jinan, jining, yantai, qingdao, zhengzhou) %>% select(., c(2,4,6:16))
colnames(data) <- c('center', 'network', 'name', 
                    'shopCode', 'region', 'longitude', 'latitude', 'address', 'sales_date', 'sales_month', 'revenue', 'revenue_tax', 
                    'quantity')
rm(list = c('shanghai', 'beijing', 'hefei','jinan', 'jining', 'yantai', 'zhengzhou', 'qingdao'))
gc()

data <- filter(data, !is.na(sales_date))

haier_data <- unique(select(data, center, network, name, shopCode, region, longitude, latitude, address))

haier_data <- mutate(haier_data, province = ifelse(center %in% c('北京', '上海'), substr(network,1,3), paste0(sapply(network, function(x) strsplit(x, '省')[[1]][1]), '省')),
                                 city = ifelse(province %in% c('北京市', '上海市'), substr(network,1,3), paste0(sapply(network, function(x) strsplit(x, '市')[[1]][1])%>%sapply(., function(x) strsplit(x, '省')[[1]][2]), '市')),
                                 district = ifelse(province %in% c('北京市', '上海市'), ifelse(grepl('县', network), paste0(sapply(network, function(x) strsplit(x, '县')[[1]][2]),'县'), paste0(sapply(network, function(x) strsplit(x, '区')[[1]][2]),'区')), sapply(network, function(x) strsplit(x, '市')[[1]][2])%>%sapply(., function(x) ifelse(grepl('网格', x), substr(x,1, nchar(x)-2), ifelse(grepl('格', x), paste0(substr(x,1,nchar(x)-1), '县'), paste0(x, '市'))))))

haier_data <- mutate(haier_data, city = ifelse(grepl('济源市', network), '济源市', city), district=ifelse(grepl('济源市', network), '济源市', district))

haier_sales <- group_by(data, network, shopCode, name)%>%
  summarize(total_revenue = sum(revenue, na.rm = TRUE), 
            total_revenue_tax = sum(revenue_tax, na.rm = TRUE),
            total_quantity = sum(quantity, na.rm = TRUE))

haier_d_sales <- group_by(data, network, shopCode, name, sales_date)%>%
  summarize(revenue = sum(revenue, na.rm = TRUE), 
            revenue_tax = sum(revenue_tax, na.rm = TRUE),
            quantity = sum(quantity, na.rm = TRUE))

haier_m_sales <- group_by(data, network, shopCode, name, sales_month)%>%
  summarize(revenue = sum(revenue, na.rm = TRUE), 
            revenue_tax = sum(revenue_tax, na.rm = TRUE),
            quantity = sum(quantity, na.rm = TRUE))

rm(list = c('data'))

haier_data <- filter(haier_data, !is.na(longitude))

write_csv(haier_data, 'haier_lon_lat.csv')

today <- max(haier_d_sales$sales_date)

# last week
haier_lw_sales <- subset(haier_d_sales, (sales_date >= today - 6)&(sales_date <= today))%>%
  summarize(lw_revenue = sum(revenue, na.rm = TRUE),
            lw_revenue_tax = sum(revenue_tax, na.rm = TRUE),
            lw_quantity = sum(quantity, na.rm = TRUE))

# last month
haier_lm_sales <- subset(haier_d_sales, (sales_date >= today - 30)&(sales_date <= today))%>%
  summarize(lm_revenue = sum(revenue, na.rm = TRUE),
            lm_revenue_tax = sum(revenue_tax, na.rm = TRUE),
            lm_quantity = sum(quantity, na.rm = TRUE))

# last half year
haier_hy_sales <- subset(haier_d_sales, (sales_date >= today - 180)&(sales_date <= today))%>%
  summarize(hy_revenue = sum(revenue, na.rm = TRUE),
            hy_revenue_tax = sum(revenue_tax, na.rm = TRUE),
            hy_quantity = sum(quantity, na.rm = TRUE))

# last year 
haier_ly_sales <- subset(haier_d_sales, (sales_date >= today - 365)&(sales_date <= today))%>%
  summarize(ly_revenue = sum(revenue, na.rm = TRUE),
            ly_revenue_tax = sum(revenue_tax, na.rm = TRUE),
            ly_quantity = sum(quantity, na.rm = TRUE))


# Load suppliment data
GDP_topCity_China <- read_excel("GDP_topCity_China.xls")

population_province_2015 <- read_excel("population_province_2015.xls")

Pop_topCity_China <- read_excel("Pop_topCity_China.xls")

PropertyPrice_topCity_China <- read_excel("PropertyPrice_topCity_China.xls")

Salary_topCity_China <- read_excel("Salary_topCity_China.xls")

GDP_topCity_China <- mutate(GDP_topCity_China, city = paste0(city, '市'), GDPRt = (y2015-y2014)/y2014, GDPArt = (y2015-y2011)/y2011/4)

Pop_topCity_China <- mutate(Pop_topCity_China, city = paste0(city, '市'), PopRt = (y2015-y2014)/y2014, PopArt = (y2015-y2011)/y2011/4)

PropertyPrice_topCity_China <- mutate(PropertyPrice_topCity_China, city = paste0(city, '市'), PropertyPriceRt = (y2015-y2014)/y2014, PropertyPriceArt = (y2015-y2011)/y2011/4)

Salary_topCity_China <- mutate(Salary_topCity_China, city = paste0(city, '市'), SalaryRt = (y2015-y2014)/y2014, SalaryArt = (y2015-y2011)/y2011/4)

china_pop_area <- read_delim("china_pop_area.txt",
                                " ", escape_double = FALSE, trim_ws = TRUE)

china_pop_area <- mutate(china_pop_area, popoverarea=round(population/area * 10000, 0)) # number of person / square km

# Produce wide table
haierTable <- inner_join(haier_china_poi, haier_sales, by = c('shopCode' = 'shopCode'))%>%
  select(-name.x, -network.x)%>%rename(name = name.y, network = network.y) %>%
    left_join(., select(ungroup(haier_lw_sales), shopCode, lw_revenue, lw_revenue_tax, lw_quantity), by = c('shopCode' = 'shopCode')) %>%
      left_join(., select(ungroup(haier_lm_sales), shopCode, lm_revenue, lm_revenue_tax, lm_quantity), by = c('shopCode' = 'shopCode')) %>%
        left_join(., select(ungroup(haier_hy_sales), shopCode, hy_revenue, hy_revenue_tax, hy_quantity), by = c('shopCode' = 'shopCode')) %>%
          left_join(., select(ungroup(haier_ly_sales), shopCode, ly_revenue, ly_revenue_tax, ly_quantity), by = c('shopCode' = 'shopCode'))


haierTable <- left_join(haierTable, GDP_topCity_China, by = c('city' = 'city')) %>%
  select(-y2014, -y2013, -y2012, -y2011) %>% rename(cityGDP = y2015) %>%
    left_join(., Pop_topCity_China, by = c('city' = 'city')) %>% 
      select(-y2014, -y2013, -y2012, -y2011) %>% rename(cityPop = y2015) %>%
        left_join(., PropertyPrice_topCity_China, by = c('city' = 'city')) %>% 
          select(-y2014, -y2013, -y2012, -y2011) %>% rename(cityPprice = y2015) %>%
            left_join(., Salary_topCity_China, by = c('city' = 'city')) %>% 
              select(-y2014, -y2013, -y2012, -y2011) %>% rename(citySalary = y2015)

newNames <- gsub('银行', 'bank', colnames(haierTable))
newNames <- gsub('小区', 'resid', newNames)
newNames <- gsub('商场', 'shop', newNames)
newNames <- gsub('公交站', 'bus', newNames)
newNames <- gsub('超市', 'mart', newNames)
newNames <- gsub('国美', 'gome', newNames)
newNames <- gsub('苏宁', 'suning', newNames)
newNames <- gsub('格力', 'gree', newNames)
newNames <- gsub('停车场', 'parking', newNames)
colnames(haierTable) <- newNames

#save(gree, midea, haierTable, haier_d_sales, haier_m_sales, china_pop_area, file = 'data/haier_china.RData')

#haierTable <- inner_join(haierTable, select(shanghai_pop_area, division, population, area, popoverarea), by = c('district' = 'division'))

#competitor
haierProvDensity <- group_by(haier, province)%>%summarise(., numCity = length(city))
greeProvDensity <- group_by(gree, province)%>%summarise(., numCity = length(city))
mideaProvDensity <- group_by(midea, province)%>%summarise(., numCity = length(city))
haierCityDensity <- group_by(haier, city)%>%summarise(., numDistrict = length(region))
greeCityDensity <- group_by(gree, city)%>%summarise(., numDistrict = length(district))
mideaCityDensity <- group_by(midea, city)%>%summarise(., numDistrict = length(district))
haierDistDensity <- group_by(haier, city, region)%>%summarise(., numShop = length(shopName))
greeDistDensity <- group_by(gree, city, district)%>%summarise(., numShop = length(name))
mideaDistDensity <- group_by(midea, city, district)%>%summarise(., numShop = length(name))

source('score_calculator.R', local = TRUE)
save(gree, midea, haierTable, cityList, haier_d_sales, haier_m_sales, 
     china_pop_area, haierProvDensity, haierCityDensity, haierDistDensity, 
     greeDistDensity, greeCityDensity, greeProvDensity, mideaDistDensity, mideaCityDensity, mideaProvDensity, 
     GDP_topCity_China, Pop_topCity_China, Salary_topCity_China, PropertyPrice_topCity_China, population_province_2015, file = 'data/eyeIn_data.RData')


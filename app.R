library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(leafletCN)
library(dplyr)
library(highcharter)
library(lubridate)
library(DT)
library(scales)

# GDP_topCity_China <- read_excel("data/GDP_topCity_China.xls")
# Pop_topCity_China <- read_excel("data/Pop_topCity_China.xls")
# Salary_topCity_China <- read_excel("data/Salary_topCity_China.xls")
# PropertyPrice_topCity_China <- read_excel("data/PropertyPrice_topCity_China.xls")
#
# load("data/haier_shanghai_demo.RData")
# load("data/haier_zhengzhou_demo.RData")
# load("data/haier_shanghai_score.RData")
# load("data/haier_zhengzhou_score.RData")
# load("data/competitor_demo.RData")

load('data/data.RData')
haierShanghaiTable <- haierShanghaiTable[-c(22, 35),]

# warning! for 2017-11-13 reporting use
finScoreDemo <-
  mutate(
    finScoreDemo,
    sales_date = sales_date + 365,
    y = year(sales_date),
    m = month(sales_date),
    ym = paste(y, m, sep = '-')
  )

source("map.R", local = TRUE)

#dbHeader <- dashboardHeader()
#dbHeader$children[[2]]$children <- tags$div('猫头鹰 ', tags$img(src = 'logo.png', height = '42'))

ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = '猫头鹰'),
  dashboardSidebar(sidebarMenu(
    br(),
    menuItem(h4("竞争品牌门店分析"), tabName = "competitor"),
    menuItem(h4("信息中心指数分析"), tabName = "placeScore"),
    menuItem(h4("财务关联分析"), tabName = "finScore"),
    menuItem(h4("门店客户画像"), tabName = 'userProfile')
  )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName = "competitor",
        br(),
        fluidRow(column(
          2, offset = 6,
          selectInput(
            "compBrand",
            label = NULL,
            choices = c("格力" = 'gree',
                        "美的" = 'midea')
          )
        ),
        column(
          2,
          selectInput(
            'compLevel',
            label = NULL,
            choices = c('省级' = 'province',
                        '市级' = 'city')
          )
        )),
        br(),
        fluidRow(
          tags$style(
            ".nav-tabs {
            background-color: black;
            }
            
            .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
            background-color: transparent;
            border-color: transparent;
            color: #FFF
            }
            
            .nav-tabs-custom .nav-tabs li.active {
            border-top-color: #FFF;
            color: #FFF;
            }
            
            .nav-tabs-custom .nav-tabs li.header {
            color: #fff;
            }
            
            .nav-tabs-custom .tab-content {
            background: none repeat scroll 0% 0% black;
            color: #fff;
            padding: 10px;
            border-bottom-right-radius: 3px;
            border-bottom-left-radius: 3px;
            }
            "
          ),
          tabBox(
            title = "海尔门店分布",
            width = 6,
            tabPanel(title = h4('分布情况'),
                     leafletOutput('haierCompMap')),
            tabPanel(title = h4('优势对比'),
                     leafletOutput('compRateMap'))
          ),
          tabBox(
            title = '竞争品牌门店分布',
            width = 6,
            tabPanel(title = h4('分布情况'),
                     leafletOutput('compMap'))
            
          )
          ),
        br(),
        
        fluidRow(
          valueBoxOutput('HaierProvCover', width = 2),
          valueBoxOutput('HaierCityCover', width = 2),
          valueBoxOutput('HaierShopCover', width = 2),
          valueBoxOutput('CompProvCover', width = 2),
          valueBoxOutput('CompCityCover', width = 2),
          valueBoxOutput('CompShopCover', width = 2)
        ),
        br(),
        fluidRow(
          tags$style(
            ".nav-tabs {
            background-color: black;
            }
            
            .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
            background-color: transparent;
            border-color: transparent;
            color: #FFF
            }
            
            .nav-tabs-custom .nav-tabs li.active {
            border-top-color: #FFF;
            color: #FFF;
            }
            
            .nav-tabs-custom .nav-tabs li.header {
            color: #fff;
            }
            
            .nav-tabs-custom .tab-content {
            background: none repeat scroll 0% 0% black;
            color: #fff;
            padding: 10px;
            border-bottom-right-radius: 3px;
            border-bottom-left-radius: 3px;
            }
            "
          ),
          tabBox(
            title = "海尔门店数排名",
            width = 6,
            tabPanel(
              title = h4('海尔'),
              highchartOutput('shopNumHaier', height = '250px')
            )
          ),
          tabBox(
            title = "竞品门店数排名",
            width = 6,
            tabPanel(title = h4('格力'),
                     highchartOutput('shopNumGree', height = '250px')),
            tabPanel(
              title = h4('美的'),
              highchartOutput('shopNumMidea', height = '250px')
            )
          )
          ),
        br(),
        fluidRow(
          tags$style(
            ".nav-tabs {
            background-color: black;
            }
            
            .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
            background-color: transparent;
            border-color: transparent;
            color: #FFF
            }
            
            .nav-tabs-custom .nav-tabs li.active {
            border-top-color: #FFF;
            color: #FFF;
            }
            
            .nav-tabs-custom .nav-tabs li.header {
            color: #fff;
            }
            
            .nav-tabs-custom .tab-content {
            background: none repeat scroll 0% 0% black;
            color: #fff;
            padding: 10px;
            border-bottom-right-radius: 3px;
            border-bottom-left-radius: 3px;
            }
            "
          ),
          tabBox(
            title = "海尔门店密度排名",
            width = 6,
            tabPanel(
              title = h4('海尔'),
              highchartOutput('shopDensityHaier', height = '300px')
            )
          ),
          tabBox(
            title = "竞品门店密度排名",
            width = 6,
            tabPanel(
              title = h4('格力'),
              highchartOutput('shopDensityGree', height = '300px')
            ),
            tabPanel(
              title = h4('美的'),
              highchartOutput('shopDensityMidea', height = '300px')
            )
          )
          
          ),
        hr(),
        fluidRow(column(
          2,
          selectInput('mapCity', '城市', choices = c('上海', '郑州'))
        )),
        br(),
        fluidRow(
          tags$style("
                     .marker-cluster-small {
          	background-color:  rgba(173, 216, 230, 0.6);
            }

            .marker-cluster-small div {
          	background-color:  rgba(30,144, 255, 0.6);
            }

            .marker-cluster-medium {
          	background-color:  rgba(84, 255, 159, 0.6);
            }

            .marker-cluster-medium div {
          	background-color:  rgba(102, 205, 0, 0.6);
            }

            .marker-cluster-large{
          	background-color:  rgba(205, 91, 69, 0.6);
            }

            .marker-cluster-large div {
          	background-color:  rgba(238, 64, 0, 0.6);
            }
            "),
          column(12,
                        leafletOutput('mapAll', height = 600))),
        br(),
        fluidRow(
          tags$style(
            ".nav-tabs {
            background-color: black;
            }
            
            .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
            background-color: transparent;
            border-color: transparent;
            color: #FFF
            }
            
            .nav-tabs-custom .nav-tabs li.active {
            border-top-color: #FFF;
            color: #FFF;
            }
            
            .nav-tabs-custom .nav-tabs li.header {
            color: #fff;
            }
            
            .nav-tabs-custom .tab-content {
            background: none repeat scroll 0% 0% black;
            color: #fff;
            padding: 10px;
            border-bottom-right-radius: 3px;
            border-bottom-left-radius: 3px;
            }
            "),
          tabBox(
            title = "门店详细信息",
            width = 12,
            tabPanel(title = h4('海尔'),
                     dataTableOutput('haierTable')),
            tabPanel(title = h4('格力'),
                     dataTableOutput('GreeTable')),
            tabPanel(title = h4('美的'),
                     dataTableOutput('MideaTable'))
          )
          )
    ),
    
    tabItem(
      tabName = 'placeScore',
      fluidRow(
        valueBoxOutput('cityGDP', width = 3),
        valueBoxOutput('cityPop', width = 3),
        valueBoxOutput('cityPprice', width = 3),
        valueBoxOutput('citySalary', width = 3)
        
      ),
      br(),
      fluidRow(
        tags$style(
          ".nav-tabs {
          background-color: black;
          }
          
          .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
          background-color: transparent;
          border-color: transparent;
          color: #FFF
          }
          
          .nav-tabs-custom .nav-tabs li.active {
          border-top-color: #FFF;
          color: #FFF;
          }
          
          .nav-tabs-custom .nav-tabs li.header {
          color: #FFF;
          }
          
          .nav-tabs-custom .tab-content {
          background: none repeat scroll 0% 0% black;
          color: #fff;
          padding: 10px;
          border-bottom-right-radius: 3px;
          border-bottom-left-radius: 3px;
          }
          "
        ),
        
        tabBox(
          width = 6,
          title = '发展指标',
          side = 'left',
          tabPanel(
            title = h4('国内生产总值&年末总人口'),
            highchartOutput('cityDev1', height = '300px')
          ),
          tabPanel(
            title = h4('平均工资&住宅均价'),
            highchartOutput('cityDev2', height = '300px')
          )
        ),
        box(
          width = 6,
          title = '城市人口密度',
          solidHeader = TRUE,
          background = 'black',
          highchartOutput('cityDensity',  height = '300px')
        )
        ),
      br(),
      fluidRow(
        tags$style(
          ".nav-tabs {
          background-color: black;
          }
          
          .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
          background-color: transparent;
          border-color: transparent;
          color: #FFF
          }
          
          .nav-tabs-custom .nav-tabs li.active {
          border-top-color: #FFF;
          color: #FFF;
          }
          
          .nav-tabs-custom .nav-tabs li.header {
          color: #FFF;
          }
          
          .nav-tabs-custom .tab-content {
          background: none repeat scroll 0% 0% black;
          color: #fff;
          padding: 10px;
          border-bottom-right-radius: 3px;
          border-bottom-left-radius: 3px;
          }
          "
        ),
        tabBox(
          title = '商圈细分',
          width = 6,
          side = 'left',
          tabPanel(title = h4('网格得分'),
                   dataTableOutput('networkScore')),
          tabPanel(
            title = h4('指数雷达'),
            highchartOutput('scoreRadar', height = '350px')
          )
        ),
        box(
          title = '信息指数排名',
          width = 6,
          solid = TRUE,
          background = 'black',
          highchartOutput('scoreBestWorst', height = '600px')
        )
        
        ),
      br(),
      fluidRow(
        column(2,
               selectInput(
                 'scoreCity',
                 'City',
                 choices = c('上海', '郑州'),
                 selected = '郑州'
               )),
        column(2,
               uiOutput('scoreDistrict')
        ),
               column(4,
                      uiOutput('scoreShop')
               )
        ),
               br(),
               fluidRow(
                 tags$style(
                   ".nav-tabs {
                   background-color: black;
                   }
                   
                   .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                   background-color: transparent;
                   border-color: transparent;
                   color: #FFF
                   }
                   
                   .nav-tabs-custom .nav-tabs li.active {
                   border-top-color: #FFF;
                   color: #FFF;
                   }
                   
                   .nav-tabs-custom .nav-tabs li.header {
                   color: #FFF;
                   }
                   
                   .nav-tabs-custom .tab-content {
                   background: none repeat scroll 0% 0% black;
                   color: #fff;
                   padding: 10px;
                   border-bottom-right-radius: 3px;
                   border-bottom-left-radius: 3px;
                   }
                   "
                 ),
                 uiOutput('scoreBox')
                 
                 ),
               fluidRow(
                 useShinyjs(),
                 tags$style("
                            #scoreWiki {
                            color: #FFF
                            }
                            "),
                 column(
                   width = 4,
                   actionButton("btn1", "如何计算信息中心指数？"),
                   br(),
                   div(
                     id = 'scoreWiki',
                     '根据每家店的店面定位位置，抓取周围特定距离内的POI信息。
                     抓取的范围包括生活信息（小区、超市、银行等）、交通信息（公交站、停车场等）、商业信息（超市、商场等）、发展信息（小区等）和竞争信息（竞争对店铺位置）。
                     信息中心指数根据上述的五个维度的信息指数再加权平均计算得出。'
                   )
                   )
                 ),
               br(),
               fluidRow(
                 box(
                   width = 12,
                   title = '信息点地图',
                   solidHeader = TRUE,
                   background = "black",
                   leafletOutput('mapPlace', height = 600)
                 )
               )
               ),
        tabItem(
          tabName = 'finScore',
          fluidRow(
            column(2,
                   selectInput(
                     'scoreCity3',
                     '城市',
                     choices = c('上海', '郑州'),
                     selected = '郑州'
                   )),
            column(2,
                   uiOutput('scoreDistrict3')),
            column(4,
                   uiOutput('scoreShop3'))
          ),
          fluidRow(uiOutput('comparisonScore')),
          fluidRow(
            useShinyjs(),
            tags$style("
                       #densityWiki {
                       color: #FFF
                       }
                       "),
            column(
              width = 4,
              actionButton("btn2", "如何计算门店客流量？"),
              br(),
              div(
                id = 'densityWiki',
                '门店客流量计算主要依托百度大数据，以每月为基准，根据门店周围半径50米内，抓取停留时间超过15分钟的客流量。百度抓取的客流量与实际客流量存在一定的转化比例。'
              )
            )
            ),
          br(),
          fluidRow(
            box(
              width = 6,
              title = '多店关联对比-收入',
              solid = TRUE,
              background = 'black',
              
              highchartOutput('revCompChart', height = '300px')
            ),
            box(
              width = 6,
              title = '多店关联对比-客流量',
              solid = TRUE,
              background = 'black',
              highchartOutput('trafficCompChart', height = '300px')
            )
          ),
          hr(),
          fluidRow(
            column(2,
                   selectInput('scoreCity2', '城市', choices = c('上海', '郑州'))),
            column(2,
                   uiOutput('scoreDistrict2')
                   ),
                   column(4,
                          uiOutput('scoreShop2')
                   )
            ),
                   
                   # fluidRow(
                   #   box(width = 12, title = '收入销量趋势', solid = TRUE, background = 'black',
                   #
                   #            highchartOutput('revChart')
                   #          )
                   # ),
                   fluidRow(
                     valueBoxOutput('lyRevenue', width = 3),
                     valueBoxOutput('hyRevenue', width = 3),
                     valueBoxOutput('lmRevenue', width = 3),
                     valueBoxOutput('lwRevenue', width = 3)
                   ),
                   fluidRow(
                     valueBoxOutput('lyQuantity', width = 3),
                     valueBoxOutput('hyQuantity', width = 3),
                     valueBoxOutput('lmQuantity', width = 3),
                     valueBoxOutput('lwQuantity', width = 3)
                   )
            ),
    tabItem(tabName = 'userProfile')
          
        )
      )
  )
    
               
      
      server <- function(input, output) {
        output$haierCompMap <- renderLeaflet({
          #haierMapInput()
          haierMap(level = input$compLevel)
        })
        
        output$compMap <- renderLeaflet({
          if (input$compBrand == 'gree') {
            greeCompMap(level = input$compLevel)
          } else if (input$compBrand == 'midea') {
            mideaCompMap(level = input$compLevel)
          } else{
            return(NULL)
          }
        })
        
        output$compRateMap <- renderLeaflet({
          compRateMap(level = input$compLevel,
                      competitor = input$compBrand)
        })
        
        output$HaierProvCover <- renderValueBox({
          tmpTable <- haierProvDensity
          col <- 'aqua'
          valueBox(
            value = length(tmpTable$province),
            subtitle = '覆盖省份',
            color = col
          )
        })
        
        output$HaierCityCover <- renderValueBox({
          tmpTable <- haierCityDensity
          col <- 'aqua'
          valueBox(
            value = prettyNum(length(tmpTable$city), big.mark = ','),
            subtitle = '覆盖城市',
            color = col
          )
        })
        
        output$HaierShopCover <- renderValueBox({
          tmpTable <- haierDistDensity
          col <- 'aqua'
          valueBox(
            value = prettyNum(sum(tmpTable$numShop), big.mark = ','),
            subtitle = '门店总数',
            color = col
          )
        })
        
        output$CompProvCover <- renderValueBox({
          if (input$compBrand == 'gree') {
            tmpTable <- greeProvDensity
            col <- 'teal'
          } else if (input$compBrand == 'midea') {
            tmpTable <- mideaProvDensity
            col <- 'maroon'
          } else{
            tmpTable <- NULL
          }
          valueBox(
            value = prettyNum(length(tmpTable$province), big.mark = ','),
            subtitle = '覆盖省份',
            color = col
          )
        })
        
        output$CompCityCover <- renderValueBox({
          if (input$compBrand == 'gree') {
            tmpTable <- greeCityDensity
            col <- 'teal'
          } else if (input$compBrand == 'midea') {
            tmpTable <- mideaCityDensity
            col <- 'maroon'
          } else{
            tmpTable <- NULL
          }
          valueBox(
            value = prettyNum(length(tmpTable$city), big.mark = ','),
            subtitle = '覆盖城市',
            color = col
          )
        })
        
        output$CompShopCover <- renderValueBox({
          if (input$compBrand == 'gree') {
            tmpTable <- greeDistDensity
            col <- 'teal'
          } else if (input$compBrand == 'midea') {
            tmpTable <- mideaDistDensity
            col <- 'maroon'
          } else{
            tmpTable <- NULL
          }
          valueBox(
            value = prettyNum(sum(tmpTable$numShop), big.mark = ','),
            subtitle = '门店总数',
            color = col
          )
        })
        
        output$shopNumHaier <- renderHighchart({
          if (input$compLevel == 'province') {
            tmpTable <-
              haierProvDensity %>% rename(., name = province, num = numCity) %>% arrange(., desc(num)) %>%
              top_n(10)
          } else if (input$compLevel == 'city') {
            tmpTable <-
              haierCityDensity %>% rename(., name = city, num = numDistrict) %>% arrange(., desc(num)) %>%
              top_n(10)
          } else{
            tmpTable <- NULL
          }
          highchart() %>%
            hc_chart(type = 'bar') %>%
            hc_xAxis(categories = tmpTable$name,
                     labels = list(style = list(
                       color = 'white', `font-size` = '15px'
                     ))) %>%
            hc_yAxis(labels = list(style = list(color = 'white'))) %>%
            hc_add_series(data = tmpTable$num,
                          color = 'lightblue',
                          name = '门店数') %>%
            hc_legend(enabled = FALSE)
        })
        
        output$shopNumGree <- renderHighchart({
          if (input$compLevel == 'province') {
            tmpTable <-
              greeProvDensity %>% rename(., name = province, num = numCity) %>% arrange(., desc(num)) %>%
              top_n(10)
          } else if (input$compLevel == 'city') {
            tmpTable <-
              greeCityDensity %>% rename(., name = city, num = numDistrict) %>% arrange(., desc(num)) %>%
              top_n(10)
          } else{
            tmpTable <- NULL
          }
          highchart() %>%
            hc_chart(type = 'bar') %>%
            hc_xAxis(categories = tmpTable$name,
                     labels = list(style = list(
                       color = 'white', `font-size` = '15px'
                     ))) %>%
            hc_yAxis(labels = list(style = list(color = 'white'))) %>%
            hc_add_series(data = tmpTable$num,
                          color = 'turquoise',
                          name = '门店数') %>%
            hc_legend(enabled = FALSE)
        })
        
        output$shopNumMidea <- renderHighchart({
          if (input$compLevel == 'province') {
            tmpTable <-
              mideaProvDensity %>% rename(., name = province, num = numCity) %>% arrange(., desc(num)) %>%
              top_n(10)
          } else if (input$compLevel == 'city') {
            tmpTable <-
              mideaCityDensity %>% rename(., name = city, num = numDistrict) %>% arrange(., desc(num)) %>%
              top_n(10)
          } else{
            tmpTable <- NULL
          }
          highchart() %>%
            hc_chart(type = 'bar') %>%
            hc_xAxis(categories = tmpTable$name,
                     labels = list(style = list(
                       color = 'white', `font-size` = '15px'
                     ))) %>%
            hc_yAxis(labels = list(style = list(color = 'white'))) %>%
            hc_add_series(data = tmpTable$num,
                          color = 'LightCoral',
                          name = '门店数') %>%
            hc_legend(enabled = FALSE)
        })
        
        output$shopDensityHaier <- renderHighchart({
          #if(input$compLevel == 'province'){
          tmpTable <-
            haierProvDensity %>% rename(., name = province, num = numCity)
          tmpTable <-
            inner_join(tmpTable,
                       population_province_2015,
                       by = c('name' = 'province'))
          tmpTable <-
            mutate(tmpTable, density = round(1000 * (num / population)))
          #}else if(input$compLevel == 'city'){
          #tmpTable <- mideaCityDensity%>%rename(., name = city, num = numDistrict)%>%arrange(., desc(num))%>%top_n(10)
          #}else{
          #tmpTable <- NULL
          #}
          highchart() %>%
            hc_tooltip(pointFormat = "<span style=\"color:black\">{point.name}每千万人拥有店家数</span>:
                       {point.z:,.0f}<br/>") %>%
            hc_xAxis(labels = list(style = list(color = 'white'))) %>%
            hc_yAxis(labels = list(style = list(color = 'white'))) %>%
            hc_add_series(
              data = tmpTable,
              type = 'scatter',
              hcaes(
                x = num,
                y = population,
                size = density,
                color = density
              ),
              name = '门店密度'
            ) %>%
            hc_legend(enabled = FALSE)
        })
        
        output$shopDensityGree <- renderHighchart({
          #if(input$compLevel == 'province'){
          tmpTable <-
            greeProvDensity %>% rename(., name = province, num = numCity)
          tmpTable <-
            inner_join(tmpTable,
                       population_province_2015,
                       by = c('name' = 'province'))
          tmpTable <-
            mutate(tmpTable, density = round(1000 * (num / population)))
          #}else if(input$compLevel == 'city'){
          #tmpTable <- mideaCityDensity%>%rename(., name = city, num = numDistrict)%>%arrange(., desc(num))%>%top_n(10)
          #}else{
          #tmpTable <- NULL
          #}
          highchart() %>%
            hc_tooltip(pointFormat = "<span style=\"color:black\">{point.name}每千万人拥有门店数</span>:
                       {point.z:,.0f}<br/>") %>%
            hc_xAxis(labels = list(style = list(color = 'white'))) %>%
            hc_yAxis(labels = list(style = list(color = 'white'))) %>%
            hc_add_series(
              data = tmpTable,
              type = 'scatter',
              hcaes(
                x = num,
                y = population,
                size = density,
                color = density
              ),
              name = '门店密度'
            ) %>%
            hc_legend(enabled = FALSE)
        })
        
        output$shopDensityMidea <- renderHighchart({
          #if(input$compLevel == 'province'){
          tmpTable <-
            mideaProvDensity %>% rename(., name = province, num = numCity)
          tmpTable <-
            inner_join(tmpTable,
                       population_province_2015,
                       by = c('name' = 'province'))
          tmpTable <-
            mutate(tmpTable, density = round(1000 * (num / population)))
          #}else if(input$compLevel == 'city'){
          #tmpTable <- mideaCityDensity%>%rename(., name = city, num = numDistrict)%>%arrange(., desc(num))%>%top_n(10)
          #}else{
          #tmpTable <- NULL
          #}
          highchart() %>%
            hc_tooltip(pointFormat = "<span style=\"color:black\">{point.name}每千万人拥有店家数</span>:
                       {point.z:,.0f}<br/>") %>%
            hc_xAxis(labels = list(style = list(color = 'white'))) %>%
            hc_yAxis(labels = list(style = list(color = 'white'))) %>%
            hc_add_series(
              data = tmpTable,
              type = 'scatter',
              hcaes(
                x = num,
                y = population,
                size = density,
                color = density
              ),
              name = '门店密度'
            ) %>%
            hc_legend(enabled = FALSE)
        })
        
        output$mapAll <- renderLeaflet({
          if (input$mapCity == '上海') {
            shMap()
          } else{
            zzMap()
          }
        })
        
        output$haierTable <- renderDataTable({
          if (input$mapCity == '上海') {
            tmpTable <-
              select(haierShanghaiTable,
                     province,
                     city,
                     district,
                     shopCode,
                     network,
                     name)
          } else{
            tmpTable <-
              select(haierZhengzhouTable,
                     province,
                     city,
                     district,
                     shopCode,
                     network,
                     name)
          }
          datatable(tmpTable,
                    rownames = FALSE,
                    options = list(scrollX = TRUE)) %>% formatStyle(colnames(tmpTable),
                                                                    color = '#fff',
                                                                    backgroundColor = 'black')
        })
        output$GreeTable <- renderDataTable({
          if (input$mapCity == '上海') {
            tmpTable <- greeShanghai[, 1:6]
          } else{
            tmpTable <- greeZhengzhou[, 1:6]
          }
          datatable(tmpTable,
                    rownames = FALSE,
                    options = list(scrollX = TRUE)) %>% formatStyle(colnames(tmpTable),
                                                                    color = '#fff',
                                                                    backgroundColor = 'black')
        })
        output$MideaTable <- renderDataTable({
          if (input$mapCity == '上海') {
            tmpTable <- mideaShanghai[, 1:6]
          } else{
            tmpTable <- mideaZhengzhou[, 1:6]
          }
          datatable(tmpTable,
                    rownames = FALSE,
                    options = list(scrollX = TRUE)) %>% formatStyle(colnames(tmpTable),
                                                                    color = '#fff',
                                                                    backgroundColor = 'black')
        })
        
        observeEvent(input$btn1 == FALSE, {
          # Change the following line for more examples
          toggle("scoreWiki")
        })
        
        observeEvent(input$btn2 == FALSE, {
          # Change the following line for more examples
          toggle("densityWiki")
        })
        output$scoreDistrict <- renderUI({
          if (input$scoreCity == '上海') {
            choiceList <- unique(haierShanghaiTable$district)
          } else{
            choiceList <- unique(haierZhengzhouTable$district)
          }
          selectInput('scoreDistrict', '行政区', choices = choiceList)
        })
        
        output$scoreShop <- renderUI({
          if (input$scoreCity == '上海') {
            choiceList <-
              subset(haierShanghaiTable, district == input$scoreDistrict) %>% .$name %>%
              unique
          } else{
            choiceList <-
              subset(haierZhengzhouTable, district == input$scoreDistrict) %>% .$name %>%
              unique
          }
          selectizeInput(
            'scoreShop',
            '门店',
            choices = choiceList,
            selected = choiceList[1],
            multiple = TRUE,
            options = list(maxItems = 3)
          )
        })
        
        output$scoreDistrict2 <- renderUI({
          if (input$scoreCity2 == '上海') {
            choiceList <- unique(haierShanghaiTable$district)
          } else{
            choiceList <- unique(haierZhengzhouTable$district)
          }
          selectInput('scoreDistrict2', '行政区', choices = choiceList)
        })
        
        output$scoreShop2 <- renderUI({
          if (input$scoreCity2 == '上海') {
            choiceList <-
              subset(haierShanghaiTable, district == input$scoreDistrict2) %>% .$name %>%
              unique
          } else{
            choiceList <-
              subset(haierZhengzhouTable, district == input$scoreDistrict2) %>% .$name %>%
              unique
          }
          selectizeInput('scoreShop2',
                         '门店',
                         choices = choiceList,
                         selected = choiceList[1])
        })
        
        output$scoreDistrict3 <- renderUI({
          if (input$scoreCity3 == '上海') {
            choiceList <- unique(haierShanghaiTable$district)
          } else{
            choiceList <- unique(haierZhengzhouTable$district)
          }
          selectInput('scoreDistrict3',
                      '行政区',
                      choices = choiceList,
                      selected = '金水区')
        })
        
        output$scoreShop3 <- renderUI({
          if (input$scoreCity3 == '上海') {
            choiceList <-
              subset(haierShanghaiTable, district == input$scoreDistrict3) %>% .$name %>%
              unique
          } else{
            choiceList <-
              subset(haierZhengzhouTable, district == input$scoreDistrict3) %>% .$name %>%
              unique
          }
          selectizeInput(
            'scoreShop3',
            '门店',
            choices = choiceList,
            selected = c('郑州丹尼斯人民路锐达店', '河南清河工贸CBD店', '郑州锐达CBD店'),
            multiple = TRUE,
            options = list(maxItems = 3)
          )
        })
        output$scoreRadar <- renderHighchart({
          tmpTable <-
            if (input$scoreCity == '上海') {
              subset(haierShanghaiTable,
                     district == input$scoreDistrict & name %in% input$scoreShop) %>%
                select(starts_with('score.'))
            } else{
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict & name %in% input$scoreShop
              ) %>%
                select(starts_with('score.'))
            }
          
          tmpTable2 <-
            if (input$scoreCity == '上海') {
              subset(haierShanghaiTable, district == input$scoreDistrict) %>%
                select(starts_with('score.'))
            } else{
              subset(haierZhengzhouTable, district == input$scoreDistrict) %>%
                select(starts_with('score.'))
            }
          
          tmpTable3 <-
            if (input$scoreCity == '上海') {
              subset(haierShanghaiTable) %>%
                select(starts_with('score.'))
            } else{
              subset(haierZhengzhouTable) %>%
                select(starts_with('score.'))
            }
          
          
          hc <- highchart() %>%
            hc_chart(polar = TRUE, type = "line") %>%
            hc_xAxis(
              categories = c("竞争", "生活", "交通",
                             "商业", "发展"),
              tickmarkPlacement = "on",
              lineWidth = 0,
              labels = list(style = list(
                color = 'white', `font-size` = '15px'
              ))
            ) %>%
            hc_yAxis(
              gridLineInterpolation = "polygon",
              lineWidth = 0,
              min = 0
            ) %>%
            hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
                       {point.y:,0f}<br/>",
                       shared = TRUE) %>%
            hc_legend(
              layout = 'vertical',
              align = 'left',
              verticalAlign = 'top',
              x = 60,
              floating = TRUE,
              itemStyle = list(color = 'white', `font-size` = '15px')
            )
          if (length(input$scoreShop) == 1) {
            hc %>% hc_series(
              list(
                name = "店铺指数",
                data = c(
                  tmpTable$score.competition,
                  tmpTable$score.life,
                  tmpTable$score.traffic,
                  tmpTable$score.business,
                  tmpTable$score.development
                ),
                pointPlacement = "on",
                color = 'steelblue'
              ),
              list(
                name = "区划指数",
                data = c(
                  round(mean(tmpTable2$score.competition), 0),
                  round(mean(tmpTable2$score.life), 0),
                  round(mean(tmpTable2$score.traffic), 0),
                  round(mean(tmpTable2$score.business), 0),
                  round(mean(tmpTable2$score.development) , 0)
                ),
                pointPlacement = "on",
                color = 'LightCoral'
              ),
              list(
                name = "城市指数",
                data = c(
                  round(mean(tmpTable3$score.competition), 0),
                  round(mean(tmpTable3$score.life), 0),
                  round(mean(tmpTable3$score.traffic), 0),
                  round(mean(tmpTable3$score.business), 0),
                  round(mean(tmpTable3$score.development) , 0)
                ),
                pointPlacement = "on",
                color = 'turquoise'
              )
            )
          } else if (length(input$scoreShop) == 2) {
            hc %>% hc_series(
              list(
                name = "店铺指数 - 1",
                data = c(
                  tmpTable$score.competition[1],
                  tmpTable$score.life[1],
                  tmpTable$score.traffic[1],
                  tmpTable$score.business[1],
                  tmpTable$score.development[1]
                ),
                pointPlacement = "on",
                color = 'steelblue'
              ),
              list(
                name = "店铺指数 - 2",
                data = c(
                  tmpTable$score.competition[2],
                  tmpTable$score.life[2],
                  tmpTable$score.traffic[2],
                  tmpTable$score.business[2],
                  tmpTable$score.development[2]
                ),
                pointPlacement = "on",
                color = 'lightblue'
              ),
              list(
                name = "区划指数",
                data = c(
                  round(mean(tmpTable2$score.competition), 0),
                  round(mean(tmpTable2$score.life), 0),
                  round(mean(tmpTable2$score.traffic), 0),
                  round(mean(tmpTable2$score.business), 0),
                  round(mean(tmpTable2$score.development) , 0)
                ),
                pointPlacement = "on",
                color = 'LightCoral'
              ),
              list(
                name = "城市指数",
                data = c(
                  round(mean(tmpTable3$score.competition), 0),
                  round(mean(tmpTable3$score.life), 0),
                  round(mean(tmpTable3$score.traffic), 0),
                  round(mean(tmpTable3$score.business), 0),
                  round(mean(tmpTable3$score.development) , 0)
                ),
                pointPlacement = "on",
                color = 'turquoise'
              )
            )
          } else if (length(input$scoreShop) == 3) {
            hc %>% hc_series(
              list(
                name = "店铺指数 - 1",
                data = c(
                  tmpTable$score.competition[1],
                  tmpTable$score.life[1],
                  tmpTable$score.traffic[1],
                  tmpTable$score.business[1],
                  tmpTable$score.development[1]
                ),
                pointPlacement = "on",
                color = 'steelblue'
              ),
              list(
                name = "店铺指数 - 2",
                data = c(
                  tmpTable$score.competition[2],
                  tmpTable$score.life[2],
                  tmpTable$score.traffic[2],
                  tmpTable$score.business[2],
                  tmpTable$score.development[2]
                ),
                pointPlacement = "on",
                color = 'lightblue'
              ),
              list(
                name = "店铺指数 - 3",
                data = c(
                  tmpTable$score.competition[3],
                  tmpTable$score.life[3],
                  tmpTable$score.traffic[3],
                  tmpTable$score.business[3],
                  tmpTable$score.development[3]
                ),
                pointPlacement = "on",
                color = 'blue'
              ),
              list(
                name = "区划指数",
                data = c(
                  round(mean(tmpTable2$score.competition), 0),
                  round(mean(tmpTable2$score.life), 0),
                  round(mean(tmpTable2$score.traffic), 0),
                  round(mean(tmpTable2$score.business), 0),
                  round(mean(tmpTable2$score.development) , 0)
                ),
                pointPlacement = "on",
                color = 'LightCoral'
              ),
              list(
                name = "城市指数",
                data = c(
                  round(mean(tmpTable3$score.competition), 0),
                  round(mean(tmpTable3$score.life), 0),
                  round(mean(tmpTable3$score.traffic), 0),
                  round(mean(tmpTable3$score.business), 0),
                  round(mean(tmpTable3$score.development) , 0)
                ),
                pointPlacement = "on",
                color = 'turquoise'
              )
            )
          } else
            (return(NULL))
        })
        
        output$networkScore <- renderDataTable({
          tmpTable <- if (input$scoreCity == '上海') {
            haierShanghaiTable %>%
              select(district, network, starts_with('score.'))
          } else{
            haierZhengzhouTable %>%
              select(district, network, starts_with('score.'))
          }
          scoreTable <-
            group_by(tmpTable, district, network) %>% summarize(
              life = round(mean(score.life)),
              traffic = round(mean(score.traffic)),
              business = round(mean(score.business)),
              development = round(mean(score.development)),
              competition = round(mean(score.competition)),
              place = round(mean(score.final))
            )
          scoreTable <-
            rename(
              scoreTable,
              '行政区' = 'district',
              '网格' = 'network',
              '信息' = 'place',
              '生活' = 'life',
              '交通' = 'traffic',
              '商业' = 'business',
              '发展' = 'development',
              '竞争' = 'competition'
            )
          
          datatable(scoreTable,
                    rownames = FALSE,
                    options = list(dom = 'ft')) %>%
            formatStyle(
              colnames(scoreTable)[-(1:2)],
              fontSize = '20px',
              backgroundColor = styleInterval(c(70, 80), c('grey', 'steelblue', 'green'))
            ) %>%
            formatStyle('信息',
                        color = 'gold',
                        fontWeight = 'bold',
                        fontSize = '20px') %>%
            formatStyle(
              colnames(scoreTable)[1:2],
              backgroundColor = 'black',
              color = 'white',
              fontSize = '20px'
            )
        })
        
        output$place <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict & name %in% input$scoreShop) %>%
              select(starts_with('score.'))
          } else{
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict & name %in% input$scoreShop
              ) %>%
              select(starts_with('score.'))
          }
          valueBox(
            subtitle = paste(unique(tmpTable$city), '信息中心指数', sep = ' '),
            value = mean(tmpTable$score.final),
            color = 'maroon',
            icon = icon('location-arrow')
          )
        })
        
        output$life <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict & name %in% input$scoreShop) %>%
              select(starts_with('score.'))
          } else{
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict & name %in% input$scoreShop
              ) %>%
              select(starts_with('score.'))
          }
          valueBox(
            subtitle = paste(unique(tmpTable$city), '生活中心指数', sep = ' '),
            value = mean(tmpTable$score.life),
            color = 'light-blue',
            icon = icon('building')
          )
        })
        
        output$traffic <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict & name %in% input$scoreShop) %>%
              select(starts_with('score.'))
          } else{
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict & name %in% input$scoreShop
              ) %>%
              select(starts_with('score.'))
          }
          valueBox(
            subtitle = paste(unique(tmpTable$city), '交通中心指数', sep = ' '),
            value = mean(tmpTable$score.traffic),
            color = 'light-blue',
            icon = icon('bus')
          )
        })
        
        output$business <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict & name %in% input$scoreShop) %>%
              select(starts_with('score.'))
          } else{
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict & name %in% input$scoreShop
              ) %>%
              select(starts_with('score.'))
          }
          valueBox(
            subtitle = paste(unique(tmpTable$city), '商业中心指数', sep = ' '),
            value = mean(tmpTable$score.business),
            color = 'light-blue',
            icon = icon('money')
          )
        })
        
        output$development <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict & name %in% input$scoreShop) %>%
              select(starts_with('score.'))
          } else{
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict & name %in% input$scoreShop
              ) %>%
              select(starts_with('score.'))
          }
          valueBox(
            subtitle = paste(unique(tmpTable$city), '发展中心指数', sep = ' '),
            value = mean(tmpTable$score.development),
            color = 'light-blue',
            icon = icon('line-chart')
          )
        })
        
        output$competition <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict & name %in% input$scoreShop) %>%
              select(starts_with('score.'))
          } else{
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict & name %in% input$scoreShop
              ) %>%
              select(starts_with('score.'))
          }
          valueBox(
            subtitle = paste(unique(tmpTable$city), '竞争中心指数', sep = ' '),
            value = mean(tmpTable$score.competition),
            color = 'light-blue',
            icon = icon('line-chart')
          )
        })
        
        output$scoreShopMT <- renderDataTable({
          if (input$scoreCity == '上海') {
            tmpTable <-
              filter(haierShanghaiTable,
                     district == input$scoreDistrict & name %in% input$scoreShop) %>%
              select(name, starts_with('score.'))
          } else{
            tmpTable <-
              filter(
                haierZhengzhouTable,
                district == input$scoreDistrict & name %in% input$scoreShop
              ) %>%
              select(name, starts_with('score.'))
          }
          tmpTable <-
            rename(
              tmpTable,
              '门店' = 'name',
              '信息' = 'score.final',
              '生活' = 'score.life',
              '交通' = 'score.traffic',
              '商业' = 'score.business',
              '发展' = 'score.development',
              '竞争' = 'score.competition'
            )
          datatable(
            tmpTable,
            rownames = FALSE,
            options = list(
              searching = FALSE,
              paging = FALSE,
              info = FALSE
            )
          ) %>%
            formatStyle(colnames(tmpTable)[-1],
                        fontSize = '22px',
                        backgroundColor = styleInterval(c(70, 80), c('grey', 'steelblue', 'green'))) %>%
            formatStyle('信息', color = 'gold', fontWeight = 'bold') %>%
            formatStyle(
              '门店',
              fontSize = '22px',
              backgroundColor = 'black',
              color = 'white'
            )
        })
        
        output$scoreBox <- renderUI({
          single <- list(
            valueBoxOutput('place', width = 2),
            valueBoxOutput('life', width = 2),
            valueBoxOutput('traffic', width = 2),
            valueBoxOutput('business', width = 2),
            valueBoxOutput('development', width = 2),
            valueBoxOutput('competition', width = 2)
          )
          multiple <- list(box(
            width = 12,
            dataTableOutput('scoreShopMT'),
            solidHeader = TRUE,
            background = 'black'
          ))
          if (length(input$scoreShop) > 1) {
            return(tagList(multiple))
          } else if (length(input$scoreShop == 1)) {
            return(tagList(single))
          } else{
            return(NULL)
          }
        })
        
        output$cityGDP <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <- select(haierShanghaiTable, starts_with('city')) %>% unique
          } else{
            tmpTable <-
              select(haierZhengzhouTable, starts_with('city')) %>% unique
          }
          valueBox(
            subtitle = paste(tmpTable$city, '国内生产总值', sep = ''),
            value = div(h3(paste(
              prettyNum(tmpTable$cityGDP, big.mark = ','), '亿元', sep = ''
            )),
            p(
              '同比增长 ', percent(tmpTable$cityGDPrRt)
            ),
            p(
              '近五年平均增长 ', percent(tmpTable$cityGDPArt)
            )),
            color = 'blue',
            icon = icon('area-chart')
          )
        })
        
        output$cityPop <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <- select(haierShanghaiTable, starts_with('city')) %>% unique
          } else{
            tmpTable <-
              select(haierZhengzhouTable, starts_with('city')) %>% unique
          }
          valueBox(
            subtitle = paste(tmpTable$city, '人口', sep = ''),
            value = div(h3(paste(
              prettyNum(tmpTable$cityPop, big.mark = ','), '万人', sep = ''
            )),
            p(
              '同比增长 ', percent(tmpTable$cityPoprRt)
            ),
            p(
              '近五年平均增长 ', percent(tmpTable$cityPopArt)
            )),
            color = 'blue',
            icon = icon('users')
          )
        })
        
        output$cityPprice <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <- select(haierShanghaiTable, starts_with('city')) %>% unique
          } else{
            tmpTable <-
              select(haierZhengzhouTable, starts_with('city')) %>% unique
          }
          valueBox(
            subtitle = paste(tmpTable$city, '住宅商品房均价', sep = ''),
            value = div(h3(paste(
              prettyNum(tmpTable$cityPprice, big.mark = ','),
              '元/平方米',
              sep = ''
            )),
            p(
              '同比增长 ', percent(tmpTable$cityPpricerRt)
            ),
            p(
              '近五年平均增长 ', percent(tmpTable$cityPpriceArt)
            )),
            color = 'blue',
            icon = icon('building')
          )
        })
        
        output$citySalary <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <- select(haierShanghaiTable, starts_with('city')) %>% unique
          } else{
            tmpTable <-
              select(haierZhengzhouTable, starts_with('city')) %>% unique
          }
          valueBox(
            subtitle = paste(tmpTable$city, '职工平均工资', sep = ''),
            value = div(h3(paste(
              prettyNum(tmpTable$citySalary, big.mark = ','), '元', sep = ''
            )),
            p(
              '同比增长 ', percent(tmpTable$citySalaryrRt)
            ),
            p(
              '近五年平均增长 ', percent(tmpTable$citySalaryArt)
            )),
            color = 'blue',
            icon = icon('money')
          )
        })
        
        output$mapPlace <- renderLeaflet({
          placeMap(city = input$scoreCity, shop = input$scoreShop)
        })
        
        output$districtTable <- renderDataTable({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable, district == input$scoreDistrict) %>%
              select(province, city, district, population, area, popoverarea) %>%
              unique
          } else{
            tmpTable <-
              subset(haierZhengzhouTable, district == input$scoreDistrict) %>%
              select(province, city, district, population, area, popoverarea) %>%
              unique
          }
          datatable(
            tmpTable,
            rownames = FALSE,
            options = list(
              scrollX = TRUE,
              searching = FALSE,
              paging = FALSE,
              info = FALSE
            )
          ) %>%
            formatStyle(colnames(tmpTable),
                        color = '#fff',
                        backgroundColor = 'black')
        })
        
        output$cityTable <- renderDataTable({
          if (input$scoreCity == '上海') {
            tmpTable <-
              select(haierShanghaiTable,
                     province,
                     city,
                     cityGDP,
                     cityGDPrRt,
                     cityGDPArt) %>% unique
          } else{
            tmpTable <-
              select(haierZhengzhouTable,
                     province,
                     city,
                     cityGDP,
                     cityGDPrRt,
                     cityGDPArt) %>% unique
          }
          datatable(
            tmpTable,
            rownames = FALSE,
            options = list(
              scrollX = TRUE,
              searching = FALSE,
              paging = FALSE,
              info = FALSE
            )
          ) %>%
            formatStyle(colnames(tmpTable),
                        color = '#fff',
                        backgroundColor = 'black') %>% formatPercentage(c('cityGDPrRt', 'cityGDPArt'), digits = 1)
        })
        
        output$cityDensity <- renderHighchart({
          if (input$scoreCity == '上海') {
            tmpTable <- shanghai_pop_area
          } else{
            tmpTable <- zhengzhou_pop_area
          }
          hchart(tmpTable,
                 "treemap",
                 hcaes(
                   x = division,
                   value = population,
                   color = popoverarea
                 )) %>%
            hc_plotOptions(treemap = list(
              `font-size` = '20px'
            ))%>%
            hc_legend(enabled = FALSE)
        })
        
        output$scoreBestWorst <- renderHighchart({
          if (input$scoreCity == '上海') {
            tmpTable <-
              select(haierShanghaiTable, city, shopCode, name, score.final) %>% unique
          } else{
            tmpTable <-
              select(haierZhengzhouTable, city, shopCode, name, score.final) %>% unique
          }
          tmpTable$score <- round(as.numeric(tmpTable$score.final), 2)
          bestTable <-
            arrange(tmpTable, desc(score)) %>% top_n(10) %>% mutate(color = 'turquoise', group = '得分最高')
          worstTable <-
            arrange(tmpTable, desc(score)) %>% top_n(-10) %>% mutate(color = 'LightCoral', group = '得分最低')
          tmpTable <-
            rbind(bestTable, worstTable) %>% select(name, score, color, group) %>% rename(y = score)
          highchart() %>%
            hc_chart(type = 'bar') %>%
            hc_xAxis(categories = tmpTable$name,
                     labels = list(style = list(
                       color = 'white', `font-size` = '15px'
                     ))) %>%
            hc_yAxis(labels = list(style = list(color = 'white'))) %>%
            hc_add_series(tmpTable, name = '信息指数得分') %>%
            hc_legend(enabled = FALSE)
        })
        
        output$cityDev1 <- renderHighchart({
          GDPTable <- subset(GDP_topCity_China, city == input$scoreCity)
          PopTable <- subset(Pop_topCity_China, city == input$scoreCity)
          highchart() %>%
            hc_chart(type = 'line') %>%
            hc_yAxis_multiples(
              list(
                lineWidth = 1,
                title = list(text = ''),
                labels = list(style = list(color = 'white'))
              ),
              list(
                lineWidth = 1,
                title = list(text = ''),
                labels = list(style = list(color = 'white')),
                opposite = TRUE
              )
            ) %>%
            hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
                       {point.y:,.2f}<br/>",
                       shared = TRUE) %>%
            hc_xAxis(categories = rev(colnames(GDPTable)[-1]),
                     labels = list(style = list(color = 'white'))) %>%
            hc_add_series(
              data = as.numeric(select(GDPTable, rev(
                colnames(GDPTable)[-1]
              ))),
              color = 'LightCoral',
              name = 'GDP',
              yAxis = 1
            ) %>%
            hc_add_series(data = as.numeric(select(PopTable, rev(
              colnames(PopTable)[-1]
            ))),
            color = 'turquoise',
            name = '人口') %>%
            hc_legend(itemStyle = list(color = 'white'))
        })
        
        output$cityDev2 <- renderHighchart({
          SalaryTable <- subset(Salary_topCity_China, city == input$scoreCity)
          PropertyPriceTable <-
            subset(PropertyPrice_topCity_China, city == input$scoreCity)
          highchart() %>%
            hc_yAxis_multiples(
              list(
                lineWidth = 1,
                title = list(text = ''),
                labels = list(style = list(color = 'white'))
              ),
              list(
                lineWidth = 1,
                title = list(text = ''),
                labels = list(style = list(color = 'white')),
                opposite = TRUE
              )
            ) %>%
            hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
                       {point.y:,.0f}<br/>",
                       shared = TRUE) %>%
            hc_xAxis(categories = rev(colnames(SalaryTable)[-1]),
                     labels = list(style = list(color = 'white'))) %>%
            hc_add_series(
              data = as.numeric(select(SalaryTable, rev(
                colnames(SalaryTable)[-1]
              ))),
              color = 'LightCoral',
              name = '平均工资',
              yAxis = 1
            ) %>%
            hc_add_series(data = as.numeric(select(PropertyPriceTable, rev(
              colnames(PropertyPriceTable)[-1]
            ))),
            color = 'SteelBlue',
            name = '住宅均价') %>%
            hc_legend(itemStyle = list(color = 'white'))
        })
        
        output$placeTable <- renderDataTable({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict & name == input$scoreShop) %>%
              select(province, city, district, shopCode, name)
          } else{
            tmpTable <-
              subset(haierZhengzhouTable,
                     district == input$scoreDistrict & name == input$scoreShop) %>%
              select(province, city, district, shopCode, name)
          }
          datatable(
            tmpTable,
            rownames = FALSE,
            options = list(
              scrollX = TRUE,
              searching = FALSE,
              paging = FALSE,
              info = FALSE
            )
          ) %>%
            formatStyle(colnames(tmpTable),
                        color = '#fff',
                        backgroundColor = 'black')
        })
        
        output$placeChart <- renderHighchart({
          if (input$scoreCity == '上海') {
            placeTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict & name == input$scoreShop)
          } else{
            placeTable <-
              subset(haierZhengzhouTable,
                     district == input$scoreDistrict & name == input$scoreShop)
          }
          placeName <-
            c(
              'resid_500',
              'resid_1000',
              'resid_5000',
              'shop_500',
              'shop_1000',
              'shop_5000',
              'mart_500',
              'mart_1000',
              'mart_5000',
              'gree_500',
              'gree_1000',
              'suning_500',
              'suning_1000',
              'gome_500',
              'gome_1000',
              'parking_500',
              'bank_500',
              'bank_1000',
              'bank_5000',
              'bus_500',
              'bus_1000'
            )
          if (input$scoreCity == '上海') {
            cityPlaceTb <-
              summarise_at(haierShanghaiTable, placeName, mean, na.rm = TRUE)
          } else{
            cityPlaceTb <-
              summarise_at(haierZhengzhouTable, placeName, mean, na.rm = TRUE)
          }
          
          if (input$scoreCity == '上海') {
            districtPlaceTb <-
              summarise_at(
                subset(haierShanghaiTable, district == input$scoreDistrict),
                placeName,
                mean,
                na.rm = TRUE
              )
          } else{
            districtPlaceTb <-
              summarise_at(
                subset(haierZhengzhouTable, district == input$scoreDistrict),
                placeName,
                mean,
                na.rm = TRUE
              )
          }
          
          highchart() %>%
            hc_chart(type = "line") %>%
            # hc_yAxis_multiples(
            #   list(lineWidth = 1),
            #   list(lineWidth = 1),
            #   list(lineWidth = 1)
            #   )%>%
            hc_yAxis(title = list(text = "Count")) %>%
            hc_xAxis(categories = placeName) %>%
            hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
                       {point.y:,.0f}<br/>",
                       shared = TRUE) %>%
            hc_add_series(data = as.numeric(select(placeTable, placeName)),
                          color = 'turquoise',
                          name = "Shop") %>%
            hc_add_series(data = as.numeric(select(cityPlaceTb, placeName)),
                          color = 'LightCoral',
                          name = "City") %>%
            hc_add_series(data = as.numeric(select(districtPlaceTb, placeName)),
                          color = 'SteelBlue',
                          name = "District")
        })
        
        output$comparisonScore <- renderUI({
          score <- select(finScoreDemo, name, score.final) %>% unique
          if (length(input$scoreShop3) == 1) {
            tagList(
              valueBox(
                value = filter(score, name == input$scoreShop3)$score.final,
                subtitle = input$scoreShop3,
                color = 'green'
              )
            )
          } else if (length(input$scoreShop3) == 2) {
            tagList(
              valueBox(
                value = filter(score, name == input$scoreShop3[1])$score.final,
                subtitle = input$scoreShop3[1],
                color = 'green'
              ),
              valueBox(
                value = filter(score, name == input$scoreShop3[2])$score.final,
                subtitle = input$scoreShop3[2],
                color = 'light-blue'
              )
            )
          } else if (length(input$scoreShop3) == 3) {
            tagList(
              valueBox(
                value = filter(score, name == input$scoreShop3[1])$score.final,
                subtitle = input$scoreShop3[1],
                color = 'green'
              ),
              valueBox(
                value = filter(score, name == input$scoreShop3[2])$score.final,
                subtitle = input$scoreShop3[2],
                color = 'light-blue'
              ),
              valueBox(
                value = filter(score, name == input$scoreShop3[3])$score.final,
                subtitle = input$scoreShop3[3],
                color = 'red'
              )
            )
          } else{
            return(NULL)
          }
        })
        
        output$revCompChart <- renderHighchart({
          table1 <- group_by(finScoreDemo, network, shopCode, name, ym) %>%
            summarize(
              ym_revenue = sum(revenue, na.rm = TRUE),
              ym_quantity = sum(quantity, na.rm = TRUE)
            ) %>% filter(ym != '2016-12')
          table2 <-
            group_by(finScoreDemo, name) %>% summarize(
              revenue = sum(revenue, na.rm = TRUE),
              quantity = sum(quantity, na.rm = TRUE)
            ) %>% mutate(color = c('turquoise', 'steelblue', 'lightcoral'))
          
          highchart() %>%
            #hc_chart(type = 'column') %>%
            hc_xAxis(categories = table1$ym,
                     labels = list(style = list(color = 'white'))) %>%
            hc_yAxis(title = list(text = '收入', style = list(color = 'white')),
                     labels = list(style = list(color = 'white'))) %>%
            hc_add_series(
              data = filter(table1, name == '河南清河工贸CBD店')$ym_revenue,
              type = 'column',
              color = 'turquoise',
              name = '河南清河工贸CBD店-收入'
            ) %>%
            hc_add_series(
              data = filter(table1, name == '郑州锐达CBD店')$ym_revenue,
              type = 'column',
              color = 'LightCoral',
              name = '郑州锐达CBD店-收入'
            ) %>%
            hc_add_series(
              data = filter(table1, name == '郑州丹尼斯人民路锐达店')$ym_revenue,
              type = 'column',
              color = 'SteelBlue',
              name = '郑州丹尼斯人民路锐达店-收入'
            ) %>%
            hc_add_series(
              data = rename(table2, y = revenue),
              type = 'pie',
              name = '收入占比',
              colorByPoint = TRUE,
              center = c(50, 10),
              size = 80,
              dataLabels = list(enabled = FALSE)
            ) %>%
            hc_legend(itemStyle = list(color = 'white'))
          
        })
        
        output$trafficCompChart <- renderHighchart({
          table1 <- group_by(finScoreDemo, network, shopCode, name, ym) %>%
            summarize(
              ym_revenue = sum(revenue, na.rm = TRUE),
              ym_quantity = sum(quantity, na.rm = TRUE)
            ) %>% filter(ym != '2016-12')
          
          highchart() %>%
            #hc_chart(type = 'column') %>%
            hc_xAxis(
              categories = table1$ym,
              startOnTick = FALSE,
              labels = list(style = list(color = 'white'))
            ) %>%
            hc_yAxis(title = list(text = '客流量', style = list(color = 'white')),
                     labels = list(style = list(color = 'white'))) %>%
            hc_add_series(
              data = filter(table1, name == '河南清河工贸CBD店')$ym_quantity,
              type = 'areaspline',
              color = 'turquoise',
              name = '河南清河工贸CBD店-客流量'
            ) %>%
            hc_add_series(
              data = filter(table1, name == '郑州锐达CBD店')$ym_quantity,
              type = 'areaspline',
              color = 'LightCoral',
              name = '郑州锐达CBD店-客流量'
            ) %>%
            hc_add_series(
              data = filter(table1, name == '郑州丹尼斯人民路锐达店')$ym_quantity,
              type = 'areaspline',
              color = 'SteelBlue',
              name = '郑州丹尼斯人民路锐达店-客流量'
            ) %>%
            hc_tooltip(split = TRUE) %>%
            hc_plotOptions(areaspline = list(
              stacking = 'normal',
              lineColor = 'white',
              lineWidth = 1
            )) %>%
            hc_legend(itemStyle = list(color = 'white'))
          
        })
        # output$revChart <- renderHighchart({
        #   if(input$scoreCity2 == '上海'){
        #     salesTable <- subset(haier_shanghai_d_sales, name == input$scoreShop2)
        #   }else{
        #     salesTable <- subset(haier_zhengzhou_d_sales, name == input$scoreShop2)
        #   }
        #
        #   #waring! for 2017-11-13 reporting use
        #   if(input$scoreShop2 %in% c('郑州丹尼斯人民路锐达店', '郑州锐达CBD店', '河南清河工贸CBD店')){
        #     salesTable <- mutate(salesTable, sales_date = sales_date + 365)
        #   }else{
        #     salesTable <- salesTable}
        #   highchart()%>%
        #     hc_title(text = paste(input$scoreCity2, input$scoreDistrict2, input$scoreShop2, sep = ' - '),
        #              style = list(color = '#FFF'))%>%
        #     hc_yAxis_multiples(
        #         list(lineWidth = 1, title = list(text = '销量'), labels = list(style = list(color = 'white'))),
        #         list(opposite = TRUE, title = list(text = '收入'), labels = list(style = list(color = 'white')))
        #     )%>%
        #     hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
        #             {point.y:,.0f}<br/>",
        #                shared = TRUE) %>%
        #     hc_xAxis(categories = salesTable$sales_date, labels = list(style = list(color = 'white')))%>%
        #     hc_add_series(data = as.numeric(salesTable$revenue), type = 'line', color = 'LightCoral', name = '收入', yAxis = 1)%>%
        #     hc_add_series(data = as.integer(salesTable$quantity), type ='column', color = 'turquoise', name = '销量')
        # })
        
        output$lyRevenue <- renderValueBox({
          if (input$scoreCity2 == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict2 & name == input$scoreShop2)
          } else if (input$scoreCity2 == '郑州') {
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict2 & name == input$scoreShop2
              )
          } else{
            tmpTable <- NULL
          }
          if (input$scoreShop2 %in% c('郑州丹尼斯人民路锐达店', '郑州锐达CBD店', '河南清河工贸CBD店')) {
            tmpTable <-
              filter(haier_zhengzhou_sales_com, name == input$scoreShop2)
          }
          col <- 'black'
          valueBox(
            value = prettyNum(tmpTable$ly_revenue, big.mark = ','),
            subtitle = '最近一年收入',
            color = col
          )
        })
        
        output$hyRevenue <- renderValueBox({
          if (input$scoreCity2 == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict2 & name == input$scoreShop2)
          } else if (input$scoreCity2 == '郑州') {
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict2 & name == input$scoreShop2
              )
          } else{
            tmpTable <- NULL
          }
          if (input$scoreShop2 %in% c('郑州丹尼斯人民路锐达店', '郑州锐达CBD店', '河南清河工贸CBD店')) {
            tmpTable <-
              filter(haier_zhengzhou_sales_com, name == input$scoreShop2)
          }
          col <- 'black'
          valueBox(
            value = prettyNum(tmpTable$hy_revenue, big.mark = ','),
            subtitle = '最近半年收入',
            color = col
          )
        })
        
        output$lmRevenue <- renderValueBox({
          if (input$scoreCity2 == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict2 & name == input$scoreShop2)
          } else if (input$scoreCity2 == '郑州') {
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict2 & name == input$scoreShop2
              )
          } else{
            tmpTable <- NULL
          }
          if (input$scoreShop2 %in% c('郑州丹尼斯人民路锐达店', '郑州锐达CBD店', '河南清河工贸CBD店')) {
            tmpTable <-
              filter(haier_zhengzhou_sales_com, name == input$scoreShop2)
          }
          col <- 'red'
          valueBox(
            value = prettyNum(tmpTable$lm_revenue, big.mark = ','),
            subtitle = '最近一个月收入',
            color = col
          )
        })
        
        output$lwRevenue <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict2 & name == input$scoreShop2)
          } else if (input$scoreCity == '郑州') {
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict2 & name == input$scoreShop2
              )
          } else{
            tmpTable <- NULL
          }
          if (input$scoreShop2 %in% c('郑州丹尼斯人民路锐达店', '郑州锐达CBD店', '河南清河工贸CBD店')) {
            tmpTable <-
              filter(haier_zhengzhou_sales_com, name == input$scoreShop2)
          }
          col <- 'red'
          valueBox(
            value = prettyNum(tmpTable$lw_revenue, big.mark = ','),
            subtitle = '最近一周收入',
            color = col
          )
        })
        
        output$lyQuantity <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict2 & name == input$scoreShop2)
          } else if (input$scoreCity == '郑州') {
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict2 & name == input$scoreShop2
              )
          } else{
            tmpTable <- NULL
          }
          if (input$scoreShop2 %in% c('郑州丹尼斯人民路锐达店', '郑州锐达CBD店', '河南清河工贸CBD店')) {
            tmpTable <-
              filter(haier_zhengzhou_sales_com, name == input$scoreShop2)
          }
          col <- 'black'
          valueBox(
            value = prettyNum(tmpTable$ly_quantity, big.mark = ','),
            subtitle = '最近一年销量',
            color = col
          )
        })
        
        output$hyQuantity <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict2 & name == input$scoreShop2)
          } else if (input$scoreCity == '郑州') {
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict2 & name == input$scoreShop2
              )
          } else{
            tmpTable <- NULL
          }
          if (input$scoreShop2 %in% c('郑州丹尼斯人民路锐达店', '郑州锐达CBD店', '河南清河工贸CBD店')) {
            tmpTable <-
              filter(haier_zhengzhou_sales_com, name == input$scoreShop2)
          }
          col <- 'black'
          valueBox(
            value = prettyNum(tmpTable$hy_quantity, big.mark = ','),
            subtitle = '最近半年销量',
            color = col
          )
        })
        
        output$lmQuantity <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict2 & name == input$scoreShop2)
          } else if (input$scoreCity == '郑州') {
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict2 & name == input$scoreShop2
              )
          } else{
            tmpTable <- NULL
          }
          if (input$scoreShop2 %in% c('郑州丹尼斯人民路锐达店', '郑州锐达CBD店', '河南清河工贸CBD店')) {
            tmpTable <-
              filter(haier_zhengzhou_sales_com, name == input$scoreShop2)
          }
          col <- 'red'
          valueBox(
            value = prettyNum(tmpTable$lm_quantity, big.mark = ','),
            subtitle = '最近一个月销量',
            color = col
          )
        })
        
        output$lwQuantity <- renderValueBox({
          if (input$scoreCity == '上海') {
            tmpTable <-
              subset(haierShanghaiTable,
                     district == input$scoreDistrict2 & name == input$scoreShop2)
          } else if (input$scoreCity == '郑州') {
            tmpTable <-
              subset(
                haierZhengzhouTable,
                district == input$scoreDistrict2 & name == input$scoreShop2
              )
          } else{
            tmpTable <- NULL
          }
          if (input$scoreShop2 %in% c('郑州丹尼斯人民路锐达店', '郑州锐达CBD店', '河南清河工贸CBD店')) {
            tmpTable <-
              filter(haier_zhengzhou_sales_com, name == input$scoreShop2)
          }
          col <- 'red'
          valueBox(
            value = prettyNum(tmpTable$lw_quantity, big.mark = ','),
            subtitle = '最近一周销量',
            color = col
          )
        })
      }
      
      shinyApp(ui, server)
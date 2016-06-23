setwd("C:/Users/Upamanyu/Documents/R Codes/Digitization/Dashboard")

rm(list=ls())

library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
library(rio)
library(magrittr)
library(highcharter)
library(DT)
library(htmlwidgets)
library(leaflet)
library(shiny)
library(shinythemes)

load("Cooked.RData")

x<-c("Members", "SHGs", "VOs", "VOs with Transactions", "VOs LIVE", "CLFs", "CLFs with Transactions", "CLFs LIVE")

Block_Facesheet<-mutate(Block_Facesheet, `Geography`=`Block`)%>%
  arrange(`District`, `Block`)
Block<-arrange(Block, `District`, `Block`)
District_Facesheet<-mutate(District_Facesheet, `Geography`=`District`)%>%
  arrange(`District`)
District<-arrange(District, `District`)

SHGs<-arrange(SHGs, `District`, `Block`, `CLF`, `VO`, `SHG`)
VOs<-arrange(VOs, `District`, `Block`, `CLF`, `VO`)
CLFs<-arrange(CLFs, `District`, `Block`, `CLF`)

ui<-shinyUI(navbarPage(h4(strong("JEEViKA Dashboard: Digitization")),
                       theme=shinytheme("cosmo"),
                       tabPanel(h6(strong("Home- Glance @ Digitization")),
                                sidebarLayout(
                                  sidebarPanel(
                                    width=3, 
                                    selectInput("glance", label="Select View", choices=c("By Project", "By District"), selected=NULL, multiple = FALSE),
                                    br(),                 
                                    selectInput("parametersglance", label="Select Topic", choices=c("Profiles", "Mapping", "Accounts", "Transactions", "Live"), selected=NULL, multiple = FALSE),
                                    br(),
                                    actionButton("triggerglance", "Glance It..", width="100%"),
                                    br(),
                                    br(),
                                    downloadButton("downloadglance", paste("Download @ A Glance: ",Sys.Date()))
                                  ),
                                  mainPanel(
                                    DT::dataTableOutput("glancetable", width = "100%", height = "auto")
                                  )
                                )
                       ),
                       tabPanel(h6(strong("Digitization Progress - GIS")),
                                sidebarLayout(
                                  sidebarPanel(
                                    width=3, 
                                    selectInput("mapindicator", label="Select Indicator", choices=x, multiple = FALSE),
                                    actionButton("triggermap", "Map It..", width="100%")
                                  ),
                                  mainPanel(leafletOutput("Map", width = "100%", height = 500)
                                  )
                                )
                       ),
                       tabPanel(h6(strong("Digitization Progress & Problems - Charts")),
                                sidebarLayout(
                                  sidebarPanel(width=3, 
                                               p("Select Geography"),
                                               br(),
                                               selectInput("districthc", label="Select District(s)", choices=District$`District`, selected=NULL, multiple = TRUE),
                                               br(),                 
                                               uiOutput("blockselect"),
                                               br(),
                                               selectInput("chartindicator", label="Select Institution", choices=c("SHG", "VO", "CLF"), selected=NULL, multiple = FALSE),
                                               actionButton("triggerchart", "Chart It..", width="100%")
                                  ),
                                  mainPanel(
                                    highchartOutput("hc",height = "500px")
                                  )
                                )
                       ),
                       tabPanel(h6(strong("Digitization Problems - Escalations")),
                                sidebarLayout(
                                  sidebarPanel(
                                    width=3, 
                                    p("Select Geography"),
                                    br(),
                                    selectInput("districtDT", label="Select District(s)", choices=District$`District`, selected=NULL, multiple = TRUE),
                                    br(),                 
                                    uiOutput("blockselectDT"),
                                    br(),
                                    selectInput("topicDT", label="Select Your Topic", choices=c("SHG Profiles", "SHG Accounts", "VO Profiles", "VO Accounts", "VO Transactions", "CLF Profiles", "CLF Accounts", "CLF Transactions"), selected=NULL, multiple = FALSE),
                                    actionButton("triggerDT", "Detail It..", width="100%"),
                                    br(),
                                    br(),
                                    downloadButton("downloadDT", paste("Download Escalations:",Sys.Date()))
                                  ),
                                  mainPanel(
                                    DT::dataTableOutput("table", width = "100%", height = "auto")
                                  )
                                )
                       ),
                       header=h6(p("Brought to you by ", strong(a("JEEViKA", href="http://223.30.251.85:8090/")), " and the ", strong(a("Social Observatory", href="http://so-prototype.webflow.io/"))),align="center")
))


server<-function(input, output, session){
  
  glancedatabase<-reactive({
    if(input$glance=="By Project"){
      switch(input$parametersglance,
             "Profiles"=select(Scheme_Facesheet, `Scheme`, `MPPR_SHGs`, `SHGs`, `Ratio SHGs`, `MPPR_VOs`, `VOs`, `Ratio VOs`, `MPPR_CLFs`, `CLFs`, `Ratio CLFs`),
             "Mapping"=select(Scheme_Facesheet, `Scheme`, `SHGs`, `Ratio Mapped SHGs`, `VOs`, `Ratio Mapped VOs`, `CLFs`, `Ratio Mapped CLFs`),
             "Accounts"=select(Scheme_Facesheet, `Scheme`, `SHGs`, `Ratio SHGs with Accounts`, `VOs`, `Ratio VOs with Accounts`, `CLFs`, `Ratio CLFs with Accounts`),
             "Transactions"=select(Scheme_Facesheet, `Scheme`, `VOs`, `VOs with Transactions`, `Ratio VOs with Transactions`,`CLFs`,  `CLFs with Transactions`, `Ratio CLFs with Transactions`),
             "Live"= select(Scheme_Facesheet, `Scheme`,`VOs`,  `VOs LIVE`, `Ratio VOs LIVE`,`CLFs`,  `CLFs LIVE`, `Ratio CLFs LIVE`)
      )
    } else {
      switch(input$parametersglance,
             "Profiles"=select(District_Facesheet, `District`, `MPPR_SHGs`, `SHGs`, `Ratio SHGs`, `Rank_Ratio SHGs`, `MPPR_VOs`, `VOs`, `Ratio VOs`, `Rank_Ratio VOs`, `MPPR_CLFs`, `CLFs`, `Ratio CLFs`, `Rank_Ratio CLFs`),
             "Mapping"=select(District_Facesheet, `District`, `SHGs`, `Ratio Mapped SHGs`, `Rank_Ratio Mapped SHGs`, `VOs`, `Ratio Mapped VOs`, `Rank_Ratio Mapped VOs`, `CLFs`, `Ratio Mapped CLFs`, `Rank_Ratio Mapped CLFs`),
             "Accounts"=select(District_Facesheet, `District`, `SHGs`, `Ratio SHGs with Accounts`, `Rank_Ratio SHGs with Accounts`, `VOs`, `Ratio VOs with Accounts`, `Rank_Ratio VOs with Accounts`, `CLFs`, `Ratio CLFs with Accounts`, `Rank_Ratio CLFs with Accounts`),
             "Transactions"=select(District_Facesheet, `District`,`VOs`,  `VOs with Transactions`, `Ratio VOs with Transactions`, `Rank_Ratio VOs with Transactions`,`CLFs`, `CLFs with Transactions`, `Ratio CLFs with Transactions`, `Rank_Ratio CLFs with Transactions`),
             "Live"= select(District_Facesheet, `District`,`VOs`,  `VOs LIVE`, `Ratio VOs LIVE`,  `Rank_Ratio VOs LIVE`,`CLFs`, `CLFs LIVE`, `Ratio CLFs LIVE`, `Rank_Ratio CLFs LIVE`)
      )
    } 
  })
  
  observe({
    input$triggerglance
    isolate({
      dataset<-glancedatabase()
      output$glancetable<-DT::renderDataTable(dataset, escape=TRUE, style="bootstrap")
      
      output$downloadglance<-downloadHandler(filename=function(){
        paste(input$parametersglance, " @ ", Sys.Date(), ".csv", sep=" ")},
        content = function(file) {
          export(dataset, format="csv", file)
        }
      )
    })
  })
  
  
  
  
  output$Map<-renderLeaflet({
    leaflet(DistrictGIS)%>%
      setView(86, 25.75, 7)%>%
      addProviderTiles("MapQuestOpen")
  })
  
  isolate({
    colorpal<-reactive({
      colorNumeric("YlOrRd", DistrictGIS@data[paste("Ratio ",input$mapindicator, sep="")])
    })
    
    text2<-reactive({
      paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "In MIS: ", DistrictGIS@data[,input$mapindicator], "<br>", "Percent Digitized ", DistrictGIS@data[,paste("Ratio ",input$mapindicator, sep="")]) 
    })
  })
  
  observe({
    input$triggermap
    isolate({
      pal<-colorpal()
      popuptext<-text2()
      
      leafletProxy("Map", data=DistrictGIS)%>%
        clearShapes%>%
        clearControls%>%
        clearMarkers%>%
        addPolygons(opacity=.5, weight=1, fillColor=pal(DistrictGIS@data[,paste("Ratio ",input$mapindicator, sep="")]), fillOpacity = .6)%>%
        addLegend(pal=pal, values=DistrictGIS@data[,paste("Ratio ",input$mapindicator, sep="")], "bottomright", bins=5)%>%
        addMarkers(lng=~V1, lat=~V2, popup=paste(popuptext))
    })
  })
  
  
  
  
  
  output$blockselect<-renderUI({
    if(is.null(input$districthc))
      return("Select District Above")
    if(length(input$districthc)>1)
      return("To Activate Block View, select only One District above")
    block_name<-filter(Block, `District`==input$districthc)%>%select(`Block`)
    selectInput("blockhc", "Select Block(s)", choices = block_name, multiple=TRUE, selected=NULL)
  })
  
  slicer<-isolate({
    reactive({
      if(!is.null(input$blockhc)){
        subset(Block_Facesheet, `District` %in% input$districthc & `Block` %in% input$blockhc)
      } else{
        subset(District_Facesheet, `District` %in% input$districthc)
      }
    })
  })
  
  
  output$hc<-renderHighchart({
    input$triggerchart
    isolate({
      content<-slicer()
      if(input$chartindicator=="SHG"){
        hc<-highchart()%>%
          hc_chart(type="column")%>%
          hc_add_theme(hc_theme_chalk())%>%
          hc_add_serie(name="Members", data=content$`Member`, color="white")%>%
          hc_add_serie(name="SHGs", data=content$`SHGs`, color="cornflowerblue")%>%
          hc_add_serie(name="Empty SHGs", data=content$`Empty SHGs`, color="red")%>%
          hc_add_serie(name="SHGs w/o Accounts", data=content$`SHGs wo Accounts`, color="red")%>%
          hc_xAxis(categories=content$`Geography`)
      } else {
        if(input$chartindicator=="VO"){
          hc<-highchart()%>%
            hc_chart(type="column")%>%
            hc_add_theme(hc_theme_chalk())%>%
            hc_add_serie(name="SHGs Federated", data=content$`SHGs Federated`, color="white")%>%
            hc_add_serie(name="VOs", data=content$`VOs`, color="cornflowerblue")%>%
            hc_add_serie(name="Empty VOs", data=content$`Empty VOs`, color="red")%>%
            hc_add_serie(name="VOs w/o Accounts", data=content$`VOs wo Accounts`, color="red")%>%
            hc_add_serie(name="VOs with Transactions", data=content$`VOs with Transactions`, color="plum")%>%
            hc_add_serie(name="VOs LIVE", data=content$`VOs LIVE`, color="chartreuse")%>%
            hc_add_serie(name="VOs w/o Transactions", data=content$`VOs wo Transactions`, color="red")%>%
            hc_add_serie(name="VOs LAG", data=content$`VOs in LAG`, color="red")%>%
            hc_xAxis(categories=content$`Geography`)
        } else {
          if(input$chartindicator=="CLF"){ 
            hc<-highchart()%>%
              hc_chart(type="column")%>%
              hc_add_theme(hc_theme_chalk())%>%
              hc_add_serie(name="VOs Federated", data=content$`VOs Federated`, color="white")%>%
              hc_add_serie(name="CLFs", data=content$`CLFs`, color="cornflowerblue")%>%
              hc_add_serie(name="Empty CLFs", data=content$`Empty CLFs`, color="red")%>%
              hc_add_serie(name="CLFs w/o Accounts", data=content$`CLFs wo Accounts`, color="red")%>%
              hc_add_serie(name="CLFs with Transactions", data=content$`CLFs with Transactions`, color="plum")%>%
              hc_add_serie(name="CLFs LIVE", data=content$`CLFs LIVE`, color="chartreuse")%>%
              hc_add_serie(name="CLFs w/o Transactions", data=content$`CLFs wo Transactions`, color="red")%>%
              hc_add_serie(name="CLFs LAG", data=content$`CLFs in LAG`, color="red")%>%
              hc_xAxis(categories=content$`Geography`)
          }
        }
      }
    })
  })
  
  
  
  
  output$blockselectDT<-renderUI({
    if(is.null(input$districtDT))
      return("Select District Above")
    if(length(input$districtDT)>1)
      return("To Activate Block View, select only One District above")
    block_name<-filter(Block, `District`==input$districtDT)%>%select(`Block`)
    selectInput("blockDT", "Select Block(s)", choices = block_name, multiple=TRUE, selected=NULL)
  })
  
  
  database<-reactive({
    if(is.null(input$districtDT)){
      switch(input$topicDT,
             "SHG Profiles"=filter(SHGs, `Alert Profile`!="OK") %>% select(`District`, `Block`, `CLF`, `VO`, `Village`, `SHG`, `SHG DoF`, `Alert Profile`, `Total Member`),
             "SHG Accounts"=filter(SHGs, `Alert Account`=="No Account") %>% select(`District`, `Block`, `CLF`, `VO`, `Village`, `SHG`, `SHG DoF`),
             "VO Profiles"=filter(VOs, `Alert Profile`!="OK") %>% select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Alert Profile`, `Total SHG`),
             "VO Accounts"=filter(VOs, `Alert Account`=="No Account") %>% select(`District`, `Block`, `CLF`, `VO`, `VO DoF`),
             "VO Transactions"=filter(VOs, `Alert Transactions`!="LIVE") %>% select(`District`, `Block`, `CLF`,`VO`, `VO DoF`, `Alert Transactions`, `Last Voucher VO`),
             "CLF Profiles"=filter(CLFs, `Alert Profile`!="OK") %>% select(`District`, `Block`, `CLF`, `CLF DoF`, `Alert Profile`, `Total VO`),
             "CLF Accounts"=filter(CLFs, `Alert Account`=="No Account") %>% select(`District`, `Block`, `CLF`, `CLF DoF`),
             "CLF Transactions"=filter(CLFs, `Alert Transactions`!="LIVE") %>% select(`District`, `Block`, `CLF`, `CLF DoF`, `Alert Transactions`, `Last Voucher CLF`)
      )
    } else {
      if(is.null(input$blockDT)){
        switch(input$topicDT,
               "SHG Profiles"=filter(SHGs, `District` %in% input$districtDT & `Alert Profile`!="OK") %>% select(`District`, `Block`, `CLF`, `VO`, `Village`, `SHG`, `SHG DoF`, `Alert Profile`, `Total Member`),
               "SHG Accounts"=filter(SHGs, `District` %in% input$districtDT & `Alert Account`=="No Account") %>% select(`District`, `Block`, `CLF`, `VO`, `Village`, `SHG`, `SHG DoF`),
               "VO Profiles"=filter(VOs, `District` %in% input$districtDT & `Alert Profile`!="OK") %>% select(`District`, `Block`, `CLF`,  `VO`, `VO DoF`, `Alert Profile`, `Total SHG`),
               "VO Accounts"=filter(VOs, `District` %in% input$districtDT & `Alert Account`=="No Account") %>% select(`District`, `Block`, `CLF`,  `VO`, `VO DoF`),
               "VO Transactions"=filter(VOs, `District` %in% input$districtDT & `Alert Transactions`!="LIVE") %>% select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Alert Transactions`, `Last Voucher VO`),
               "CLF Profiles"=filter(CLFs, `District` %in% input$districtDT & `Alert Profile`!="OK") %>% select(`District`, `Block`, `CLF`, `CLF DoF`, `Alert Profile`, `Total VO`),
               "CLF Accounts"=filter(CLFs, `District` %in% input$districtDT & `Alert Account`=="No Account") %>% select(`District`, `Block`, `CLF`, `CLF DoF`),
               "CLF Transactions"=filter(CLFs, `District` %in% input$districtDT & `Alert Transactions`!="LIVE") %>% select(`District`, `Block`, `CLF`, `CLF DoF`, `Alert Transactions`, `Last Voucher CLF`)
        )
      } else{
        switch(input$topicDT,
               "SHG Profiles"=filter(SHGs, `District` %in% input$districtDT & `Block` %in% input$blockDT & `Alert Profile`!="OK") %>% select(`District`, `Block`, `CLF`, `VO`, `Village`, `SHG`, `SHG DoF`, `Alert Profile`, `Total Member`),
               "SHG Accounts"=filter(SHGs, `District` %in% input$districtDT & `Block` %in% input$blockDT & `Alert Account`=="No Account") %>% select(`District`, `Block`, `CLF`, `VO`, `Village`, `SHG`, `SHG DoF`),
               "VO Profiles"=filter(VOs, `District` %in% input$districtDT & `Block` %in% input$blockDT & `Alert Profile`!="OK") %>% select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Alert Profile`, `Total SHG`),
               "VO Accounts"=filter(VOs, `District` %in% input$districtDT & `Block` %in% input$blockDT & `Alert Account`=="No Account") %>% select(`District`, `Block`, `CLF`,  `VO`, `VO DoF`),
               "VO Transactions"=filter(VOs, `District` %in% input$districtDT & `Block` %in% input$blockDT & `Alert Transactions`!="LIVE") %>% select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Alert Transactions`, `Last Voucher VO`),
               "CLF Profiles"=filter(CLFs, `District` %in% input$districtDT & `Block` %in% input$blockDT & `Alert Profile`!="OK") %>% select(`District`, `Block`, `CLF`, `CLF DoF`, `Alert Profile`, `Total VO`),
               "CLF Accounts"=filter(CLFs, `District` %in% input$districtDT & `Block` %in% input$blockDT & `Alert Account`=="No Account") %>% select(`District`, `Block`, `CLF`, `CLF DoF`),
               "CLF Transactions"=filter(CLFs, `District` %in% input$districtDT & `Block` %in% input$blockDT & `Alert Transactions`!="LIVE") %>% select(`District`, `Block`, `CLF`, `CLF DoF`, `Alert Transactions`, `Last Voucher CLF`)
        )
      }
    }
  })
  
  observe({
    input$triggerDT
    isolate({
      dataset<-database()
      output$table<-DT::renderDataTable(dataset, filter="top", escape=TRUE, style="bootstrap")
      
      output$downloadDT<-downloadHandler(filename=function(){
        paste(input$topicDT, " @ ", Sys.Date(), ".csv", sep=" ")},
        content = function(file) {
          export(dataset, format="csv", file)
        }
      )
    })
  }) 
  
  
}


shinyApp(ui = ui, server = server)

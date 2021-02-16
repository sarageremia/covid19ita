library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(readr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(gganimate)
library(gifski)
library(prettyunits)
library(dplyr)
library(plotly)
library(DT)
library(googleVis)
library(rvest)
library(httr)
library(readxl)
library(zoo)
library(tidyr)
library(RCurl)
library(wesanderson)
library(lubridate)
library(scales)

source(here::here("data-raw", "compute_change.R"))

# Data 
covid19 <- readRDS(file = here::here("data-raw", "covid19.rds")) 
country <- readRDS(file = here::here("data-raw", "country.rds"))
provinces <- readRDS(file = here::here("data-raw", "provinces.rds"))
# latest date
daily_date <- as.Date(max(covid19$date))
# x-axis length to give space for country/county name in plots
maxdays <- as.numeric(as.Date(max(covid19$date)) - as.Date(min(covid19$date))) + 24

# use round away from zero form of rounding (sometimes called banker's rounding)
round2 <- function(x, n = 0) (trunc((abs(x) * 10 ^ n) + 0.5) / 10 ^ n) * sign(x)

# Any JS functions
jsCode = 'shinyjs.clear_warning = function(){document.getElementById("note_save_confirm").innerHTML = "";}'

# Loading Spinner Settings
options(spinner.color="#ffd700", spinner.type = 4, spinner.color.background = 'white', 
        digits = 3, scipen = 99)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


# Header
header <- dashboardHeader(disable = TRUE)

# Sidebar
sidebar <- dashboardSidebar(disable=FALSE, width='190px',
                            # Custom CSS to hide the default logout panel
                            tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
                            tags$head(tags$style(HTML('.box.box-solid.box-primary>.box-header{ background-color: #D2D6DE; }'))),
                            tags$head(tags$style(HTML('.box.box-solid.box-primary{ border: 1px solid #D2D6DE;'))),
                            tags$head(tags$style(HTML('.box.box-primary{ border-top-color: #D2D6DE;}'))),
                            tags$head(tags$style(HTML('.box.box-solid.box-primary>.box-header>.box-title{ color: #242D32;}'))),
                            
                            tags$head(tags$style(HTML('.content-wrapper{ background-color: #c8c9c7; }'))),
                            tags$head(tags$style(HTML('.content-wrapper{ background-color: #ECF1F5; }'))),
                            ###
                            tags$head(tags$style(HTML('.irs-bar{ background: #ffd700; border-top: #ffd700; border-bottom: #ffd700;}'))),
                            tags$head(tags$style(HTML('.irs-bar-edge{ background: #ffd700; border: #ffd700;}'))),
                            tags$head(tags$style(HTML('.irs-single{ background: #888B8D;}'))),
                            tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs>li.active{ border-top-color: #888B8D;}'))),
                            
                            # Hide error messages while calculating predictions/plots
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            ),
                            # Add icons
                            tags$link(rel="stylesheet", href="https://fonts.googleapis.com/icon?family=Material+Icons"),
                            tags$link(rel = 'stylesheet', href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
                            #Lato Font
                            tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Lato'),
                            # JavaScript
                            tags$head(tags$script(src="script.js")),
                            # CSS
                            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style_sheet.css")),
                            # Extend JS
                            useShinyjs(),
                            extendShinyjs(text = jsCode, functions = 'clear_warning'),
                            
                            tags$head(tags$style(HTML('.skin-black .wrapper {background-color: 	#ECF1F5;}'))),
                            
                            ########
                            sidebarMenu(id = 'tabs',
                                        menuItem("Home", icon = icon("eye"), tabName = "indices"),
                                        menuItem("Epidemic", tabName = "trend", icon = icon("line-chart"), 
                                                 uiOutput("trend"), selected = TRUE,
                                        menuSubItem("National", icon = icon("flag"), 
                                                    tabName = "national"),
                                        menuSubItem("Provincial", icon = icon("location-arrow"),
                                                    tabName = "provincial")),
                                        menuItem("Map", icon = icon("map"), tabName = "map"), 
                                        
                                        selectInput("partition", paste0("Select Area (here below the data updated to ",
                                                                        max(covid19$date), "):"),
                                                    choices = unique(covid19$region_name),
                                                    selected = "Italy"),
                                        
                                        fluidRow(valueBoxOutput("CasesBox")),
                                        fluidRow(valueBoxOutput("MortBox")),
                                        fluidRow(valueBoxOutput("RecBox")),
                                        fluidRow(valueBoxOutput("SwabBox")),
                                        fluidRow(valueBoxOutput("ICUBox")),
                                        fluidRow(valueBoxOutput("HospBox"))
                                        )
)

                            


# Body
body <- dashboardBody(
  
  
  titlePanel("", windowTitle = "Covid-19 Italy"),
    
    # tabs
    tabItems(
        
        # Time series Tab #
        tabItem("indices", 
                h4(paste0("Data are made available by the"),
                   tags$a(href="http://www.protezionecivile.it/web/guest/home", 
                          " Presidenza del Consiglio dei Ministri - Dipartimento di Protezione Civile ",
                                 target="_blank"),
                   paste0("(Italian Civil Protection Department) and licensed under "),
                   tags$a(href="https://creativecommons.org/licenses/by/4.0/deed.it", "CC-BY-4.0", 
                          target="_blank"), 
                   paste0(" as provided by the Ministero della Salute (Ministry of Health).")),
                h2("Covid19 Italy: Day-to-day Trends In The Selected Area", align = "center"),
                plotlyOutput("day_to_day_plot"),
                plotlyOutput("daily_plot"),
                plotlyOutput("daily_swabs"),
                plotlyOutput("daily_icu")
                ),
        tabItem("national",
                h2("National Indeces", align = "center"),
                tabsetPanel(
                  tabPanel("Linear Scale",
                plotlyOutput("percentage_change_plot")
                ),
                tabPanel("Logarithmic Scale",
                            plotlyOutput("log_percentage_change_plot")
                )),
                fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                plotlyOutput("rate_deaths"),
                plotlyOutput("rate_recovered")
                )),
                plotlyOutput("ratio_swabs")
                ),
        
        tabItem("provincial",
                h2("Provincial Cases", align = "center"),
                h3("The five provinces reporting most cases"),
                h3(""),
                fluidRow(
                valueBoxOutput("ProvinceBox1", width = 2),
                valueBoxOutput("ProvinceBox2", width = 2),
                valueBoxOutput("ProvinceBox3", width = 2),
                valueBoxOutput("ProvinceBox4", width = 2),
                valueBoxOutput("ProvinceBox5", width = 2)
                ),
                selectInput("provinces", "Select the provinces to compare:",
                            choices = unique(provinces$province_name),
                            selected = tail(provinces[order(provinces$date, provinces$total_cases),], 5)$province_name,
                            multiple = TRUE),
                plotlyOutput("ProvincesTrend"),
                DT::DTOutput("ProvinceTab")
                ),
        tabItem("map",
                fluidRow(column(8, leafletOutput("map", height = 700)),
                  column(2, selectInput("event", "", choices = c("Total Cases", "Deceased Patients",
                                                                        "Hospital Discharged", "Total Positive"
                                                                        ),
                                               selected = "total_cases"),
                                align = "left"
                                     )))
    ) # end of tabItems
    
)# end of body

server <- function(input, output, session) {
    
    ################### Info boxes ##########
    ## Info box
    
    dataRaw <- reactive({
        covid19 %>% 
            filter(region_name == input$partition)
    })
    
    output$CasesBox <- renderValueBox({
        dat <- dataRaw()
        valueBox(
            max(dat$total_cases, na.rm = TRUE),
            paste0("Cases (+", dat$total_cases[which.max(dat$date)] - dat$total_cases[which.max(dat$date) - 1],
                   ")"),
            color = "maroon"        )
    })
    
    ## mort box
    output$MortBox <- renderValueBox({
        dat <- dataRaw()
        valueBox(
            max(dat$deceased_patients, na.rm=TRUE),
            paste0("Deaths (+", dat$deceased_patients[which.max(dat$date)] - dat$deceased_patients[which.max(dat$date) - 1],
                   ")"),
            color = "black"
        )
    })  
    
    ## disc box
    output$RecBox <- renderValueBox({
        dat <- dataRaw()
        valueBox(
            max(dat$hospital_discharged, na.rm=TRUE),
            paste0("Recovered (", signs::signs(dat$hospital_discharged[which.max(dat$date)] - dat$hospital_discharged[which.max(dat$date) - 1],
                                               add_plusses = T),
                   ")"),
            color = "olive"
        )
    })  
    
    
    
    ## hosp box
    output$HospBox <- renderValueBox({
        dat <- dataRaw()
        valueBox(
            max(dat$total_hospitalized, na.rm=TRUE),
            paste0("Hospitalized (", signs::signs(dat$total_hospitalized[which.max(dat$date)] - dat$total_hospitalized[which.max(dat$date) - 1],
                                                  add_plusses = T),
                   ")"),
            color = "purple"
        )
    })
    
    ## icu box
    output$ICUBox <- renderValueBox({ 
        dat <- dataRaw()
        valueBox(
            max(dat$intensive_care, na.rm=TRUE),
            paste0("Intensive care (", signs::signs(dat$intensive_care[which.max(dat$date)] - dat$intensive_care[which.max(dat$date) - 1],
                                                    add_plusses = T),
                   ")"),
            color = "fuchsia"
        )
    })
    
    ## swabs total box
    output$SwabBox <- renderValueBox({ 
        dat <- dataRaw()
        valueBox(
            max(dat$swabs, na.rm=TRUE),
            paste0("Swabs (+", dat$swabs[which.max(dat$date)] - dat$swabs[which.max(dat$date)- 1], 
                   ")"),
            color = "teal"
        )
    })
    
    
    ### day-to-day trends
    output$day_to_day_plot <- renderPlotly({
        
        dat <- dataRaw()
        
        ggplotly(
        ggplot(dat, aes(x = as.POSIXct(date))) +
            geom_line(aes(y = total_cases, color = "Total Cases"), size = .5) +
            geom_line(aes(y = total_positive, color = "Total Positive"), size = .5) +
            geom_line(aes(y = hospital_discharged, color = "Hospital Discharged"), size = .5) +
            geom_line(aes(y = deceased_patients, color = "Deceased Patients"), size = .5) +
            labs(x = "Date", y = "N", title = "Cumulative events", color = "") +
            theme(plot.title = element_text(size=13)) +
            scale_x_datetime(date_labels = "%d %b", date_breaks = "14 day") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 70, vjust = 0.5)) + 
            scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
            scale_color_manual(values = c("Total Cases" = "#FF0066", "Total Positive" = "pink",
                                          "Hospital Discharged" = "springgreen3", 
                                          "Deceased Patients" = "black"))
    )
    })
    
    ### daily trends
    output$daily_plot <- renderPlotly({
        
        dat <- dataRaw() 
        dat <- compute_change(dat, dat$total_cases)
        colnames(dat)[length(dat)] <- "change_total_cases"
        dat <- compute_change(dat, dat$hospital_discharged)
        colnames(dat)[length(dat)] <- "change_hospital_discharged"
        dat <- compute_change(dat, dat$deceased_patients)
        colnames(dat)[length(dat)] <- "change_deceased_patients"
        
        ggplotly(
        ggplot(dat, aes(x = as.POSIXct(date))) +
            geom_line(aes(y = change_total_cases, color = "Total Cases"), size = .5) +
            geom_line(aes(y = change_total_positive, color = "Total Positive"), size = .5) +
            geom_line(aes(y = change_hospital_discharged, color = "Hospital Discharged"), size = .5) +
            geom_line(aes(y = change_deceased_patients, color = "Deceased Patients"), size = .5) +
            labs(x = "Date", y = "N", title = "Daily events", color = "") +
            theme(plot.title = element_text(size=13)) +
            scale_x_datetime(date_labels = "%d %b", date_breaks = "14 day") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 70, vjust = 0.5)) + 
            scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
            scale_color_manual(values = c("Total Cases" = "#FF0066", "Total Positive" = "pink",
                                          "Hospital Discharged" = "springgreen3", 
                                          "Deceased Patients" = "black")) 
    )
    })
    
    ### daily swabs
    output$daily_swabs <- renderPlotly({
        
        dat <- dataRaw() 
        dat <- compute_change(dat, dat$total_cases)
        colnames(dat)[length(dat)] <- "change_total_cases"
        dat <- compute_change(dat, dat$swabs)
        colnames(dat)[length(dat)] <- "change_swabs"
        
        ggplotly(
        ggplot(dat, aes(x = as.POSIXct(date))) +
            geom_col(aes(y = change_swabs, fill = "Swabs Performed"), size = .5,
                     alpha = 0.3)+
            geom_col(aes(y = change_total_cases, fill = "Total Cases"), size = .5) +
            labs(x = "Date", y = "N", title = "Daily swabs and new cases", fill = "") +
            theme(plot.title = element_text(size=13)) +
            scale_x_datetime(date_labels = "%d %b", date_breaks = "14 day") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 70, vjust = 0.5)) + 
            scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
            scale_fill_manual(values = c("Swabs Performed" = "cyan2", 
                                          "Total Cases" = "#FF0066"))
    )
    })
    
    ### daily icu
    output$daily_icu <- renderPlotly({
        
        dat <- dataRaw() 
        dat <- compute_change(dat, dat$intensive_care)
        colnames(dat)[length(dat)] <- "change_intensive_care"
        dat <- compute_change(dat, dat$total_hospitalized)
        colnames(dat)[length(dat)] <- "change_total_hospitalized"
        dat <- compute_change(dat, dat$home_isolation)
        colnames(dat)[length(dat)] <- "change_home_isolation"
        
        ggplotly(
            ggplot(dat, aes(x = as.POSIXct(date))) +
                geom_col(aes(y = change_intensive_care, fill = "Intensive Care"), size = .5)+
                geom_col(aes(y = change_total_hospitalized, fill = "Hospitalized"), size = .5, 
                         alpha = 0.7) +
                geom_col(aes(y = change_home_isolation, fill = "Home Isolation"), size = .5, 
                         alpha = 0.5) +
                labs(x = "Date", y = "N", title = "Daily intensive cares and hospitalizations", 
                     fill = "") +
                theme(plot.title = element_text(size=13)) +
                scale_x_datetime(date_labels = "%d %b", date_breaks = "14 day") +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 70, vjust = 0.5)) + 
                scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
                scale_fill_manual(values = c("Intensive Care" = "#FF00FF", 
                                             "Hospitalized" = "slateblue4",
                                             "Home Isolation" = "mediumpurple1"))
        )
    })
    
    ### daily percentage change
    output$percentage_change_plot <- renderPlotly({
        
        country <- compute_change(country, country$total_cases, perc = T)
        colnames(country)[length(country)] <- "perchange_total_cases"
        country <- compute_change(country, country$hospital_discharged, perc = T)
        colnames(country)[length(country)] <- "perchange_hospital_discharged"
        country <- compute_change(country, country$deceased_patients, perc = T)
        colnames(country)[length(country)] <- "perchange_deceased_patients"
        country <- compute_change(country, country$total_positive, perc = T)
        colnames(country)[length(country)] <- "perchange_total_positive"
        
        ggplotly(
        ggplot(country, aes(x = as.POSIXct(date))) +
            geom_line(aes(y = perchange_total_cases, color = "Total Cases"), size = .5) +
            geom_line(aes(y = perchange_total_positive, color = "Total Positive"), size = .5) +
            geom_line(aes(y = perchange_hospital_discharged, color = "Hospital Discharged"), size = .5) +
            geom_line(aes(y = perchange_deceased_patients, color = "Deceased Patients"), size = .5) +
            labs(x = "Date", y = "%", title = "Daily percentage variation of events", color = "") +
            theme(plot.title = element_text(size=13)) +
            theme_bw() +
            scale_x_datetime(date_labels = "%d %b", date_breaks = "14 day") +
            theme(axis.text.x = element_text(angle = 70, vjust = 0.5)) +
            scale_color_manual(values = c("Total Cases" = "#FF0066", "Total Positive" = "pink",
                                          "Hospital Discharged" = "springgreen3", 
                                          "Deceased Patients" = "black"))
    )
    })
    
    ### log daily percentage change
    output$log_percentage_change_plot <- renderPlotly({
      
      country <- compute_change(country, country$total_cases, perc = T)
      colnames(country)[length(country)] <- "perchange_total_cases"
      country <- compute_change(country, country$hospital_discharged, perc = T)
      colnames(country)[length(country)] <- "perchange_hospital_discharged"
      country <- compute_change(country, country$deceased_patients, perc = T)
      colnames(country)[length(country)] <- "perchange_deceased_patients"
      country <- compute_change(country, country$total_positive, perc = T)
      colnames(country)[length(country)] <- "perchange_total_positive"
      
      ggplotly(
        ggplot(country, aes(x = as.POSIXct(date))) +
          geom_line(aes(y = log(perchange_total_cases), color = "Total Cases"), size = .5) +
          geom_line(aes(y = log(perchange_hospital_discharged), color = "Hospital Discharged"), size = .5) +
          geom_line(aes(y = log(perchange_deceased_patients), color = "Deceased Patients"), size = .5) +
          labs(x = "Date", y = "", title = "", color = "") +
          theme(plot.title = element_text(size=13)) +
          theme_bw() +
          scale_x_datetime(date_labels = "%d %b", date_breaks = "14 day") +
          theme(axis.text.x = element_text(angle = 70, vjust = 0.5)) + 
          scale_color_manual(values = c("Total Cases" = "#FF0066", 
                                        "Hospital Discharged" = "springgreen3", 
                                        "Deceased Patients" = "black"))
      )
    })
    
    
    ### Map
    
    data_tc <-
      covid19 %>%
      select(
        date, region_name, total_cases,
        longitude, latitude, inhabitants
      ) %>%
      filter(
        region_name != "Italy",
        date == max(covid19$date)
      ) %>%
      group_by(region_name) %>%
      mutate(
        rate = round(total_cases / inhabitants * 1e5),
        event = "Total Cases",
        total = total_cases
      )
    
    data_tp <-
      covid19 %>%
      select(
        date, region_name, total_positive,
        longitude, latitude, inhabitants
      ) %>%
      filter(
        region_name != "Italy",
        date == max(covid19$date)
      ) %>%
      group_by(region_name) %>%
      mutate(
        rate = round(total_positive / inhabitants * 1e5),
        event = "Total Positive"
      )
    
    data_dp <-
      covid19 %>%
      select(
        date, region_name, deceased_patients,
        longitude, latitude, inhabitants
      ) %>%
      filter(
        region_name != "Italy",
        date == max(covid19$date)
      ) %>%
      group_by(region_name) %>%
      mutate(
        rate = round(deceased_patients / inhabitants * 1e5),
        event = "Deceased Patients"
      )
    
    data_hd <-
      covid19 %>%
      select(
        date, region_name, hospital_discharged,
        longitude, latitude, inhabitants
      ) %>%
      filter(
        region_name != "Italy",
        date == max(covid19$date)
      ) %>%
      group_by(region_name) %>%
      mutate(
        rate = round(hospital_discharged / inhabitants * 1e5),
        event = "Hospital Discharged"
      )
    
    dataMap <- reactive({
        bind_rows(data_tc, data_tp, data_dp, data_hd) %>% 
            filter(event == input$event)
    })
    
    output$map <-  renderLeaflet({
        
        datmap <- dataMap()
        
        labs <- lapply(seq(nrow(datmap)), function(i) {
            paste0( '<p>', datmap[i, "region_name"], ": ", round(datmap[i, "rate"]),
                    " ", 'per 100 000 inhabitants', 
                    '</p><p>')
        })
        
        col <- if(input$event == "Total Cases"){
                "#FF0066"
            } else if(input$event == "Total Positive"){
                "pink"
            } else if(input$event == "Hospital Discharged"){
                "#00BE6C"
            } else if(input$event == "Deceased Patients"){
                "black"
            }
        cutoff <- quantile(datmap$rate, probs = c(0.25,0.5,0.75))             
        
        datmap <- datmap  %>% 
            mutate(case_groups = case_when(rate < cutoff[1] ~ 0.3,
                                           rate >=  cutoff[1] & 
                                               rate < cutoff[2] ~ 0.5,
                                           rate >= cutoff[2] & 
                                               rate < cutoff[3] ~ 0.7,
                                           rate >= cutoff[3] ~ 0.9))
        
        datmap %>%
            leaflet() %>%
            addTiles(
                # urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                # attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lng = 12.5, lat = 42, zoom = 6) %>%
            clearShapes() %>%
            addCircleMarkers(lng= ~longitude, 
                             lat= ~latitude, 
                             layerId = ~region_name,
                             radius = ~3*log(rate+1),
                             color = ~col,
                             label = lapply(labs, htmltools::HTML),
                             fillOpacity = ~case_groups,
                             opacity = ~case_groups)
    })
    
    
    ### Box provincial cases
    
    datbox_p <- provinces %>% 
        filter(date %in% c(max(provinces$date), max(provinces$date) - 1)) 
    datbox_p <- datbox_p[order(datbox_p$date, -datbox_p$total_cases),]
    datbox_p <- filter(datbox_p, province_code %in% head(datbox_p, 5)$province_code)
        
    
    output$ProvinceBox1 <- renderValueBox({
        valueBox(
            datbox_p[6, "province_name"],
            paste0(datbox_p[6, "total_cases"], "(+ ", 
                   datbox_p[6, "total_cases"] - datbox_p[1, "total_cases"], 
                   ")"),
            color = "maroon"
        )
    })
    
    output$ProvinceBox2 <- renderValueBox({
        valueBox(
            datbox_p[7, "province_name"],
            paste0(datbox_p[7, "total_cases"], "(+ ", 
                   datbox_p[7, "total_cases"] - datbox_p[2, "total_cases"], 
                   ")"),
            color = "maroon"
        )
    })
    output$ProvinceBox3 <- renderValueBox({
        valueBox(
            datbox_p[8, "province_name"],
            paste0(datbox_p[8, "total_cases"], "(+ ", 
                   datbox_p[8, "total_cases"] - datbox_p[3, "total_cases"], 
                   ")"),
            color = "maroon"
        )
    })
    output$ProvinceBox4 <- renderValueBox({
        valueBox(
            datbox_p[9, "province_name"],
            paste0(datbox_p[9, "total_cases"], "(+ ", 
                   datbox_p[9, "total_cases"] - datbox_p[4, "total_cases"], 
                   ")"),
            color = "maroon"
        )
    })
    output$ProvinceBox5 <- renderValueBox({
        valueBox(
            datbox_p[10, "province_name"],
            paste0(datbox_p[10, "total_cases"], "(+ ", 
                   datbox_p[10, "total_cases"] - datbox_p[5, "total_cases"], 
                   ")"),
            color = "maroon"
        )
    })
    
    ### plot provincial cases
    
    output$ProvincesTrend <- renderPlotly({
        
        ggplotly(
            provinces %>%
              filter(province_name %in% input$provinces) %>%
              ggplot(aes(x = as.POSIXct(date), y = total_cases)) +
              geom_line(aes(color = province_name), size = .5) +
              labs(x = "Date", y = "N", title = "Cumulative events", color = "Provinces") +
              theme(plot.title = element_text(size = 13)) +
              theme_bw() +
              scale_x_datetime(date_labels = "%d %b", date_breaks = "14 day") +
              theme(axis.text.x = element_text(angle = 70, vjust = 0.5)) +
              scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
              scale_color_brewer(palette = "PuRd")
                    )
    })
    
    ### table provincial cases
    
    DataTab1 <- covid19 %>% 
      filter(date %in% c(max(covid19$date), max(covid19$date) - 1)) %>% 
      arrange(region_name, desc(date))
    DataTab1 <- compute_change(DataTab1, DataTab1$total_cases) %>% 
      filter(date == max(DataTab1$date)) %>% 
      mutate(change = -change,
             province_name = region_name)  %>% 
      select(region_name, province_name, total_cases, change)
    
    DataTab2 <- provinces %>% 
      filter(date %in% c(max(provinces$date), max(provinces$date) - 1)) %>% 
      arrange(province_name, desc(date))
    DataTab2 <- compute_change(DataTab2, DataTab2$total_cases) %>% 
      filter(province_code <= 112, date == max(DataTab2$date)) %>% 
      mutate(change = -change) %>% 
      select(region_name, province_name, total_cases, change)
    
    DataTab <- bind_rows(DataTab1, DataTab2) %>% 
      arrange(region_name) %>% 
      select(-region_name)
    colnames(DataTab) <- c("Place", "TotalCases", "NewCases")
    
    output$ProvinceTab <- DT::renderDT({
      DataTab
    })
    
    ### rate swabs
    output$ratio_swabs <- renderPlotly({
        
        dat <- compute_change(country, country$total_cases)
        colnames(dat)[length(dat)] <- "change_total_cases"
        dat <- compute_change(dat, dat$swabs)
        colnames(dat)[length(dat)] <- "change_swabs"
        
        
        ggplotly(
            ggplot(dat, aes(x = as.POSIXct(date))) +
                geom_col(aes(y = change_swabs / 1e4), size = .5,
                         alpha = 0.3, fill = "cyan2")+
                geom_col(aes(y = change_total_cases / 1e4), size = .5, 
                         fill = "#FF0066") +
                geom_line(aes(y = (change_total_cases / change_swabs * 100)),
                          color = "black") +
                labs(x = "Date", y = "Ratio (%)", title = "New cases, daily swabs and ratio (%)", 
                     fill = "", linetype = "") +
                theme(plot.title = element_text(size=13)) +
                scale_x_datetime(date_labels = "%d %b", date_breaks = "14 day") +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 70, vjust = 0.5))
        )
    })
    
    ### rate deths 
    
    output$rate_deaths <- renderPlotly({
      
      ggplotly(
      
      ggplot(country, aes(x = as.POSIXct(date))) +
        geom_col(aes(y = (deceased_patients / total_cases)),
                      fill = "black", size = .5) +
        labs(x = "Date", y = "Proportion", title = "Deceased over total cases") +
        theme(plot.title = element_text(size=13)) +
        scale_x_datetime(date_labels = "%d %b", date_breaks = "14 day") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 70, vjust = 0.5)) 
      )
    })
    
    ### rate recovered 
    
    output$rate_recovered <- renderPlotly({
      
      ggplot(country, aes(x = as.POSIXct(date))) +
        geom_col(aes(y = (hospital_discharged / total_hospitalized)),
                  fill = "springgreen3", size = .5) +
        labs(x = "Date", y = "Proportion", title = "Recovered over total hospitalized") +
        theme(plot.title = element_text(size=13)) +
        scale_x_datetime(date_labels = "%d %b", date_breaks = "14 day") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 70, vjust = 0.5)) 
    })
    
    
    

}



# Run the application 
shinyApp(
    ui = dashboardPage(header, sidebar, body, skin = 'black'),
    server = server
)


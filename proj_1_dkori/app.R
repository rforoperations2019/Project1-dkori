#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(httr)
library(RCurl)
library(scales)
library(readr)
library(plotly)
library(shinytest)
# Avoid plotly issues ----------------------------------------------
pdf(NULL)


#load data
#testing out if read_csv combined with GET will work for dashboards
remote_data<-getURL('https://raw.githubusercontent.com/fivethirtyeight/guns-data/master/full_data.csv')
#read in gun data
guns<-read_csv(remote_data)%>%
  #remove rows where intent is null
  filter(!is.na(intent))
#save(guns,file=".RData")
#load(".RData")
# #write guns to a static file in this folder
# write_csv(guns,"proj_1_dkori/gun_deaths.csv")

#read in gun death data from static file
# guns<-read_csv("gun_deaths.csv")
#concatenate month and year
guns$date<-paste(guns$year,guns$month,'01',sep=' ')
#create a datetime variable out of the date variable
guns$datetime<-as.Date(guns$date,format='%Y %m %d')
#Make Police involvement categorical
guns$`Police Involved?`=ifelse(guns$police==1,"Yes","No")
#expand name for sex
guns$Sex<-ifelse(guns$sex=="M","Male","Female")
test<-max(guns$datetime,na.rm=TRUE)-min(guns$datetime,na.rm=TRUE)

header <- dashboardHeader(title = "Gun Violence Deaths"
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Deaths over time", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Breakdown", icon = icon("pie-chart"), tabName = "plot2"),
    menuItem("Data Table", icon = icon("table"), tabName = "table"),
    
    #create input to select date range
    sliderInput("start_date",
                "Select earliest date:",
                min = min(guns$datetime),
                max = max(guns$datetime),
                value = c(min(guns$datetime),max(guns$datetime))),
    #input to select whether or not suicides are included (since those are the majority of deaths)
    checkboxInput("Checkbox", "Include Suicides?",
                  value = TRUE),
    #create dropdown for user to select series grouping
    selectInput("series_choice", "Choose Series:",
                c("Race" = "race",
                  "Sex" = "Sex",
                  "Location" = "place",
                  "Education" = "education",
                  "Police Involved?" = "Police Involved?"),
                selected="Location")
    )
  )


# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Timeseries plot page ----------------------------------------------
  tabItem("plot", class="active",
          
          # Input and Value Boxes ----------------------------------------------
 
            valueBoxOutput("deaths"),
            valueBoxOutput("timespan"),
            valueBoxOutput("deaths_per_month"),
          
          # Plot ----------------------------------------------
          #fluidRow(
            box(title = "Deaths over Time",
                   width = 12,
                   plotlyOutput("line_plot"))
          #)
          ),
  
  #Breakdown plot page
  tabItem("plot2", class = "active",
          
          # Plot ----------------------------------------------
            tabBox(title = "Breakdown",
                   width = 12,
                   tabPanel("By Age", plotlyOutput(outputId = "bar_plot")),
                   tabPanel("Seasonality",plotlyOutput(outputId="seasonality")),
                   tabPanel("Overall Share",plotOutput(outputId="donut")))
          
  ),

  # Table Page ----------------------------------------------
    tabItem("table", class = "active",
            box(title = "Data Table", DT::dataTableOutput("datatab")))
)
)

# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)


# Create plots in the server function
server <- function(input, output) {
  
  #store series choice as a string var
  series_choice<-reactive({paste0(input$series_choice)})
  #first subset of guns, whether or not suicides are included
  guns_subset<-reactive({
    #check if they have selected the "include suicides" checkbox
    subset1<-if(!input$Checkbox){
      #not selected, remove suicides
      guns%>%
        filter(intent!="Suicide")
    }else{
      #if selected, keep guns the same
      guns
    }
    subset2<-subset1[,c("datetime","month", "age",series_choice())]%>%
      #limit subset to chosen dates
      #start date
      filter(datetime>=input$start_date[1])%>%
      #end date
      filter(datetime<=input$start_date[2])
    #rename columns (only way I know how to do this when selecting column through var)
    names(subset2)<-c("datetime","month", "age", "series_choice")
    return(subset2)
    })

  
  output$datatab<-DT::renderDataTable(
    DT::datatable(guns_subset()%>%
                    #count rows for chosen series
                    group_by(series_choice)%>%
                    count(name="value")%>%
                    #undue grouping
                    ungroup()%>%
                    #have deaths display as comma
                    mutate(Deaths=comma(value))%>%
                    #change name of series_choice
                    mutate(" " = series_choice)%>%
                    dplyr::select(` `, `Deaths`)
                  ,
                  rownames=FALSE)
  )
  
  #create line_plot in ggplot
  output$line_plot <- renderPlotly({

    #create the line plot using ggplot2
    lp<-guns_subset()%>%
      #count observations by date
      group_by(datetime,series_choice)%>%
      count(name="value")%>%
      #create line plot
      ggplot(aes(x=datetime,y=value,color=series_choice))+
      geom_line()+
      #remove grey background
      theme_minimal()+
      #label chart
      labs(x="Month",y="Number of fatalities",color=names(input$series_choice),
           title="Gun Deaths Over Time")+
      scale_y_continuous(label=comma)
    lp
    
  })
  
  
  #create donut plot 
  output$donut<-renderPlot({

    guns_subset()%>%
      group_by(series_choice)%>%
      #role up by chosen series
      count(name="value")%>%
      ungroup()%>%
      mutate(ymax=cumsum(value),ymin=c(0,head(ymax,n=-1)))%>%
      ggplot(aes(fill=series_choice,ymin=ymin,ymax=ymax,xmin=3,xmax=5,label=series_choice),colour="white")+
      #make chart rectangular
      geom_rect()+
      #convert rectangular chart to polar (donut)
      coord_polar(theta="y")+
      xlim(c(0,5))+
      #remove background
      theme_void() +
      #remove x and y value labels
      xlab("")+
      ylab("")+
      #remove other aesthetic elements of chart
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      #center title
      theme(plot.title = element_text(hjust = 0.5))+
      #place legend on bottom with no legend title
      theme(legend.title=element_blank())+
      theme(legend.position="bottom")+
      #resize legend items to fit well
      guides(fill=guide_legend(nrow=2,byrow=TRUE,keywidth=.4,keyheight=.2,default.unit="inch"),
             color="none")+
      geom_label(
        aes(x=4,y=(ymax+ymin)/2,label=comma(value)),
        label.size=.175,
        show.legend=FALSE)+
      labs(title="Share of Gun Deaths by Category")
    
  })

    #Histogram by age
  output$bar_plot<-renderPlotly({
    
    
    gg_bar<-guns_subset()%>%
      #role up by chosen series
      group_by(age,series_choice)%>%
      count(name="value")%>%
      ungroup()%>%
      #ggplot histogram
      ggplot(aes(x=age,y=value,fill=series_choice))+
      geom_bar(stat="identity",position="stack")+
      #remove grey background
      theme_minimal()+
      #add labels
      labs(title="Deaths by Age",
           fill="",
           y="fatalities")+
      #add columsn to y axis label
      scale_y_continuous(label=comma)
    gg_bar
  })
  
  #seasonality chart
  output$seasonality<-renderPlotly({
    guns_subset()%>%
      #create season vars
      mutate(
        #make month numeric so the case_when I already wrote will work
        month<-as.numeric(month),
        season=case_when(
          month%in%c(12,1,2) ~ "Winter",
          month%in%c(3,4,5) ~ "Spring",
          month%in%c(6,7,8) ~ "Summer", 
          month%in%c(9,10,11) ~ "Fall"))%>%
      #role up by chosen series and by season
      group_by(season,series_choice)%>%
      count(name="value")%>%
      ungroup()%>%
      ggplot(aes(x=season,y=value,fill=series_choice))+
      geom_bar(position="stack", stat="identity")+
      scale_y_continuous(label=comma)+
      #label axes
      labs(title="Seasonal Breakdown",
           x="Season",
           y="Number of Deaths",
           fill="")+
      theme_minimal()
    
  })
  
  #value and infoboxes
  #total deaths value box
  output$deaths <- renderValueBox({
    #count total deaths
    num <- nrow(guns_subset())
    valueBox(subtitle = "Total Deaths", value = comma(num), icon = icon("cross",lib="font-awesome"), color = "red")
  })
  #time-span covered valuebox
  output$timespan <- renderValueBox({
    dat<-guns_subset()
    #subtract first date by last date, divide by 30.5 to approximate months (since we don't know exact day)
    num <- as.numeric(max(dat$datetime)-min(dat$datetime))/30.5
    
    valueBox(subtitle = "Months Covered", value = comma(num), icon = icon("calendar"), color = "red")
  })
  
  #deaths/day
  output$deaths_per_month <- renderValueBox({
    #count total deaths
    deaths<-nrow(guns_subset())
    dat<-guns_subset()
    #count total months as before
    months<- as.numeric(max(dat$datetime)-min(dat$datetime))/30.5
    num<-(deaths/months)
    valueBox(subtitle = "Deaths per Month", value = round(num), icon = icon("heartbeat",lib="font-awesome"), color = "red")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(tmap)
library(sf)
library(trelliscopejs)


packages = c('tidyverse','plotly', 'tmap', 'sf','trelliscopejs')

for(p in packages){
  if(!require(p, character.only =T)){
    install.packages(p)
  }
  library(p, character.only =T)
}

Change_Staff <- read_rds("data/Change_Staff_B.rds")
Change_Job <- read_rds("data/Change_Job_B.rds")


buildings <- read_sf("data/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")

employers <- read_sf("data/Employers.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")

Count_Checkin_Daily <- read_sf("data/Count_Checkin_Daily.csv", 
                               options = "GEOM_POSSIBLE_NAMES=location")    
Count_Checkin_Weekly <- read_sf("data/Count_Checkin_Weekly.csv", 
                                options = "GEOM_POSSIBLE_NAMES=location")  
Count_Checkin_Monthly <- read_sf("data/Count_Checkin_Monthly.csv", 
                                 options = "GEOM_POSSIBLE_NAMES=location")  
Count_Checkin_Weekday <- read_sf("data/Count_Checkin_Weekday.csv", 
                                 options = "GEOM_POSSIBLE_NAMES=location") 

Count_Checkin_Daily$Num_of_Employees <- as.numeric(Count_Checkin_Daily$Num_of_Employees)
Count_Checkin_Weekly$Num_of_Employees <- as.numeric(Count_Checkin_Weekly$Num_of_Employees)
Count_Checkin_Weekday$Num_of_Employees <- as.numeric(Count_Checkin_Weekday$Num_of_Employees)
Count_Checkin_Monthly$Num_of_Employees <- as.numeric(Count_Checkin_Monthly$Num_of_Employees)

Count_Checkin_Daily$Pay_Group <-factor(Count_Checkin_Daily$Pay_Group, levels = c("<=$15 (Low)","$16-35(Mid)", ">$36(High)"))
Count_Checkin_Weekly$Pay_Group <-factor(Count_Checkin_Weekly$Pay_Group, levels = c("<=$15 (Low)","$16-35(Mid)", ">$36(High)"))
Count_Checkin_Weekday$Pay_Group <-factor(Count_Checkin_Weekday$Pay_Group, levels = c("<=$15 (Low)","$16-35(Mid)", ">$36(High)"))
Count_Checkin_Monthly$Pay_Group <-factor(Count_Checkin_Monthly$Pay_Group, levels = c("<=$15 (Low)","$16-35(Mid)", ">$36(High)"))

ui <- navbarPage("Hello World",
  theme = bs_theme(bootswatch = "united", version = 3), 
  tabPanel("Home", "home page content"),
  tabPanel("member 1", "content 1"),
  tabPanel("member 2", "content 2"),
  tabPanel("Employer",  
    titlePanel("Changes in Employment"),
    navlistPanel( 
        widths = c(2,10),
    tabPanel("Map View", fluidRow(
      titlePanel("Interactive City Map View"),
        selectInput("period", label = "Period", choices = c("Daily", "Weekly", "Weekday", "Monthly")),
        selectInput("employee", label = "Num of Employees", choices = c("--")),
        selectInput("job", label = "Num of Jobs", choices = c("--")),
        selectInput("hired", label = "Hired Rate", choices = c("--")),
        checkboxGroupInput("pay", label = "Pay Group", choices = c("")),
        textInput("eid", label = "Employer Id", value = ""),  
      tmapOutput("plot1"),
        DT::dataTableOutput("aTable")
    )),
    tabPanel("Hiring Rate", fluidPage(
      trelliscopeOutput("HiringRate")
    )),
   tabPanel("Turnover Rate", fluidRow(
     splitLayout(
       selectInput("change_filter", label = "Filter by", choices = c("--", "Date", "Week", "Month")), 
       selectInput("change_value", label = "Options", choices = c("--"))
     ), 
     splitLayout( 
       plotOutput("ChangeStaff"),
       plotOutput("ChangeJob")
     ), 
     splitLayout( 
       plotlyOutput("ChangeStaffJ"),
       plotlyOutput("ChangeJobJ")
     )
   ))
  ))
)

server <- function (input, output, session) {
  
  #############   Employer - Map View   #########################
  selected_period <- reactive({
     if (input$period == "Daily") {
       Count_Checkin_Daily
     } else if (input$period == "Weekly") {
       Count_Checkin_Weekly
     } else if (input$period == "Weekday") {
       Count_Checkin_Weekday
     } else {
       Count_Checkin_Monthly
     } #%>%
      #filter(EMPLOYEES == input$employees) 
  })
  
  observeEvent(selected_period(), {
    updateSelectInput(inputId = "employee", choices = c("--",unique(selected_period()$Num_of_Employees)))
    updateSelectInput(inputId = "job", choices = c("--",unique(selected_period()$Num_of_Jobs)))
    updateSelectInput(inputId = "hired", choices = c("--",unique(selected_period()$HiredRate)))
    pay <- unique(selected_period()$Pay_Group)
    updateCheckboxGroupInput(inputId = "pay", choices = pay, selected = pay)
    updateTextInput(inputId = "eid", value = "")
  })
  
 
  filtered_data <- reactive({
    temp <- selected_period()
    if (input$employee != "--") {
      temp <- filter(temp, Num_of_Employees == input$employee)
    }
    if (input$job != "--") {
      temp <- filter(temp, Num_of_Jobs == input$job)
    }
    if (input$hired != "--") {
      temp <- filter(temp, HiredRate == input$hired)
    }
    temp <- filter(temp, Pay_Group == input$pay)
    if (input$eid != "") {
      temp <- filter(temp, employerId == input$eid)
    }
    temp
  })

  output$plot1 <- renderTmap({
      tm_shape(buildings)+
        tm_polygons(col = "grey60",
                    size = 1,
                    border.col = "white",
                    border.lwd = 1) +
        tm_shape(shp = filtered_data()) +
        tm_bubbles(size = 0.5, col = "Num_of_Employees", title.col = "Number of\nEmployees")
  })
  
  output$aTable <- DT::renderDataTable({
      DT::datatable(data = filtered_data() ,
                    options= list(pageLength = 10),
                    rownames = FALSE)
  }) 
  
  #############   Employer - Hiring Rate  #########################
  output$HiringRate <- renderTrelliscope({
    
    ggplot(Count_Checkin_Weekly, aes(x= as.factor(Week_Num), y= HiredRate)) +
      geom_point(color='red') +
      labs(x= 'Week', y= 'HiredRate',
           title = 'Hiring Rate of Each Employers') +
      ylim(0,600) + 
      facet_trelliscope(~ employerId, 
                        nrow = 2, ncol = 3, width = 800,
                        path = 'trellisr/',
                        self_contained = TRUE) +
      theme(axis.title.y= element_text(angle=0), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.3),
            axis.ticks.x= element_blank(),
            panel.background= element_blank(), 
            axis.line= element_line(color= 'grey'))
  })
  
  #############   Employer - Turnover Rate   #########################
  selected_filter <- reactive({
    if (input$change_filter == "Date") {
      unique(Change_Staff$Date)
    } else if (input$change_filter == "Week") {
      unique(Change_Staff$Week_Num)
    } else if (input$change_filter == "Month") {
      unique(Change_Staff$Yr_Month)
    } else {
      c("--")
    }
  })
  
  observeEvent(selected_filter(), {
    updateSelectInput(inputId = "change_value", choices = selected_filter()) 
  })
  
  selected_value <- reactive({
    if (input$change_filter == "Date") {
      filter(Change_Staff, Date == input$change_value)
    } else if (input$change_filter == "Week") {
      filter(Change_Staff, Week_Num == input$change_value)
    } else if (input$change_filter == "Month") {
      filter(Change_Staff, Yr_Month == input$change_value)
    } else {
      Change_Staff
    } 
    
  })
  
  output$ChangeStaff <- renderPlot({

    ggplot(selected_value(), aes(x= as.factor(Num_of_Employees), fill = haveKids)) +
      geom_bar() +
      facet_wrap(~educationLevel)+
      ggtitle('Employers with Turnover Staff') +
      xlab("No. of Employees") +
      ylab("No. of\nEmployers") +
      theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
            axis.line= element_line(color= 'grey'))
    
  })
  
  output$ChangeStaffJ <- renderPlotly({
    p<- ggplot(selected_value(), aes(x = educationLevel, y = Num_of_Employees, fill=joviality)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = NA) + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        ),
        aes(text = paste('Employee: ', selected_value()$participantId,
                         'Employer: ', selected_value()$employerId,
                         'Date of Exit: ', selected_value()$Date))
      ) + 
      coord_cartesian(xlim = c(1.2, NA), clip = "off")+
      coord_flip() +
      ggtitle(label = "Education Level & Joviality")+
      theme_minimal()+
      theme(plot.title = element_text(size=12, face="bold",hjust = 0.5))+
      theme(axis.title.y= element_blank(),
            panel.background= element_blank(), axis.line= element_line(color= 'grey'))
    
    ggplotly(p, tooltip = 'text') 
    
  })
  
  
  selected_job <- reactive({
    if (input$change_filter == "Date") {
      filter(Change_Job, Date == input$change_value)
    } else if (input$change_filter == "Week") {
      filter(Change_Job, Week_Num == input$change_value)
    } else if (input$change_filter == "Month") {
      filter(Change_Job, Yr_Month == input$change_value)
    } else {
      Change_Job
    } 
  })
  
  output$ChangeJob <- renderPlot({
    
    ggplot(selected_job(), aes(x= as.factor(Num_of_Employers), fill = haveKids)) +
      geom_bar() +
      facet_wrap(~educationLevel)+
      ggtitle('Employers with Turnover Staff') +
      xlab("No. of Employees") +
      ylab("No. of\nEmployers") +
      theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
            axis.line= element_line(color= 'grey'))
    
  })
  
  output$ChangeJobJ <- renderPlotly({
    p<- ggplot(selected_job(), aes(x = educationLevel, y = Num_of_Employers, fill=joviality)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = NA) + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        ),
        aes(text = paste('Employee: ', selected_job()$participantId,
                         'Employer: ', selected_job()$employerId,
                         'Date of Job Change: ', selected_job()$Date))
      ) + 
      coord_cartesian(xlim = c(1.2, NA), clip = "off")+
      coord_flip() +
      ggtitle(label = "Education Level & Joviality")+
      theme_minimal()+
      theme(plot.title = element_text(size=12, face="bold",hjust = 0.5))+
      theme(axis.title.y= element_blank(),
            panel.background= element_blank(), axis.line= element_line(color= 'grey'))
    
    ggplotly(p, tooltip = 'text') 
    
  })
  
}

shinyApp(ui, server)
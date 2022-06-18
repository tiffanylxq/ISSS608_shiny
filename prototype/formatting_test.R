library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(tmap)
library(sf)
library(trelliscopejs)
library(shinythemes)
library(ggplot2)
library(gapminder)
library(lubridate)
library(dplyr)
library(ggdist)
library(reshape)
library(reshape2)
library(scales)
library(hrbrthemes)

#========================#
#####Data Extraction######
#========================#

##### Part1 #####

##### Part2 #####
participantData <- read_csv("data/facet/Participants.csv")
incomeExpenseBalanceParticipant <- read_csv("data/participant/comprehensiveParticipantInfoFinal.csv")
incomeExpenseBalanceParticipant$timestamp <- as.Date(incomeExpenseBalanceParticipant$timestamp, format =  "%d/%m/%Y")
incomeExpenseBalanceTotal <- read_csv("data/overall/total.csv")
incomeExpenseBalanceMin <- read_csv("data/overall/min.csv")
incomeExpenseBalanceAverage <- read_csv("data/overall/average.csv")
incomeExpenseBalanceMax <- read_csv("data/overall/max.csv")
incomeExpenseBalanceTotal$timestamp <- as.Date(incomeExpenseBalanceTotal$timestamp, format =  "%d/%m/%Y")
incomeExpenseBalanceMin$timestamp <- as.Date(incomeExpenseBalanceMin$timestamp, format =  "%d/%m/%Y")
incomeExpenseBalanceAverage$timestamp <- as.Date(incomeExpenseBalanceAverage$timestamp, format =  "%d/%m/%Y")
incomeExpenseBalanceMax$timestamp <- as.Date(incomeExpenseBalanceMax$timestamp, format =  "%d/%m/%Y")
use_this_for_balance <- read_csv("data/heatmap/balance_heatmap_full.csv")
use_this_for_balance$timestamp <- as.Date(use_this_for_balance$timestamp, format =  "%d/%m/%Y")

##### Part3 #####
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


#========================#
######   Shiny UI   ######
#========================#

ui <- navbarPage(
  title = "NGAGE&XPLORE: An Interactive Exploration of Engagement's Economy",
  fluid = TRUE,
  theme=shinytheme("flatly"),
  id = "navbarID",
  tabPanel("Introduction"),
  navbarMenu("Part1"),
  navbarMenu("Income and Expense",
             tabPanel("TEST",
               titlePanel("TEST"),
             sidebarLayout(position = "left",
                           sidebarPanel(),
                           mainPanel()
             )),
             tabPanel("Income and Expense",
                      # Input values
                      sidebarPanel(
                        HTML("<h3>Input General Parameters</h3>"),
                        HTML("<p>This frames the datasets for both individual and population to make the comparison easier</p>"),
                        
                        HTML("<b>Choose Dates</b>"),
                        dateRangeInput("datee", "Date range:",
                                       start = "2022-03-01",
                                       end   = "2023-05-24", 
                                       min = "2022-03-01",
                                       max = "2023-05-24"), 
                        
                        # We use this to select the date division
                        HTML("<b>Choose Date Division</b>"),
                        #checkboxInput("dateDivCheck", label = "Use a different date division", value = FALSE),
                        selectInput("divisionn", label = "Division:", 
                                    choices = list("Daily" = "daily", 
                                                   "Monthly" = "monthly", 
                                                   "Yearly" = "yearly")),
                        
                        HTML("<h3>Input Population Parameters</h3>"),
                        HTML("<p>Use this to understand the population of Engagement</p>"),
                        HTML("<p>Note that the ranges are in percentage.</p>"),
                        HTML("<p>Eg. Setting the income range to 90-100 will give you the top 10% of income-earners 
                                           in Engagement. </p>"),
                        
                        # We allow the user to decide whether they'd like to explore Income, Expense, 
                        # and what type of Expense they will like to explore
                        
                        # We use this to select a preliminary level of aggregation
                        HTML("<b>Choose Aggregation</b>"),
                        selectInput("aggregation", label = "Aggregation:", 
                                    choices = list("Maximum" = "maximum", 
                                                   "Average" = "average", 
                                                   "Minimum" = "minimum", 
                                                   "Total" = "total")),
                        
                        # Choose Parameters
                        HTML("<b>Choose Paramaters</b>"),
                        checkboxInput("incomeCheck", label = "Evaluate income", value = TRUE),
                        checkboxInput("allExpenseCheck", label = "Evaluate all expense", value = TRUE),
                        checkboxInput("educationExpenseCheck", label = "Evaluate education expense", value = FALSE),
                        checkboxInput("foodExpenseCheck", label = "Evaluate food expense", value = FALSE),
                        checkboxInput("recreationExpenseCheck", label = "Evaluate recreation expense", value = FALSE),
                        checkboxInput("shelterExpenseCheck", label = "Evaluate shelter expense", value = FALSE),
                        checkboxInput("rentAdjustmentExpenseCheck", 
                                      label = "Evaluate rent adjustment expense", value = FALSE),
                        checkboxInput("balanceCheck", label = "Evaluate balance", value = TRUE),
                        
                        
                        HTML("<h3>Input Participants Parameters</h3>"),
                        HTML("<p>Use this to understand one participant from Engagement</p>"),
                        
                        numericInput("participantt", 
                                     label = "Participant Number", 
                                     value = 0),
                        
                        # Choose Parameters
                        HTML("<b>Choose Paramaters</b>"),
                        checkboxInput("incomeCheckParticipant", label = "Evaluate income", value = TRUE),
                        checkboxInput("allExpenseCheckParticipant", label = "Evaluate all expense", value = TRUE),
                        checkboxInput("educationExpenseCheckParticipant", label = "Evaluate education expense", value = FALSE),
                        checkboxInput("foodExpenseCheckParticipant", label = "Evaluate food expense", value = FALSE),
                        checkboxInput("recreationExpenseCheckParticipant", label = "Evaluate recreation expense", value = FALSE),
                        checkboxInput("shelterExpenseCheckParticipant", label = "Evaluate shelter expense", value = FALSE),
                        checkboxInput("rentAdjustmentExpenseCheckParticipant", 
                                      label = "Evaluate rent adjustment expense", value = FALSE),
                        checkboxInput("balanceCheckParticipant", label = "Evaluate balance", value = TRUE),
                        
                      ),
                      
                      mainPanel(
                        HTML("<h3>Engagement Statistics</h3>"),
                        plotlyOutput("plot1ks"),
                        plotlyOutput("plot1ksa"),
                        
                        HTML("<h3>Individual Statistics</h3>"),
                        plotlyOutput("plot2ks"),
                        plotlyOutput("plot2ksa")
                      )
                      
             ), #tabPanel(), Home
             
             tabPanel("Patterns with Heatmap", 
                      sidebarPanel(
                        HTML("<b>Choose Dates</b>"),
                        dateRangeInput("heatmapDate", "Date range:",
                                       start = "2022-03-01",
                                       end   = "2023-05-24", 
                                       min = "2022-03-01",
                                       max = "2023-05-24"),
                        
                        selectInput("dateDivHeatmap", label = "Division:", 
                                    choices = list("Daily" = "daily",
                                                   "Monthly" = "monthly",
                                                   "Yearly" = "yearly"
                                    )),
                        
                        HTML("<b>Ranking</b>"),
                        checkboxInput("rankingHeatMapCheck", label = "Implement ranking", value = FALSE),
                        
                        selectInput("ranking1", label = "Choose criteria to rank:", 
                                    choices = list("Balance" = "balance",
                                                   "Income" = "income",
                                                   "All Expense" = "allExpense",
                                                   "Food Expense" = "foodExpense",
                                                   "Education Expense" = "educationExpense", 
                                                   "Shelter Expense" = "shelterExpense",
                                                   "Recreation Expense" = "recreationExpense",
                                                   "Rent Adjustment Expense" = "rentAdjustmentExpense"
                                    )),
                        checkboxInput("ranking1Ascending", label = "Ascending order", value = FALSE),
                        
                        sliderInput("participantDivision", label = "Choose the number of people to view", 
                                    min = 1, 
                                    max = 1011, 
                                    value = c(1, 51))
                      ),
                      
                      mainPanel(
                        HTML("<h3>HeatMap of Residents in Engagement</h3>"),
                        plotlyOutput(outputId = "plot3ks", 
                                     width = "100%", 
                                     height=800),
                      )
             ),  tabPanel("Participant Breakdown",
                          sidebarPanel(
                            HTML("<b>Choose participants to investigate</b>"),
                            checkboxInput("manualParticipantCheck", label = "Manually select participants", value = FALSE),
                            textInput("participant1", label = "Participant Chosen:", value = "", width = NULL, placeholder = "Please enter participant number from 0 to 1010."), 
                            textInput("participant2", label = "Participant Chosen:", value = "", width = NULL, placeholder = "Please enter participant number from 0 to 1010."), 
                            textInput("participant3", label = "Participant Chosen:", value = "", width = NULL, placeholder = "Please enter participant number from 0 to 1010."), 
                            textInput("participant4", label = "Participant Chosen:", value = "", width = NULL, placeholder = "Please enter participant number from 0 to 1010."), 
                            textInput("participant5", label = "Participant Chosen:", value = "", width = NULL, placeholder = "Please enter participant number from 0 to 1010."), 
                            textInput("participant6", label = "Participant Chosen:", value = "", width = NULL, placeholder = "Please enter participant number from 0 to 1010."), 
                            textInput("participant7", label = "Participant Chosen:", value = "", width = NULL, placeholder = "Please enter participant number from 0 to 1010."), 
                            textInput("participant8", label = "Participant Chosen:", value = "", width = NULL, placeholder = "Please enter participant number from 0 to 1010."), 
                            textInput("participant9", label = "Participant Chosen:", value = "", width = NULL, placeholder = "Please enter participant number from 0 to 1010."), 
                            textInput("participant10", label = "Participant Chosen:", value = "", width = NULL, placeholder = "Please enter participant number from 0 to 1010."), 
                            HTML("<b>Choose range for joviality</b>"),
                            textInput("jovialityStart", label = "Start of joviality range:", value = "", width = NULL, placeholder = "Please enter number from 0 to 1."), 
                            textInput("jovialityEnd", label = "End of joviality range:", value = "", width = NULL, placeholder = "Please enter number from 0 to 1."), 
                            
                            
                          ), 
                          mainPanel(
                            plotlyOutput(outputId = "plot4ks", height=400), 
                            plotlyOutput(outputId = "plot5ks", height=400), 
                            plotlyOutput(outputId = "plot6ks", height=400), 
                            plotlyOutput(outputId = "plot7ks", height=400), 
                            plotlyOutput(outputId = "plot8ks", height=400), 
                            plotlyOutput(outputId = "plot9ks", height=400), 
                            HTML("<b>Number of participants that fall under range</b>"),
                            h3(textOutput("selected_var"))
                          )
             )),
  navbarMenu("Employer",
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
  )
)

#========================#
###### Shiny Server ######
#========================#

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
  
  ####################################
  # Overall                          #
  ####################################
  dataOverall <- reactive({
    
    # These are the checks for aggreagation
    if (input$aggregation == "total") {
      df <- incomeExpenseBalanceTotal 
    } 
    
    else if (input$aggregation == "maximum") {
      df <- incomeExpenseBalanceMax 
    } 
    
    else if (input$aggregation == "average") {
      df <- incomeExpenseBalanceAverage 
    } 
    
    else if (input$aggregation == "minimum") {
      df <- incomeExpenseBalanceMin 
    }
    
    
    # These are the checks for the type of information to add
    if (!input$incomeCheck) {
      df <- subset(df, select = -c(income))
    }
    
    if (!input$allExpenseCheck) {
      df <- subset(df, select = -c(allExpense))
    }
    
    if (!input$balanceCheck) {
      df <- subset(df, select = -c(balance))
    }
    
    if (!input$educationExpenseCheck) {
      df <- subset(df, select = -c(educationalExpense))
    }
    
    if (!input$foodExpenseCheck) {
      df <- subset(df, select = -c(foodExpense))
    }
    
    if (!input$recreationExpenseCheck) {
      df <- subset(df, select = -c(recreationalExpense))
    }
    
    if (!input$shelterExpenseCheck) {
      df <- subset(df, select = -c(shelterExpense))
    }
    
    if (!input$rentAdjustmentExpenseCheck) {
      df <- subset(df, select = -c(rentAdjustmentExpense))
    }
    
    #Filtering based on the timeframe selected
    df_use_this <- df %>% filter(
      between(timestamp, 
              input$datee[1], 
              input$datee[2])
    )
    
    #Choosing the type of time division
    
    if (input$divisionn != "daily") {
      
      if (input$divisionn != "yearly") {
        df_use_this$timestamp <- format(as.Date(df_use_this$timestamp), "%Y-%m")
        
        df_use_this <- df_use_this %>%
          group_by(timestamp) %>%
          summarise(across(everything(), list(sum)))
        
      } 
      
      else if (input$divisionn != "monthly") {
        df_use_this$timestamp <- format(as.Date(df_use_this$timestamp), "%Y")
        
        df_use_this <- df_use_this %>%
          group_by(timestamp) %>%
          summarise(across(everything(), list(sum)))
        
      }
      
    }
    
    df_final <- melt(df_use_this, id = c("timestamp"))
    
  })
  
  output$plot1ks <- renderPlotly({
    
    print(dataOverall())
    
    p <- ggplot(dataOverall(), 
                aes(x = timestamp, 
                    y = value, 
                    group = variable, 
                    color = variable)) + 
      geom_line() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(), 
            axis.ticks.x=element_blank())
    
    ggplotly(p)
  })
  
  #This is for the boxplot
  output$plot1ksa <- renderPlotly({
    
    df_for_boxplot <- subset(dataOverall(), select = -c(timestamp))
    
    p <- ggplot(df_for_boxplot,
                aes(x=variable, y=value, fill=variable)) +
      geom_violin() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) 
    
    ggplotly(p)
  })
  
  ####################################
  # Participant                      #
  ####################################
  dataParticipant <- reactive({
    
    df <- incomeExpenseBalanceParticipant %>% filter(participantId == input$participantt)
    
    # These are the checks for the type of information to add
    if (!input$incomeCheckParticipant) {
      df <- subset(df, select = -c(income))
    }
    
    if (!input$allExpenseCheckParticipant) {
      df <- subset(df, select = -c(allExpense))
    }
    
    if (!input$balanceCheckParticipant) {
      df <- subset(df, select = -c(balance))
    }
    
    if (!input$educationExpenseCheckParticipant) {
      df <- subset(df, select = -c(educationalExpense))
    }
    
    if (!input$foodExpenseCheckParticipant) {
      df <- subset(df, select = -c(foodExpense))
    }
    
    if (!input$recreationExpenseCheckParticipant) {
      df <- subset(df, select = -c(recreationalExpense))
    }
    
    if (!input$shelterExpenseCheckParticipant) {
      df <- subset(df, select = -c(shelterExpense))
    }
    
    if (!input$rentAdjustmentExpenseCheckParticipant) {
      df <- subset(df, select = -c(rentAdjustmentExpense))
    }
    
    #Filtering based on the timeframe selected
    df_use_this <- df %>% filter(
      between(timestamp, 
              input$datee[1], 
              input$datee[2])
    )
    
    #Choosing the type of time division
    
    if (input$divisionn != "daily") {
      
      if (input$divisionn != "yearly") {
        df_use_this$timestamp <- format(as.Date(df_use_this$timestamp), "%Y-%m")
        
        df_use_this <- df_use_this %>%
          group_by(timestamp) %>%
          summarise(across(everything(), list(sum)))
        
        names(df_use_this)[names(df_use_this) == 'participantId_1'] <- 'participantId'
        
      } 
      
      else if (input$divisionn != "monthly") {
        df_use_this$timestamp <- format(as.Date(df_use_this$timestamp), "%Y")
        
        df_use_this <- df_use_this %>%
          group_by(timestamp) %>%
          summarise(across(everything(), list(sum)))
        
        names(df_use_this)[names(df_use_this) == 'participantId_1'] <- 'participantId'
        
      }
      
    }
    
    df_use_this <- subset(df_use_this, select = -c(participantId))
    
    df_final <- melt(df_use_this, id = c("timestamp"))
    
  })
  
  output$plot2ks <- renderPlotly({
    p <- ggplot(dataParticipant(), 
                aes(x = timestamp, 
                    y = value, 
                    group = variable, 
                    color = variable)) + 
      geom_line() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(), 
            axis.ticks.x=element_blank())
    
    ggplotly(p)
  })
  
  #This is for the boxplot
  output$plot2ksa <- renderPlotly({
    
    df_for_boxplot <- subset(dataParticipant(), select = -c(timestamp))
    
    p <- ggplot(df_for_boxplot,
                aes(x=variable, y=value, fill=variable)) +
      geom_violin() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) 
    
    ggplotly(p)
  })
  
  ####################################
  # Heatmap                          #
  ####################################
  dataHeatmap <- reactive({
    
    df_total <- use_this_for_balance
    
    df_use_this <- subset(df_total, select = c(participantId, timestamp, balance))
    
    #Filtering based on the timeframe selected
    df <- df_use_this %>% filter(
      between(timestamp, 
              input$heatmapDate[1], 
              input$heatmapDate[2])
    )
    df_total <- df_total %>% filter(
      between(timestamp, 
              input$heatmapDate[1], 
              input$heatmapDate[2])
    )
    
    #Allowing for date division
    if (input$dateDivHeatmap == "daily") {
      df <- df
      
      print("daily")
      print(df)
    }
    
    if (input$dateDivHeatmap == "monthly") {
      
      df$timestamp <- format(as.Date(df$timestamp), "%Y-%m")
      
      df <- df %>%
        group_by(timestamp, participantId) %>%
        summarise(across(everything(), list(sum)))
      
      print("monthly")
      print(df)
    }
    
    if (input$dateDivHeatmap == "yearly") {
      
      df$timestamp <- format(as.Date(df$timestamp), "%Y")
      
      df <- df %>%
        group_by(timestamp, participantId) %>%
        summarise(across(everything(), list(sum)))
      
      print("yearly")
      print(df)
    }
    
    names(df)[names(df) == 'participantId_1'] <- "participantId"
    names(df)[names(df) == 'balance_1'] <- "balance"
    
    # Implement ranking
    if (input$rankingHeatMapCheck) {
      
      if (input$ranking1 != "none") { 
        
        if (input$ranking1 == "balance") { 
          
          df_stand_in <- subset(df_total, select = c(participantId, balance))
          
          df_stand_in <- df_stand_in %>%
            group_by(participantId) %>%
            summarise(across(everything(), list(sum)))
          
          df_stand_in_test <- df_stand_in[order(df_stand_in$balance_1, decreasing = !input$ranking1Ascending),]
          df_stand_in_test <- df_stand_in_test %>% 
            slice(input$participantDivision[1]:input$participantDivision[2])
          
          #Now let us keep only the values we want based on the population ranking
          list_of_participants_to_keep = df_stand_in_test$participantId
          
          df <- subset(df, participantId %in% list_of_participants_to_keep)
          
          merged_df <- merge(df, df_stand_in, by="participantId")
          
          print(merged_df)
          
          names(merged_df)[names(merged_df) == 'balance_1'] <- "criteria"
          
          merged_df$reverse <- merged_df$criteria * -1
          
          df <- merged_df
          
        }
        
        if (input$ranking1 == "income") { 
          
          df_stand_in <- subset(df_total, select = c(participantId, income))
          
          df_stand_in <- df_stand_in %>%
            group_by(participantId) %>%
            summarise(across(everything(), list(sum)))
          
          df_stand_in_test <- df_stand_in[order(df_stand_in$income_1, decreasing = !input$ranking1Ascending),]
          df_stand_in_test <- df_stand_in_test %>% 
            slice(input$participantDivision[1]:input$participantDivision[2])
          
          #Now let us keep only the values we want based on the population ranking
          list_of_participants_to_keep = df_stand_in_test$participantId
          
          df <- subset(df, participantId %in% list_of_participants_to_keep)
          
          merged_df <- merge(df, df_stand_in, by="participantId")
          
          print(merged_df)
          
          names(merged_df)[names(merged_df) == 'income_1'] <- "criteria"
          
          merged_df$reverse <- merged_df$criteria * -1
          
          df <- merged_df
          
        }
        
        if (input$ranking1 == "allExpense") { 
          
          df_stand_in <- subset(df_total, select = c(participantId, allExpense))
          
          df_stand_in <- df_stand_in %>%
            group_by(participantId) %>%
            summarise(across(everything(), list(sum)))
          
          df_stand_in_test <- df_stand_in[order(df_stand_in$allExpense_1, decreasing = !input$ranking1Ascending),]
          df_stand_in_test <- df_stand_in_test %>% 
            slice(input$participantDivision[1]:input$participantDivision[2])
          
          #Now let us keep only the values we want based on the population ranking
          list_of_participants_to_keep = df_stand_in_test$participantId
          
          df <- subset(df, participantId %in% list_of_participants_to_keep)
          
          merged_df <- merge(df, df_stand_in, by="participantId")
          
          print(merged_df)
          
          names(merged_df)[names(merged_df) == 'allExpense_1'] <- "criteria"
          
          merged_df$reverse <- merged_df$criteria * -1
          
          df <- merged_df
          
        }
        
        if (input$ranking1 == "foodExpense") { 
          
          df_stand_in <- subset(df_total, select = c(participantId, foodExpense))
          
          df_stand_in <- df_stand_in %>%
            group_by(participantId) %>%
            summarise(across(everything(), list(sum)))
          
          df_stand_in_test <- df_stand_in[order(df_stand_in$foodExpense_1, decreasing = !input$ranking1Ascending),]
          df_stand_in_test <- df_stand_in_test %>% 
            slice(input$participantDivision[1]:input$participantDivision[2])
          
          #Now let us keep only the values we want based on the population ranking
          list_of_participants_to_keep = df_stand_in_test$participantId
          
          df <- subset(df, participantId %in% list_of_participants_to_keep)
          
          merged_df <- merge(df, df_stand_in, by="participantId")
          
          print(merged_df)
          
          names(merged_df)[names(merged_df) == 'foodExpense_1'] <- "criteria"
          
          merged_df$reverse <- merged_df$criteria * -1
          
          df <- merged_df
          
        }
        
        if (input$ranking1 == "educationExpense") { 
          
          df_stand_in <- subset(df_total, select = c(participantId, educationalExpense))
          
          df_stand_in <- df_stand_in %>%
            group_by(participantId) %>%
            summarise(across(everything(), list(sum)))
          
          df_stand_in_test <- df_stand_in[order(df_stand_in$educationalExpense_1, decreasing = !input$ranking1Ascending),]
          df_stand_in_test <- df_stand_in_test %>% 
            slice(input$participantDivision[1]:input$participantDivision[2])
          
          #Now let us keep only the values we want based on the population ranking
          list_of_participants_to_keep = df_stand_in_test$participantId
          
          df <- subset(df, participantId %in% list_of_participants_to_keep)
          
          merged_df <- merge(df, df_stand_in, by="participantId")
          
          print(merged_df)
          
          names(merged_df)[names(merged_df) == 'educationalExpense_1'] <- "criteria"
          
          merged_df$reverse <- merged_df$criteria * -1
          
          df <- merged_df
          
        }
        
        if (input$ranking1 == "shelterExpense") { 
          
          df_stand_in <- subset(df_total, select = c(participantId, shelterExpense))
          
          df_stand_in <- df_stand_in %>%
            group_by(participantId) %>%
            summarise(across(everything(), list(sum)))
          
          df_stand_in_test <- df_stand_in[order(df_stand_in$shelterExpense_1, decreasing = !input$ranking1Ascending),]
          df_stand_in_test <- df_stand_in_test %>% 
            slice(input$participantDivision[1]:input$participantDivision[2])
          
          #Now let us keep only the values we want based on the population ranking
          list_of_participants_to_keep = df_stand_in_test$participantId
          
          df <- subset(df, participantId %in% list_of_participants_to_keep)
          
          merged_df <- merge(df, df_stand_in, by="participantId")
          
          print(merged_df)
          
          names(merged_df)[names(merged_df) == 'shelterExpense_1'] <- "criteria"
          
          merged_df$reverse <- merged_df$criteria * -1
          
          df <- merged_df
          
        }
        
        if (input$ranking1 == "recreationExpense") { 
          
          df_stand_in <- subset(df_total, select = c(participantId, recreationalExpense))
          
          df_stand_in <- df_stand_in %>%
            group_by(participantId) %>%
            summarise(across(everything(), list(sum)))
          
          df_stand_in_test <- df_stand_in[order(df_stand_in$recreationalExpense_1, decreasing = !input$ranking1Ascending),]
          df_stand_in_test <- df_stand_in_test %>% 
            slice(input$participantDivision[1]:input$participantDivision[2])
          
          #Now let us keep only the values we want based on the population ranking
          list_of_participants_to_keep = df_stand_in_test$participantId
          
          df <- subset(df, participantId %in% list_of_participants_to_keep)
          
          merged_df <- merge(df, df_stand_in, by="participantId")
          
          print(merged_df)
          
          names(merged_df)[names(merged_df) == 'recreationalExpense_1'] <- "criteria"
          
          merged_df$reverse <- merged_df$criteria * -1
          
          df <- merged_df
          
        }
        
        if (input$ranking1 == "rentAdjustmentExpense") { 
          
          df_stand_in <- subset(df_total, select = c(participantId, rentAdjustmentExpense))
          
          df_stand_in <- df_stand_in %>%
            group_by(participantId) %>%
            summarise(across(everything(), list(sum)))
          
          df_stand_in_test <- df_stand_in[order(df_stand_in$rentAdjustmentExpense_1, decreasing = !input$ranking1Ascending),]
          df_stand_in_test <- df_stand_in_test %>% 
            slice(input$participantDivision[1]:input$participantDivision[2])
          
          #Now let us keep only the values we want based on the population ranking
          list_of_participants_to_keep = df_stand_in_test$participantId
          
          df <- subset(df, participantId %in% list_of_participants_to_keep)
          
          merged_df <- merge(df, df_stand_in, by="participantId")
          
          print(merged_df)
          
          names(merged_df)[names(merged_df) == 'rentAdjustmentExpense_1'] <- "criteria"
          
          merged_df$reverse <- merged_df$criteria * -1
          
          df <- merged_df
          
        }
        
      }
    } 
    
    # Get the number of members if there is no ranking implemented
    else if (!input$rankingHeatMapCheck){
      
      list_of_participants = unique(df$participantId)
      
      list_of_participants[order(list_of_participants)]
      
      list_of_participants_to_use = list_of_participants[input$participantDivision[1]:input$participantDivision[2]]
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
      print("no ranking switched on")
      
      print(df)
      
    }
    
    df_use_this <- df
    
  })
  
  output$plot3ks <- renderPlotly({
    
    timestamp <- dataHeatmap()$timestamp
    participantId <- dataHeatmap()$participantId
    balance <- dataHeatmap()$balance
    
    if("criteria" %in% colnames(dataHeatmap())) {
      
      if (input$ranking1Ascending) { 
        
        participant <- reorder(participantId, dataHeatmap()$reverse)
        
        p <- ggplot(dataHeatmap(), aes(
          timestamp, 
          participant,
          fill = balance
        )) + 
          geom_tile() +
          #scale_fill_gradientn(colors = hcl.colors(20, "viridis")) +
          scale_fill_gradient(low="white", high="black") +
          theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank())
      }
      
      else if (!input$ranking1Ascending) {
        
        participant <- reorder(participantId, dataHeatmap()$criteria)
        
        p <- ggplot(dataHeatmap(), aes(
          timestamp, 
          participant,
          fill = balance
        )) + 
          geom_tile() +
          #scale_fill_gradientn(colors = hcl.colors(20, "viridis")) +
          scale_fill_gradient(low="white", high="black") +
          theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank())
        
      }
      
    }
    
    else {
      p <- ggplot(dataHeatmap(), aes(
        timestamp, 
        participantId,
        fill = balance
      )) + 
        geom_tile() +
        #scale_fill_gradientn(colors = hcl.colors(20, "viridis")) +
        scale_fill_gradient(low="white", high="black") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())
    }
    
    ggplotly(p)
    
  })
  
  ####################################
  # Facet Pie Charts                 #
  ####################################
  
  #Education level
  output$plot4ks <- renderPlotly({
    
    if (!input$manualParticipantCheck) {
      
      print(input$manualParticipantCheck)
      
      dataFromHeatMap <- dataHeatmap()
      
      list_of_participants_to_use = unique(dataFromHeatMap$participantId)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
    }
    
    if (input$manualParticipantCheck) {
      
      list_of_participants_to_use <- c(input$participant1, 
                                       input$participant2,
                                       input$participant3,
                                       input$participant4,
                                       input$participant5,
                                       input$participant6,
                                       input$participant7,
                                       input$participant8,
                                       input$participant9,
                                       input$participant10
      )
      
      print(list_of_participants_to_use)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
      print(df)
      
    }
    
    # Getting information for factor
    temp <- table(df['educationLevel'])
    temp_df <- data.frame(rbind(temp))
    temp_df_transposed <- t(temp_df)
    
    temp_df_transposed <- cbind(factor = rownames(temp_df_transposed), temp_df_transposed)
    rownames(temp_df_transposed) <- 1:nrow(temp_df_transposed)
    
    temp2 <- data.frame(temp_df_transposed)
    
    names(temp2) <- c('Factor', 'Count')
    
    temp_final <- transform(temp2, Count = as.numeric(Count))
    
    print(temp_final)
    str(temp_final)
    
    p <- ggplot(temp_final, aes(x=Factor, y=Count)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(label=Count), color="white",
                position = position_stack(vjust=0.5), size=3.5)+
      #scale_y_continuous(breaks= integer_breaks()) +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggtitle("Education Level")
    
    ggplotly(p)
    
  })
  
  #Interest Group
  output$plot5ks <- renderPlotly({
    
    if (!input$manualParticipantCheck) {
      
      print(input$manualParticipantCheck)
      
      dataFromHeatMap <- dataHeatmap()
      
      list_of_participants_to_use = unique(dataFromHeatMap$participantId)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
    }
    
    if (input$manualParticipantCheck) {
      
      list_of_participants_to_use <- c(input$participant1, 
                                       input$participant2,
                                       input$participant3,
                                       input$participant4,
                                       input$participant5,
                                       input$participant6,
                                       input$participant7,
                                       input$participant8,
                                       input$participant9,
                                       input$participant10
      )
      
      print(list_of_participants_to_use)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
      print(df)
      
    }
    
    # Getting information for factor
    temp <- table(df['interestGroup'])
    temp_df <- data.frame(rbind(temp))
    temp_df_transposed <- t(temp_df)
    
    temp_df_transposed <- cbind(factor = rownames(temp_df_transposed), temp_df_transposed)
    rownames(temp_df_transposed) <- 1:nrow(temp_df_transposed)
    
    temp2 <- data.frame(temp_df_transposed)
    
    names(temp2) <- c('Factor', 'Count')
    
    temp_final <- transform(temp2, Count = as.numeric(Count))
    
    print(temp_final)
    str(temp_final)
    
    p <- ggplot(temp_final, aes(x=Factor, y=Count)) + 
      geom_bar(stat = "identity") + 
      #scale_y_continuous(breaks= integer_breaks()) +
      geom_text(aes(label=Count), color="white",
                position = position_stack(vjust=0.5), size=3.5)+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggtitle("Interest Group")
    
    ggplotly(p)
    
  })
  
  #Household Size
  output$plot6ks <- renderPlotly({
    
    if (!input$manualParticipantCheck) {
      
      print(input$manualParticipantCheck)
      
      dataFromHeatMap <- dataHeatmap()
      
      list_of_participants_to_use = unique(dataFromHeatMap$participantId)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
    }
    
    if (input$manualParticipantCheck) {
      
      list_of_participants_to_use <- c(input$participant1, 
                                       input$participant2,
                                       input$participant3,
                                       input$participant4,
                                       input$participant5,
                                       input$participant6,
                                       input$participant7,
                                       input$participant8,
                                       input$participant9,
                                       input$participant10
      )
      
      print(list_of_participants_to_use)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
      print(df)
      
    }
    
    # Getting information for factor
    temp <- table(df['householdSize'])
    temp_df <- data.frame(rbind(temp))
    temp_df_transposed <- t(temp_df)
    
    temp_df_transposed <- cbind(factor = rownames(temp_df_transposed), temp_df_transposed)
    rownames(temp_df_transposed) <- 1:nrow(temp_df_transposed)
    
    temp2 <- data.frame(temp_df_transposed)
    
    names(temp2) <- c('Factor', 'Count')
    
    temp_final <- transform(temp2, Count = as.numeric(Count))
    
    print(temp_final)
    str(temp_final)
    
    p <- ggplot(temp_final, aes(x=Factor, y=Count)) + 
      geom_bar(stat = "identity") + 
      #scale_y_continuous(breaks= integer_breaks()) +
      geom_text(aes(label=Count), color="white",
                position = position_stack(vjust=0.5), size=3.5)+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggtitle("Household Size")
    
    ggplotly(p)
    
  })
  
  #Have Kids
  output$plot7ks <- renderPlotly({
    
    if (!input$manualParticipantCheck) {
      
      print(input$manualParticipantCheck)
      
      dataFromHeatMap <- dataHeatmap()
      
      list_of_participants_to_use = unique(dataFromHeatMap$participantId)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
    }
    
    if (input$manualParticipantCheck) {
      
      list_of_participants_to_use <- c(input$participant1, 
                                       input$participant2,
                                       input$participant3,
                                       input$participant4,
                                       input$participant5,
                                       input$participant6,
                                       input$participant7,
                                       input$participant8,
                                       input$participant9,
                                       input$participant10
      )
      
      print(list_of_participants_to_use)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
      print(df)
      
    }
    
    # Getting information for factor
    temp <- table(df['haveKids'])
    temp_df <- data.frame(rbind(temp))
    temp_df_transposed <- t(temp_df)
    
    temp_df_transposed <- cbind(factor = rownames(temp_df_transposed), temp_df_transposed)
    rownames(temp_df_transposed) <- 1:nrow(temp_df_transposed)
    
    temp2 <- data.frame(temp_df_transposed)
    
    names(temp2) <- c('Factor', 'Count')
    
    temp_final <- transform(temp2, Count = as.numeric(Count))
    
    print(temp_final)
    str(temp_final)
    
    p <- ggplot(temp_final, aes(x=Factor, y=Count)) + 
      geom_bar(stat = "identity") + 
      #scale_y_continuous(breaks= integer_breaks()) +
      geom_text(aes(label=Count), color="white",
                position = position_stack(vjust=0.5), size=3.5)+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggtitle("Have Kids")
    
    ggplotly(p)
    
  })
  
  #Age
  output$plot8ks <- renderPlotly({
    
    if (!input$manualParticipantCheck) {
      
      print(input$manualParticipantCheck)
      
      dataFromHeatMap <- dataHeatmap()
      
      list_of_participants_to_use = unique(dataFromHeatMap$participantId)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
    }
    
    if (input$manualParticipantCheck) {
      
      list_of_participants_to_use <- c(input$participant1, 
                                       input$participant2,
                                       input$participant3,
                                       input$participant4,
                                       input$participant5,
                                       input$participant6,
                                       input$participant7,
                                       input$participant8,
                                       input$participant9,
                                       input$participant10
      )
      
      print(list_of_participants_to_use)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
      print(df)
      
    }
    
    # Getting information for factor
    temp <- table(df['age'])
    temp_df <- data.frame(rbind(temp))
    temp_df_transposed <- t(temp_df)
    
    temp_df_transposed <- cbind(factor = rownames(temp_df_transposed), temp_df_transposed)
    rownames(temp_df_transposed) <- 1:nrow(temp_df_transposed)
    
    temp2 <- data.frame(temp_df_transposed)
    
    names(temp2) <- c('Factor', 'Count')
    
    temp_final <- transform(temp2, Count = as.numeric(Count))
    
    print(temp_final)
    str(temp_final)
    
    p <- ggplot(temp_final, aes(x=Factor, y=Count)) + 
      geom_bar(stat = "identity") + 
      #scale_y_continuous(breaks= integer_breaks()) +
      geom_text(aes(label=Count), color="white",
                position = position_stack(vjust=0.5), size=3.5)+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggtitle("Age")
    
    ggplotly(p)
    
  })
  
  #Joviality Info
  output$plot9ks <- renderPlotly({
    
    if (!input$manualParticipantCheck) {
      
      print(input$manualParticipantCheck)
      
      dataFromHeatMap <- dataHeatmap()
      
      list_of_participants_to_use = unique(dataFromHeatMap$participantId)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
    }
    
    if (input$manualParticipantCheck) {
      
      list_of_participants_to_use <- c(input$participant1, 
                                       input$participant2,
                                       input$participant3,
                                       input$participant4,
                                       input$participant5,
                                       input$participant6,
                                       input$participant7,
                                       input$participant8,
                                       input$participant9,
                                       input$participant10
      )
      
      print(list_of_participants_to_use)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
      print(df)
      
    }
    
    # Getting information for factor
    joviality_df <- subset(df, select = c(participantId, joviality))
    
    p <- ggplot(joviality_df, aes(x=reorder(participantId, joviality)
                                  , y=joviality)) +
      geom_point() + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggtitle("Joviality Basic Info")
    
    ggplotly(p)
    
  })
  
  # Joviality Count
  output$selected_var <- renderText({ 
    
    if (!input$manualParticipantCheck) {
      
      print(input$manualParticipantCheck)
      
      dataFromHeatMap <- dataHeatmap()
      
      list_of_participants_to_use = unique(dataFromHeatMap$participantId)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
    }
    
    if (input$manualParticipantCheck) {
      
      list_of_participants_to_use <- c(input$participant1, 
                                       input$participant2,
                                       input$participant3,
                                       input$participant4,
                                       input$participant5,
                                       input$participant6,
                                       input$participant7,
                                       input$participant8,
                                       input$participant9,
                                       input$participant10
      )
      
      print(list_of_participants_to_use)
      
      df <- participantData
      
      df <- subset(df, participantId %in% list_of_participants_to_use)
      
      print(df)
      
    }
    
    # Getting information for factor
    joviality_df <- subset(df, select = c(joviality))
    
    #joviality_df$joviality <- as.numeric(joviality_df$joviality)
    
    start <- as.numeric(input$jovialityStart)
    end <- as.numeric(input$jovialityEnd)
    
    number_of_participants <- sum(joviality_df >= start & joviality_df <= end)
    
  })
  
}

shinyApp(ui, server)
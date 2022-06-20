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
library(ggstatsplot)
library(RColorBrewer)
library(sftime)
library(ggthemes)
library(FunnelPlotR)

#========================#
#####Data Extraction######
#========================#

##### Part1 #####
workplaces <- read_rds("data/workplaces.rds")
sales <- read_rds("data/sales.rds")
pubs <- read_sf("data/Pubs.csv", options = "GEOM_POSSIBLE_NAMES=location")
restaurants <- read_sf("data/Restaurants.csv", options = "GEOM_POSSIBLE_NAMES=location")

pubs <- pubs %>%
  dplyr::rename(venueId = pubId) %>%
  select(venueId, maxOccupancy, location, buildingId)

restaurants <- restaurants %>%
  dplyr::rename(venueId = restaurantId) %>%
  select(venueId, maxOccupancy, location, buildingId)

pubs_resto <- rbind(pubs, restaurants)
pubs_resto_v <- left_join(pubs_resto, sales, by = 'venueId')

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
#Count_Checkin_Daily$payGroup <-factor(Count_Checkin_Daily$payGroup, levels = c("<=$15 (Low)","$16-35(Mid)", ">$36(High)"))
#Count_Checkin_Weekly$payGroup <-factor(Count_Checkin_Weekly$payGroup, levels = c("<=$15 (Low)","$16-35(Mid)", ">$36(High)"))
#Count_Checkin_Weekday$payGroup <-factor(Count_Checkin_Weekday$payGroup, levels = c("<=$15 (Low)","$16-35(Mid)", ">$36(High)"))
#Count_Checkin_Monthly$payGroup <-factor(Count_Checkin_Monthly$payGroup, levels = c("<=$15 (Low)","$16-35(Mid)", ">$36(High)"))


#========================#
######   Shiny UI   ######
#========================#

ui <- navbarPage(
  title = "ENGAGE & EXPLORE: An Interactive Exploration of Engagement's Economy",
  fluid = TRUE,
  theme=shinytheme("flatly"),
  id = "navbarID",
  tabPanel("Introduction"),
  navbarMenu("Business Performance",
             tabPanel("Pubs & Restaurants",
                      sidebarPanel(
                        radioButtons(inputId = "hBusiness_type",
                                     label = "Category",
                                     choices = c("Pubs" = "Recreation (Social Gathering)",
                                                 "Restaurants" = "Eating"),
                                     selected = "Recreation (Social Gathering)"),
                        
                        HTML("<p>To view top n businesses, select n > 0.</p>"),
                        HTML("<p>To view bottom n businesses, select n < 0.</p>"),
                        sliderInput(inputId = "hNumber",
                                    label = "Scenario",
                                    min = -10,
                                    max = 10,
                                    value = 5),
                        # selectInput(inputId = "Case",
                        #             label = "Scenario",
                        #             choices = list("Top 5" = 5,
                        #                            "Top 10" = 10,
                        #                            "Bottom 5" = -5,
                        #                            "Bottom 10" = -10)),
                        
                        dateRangeInput(inputId = "hDate",
                                       label = "Select Date Range:",
                                       start = "2022-03-01",
                                       end = "2023-03-01"),
                        
                        HTML("<p>To deep dive on the weekday sales pattern, select a venueID.</p>"),
                        selectInput(inputId = "hBusinessID",
                                    label = "VenueID",
                                    "venueID")),
                      
                      mainPanel(
                        splitLayout(
                         plotlyOutput("hplot1"),
                         plotlyOutput("hplot2")),
                        
                        HTML("<h3>Total Sales by venueId</h3>"),
                        splitLayout(
                          tmapOutput("hplot5"),
                          plotOutput("hplot6")),
                        
                        splitLayout(
                         plotlyOutput("hplot3"),
                         plotlyOutput("hplot4")),

                        )),
             tabPanel("Workplaces",
                      sidebarPanel(
                        HTML("<p>To view top n businesses, select n > 0.</p>"),
                        HTML("<p>To view bottom n businesses, select n < 0.</p>"),
                        sliderInput(inputId = "h2Number",
                                    label = "Scenario",
                                    min = -10,
                                    max = 10,
                                    value = 5),
                        
                        dateRangeInput(inputId = "h2Date",
                                       label = "Select Date Range:",
                                       start = "2022-03-01",
                                       end = "2023-03-01"),
                        
                        HTML("<p>To deep dive on the weekday sales pattern, select a venueID.</p>"),
                        numericInput(inputId = "h2BusinessID",
                                    label = "VenueID",
                                    "venueID")),
                      
                      mainPanel(
                        splitLayout(
                          plotlyOutput("hplot7"),
                          plotlyOutput("hplot8")),
                        splitLayout(
                          plotlyOutput("hplot9"))
                      )
             )),
  
  
  navbarMenu("Income and Expense",
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
                        splitLayout(
                          plotlyOutput("plot1ks"),
                          plotlyOutput("plot2ks")),
                        
                        HTML("<h3>Individual Statistics</h3>"),
                        splitLayout(
                          plotlyOutput("plot1ksa"),
                          plotlyOutput("plot2ksa"))
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
                            splitLayout(
                              plotlyOutput(outputId = "plot4ks", height=400), 
                              plotlyOutput(outputId = "plot5ks", height=400)),
                            splitLayout(
                              plotlyOutput(outputId = "plot6ks", height=400), 
                              plotlyOutput(outputId = "plot7ks", height=400)), 
                            splitLayout(
                              plotlyOutput(outputId = "plot8ks", height=400), 
                              plotlyOutput(outputId = "plot9ks", height=400)), 
                            HTML("<b>Number of participants that fall under range</b>"),
                            h3(textOutput("selected_var"))
                          )
             )),
  

                        
  navbarMenu("Employer",
             tabPanel("Map View", 
              sidebarPanel(width = 3,
               HTML("<h3>Input General Parameters</h3>"),
               selectInput("period", label = "Choose Time Period to View", choices = c("Daily", "Weekly", "Weekday", "Monthly")),
               selectInput("employee", label = "Choose Number of Employees Employed by Each Employer", choices = c("See All")),
               selectInput("job", label = "Choose Number of Jobs Offered by Each Employer", choices = c("See All")),
               HTML("<b>Choose Hiring Rate<sup>1</sup> of Each Employer</b>"),
               selectInput("hired", label = " ", choices = c("See All")),
               HTML("<h6><sup>1</sup>Hiring Rate = Number of Employees Employed / Number of Jobs Offered</h6>"),
               checkboxGroupInput("pay", label = "Choose At Least 1 Average Pay Given by Each Employer", 
                                  choices = list("<=$15 (Low)" = "<=$15 (Low)", 
                                                 "$16-35(Mid)" = "$16-35(Mid)", 
                                                 ">$36(High)s" = ">$36(High)"),
                                  selected = c("<=$15 (Low)","$16-35(Mid)", ">$36(High)")),
               HTML("<b>Fliter the Entire Map and Datable based on EmployerId</b>"),
               HTML("<h6> Use comma (,) to select multiple employers eg. 379,862,884 </h6>"),
               textInput("eid", label = " ", value = "")),
             mainPanel(width =9,
                HTML("<h3>Interactive City Map View</h3>"),
                HTML("<p> This map shows number of employees employed by each employer. 
                    If there are no corresponding data from the selected parameters, an error message will be displayed. </p>"),
                
               tmapOutput("plot1"),
               DT::dataTableOutput("aTable")
             )),
             tabPanel("Hiring Rate", 
              mainPanel(
                HTML("<h3>Hiring Rate of Each Employer</h3>"),
                trelliscopeOutput("HiringRate",width = "100%", height = "400px"),
                plotOutput("test")
             )),
             tabPanel("Turnover Rate", 
              sidebarPanel(width = 3,
                 HTML("<h3>Input General Parameters</h3>"),
                
                 selectInput("change_filter", label = "Choose Time Period to View", choices = c("See All", "Date", "Week", "Month")), 
                 selectInput("change_value", label = "Refine Time Period", choices = c("See All")),
                             selectInput(inputId = "xvariableXQ",
                                         label = "Select x-variable:",
                                         choices = c("Household Size" = "householdSize",
                                                     "Have Kids?" = "haveKids",
                                                     "Education" = "educationLevel",
                                                     "Interest Group" = "interestGroup",
                                                     "No. of Employers/Employees" = "???"),
                                         selected = "educationLevel"),
                             selectInput(inputId = "yvariableXQ",
                                         label = "Select y-variable:",
                                         choices = c("Joviality" = "joviality",
                                                     "Age" = "age",
                                                     "No. of Employers/Employees" = "???"),
                                         selected = "joviality"),
                             selectInput(inputId = "testXQ",
                                         label = "Type of statistical test:",
                                         choices = c("parametric" = "p",
                                                     "nonparametric" = "np",
                                                     "robust" = "r",
                                                     "Bayes Factor" = "bf"),
                                         selected = "p"),
                             selectInput(inputId = "plotTypeXQ",
                                         label = "Type of plot:",
                                         choices = c("boxviolin" = "boxviolin",
                                                     "box" = "box",
                                                     "violin" = "violin"),
                                         selected = "boxviolin"),
                             textInput(inputId = "plotTitleXQ",
                                       label = "Plot title",
                                       placeholder = "Enter text to be used as plot title"),
                             actionButton(inputId = "goButton", 
                                          "Go!")
                ),
                mainPanel(width = 9,
                          box(
                            plotOutput("ChangeStaffJ",
                                       height = "500px")
                )
              #mainPanel(width = 9,
               #splitLayout( 
                # plotlyOutput("ChangeStaff",
                #                height = "500px"),
                 #plotOutput("ChangeJob",
                #               height = "500px")
              # ), 
               #splitLayout( 
                # plotlyOutput("ChangeStaffJ",
              #               height = "500px"),
                 #plotlyOutput("ChangeJobJ",
              #               height = "500px")
              # )
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
    updateSelectInput(inputId = "employee", choices = c("See All", order(sort(unique(selected_period()$Num_of_Employees)))))
    updateSelectInput(inputId = "job", choices = c("See All",order(sort(unique(selected_period()$Num_of_Jobs)))))
    updateSelectInput(inputId = "hired", choices = c("See All",order(sort(unique(selected_period()$hiringRate)))))
    #pay <- unique(selected_period()$payGroup)
    #updateCheckboxGroupInput(inputId = "pay", choices = pay, selected = pay)
    updateTextInput(inputId = "eid", value = "")
  })
  
  
  filtered_data <- reactive({
    temp <- selected_period()
    if (input$employee != "See All") {
      temp <- filter(temp, Num_of_Employees == input$employee)
    }
    if (input$job != "See All") {
      temp <- filter(temp, Num_of_Jobs == input$job)
    }
    if (input$hired != "See All") {
      temp <- filter(temp, hiringRate == input$hired)
    }
    temp <- filter(temp, payGroup == input$pay)
    if (input$eid != "") {
      temp <- filter(temp, employerId ==  unlist(strsplit(input$eid, ",")))
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
    
  r <- ggplot(Count_Checkin_Weekly, aes(x= as.factor(Week_Num), y= as.numeric(HiredRate))) +
    geom_point(color='red') +
    labs(x= 'Week', y= 'HiredRate',
         title = 'Hiring Rate of Each Employers') +
    ylim(0,600) +
      facet_trelliscope(~ employerId, 
                        nrow = 2, ncol = 3, width = 800,
                       path = 'trellisr',
                      self_contained = TRUE) +
      theme(axis.title.y= element_text(angle=0), 
           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.3),
          axis.ticks.x= element_blank(),
         panel.background= element_blank(), 
            axis.line= element_line(color= 'grey'))
   print(r)
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
  
  output$ChangeStaff <- renderPlotly({
    
    ggplot(selected_value(), aes(x= as.factor(Num_of_Employees), fill = haveKids)) +
      geom_bar() +
      facet_wrap(~educationLevel)+
      ggtitle('No. of Different Employees in Different Education Level') +
      xlab("No. of Employees Employed") +
      ylab("No. of\nEmployers") +
      theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
            axis.line= element_line(color= 'grey'))
    
  })
  
  ###change ggstats##
  output$ChangeStaffJ <- renderPlot({
    input$goButton
    set.seed(1234)
    
  ggbetweenstats(
    data = selected_value(),
    x = !!input$xvariable, 
    y = !!input$yvariable,
    type = input$test,
    title = isolate({
      toTitleCase(input$plotTitle)
    }),
    plot.type = input$plotType,
    mean.ci = TRUE, 
    pairwise.comparisons = TRUE, 
    pairwise.display = "s",
    p.adjust.method = "fdr",
    messages = FALSE)
  })
  #
 # output$ChangeStaffJ <- renderPlotly({
  #  p<- ggplot(selected_value(), aes(x = educationLevel, y = Num_of_Employees, fill=joviality)) + 
  #   ggdist::stat_halfeye(
  #     adjust = .5, 
  #     width = .6, 
  #     .width = 0, 
  #     justification = -.3, 
  #     point_colour = NA) + 
  #   geom_boxplot(
  #     width = .25, 
  #     outlier.shape = NA
  #   ) +
  #   geom_point(
  #     size = 1.3,
  #     alpha = .3,
  #     position = position_jitter(
  #       seed = 1, width = .1
  #     ),
  #     aes(text = paste('Employee: ', selected_value()$participantId,
  #                      'Employer: ', selected_value()$employerId,
  #                      'Date of Exit: ', selected_value()$Date))
  #   ) + 
  #   coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  #   coord_flip() +
  #   ggtitle(label = "Education Level & Joviality")+
  #   theme_minimal()+
  #   theme(plot.title = element_text(size=12, face="bold",hjust = 0.5))+
  #   theme(axis.title.y= element_blank(),
  #         panel.background= element_blank(), axis.line= element_line(color= 'grey'))
    
  # ggplotly(p, tooltip = 'text') 
    
  #  })
  
  
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
      ggtitle('Residents with >1 Employers') +
      xlab("No. of Employers") +
      ylab("No. of\nResidents") +
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
  
  ##### Pubs & Restaurants #####
  
  data1 <- reactive({
    req(input$hBusiness_type)
    df <- sales %>% filter(purpose %in% input$hBusiness_type) %>%
      filter(date_in >= input$hDate[1] & date_in <= input$hDate[2]) %>%
      group_by(venueId) %>% summarise(total_sales = sum(spend)) %>%
      top_n(input$hNumber)
  })
  
  output$hplot1 <- renderPlotly({
    p <- ggplot(data1(),
                aes(x=reorder(venueId, - total_sales), y= total_sales, fill=venueId)) +
      geom_bar(stat = "identity") +
      labs(x = "Business ID" , y = "Total Sales", 
           title = "Top Overall Performing Business")
    ggplotly(p)
  })
  
  data2 <- reactive({
    df <- sales %>% 
      filter(venueId %in% data1()$venueId) %>% 
      filter(date_in >= input$hDate[1] & date_in < input$hDate[2]) %>%
      mutate(YearMonth = format(as.Date(date_in), "%Y-%m")) %>%
      group_by(venueId, YearMonth) %>% 
      summarise(monthly_sales = sum(spend))
  })
  
  output$hplot2 <- renderPlotly({
    p <- ggplot(data2(),
                aes(x=YearMonth, y= monthly_sales, group=venueId)) +
      geom_line(aes(color=venueId)) + 
      labs(x = "Year-Month" , y = "Total Monthly Sales", 
           title = "Monthly Sales")
    ggplotly(p)
  })
  
  ### to list down the venue dynamically for hplot3 and hplot4 ###
  observe({
    updateSelectInput(session, "hBusinessID", choices = unique(sales$venueId))
  })
  
  data3 <- reactive({
    req(input$hBusinessID)
    df <- sales %>%
      filter(venueId %in% input$hBusinessID) %>%
      filter(date_in >= input$hDate[1] & date_in < input$hDate[2]) %>%
      group_by(venueId, date_in, wday_in) %>%
      summarise(daily_sales = sum(spend)) %>%
      ungroup()
  })
  
  output$hplot3 <- renderPlotly({
    # d <- event_data("plotly_click")
    # if(is.null(d)) return(NULL)
    p <- ggbetweenstats(data = data3(), 
                        x = wday_in, 
                        y = daily_sales, 
                        type = "p", 
                        mean.ci = TRUE, 
                        pairwise.comparisons = TRUE, 
                        pairwise.display = "s",
                        p.adjust.method = "fdr",
                        messages = FALSE,
                        title = "Distribution of Weekday Sales",
                        xlab = "Weekday",
                        ylab = "Daily Sales Amount",
                        centrality.point.args = list(size  = 2, color = "darkred"),
                        #centrality.label.args = list(size  = 5, color = "red"),
                        package = "RColorBrewer",
                        palette = "Set2")
    ggplotly(p)
  })
  
  data4 <- reactive({
    req(input$hBusinessID)
    df <- sales %>%
      filter(venueId %in% input$hBusinessID) %>%
      filter(date_in >= input$hDate[1] & date_in < input$hDate[2]) %>%
      group_by(venueId, date_in, wday_in, time_in) %>% 
      summarise(customers_check_in = n()) %>%
      group_by(venueId, wday_in, time_in) %>%
      summarise(ave_checkin = mean(customers_check_in)) %>% na.omit
  })
  
  output$hplot4 <- renderPlotly({
    p <-  ggplot(data4(), aes(time_in, wday_in, fill = ave_checkin)) + 
      geom_tile(color = "white", size = 0.5) + 
      theme_tufte(base_family = "Helvetica") + 
      coord_equal() +
      scale_fill_gradient(name = "# of customers",
                          low = "sky blue", 
                          high = "dark blue") +
      #facet_wrap(~venueId, ncol = 1) +
      labs(x = NULL, y = NULL, 
           title = "Average Check in by weekday and time of the day") +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 7),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6) )
    ggplotly(p)
  })
  
  data5 <- reactive({
    df <- pubs_resto_v %>%
      filter(venueId %in% data1()$venueId) %>%
      filter(date_in >= input$hDate[1] & date_in < input$hDate[2]) %>%
      group_by(venueId) %>%
      summarise(total_sales = sum(spend))
  })
  
  output$hplot5 <-renderTmap({
    tm_shape(buildings)+
      tm_polygons(col = "grey60",
                  size = 2,
                  border.col = "black",
                  border.lwd = 1) +
      tm_shape(data5()) +
      tm_bubbles(col = "red",
                 size = "total_sales",
                 alpha = 0.3) +
      tm_layout(bg.color="white",
                main.title = "Total Sales by Businesses", 
                main.title.position = "center")
  })
  
  data6 <- reactive({
    df <- pubs_resto_v %>%
      filter(purpose %in% input$hBusiness_type) %>%
      filter(date_in >= input$hDate[1] & date_in < input$hDate[2]) %>%
      group_by(venueId) %>%
      summarise(total_sales = sum(spend), visitors = n())
  })
  
  output$hplot6 <- renderPlot({
    funnel_plot(
      numerator = data6()$total_sales,
      denominator = data6()$visitors,
      group = data6()$venueId,
      y_range = c(10,25),
      title = "Cumulative Sales by Cumulative Visitors",           
      x_label = "Total Sales", 
      y_label = "Total Visitors" 
    )
  })
  
  ##### Workplaces #####
  
  data7 <- reactive({
    df <- workplaces %>% 
      filter(Date >= input$h2Date[1] & Date <= input$h2Date[2]) %>%
      group_by(venueId) %>% summarise(total_sales = sum(total_amt)) %>%
      top_n(input$h2Number)
  })
  
  output$hplot7 <- renderPlotly({
    p <- ggplot(data7(),
                aes(x=reorder(venueId, - total_sales), y= total_sales, fill=venueId)) +
      geom_bar(stat = "identity") +
      labs(x = "Employer ID", y = "Total Wages Paid", 
           title = "Total Wages Paid by an Employer") +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 7),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6) )
    ggplotly(p)
  })
  
  data8 <- reactive({
    df <- workplaces %>% 
      filter(venueId %in% data7()$venueId) %>% 
      filter(Date >= input$h2Date[1] & Date < input$h2Date[2]) %>%
      mutate(YearMonth = format(as.Date(Date), "%Y-%m")) %>%
      group_by(venueId, YearMonth) %>% 
      summarise(monthly_sales = sum(total_amt))
  })
  
  output$hplot8 <- renderPlotly({
    p <- ggplot(data8(),
                aes(x=YearMonth, y= monthly_sales, group=venueId)) +
      geom_line(aes(color=venueId)) +
      labs(x = "Year-Month", y = "Wages Paid", 
           title = "Monthly Total Wages Paid by an Employer") +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 7),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6) )
    ggplotly(p)
  })
  
  observe({
    updateSelectInput(session, "h2BusinessID", choices = unique(workplaces$venueId))
  })
  
  data9 <- reactive({
    #req(input$hBusinessID)
    df <- workplaces %>%
      filter(venueId %in% input$h2BusinessID) %>%
      filter(Date >= input$h2Date[1] & Date < input$h2Date[2]) %>%
      mutate(wkday = wday(Date, label = TRUE, abbr = TRUE)) %>%
      group_by(venueId, Date, wkday) %>%
      summarise(daily_sales = sum(total_amt)) %>%
      ungroup()
  })
  
  output$hplot9 <- renderPlotly({
    p <- ggbetweenstats(data = data9(), 
                        x = wkday, 
                        y = daily_sales, 
                        type = "p", 
                        mean.ci = TRUE, 
                        pairwise.comparisons = TRUE, 
                        pairwise.display = "s",
                        p.adjust.method = "fdr",
                        messages = FALSE,
                        title = "Distribution of Total Daily Wages Paid",
                        xlab = "Weekday",
                        ylab = "Total Daily Wages",
                        centrality.point.args = list(size  = 2, color = "darkred"),
                        #centrality.label.args = list(size  = 5, color = "red"),
                        package = "RColorBrewer",
                        palette = "Set2")
    ggplotly(p)
  })
}

shinyApp(ui, server)
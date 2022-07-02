
packages = c('shiny','bslib','tidyverse','plotly','tmap','sf','shinythemes','ggplot2',
             'lubridate','gapminder','dplyr','ggdist','gganimate','reshape',
             'reshape2','scales','hrbrthemes','ggstatsplot','RColorBrewer','sftime',
             'ggthemes','FunnelPlotR','tools','rPackedBar','gt','gtExtras')

for(p in packages){
  if(!require(p, character.only =T)){
    install.packages(p)
  }
  library(p, character.only =T)
}


#========================#
#####Data Extraction######
#========================#

##### Part1 #####
workplaces <- read_rds("data/workplaces.rds")
jobs <- read_csv("data/Jobs.csv")
sales_t <- read_rds("data/sales.rds")
sales <- read_rds("data/sales_r.rds")
pubs <- read_sf("data/Pubs.csv", options = "GEOM_POSSIBLE_NAMES=location")
restaurants <- read_sf("data/Restaurants.csv", options = "GEOM_POSSIBLE_NAMES=location")

### For tmap ###
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
Change_Staff <- read_rds("data/Change_Staff.rds")
Change_Job <- read_rds("data/Change_Job.rds")
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

Count_Checkin_Daily <- Count_Checkin_Daily %>% mutate_at(c(3:8), as.numeric)
Count_Checkin_Weekly <- Count_Checkin_Weekly %>% mutate_at(c(3:8), as.numeric)
Count_Checkin_Weekday <- Count_Checkin_Weekday %>% mutate_at(c(3:8), as.numeric)
Count_Checkin_Monthly <- Count_Checkin_Monthly %>% mutate_at(c(3:8), as.numeric)

Jobs <- read_csv("data/Jobs.csv")
Employers <- read_csv("data/Employers.csv")


#========================#
######   Shiny UI   ######
#========================#

ui <- navbarPage(
  title = "ENGAGE & EXPLORE: An Interactive Exploration of Engagement's Economy",
  fluid = TRUE,
  theme=shinytheme("flatly"),
  id = "navbarID",
  tabPanel("Introduction", 
           #titlePanel("Welcome to our app!"),
           mainPanel(
             HTML("<h1>Instructions For Use</h1>"), 
             HTML("<p></p>"),
             HTML("<h4>The tabs have been arranged sequentially to asnwer the questions in the 2022 IEEE VAST Challenge."), 
             HTML("For a recap on the challenge, please visit <a href='https://isss608group4.netlify.app/introduction.html'>here.</a></h4>"), 
             HTML("<p></p>"),
             HTML("<h4>There are also instructions and guidance on how to use each tab and the sub-tabs attached to the controls."),
             HTML("For further clarification, please visit <a href='https://isss608group4.netlify.app/proposal'>here.</a></h4>"), 
             HTML("<p></p>"),
             HTML("<h4>Lastly, we've also taken a crack at the challenge."),
             HTML("Visit <a href=''>here</a> to check out our solution.</h4>"), 
             HTML("<p></p>"),
             HTML("<h4>Thank you, and we hope that you have as much fun using this R Shiny App as we did creating it.</h4>"),
           )),
  navbarMenu("Business Performance",
             tabPanel("By Revenue",
                      sidebarPanel(
                        conditionalPanel(condition = "input.tabselected==1",
                                         radioButtons(inputId = "hBusiness_type",
                                                      label = "Category",
                                                      choices = c("Pubs" = "Recreation (Social Gathering)",
                                                                  "Restaurants" = "Eating"),
                                                      selected = "Recreation (Social Gathering)"),
                                         dateRangeInput(inputId = "hDate",
                                                        label = "Select Date Range:",
                                                        start = "2022-03-01",
                                                        end = "2023-05-23")),
                        
                        conditionalPanel(condition = "input.tabselected==2",
                                         radioButtons(inputId = "h2Business_type",
                                                      label = "Category",
                                                      choices = c("Pubs" = "Recreation (Social Gathering)",
                                                                  "Restaurants" = "Eating"),
                                                      selected = "Recreation (Social Gathering)"),
                                         HTML("<p>Select a venueID and the available options to view the daily sales distribution across the weekdays.</p>"),
                                         selectInput(inputId = "hBusinessID",
                                                     label = "VenueID",
                                                     "venueID"),
                                         selectInput(inputId = "hPlotType",
                                                     label = "Type of plot",
                                                     choices = c("Box" = "box",
                                                                 "Violin" = "violin",
                                                                 "Box-Violin" = "boxviolin"),
                                                     selected = "boxviolin"),
                                         selectInput(inputId = "hTestType",
                                                     label = "Test Type",
                                                     choices = c("Parametric" = "p",
                                                                 "Non-parametric" = "np",
                                                                 "Robust" = "r",
                                                                 "Bayes Factor" = "bf"),
                                                     selected = "p"),
                                         selectInput(inputId = "hPairwiseDis",
                                                     label  = "Pairwise Display",
                                                     choices = c("Only significant" = "s",
                                                                 "Only non-significant" = "ns",
                                                                 "Everything" = "all"),
                                                     selected = "s"),
                                         selectInput(inputId = "hPAdjust",
                                                     label  = "P Adjust Methods",
                                                     choices = c("Holm" = "holm",
                                                                 "Hochberg" = "hochberg",
                                                                 "Hommel" = "hommel",
                                                                 "Bonferroni" = "bonferroni",
                                                                 "BH" = "BH",
                                                                 "BY" = "BY",
                                                                 "FDR" = "fdr",
                                                                 "None" = "none"),
                                                     selected = "fdr"))),
                      
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Overview", value =1,
                                   splitLayout(
                                     verticalLayout(
                                     plotlyOutput("hplot1"),
                                     HTML("<h5></h5>"),
                                     HTML("<h4> Location of Businesses </h4>"),
                                     tmapOutput("hplot5")),
                                     gt_output("hplot2a")),
                                   splitLayout(
                                     plotOutput("hplot6"))),
                          tabPanel("Deep Dive", value = 2,
                                   plotOutput("hplot3"),
                                   HTML("<h3>    </h3>"),
                                   plotlyOutput("hplot4")),
                          id = "tabselected"))),
             
             tabPanel("By Wages",
                      sidebarPanel(
                        selectInput(inputId = "hEduLvl",
                                    label = "Education Level",
                                    choices = c("Low",
                                                "High School Or College" = "HighSchoolOrCollege",
                                                "Bachelors",
                                                "Graduate",
                                                "All"),
                                    selected = "Low")),
                      
                      mainPanel(
                        splitLayout(
                          plotlyOutput("hplot10"),
                          plotlyOutput("hplot12")),
                        HTML("<h4>    </h4>"),
                        HTML("<h4>Select points on the right chart, to view more details.</h4>"),
                        DT::dataTableOutput("dplot10")))),
  
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
               #selectInput("period", label = "Choose Time Period to View", choices = c("Daily", "Weekly", "Weekday", "Monthly")),
               selectInput("period", 
                           label = "Choose Time Period to View", 
                           choices = list("Daily" = "Date", 
                                          "Weekly" = "weekNum", 
                                          "Monthly" = "yearMonth",
                                          "Weekday" = "Weekday"),
                           selected = c("weekNum")),
               selectInput("employee", label = "Choose Number of Employees Employed by Each Employer", choices = c("See All")),
               selectInput("job", label = "Choose Number of Jobs Offered by Each Employer", choices = c("See All")),
               HTML("<b>Choose Hiring Rate<sup>1</sup> of Each Employer</b>"),
               selectInput("hired", label = " ", choices = c("See All")),
               HTML("<h6><sup>1</sup>Hiring Rate = Number of Employees Employed / Number of Jobs Offered</h6>"),
               selectInput("computepay", label = "Pay Given by Each Employer", 
                                  choices = list("Minimum" = "minPay", 
                                                 "Maximum" = "maxPay", 
                                                 "Average" = "avgPay",
                                                 "Median" = "medPay"),
                                  selected = "avgPay"),
               checkboxGroupInput("pay", label = "Select At Least 1 Pay Group Category", 
                                  choices = list("<=$15 (Low)" = "<=$15 (Low)", 
                                                 "$16-35 (Mid)" = "$16-35 (Mid)", 
                                                 ">$36 (High)" = ">$36 (High)"),
                                  selected = c("<=$15 (Low)","$16-35 (Mid)", ">$36 (High)")),
               #boxplot
               selectInput(inputId = "y_varMAP",
                           label = "Select y-variable For Statistical Plot:",
                           choices = c("Jobs" = "Num_of_Jobs",
                                       "Minimum Pay" = "minPay", 
                                       "Maximum Pay" = "maxPay", 
                                       "Average Pay" = "avgPay",
                                       "Median Pay" = "medPay",
                                       "Hiring Rate" = "hiringRate"),
                           selected = "Num_of_Jobs"),
               selectInput(inputId = "stat_testMAP",
                           label = "Type of Statistical Test:",
                           choices = c("Parametric" = "p",
                                       "Nonparametric" = "np",
                                       "Robust" = "r",
                                       "Bayes Factor" = "bf"),
                           selected = "p"),
               selectInput(inputId = "plot_typeMAP",
                           label = "Type of Plot:",
                           choices = c("Box-Violin" = "boxviolin",
                                       "Box" = "box",
                                       "Violin" = "violin"),
                           selected = "boxviolin"),
               
               HTML("<b>Fliter the Entire Page based on EmployerId</b>"),
               HTML("<h6> Use comma (,) to select multiple employers.</h6>"),
               textInput("eid", label = " ", value = "",placeholder = "eg. 379,862,884"),
               textInput(inputId = "input_title", label = "Map title", placeholder = "Enter text to be used as map title")),
             mainPanel(width =9,
                HTML("<h3 style = 'text-align:center'>Interactive City Map View</h3><br>"),
                splitLayout(cellWidths = c("45%", "55%"),
                HTML("<p style = 'text-align:center'> This map shows the location of each employer and <br>the number of employees employed by them. </p><br>"),
                HTML("<p style = 'text-align:center'> This plot shows distribution of employees employed <br>by each employer with other factors. </p><br>")
                ),
                HTML("<p style = 'text-align:center'>If there are no corresponding data from the selected parameters, an error message will be displayed. </p><br>"),
                textOutput("map_title"),
                splitLayout(cellWidths = c("45%", "55%"),
                  tmapOutput("plot1", height = 800),
                  plotOutput('mapbox', height = 800)),
             ),
             DT::dataTableOutput("aTable")
             ),
             tabPanel("Turnover Rate", 
              sidebarPanel(width = 3,
                 HTML("<h3>Input General Parameters</h3>"),
                 selectInput("change_filter", label = "Choose Time Period to View", choices = c("See All", "Date", "Week", "Month")), 
                 selectInput("change_value", label = "Refine Time Period", choices = c("See All")),
                 selectInput(inputId = "x_var",
                      label = "Select x-variable:",
                      choices = c("Household Size" = "householdSize",
                                    "Have Kids?" = "haveKids",
                                    "Education" = "educationLevel",
                                    "Interest Group" = "interestGroup",
                                    "No. of Employers/Employees" = "NumEmployees.Employers"),
                      selected = "educationLevel"),
                 selectInput(inputId = "y_var",
                      label = "Select y-variable:",
                      choices = c("Joviality" = "joviality",
                                  "Age" = "age",
                                  "No. of Employers/Employees" = "NumEmployees.Employers"),
                      selected = "joviality"),
                 selectInput(inputId = "stat_test",
                      label = "Type of Statistical Test:",
                      choices = c("Parametric" = "p",
                                  "Nonparametric" = "np",
                                  "Robust" = "r",
                                  "Bayes Factor" = "bf"),
                      selected = "p"),
                 selectInput(inputId = "plot_type",
                      label = "Type of Plot:",
                      choices = c("Box-Violin" = "boxviolin",
                                  "Box" = "box",
                                  "Violin" = "violin"),
                      selected = "boxviolin"),
                textInput(inputId = "plot_title",
                      label = "Plot title",
                      placeholder = "Enter text to be used as plot title"),
                      actionButton(inputId = "update_title", "Update Title")
                ),
              mainPanel(width = 9,
               splitLayout( 
                 plotOutput("ChangeStaff",
                                height = "500px"),
                 plotOutput("ChangeJob",
                               height = "500px")
               ), 
               splitLayout( 
                 plotOutput("stats_staff",
                            height = "500px"),
                 plotOutput("stats_job",
                             height = "500px")
               )
             )
  ))
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
    }  
  })
  
  observeEvent(selected_period(), {
    updateSelectInput(inputId = "employee", choices = c("See All", order(sort(unique(selected_period()$Num_of_Employees)))))
    updateSelectInput(inputId = "job", choices = c("See All",order(sort(unique(selected_period()$Num_of_Jobs)))))
    updateSelectInput(inputId = "hired", choices = c("See All",order(sort(unique(selected_period()$hiringRate)))))
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
    if (input$computepay == "minPay") {
     temp <- filter(temp, payGroupMin == input$pay)
    } else if (input$computepay == "maxPay") {
      temp <- filter(temp, payGroupMax == input$pay)
    } else if (input$computepay == "medPay") {
      temp <- filter(temp, payGroupMed == input$pay)
    } else {
      temp <- filter(temp, payGroupAvg == input$pay)
    }
    if (input$eid != "") {
      temp <- filter(temp, employerId ==  unlist(strsplit(input$eid, ",")))
    }
    temp
  })
  
  output$map_title <- renderText({as.character(input$input_title)})
  
  output$plot1 <- renderTmap({
    tm_shape(buildings)+
      tm_polygons(col = "grey60",
                  size = 1,
                  border.col = "white",
                  border.lwd = 1) +
      tm_shape(shp = filtered_data()) +
      tm_bubbles(size = 0.5, col = "Num_of_Employees", title.col = "Number of\nEmployees")
  })
  
    output$mapbox <- renderPlot({
      
      ggbetweenstats(
        data = filtered_data() %>%
          group_by(employerId),
        x = Num_of_Employees, 
        y = !!rlang::sym(input$y_varMAP),
        type = input$stat_testMAP,
        title = 'Distribution of Number of Employees with Other Factors',
        xlab = "Number of Employees Employed by Each Employer",
        plot.type = input$plot_typeMAP,
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE,
        ggtheme = ggplot2::theme_gray())
  })
  
  output$aTable <- DT::renderDataTable({
    DT::datatable(data = filtered_data()%>%
                  select(1:12,14:15),
                  filter = list(position = 'bottom', clear = FALSE, plain = TRUE),
                  rownames = FALSE,
                  options = list(pageLength = 10, autowidth = TRUE,
                    columnDefs = list(list(className = 'dt-center', targets = "_all")))
    )
  }) 
  
  #############   Employer - Turnover Rate   #########################
  selected_filter <- reactive({
    if (input$change_filter == "Date") {
      unique(Change_Staff$Date)
    } else if (input$change_filter == "Week") {
      unique(Change_Staff$weekNum)
    } else if (input$change_filter == "Month") {
      unique(Change_Staff$yearMonth)
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
      filter(Change_Staff, weekNum == input$change_value)
    } else if (input$change_filter == "Month") {
      filter(Change_Staff, yearMonth == input$change_value)
    } else {
      Change_Staff
    } 
    
  })
  
  output$ChangeStaff <- renderPlot({
    
    ggplot(selected_value(), aes(x= as.factor(NumEmployees.Employers), fill = haveKids)) +
      geom_bar() +
      facet_wrap(~educationLevel)+
      ggtitle('No. of Different Employees in Different Education Level') +
      xlab("No. of Employees Employed") +
      ylab("No. of\nEmployers") +
      geom_text(stat='count', aes(label=paste0(stat(count))), position = position_stack(vjust=0.5),size=3)+
      theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
            axis.line= element_line(color= 'grey'))
  })

  output$stats_staff <- renderPlot({
    input$update_title
    ggbetweenstats(
      data = selected_value(),
      x = !!input$x_var, 
      y = !!input$y_var,
      type = input$stat_test,
      title = isolate({
        toTitleCase(input$plot_title)
      }),
      plot.type = input$plot_type,
      mean.ci = TRUE, 
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE)
  })
  
  selected_job <- reactive({
    if (input$change_filter == "Date") {
      filter(Change_Job, Date == input$change_value)
    } else if (input$change_filter == "Week") {
      filter(Change_Job, weekNum == input$change_value)
    } else if (input$change_filter == "Month") {
      filter(Change_Job, yearMonth == input$change_value)
    } else {
      Change_Job
    } 
  })
  
  output$ChangeJob <- renderPlot({
    
    ggplot(selected_job(), aes(x= as.factor(NumEmployees.Employers), fill = haveKids)) +
      geom_bar() +
      facet_wrap(~educationLevel)+
      ggtitle('Employees with >1 Employers') +
      xlab("No. of Employers") +
      ylab("No. of\nResidents") +
      geom_text(stat='count', aes(label=paste0(stat(count))), position = position_stack(vjust=0.5),size=3)+
      theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
            axis.line= element_line(color= 'grey'))
    
  })
  
  output$stats_job <- renderPlot({
    input$update_title
    ggbetweenstats(
      data = selected_job(),
      x = !!input$x_var, 
      y = !!input$y_var,
      type = input$stat_test,
      title = isolate({
        toTitleCase(input$plot_title)
      }),
      plot.type = input$plot_type,
      mean.ci = TRUE, 
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE) 
    
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
  
  ##### By revenue #####
  
  data1 <- reactive({
    req(input$hBusiness_type)
    df <- sales %>% 
      filter(purpose %in% input$hBusiness_type) %>%
      filter(date_in >= input$hDate[1] & date_in <= input$hDate[2]) %>%
      group_by(venueId) %>% 
      summarise(total_sales = sum(daily_sales), 
                min_daily_sales = min(daily_sales),
                avg_daily_sales = mean(daily_sales),
                max_daily_sales = max(daily_sales),
                total_visitors = sum(daily_visitors),
                avg_daily_visitors = mean(daily_visitors)) %>%
      ungroup()
  })
  
  output$hplot1 <- renderPlotly({
    # p <- ggplot(data1(),
    #             aes(x=reorder(venueId, - total_sales), y= total_sales)) +
    #   geom_bar(stat = "identity", fill = "navy blue") +
    #   labs(x = "Business ID" , y = "Total Sales",
    #        title = "Top Overall Performing Businesses") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # ggplotly(p)
    plot_ly(data = data1(),
            x = ~ venueId,
            y = ~ total_sales,
            type ="bar")  %>% 
      layout(xaxis = list(categoryorder = "total descending"),
             title = 'Businesses Performance Ranking By Total Revenue')
    
  })
  
  

  data2 <- reactive({
    df <- sales %>% 
      filter(venueId %in% data1()$venueId) %>% 
      mutate(YearMonth = format(as.Date(date_in), "%Y-%m")) %>%
      group_by(venueId, YearMonth) %>% 
      summarise(monthly_sales = sum(daily_sales))
  })
  

  
  data2a <- reactive({
    df <- sales %>% 
      filter(venueId %in% data1()$venueId) %>% 
      filter(date_in >= input$hDate[1] & date_in <= input$hDate[2]) %>%
      mutate(YearMonth = format(as.Date(date_in), "%Y-%m")) %>%
      group_by(venueId, YearMonth) %>% 
      summarise(monthly_sales = sum(daily_sales))%>%
      ungroup()
      
  })
  
  output$hplot2a <- render_gt({
    spark <- data2a() %>%
      group_by(venueId) %>%
      summarize('Monthly Sales' = list(monthly_sales), 
                .groups = "drop")
    sales_fig <- data2a() %>%
      group_by(venueId) %>%
      summarise("Min" = min(monthly_sales, na.rm = T),
                "Max" = max(monthly_sales, na.rm = T),
                "Average" = mean(monthly_sales, na.rm = T)) %>%
      arrange(desc(Average))
    
    sales_data <- left_join(sales_fig, spark) %>%
      gt() %>%
      gt_plt_sparkline('Monthly Sales')
    
  })
  
  output$hplot2 <- renderPlotly({
    p <- ggplot(data2(),
                aes(x=YearMonth, y= monthly_sales, group=venueId)) +
      geom_line(aes(color=venueId)) + 
      labs(x = "Year-Month" , y = "Total Monthly Sales", 
           title = "Monthly Sales")
    ggplotly(p)
  })
  
  observe({
    x <- sales %>% filter(purpose == input$h2Business_type) 
    updateSelectInput(session, "hBusinessID", choices = sort(as.numeric(unique(x$venueId))))
  })
  
  data3 <- reactive({
    req(input$hBusinessID)
    
    df <- data.frame(sales) %>%
      filter(venueId %in% input$hBusinessID) %>%
      select(venueId,wday_in,daily_sales)

  })

  
  output$hplot3 <- renderPlot({
  ggbetweenstats(data = data3(),
                        x = wday_in,
                        y = daily_sales,
                        type = input$hTestType,
                        plot.type = input$hPlotType,
                        mean.ci = TRUE,
                        # pairwise.comparisons = input$hPairwiseCom,
                        pairwise.display = input$hPairwiseDis,
                        p.adjust.method = input$hPAdjust,
                        messages = FALSE,
                        title = "Distribution of Daily Sales by Weekday",
                        xlab = "Weekday",
                        ylab = "Daily Sales Amount",
                        centrality.point.args = list(size  = 2, color = "darkred"),
                        #centrality.label.args = list(size  = 5, color = "red"),
                        package = "RColorBrewer",
                        palette = "Paired")
      

  })
  
  data4 <- reactive({
    req(input$hBusinessID)
    df <- sales_t %>%
      filter(venueId %in% input$hBusinessID) %>%
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
           title = "Average Check-In by Weekday and Time of the Day") +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 7),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6) )
    ggplotly(p)
  })
  
  revenue <- reactive({
    df <- pubs_resto_v %>%
      filter(venueId %in% data1()$venueId) %>%
      filter(date_in >= input$hDate[1] & date_in <= input$hDate[2]) %>%
      group_by(venueId) %>%
      summarise(total_sales = sum(daily_sales))
  })

  output$hplot5 <-renderTmap({
    #tmap_mode("plot")
    tm_shape(buildings)+
      tm_polygons(col = "grey60",
                  size = 2,
                  border.col = "black",
                  border.lwd = 1) +
      tm_shape(revenue()) +
      tm_bubbles(col = "total_sales",
                 palette = "Blues",
                 alpha = 0.8,
                 colorNA = "white") +
      tm_layout(bg.color="white",
                main.title = "Location of Bussinesses",
                main.title.position = "center")

  })
  

  # output$hplot6 <- renderPlot({
  #   funnel_plot(
  #     numerator = data1()$avg_daily_sales,
  #     denominator = data1()$avg_daily_visitors,
  #     group = data1()$venueId,
  #     y_range = c(0,350),
  #     title = "Cumulative Visitors by Sales per Visitor",
  #     x_label = "Average Daily Visitors",
  #     y_label = "Average Daily Sales per Visitor"
  #   )
  # })

  ##### By Wages #####
  
  data10 <- reactive({
    df <- if(input$hEduLvl == "All"){
      jobs %>%
        mutate(employerId = as.factor(employerId))  %>%
        group_by(employerId) %>% 
        summarise(number_of_jobs = n(),
                  avg_hourly_pay = round(mean(hourlyRate),2),
                  total_hourly_pay = sum(hourlyRate))}
    else{jobs %>%
        filter(educationRequirement == input$hEduLvl) %>%
        mutate(employerId = as.factor(employerId))  %>%
      group_by(employerId, educationRequirement) %>% 
      summarise(number_of_jobs = n(),
                avg_hourly_pay = round(mean(hourlyRate),2),
                total_hourly_pay = sum(hourlyRate))}
  })
  
  output$hplot10 <- renderPlotly({
    plotly_packed_bar(input_data = data10(), 
                      label_column = 'employerId',
                      value_column = 'avg_hourly_pay',
                      number_rows = 10,
                      plot_title = 'Top 10 Average Hourly Wage by Education Level', 
                     # xaxis_label = 'Average Hourly Wage',
                      hover_label = 'Average Hourly Wage',
                      min_label_width = 0.001,
                      label_color = 'white') 
  })


  output$hplot12 <- renderPlotly({
    
    q2 <- ggplot(data = data10(), aes(x = input$hEduLvl , y = avg_hourly_pay)) +
      geom_boxplot() +
      #geom_dotplot(binaxis = 'y', stackdir = 'center',  dotsize = 0.3)
      geom_point(aes(color = educationRequirement), alpha = 0.8,
                 position = position_jitterdodge()) +
      scale_color_manual(values=c("#3182bd")) +
      labs(title = "Distribution of Average Hourly Wage by Employer",
           x = "Education Level",
           y = "Average Hourly Wage") +
      theme(text = element_text(size = 10),
            # axis.title.x = element_blank(),
            # axis.title.y = element_blank(),
            legend.position = "none") 

    q1 <- ggplot(data = data10(), aes(x = input$hEduLvl , y = avg_hourly_pay)) +
      geom_boxplot() +
      #geom_dotplot(binaxis = 'y', stackdir = 'center',  dotsize = 0.3)
      geom_point(alpha = 0.8, aes(color = "#3182bd" )) +
       scale_color_manual(values=c("#3182bd")) +
      labs(title = "Distribution of Average Hourly Wage by Employer",
           x = "Education Level",
          y = "Average Hourly Wage") +
      theme(text = element_text(size = 10),
            #axis.title.x = element_blank(),
            # axis.title.y = element_blank(),
            legend.position = "none")

    p<- if(input$hEduLvl == "All"){
        q1}
        else{q2}
    
     ggplotly(p) %>% layout(dragmode = "select")
    
  })
  
  
  
  output$dplot10 <- DT::renderDataTable({
    d <-  event_data("plotly_selected")
    if (is.null(d)) return(NULL)
    
    df<- data.frame(d)
    
    p <- data10() %>%
      filter(avg_hourly_pay %in% df$y)

      DT::datatable(p,
                    filter = 'top',
                    rownames = TRUE,
                    options = list(lengthMenu = c(5,10,20,50), pageLength = 5))
  })

  
}

shinyApp(ui, server)
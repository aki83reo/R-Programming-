library(shiny)
library(plotly)
library(shinydashboard)
library(shinyBS)


ui<-dashboardPage(
    dashboardHeader(title = "Payzello Analytics"),
    dashboardSidebar(
    sidebarMenu(
     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
       menuItem("Transactions",tabName='txn_alter',icon = icon("dashboard"),
             menuSubItem("txn1",tabName='txn_new'),
             menuSubItem("txn2",tabName='txn'),
             menuSubItem("txn3",tabName='txn_old' )
            
    ),
    menuItem("KYC",tabName='txn_alter',icon = icon("th"))
      )
  
  ),
  dashboardBody(

    
#######################################Main tab item ####################################################    

  tabItems(
  
        ############################################################## 1st  tab #########################
        tabItem("dashboard", "Dashboard Initial  content"),
        
        ############################################################## 2nd tab ###########################
      	tabItem(tabName='txn_new',
      
        fluidRow(
            fluidRow(
      	    div(style="display: inline-block;vertical-align:left; width:40px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:left; width: 150px;",selectInput("cats", "Categories:",c('pos','atm','ecom','bharathQR'), selected='pos')),
      	    div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("rng", "Range:",c('Daily','Weekly','Monthly'), selected='Daily')),
      
      
       div(style="display: inline-block;vertical-align:left; width: 50px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:left; width: 150px;",selectInput("ddllgra", "Function:",c('mean','median','sd','count','min','max'), selected='mean')),
      	    div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("ddllgra", "Function:",c('mean','median','sd','count','min','max'), selected='mean')),
      
      div(style="display: inline-block;vertical-align:left; width: 50px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:left; width: 150px;",selectInput("ddllgra", "Function:",c('mean','median','sd','count','min','max'), selected='mean')),
      	    div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("ddllgra", "Function:",c('mean','median','sd','count','min','max'), selected='mean'))
      
      
      
                          ),
      	#infoBox("New Orders", 5*2, icon = icon("credit-card")),
         #   infoBox("New Orders", 5 * 2, icon = icon("credit-card")),
          #  infoBox("New Orders",5 * 2, icon = icon("credit-card"))
      
      #infoBox("progrs1"),
      valueBoxOutput("progrs1"),
      infoBox("progrs2"),
      infoBox("progrs3")
            
      ),
      fluidRow(
        
      ###  first  box   to  be  plotted  with  graph  
      box(
        
          
          fluidRow(column(6,h3("Debit Credit")),
                   column(6, selectInput(inputId = "options", label = "", 
                                         choices = list("Daily","Weekly",
                                                        "Monthly"))))
      
        
      ,background = "orange", solidHeader = TRUE,height=500,
         column(6,
                fluidRow(column(3)),fluidRow(),fluidRow(),fluidRow(),
      div(plotlyOutput("plot",width = "500px", height = "300px"), align = "center")),
      
      #######################################Debit-Credit############################
      bsModal('boxPopUp', '', '',   ####  popup  for debit-credit  graph  1st popup
      h4("My bar graph ",align="center"),
      div(plotlyOutput("plot2",width = "500px", height = "300px"), align = "center")),
      
      bsModal('boxPopUp2', '', '', ####  popup  for debit-credit graph  2nd popup
              h4("My bar graph ",align="center"),
              div(plotlyOutput("plot3",width = "500px", height = "300px"), align = "center"))
      
      ##############################################################################
      
      #######################################Cards###############################
      
     
      
      ),
      
      
      
      
      #######  first box ends  here  ....
      
      ########################secound box  - physical & virtual card  details ########
      
      box(fluidRow(column(6,h3("Cards")),
                   column(6, selectInput(inputId = "options_cards", label = "", 
                                         choices = list("Daily","Weekly",
                                                        "Monthly")))),
          
          column(6,
                 fluidRow(column(3)),fluidRow(),fluidRow(),fluidRow(),
                 div(plotlyOutput("plot_card_1",width = "500px", height = "300px"), align = "center")),
          bsModal('box_card', '', '',   ####  popup  for Cards  graph  1st popup
                  h4("My bar graph ",align="center"),
                  div(plotlyOutput("plot_card_2",width = "500px", height = "300px"), align = "center")),
          
          bsModal('box_card_2', '', '', ####  popup  for Cardsgraph  2nd popup
                  h4("My bar graph ",align="center"),
                  div(plotlyOutput("plot_card_3",width = "500px", height = "300px"), align = "center"))
          ##############################################################################
          , background = "yellow", solidHeader = TRUE,height=500),
      
      ##############################################secound card  box ends  here ######
      
      box(title = "Histogram", background = "aqua", solidHeader = TRUE,height=300),
      box(title = "Histogram", background = "red", solidHeader = TRUE,height=300)
      )
      ),
      
      ############################################################## 3rd tab ###########################
      tabItem(tabName='txn',
              h2('txn  first results')),
      
      ############################################################## 4th tab ###########################
      
      tabItem(tabName='txn_old',
              h2('txn  secound results'))
      ##############################################################################################################

))
)

###############################################################################################################
###########################################################UI  finish ########################################
##############################################################################################################


##############################################Server  Starts #############################################
server <- function(input, output,session) { 
  
  ########################info box 1 ###############(shows category values daily,weekly,monthly)
  output$progrs1 <- renderValueBox({
    valueBox(
      h4("category values"),
      
      if(input$rng=="Daily"){
        daily_cat_info<-read.csv("F:/analytical_dashboard/dummy_dataset/category_info_box1/trans_cat_daily.csv")
        
        subset_value_daily<-daily_cat_info[daily_cat_info$trans_categories==input$cats,]$total
        paste0(subset_value_daily)
        
      }else if(input$rng=="Weekly"){
      weekly_cat_info<-read.csv("F:/analytical_dashboard/dummy_dataset/category_info_box1/trans_cat_weekly.csv")
      subset_value_weekly<-weekly_cat_info[weekly_cat_info$trans_categories==input$cats,]$total
      paste0(subset_value_weekly)

      }else if(input$rng=="Monthly"){
        monthly_cat_info<-read.csv("F:/analytical_dashboard/dummy_dataset/category_info_box1/trans_cat_monthly.csv")
        subset_value_monthly<-monthly_cat_info[monthly_cat_info$trans_categories==input$cats,]$total
        paste0(subset_value_monthly)
       
      },
      icon = icon("credit-card"),
      color = "purple"
    )
    
  })
  
###############################info Box 1  ends  here  ##########################
  
#######  First box  to be  filled  with  graph  

output$plot <- renderPlotly({   ###plot  inside  the  app  
  
  
  if (input$options=="Daily"){
    
    #####################################Daily plot 
    txn_dat<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_dc/debit_credit_daily.csv")
    
    txn_cat<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_dc/deb_int_ext_daily.csv")
    
    txn_sub_cat<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_dc/deb_trn_sub_cat_daily.csv")
    
  plot_ly(txn_dat, x=txn_dat$transaction, y=txn_dat$total, type = 'bar',
               marker = list(color = '#12550B',
              
                                         width = 0.1)) %>%layout(
                                           paper_bgcolor='rgba(0,0,0,0,0)'
                                         )%>%
    layout(title = "Transasctional details daily",
           xaxis = list(title = 'transactions'),
           yaxis = list(title = 'total'))
    #layout(plot_bgcolor='rgb(0, 0, 0)') 
  
  #######################################check  weekly 
  }else if (input$options=="Weekly"){
    
    txn_dat_weekly<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_dc/debit_credit_weekly.csv")
    
    txn_cat_weekly<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_dc/deb_int_ext_weekly.csv")
    
    txn_sub_cat_weekly<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_dc/deb_trn_sub_cat_weekly.csv")
    
    plot_ly(txn_dat_weekly, x=txn_dat_weekly$transaction, y=txn_dat_weekly$total, type = 'bar',
            marker = list(color = '#12550B',
                          
                          width = 0.1)) %>%layout(
                            paper_bgcolor='rgba(0,0,0,0,0)'
                          )%>%
      layout(title = "Transasctional details weekly",
             xaxis = list(title = 'transactions'),
             yaxis = list(title = 'total'))
    #layout(plot_bgcolor='rgb(0, 0, 0)') 
    
    
    #######################################check monthly
    
  }else if (input$options=="Monthly"){
    
    txn_dat_monthly<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_dc/debit_credit_monthly.csv")
    
    txn_cat_monthly<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_dc/deb_int_ext_monthly.csv")
    
    txn_sub_cat_monthly<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_dc/deb_trn_sub_cat_monthly.csv")
    
    plot_ly(txn_dat_monthly, x=txn_dat_monthly$transaction, y=txn_dat_monthly$total, type = 'bar',
            marker = list(color = '#12550B',
                          
                          width = 0.1)) %>%layout(
                            paper_bgcolor='rgba(0,0,0,0,0)'
                          )%>%
      layout(title = "Transasctional details monthly",
             xaxis = list(title = 'transactions'),
             yaxis = list(title = 'total'))
    #layout(plot_bgcolor='rgb(0, 0, 0)') 
  
  }
})



####  function  to  create  a  pop up window  to show 
###  observe  event  will  execute everytime  a  click  happenss the  graph  .

############################################################# First  pop up window (1st box graph)
observeEvent(event_data("plotly_click", source = "A"), {
  toggleModal(session, "boxPopUp", toggle = "toggle")
})

############################################################# secound pop up window (1st box graph)
observeEvent(event_data("plotly_click", source = "A"), {
  toggleModal(session, "boxPopUp2", toggle = "toggle")
})

############################################################################
############################################################# cards pop up window (1st box graph)
observeEvent(event_data("plotly_click", source = "A"), {
  toggleModal(session, "box_card", toggle = "toggle")
})

############################################################# cards pop up window (1st box graph)
observeEvent(event_data("plotly_click", source = "A"), {
  toggleModal(session, "box_card_2", toggle = "toggle")
})

#############################################################first plot in window
output$plot2 <- renderPlotly({  ###   plot to  be shown in the pop  up window  

  #########################################Daily#######################  
  if (input$options=="Daily"){
    #txn_dat_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_dc/debit_credit_daily.csv")
    
    txn_cat_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_dc/deb_int_ext_daily.csv")
    
    #txn_sub_cat_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_dc/deb_trn_sub_cat_daily.csv")
    
  mouse_event <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
  deb_cred<- mouse_event  ###  data we will  get  after  clicking  the data 
  print(deb_cred)
  trans_subset <- txn_cat_daily[txn_cat_daily$trans_types==deb_cred$x,]
  plot_ly(trans_subset, x=~trans_categories, y=~trans_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
    layout(title = "Transasctional details categories",
           xaxis = list(title = 'categories'),
           yaxis = list(title = 'total'))%>%layout(
             paper_bgcolor='#ED75EF')
  
  #######################################weekly
  }else if (input$options=="Weekly"){
    #txn_dat_weekly<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_dc/debit_credit_weekly.csv")
    
    txn_cat_weekly<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_dc/deb_int_ext_weekly.csv")
    
    #txn_sub_cat_weekly<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_dc/deb_trn_sub_cat_weekly.csv")
    
    mouse_event <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
    deb_cred<- mouse_event  ###  data we will  get  after  clicking  the data 
    print(deb_cred)
    trans_subset <- txn_cat_weekly[txn_cat_weekly$trans_types==deb_cred$x,]
    plot_ly(trans_subset, x=~trans_categories, y=~trans_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
      layout(title = "Transasctional details categories",
             xaxis = list(title = 'categories'),
             yaxis = list(title = 'total'))%>%layout(
               paper_bgcolor='#ED75EF')
    
  }else if (input$options=="Monthly"){
   # txn_dat_monthly<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_dc/debit_credit_monthly.csv")
    
    txn_cat_monthly<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_dc/deb_int_ext_monthly.csv")
    
    #txn_sub_cat_monthly<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_dc/deb_trn_sub_cat_monthly.csv")
    
    mouse_event <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
    deb_cred<- mouse_event  ###  data we will  get  after  clicking  the data 
    print(deb_cred)
    trans_subset <- txn_cat_monthly[txn_cat_monthly$trans_types==deb_cred$x,]
    plot_ly(trans_subset, x=~trans_categories, y=~trans_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
      layout(title = "Transasctional details categories",
             xaxis = list(title = 'categories'),
             yaxis = list(title = 'total'))%>%layout(
               paper_bgcolor='#ED75EF')
  }
})

#############################################################secound plot in pop up window

output$plot3 <- renderPlotly({  ###   plot to  be shown in the pop  up window 
  if (input$options=="Daily"){
    
   # txn_dat_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_dc/debit_credit_daily.csv")
    
    #txn_cat_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_dc/deb_int_ext_daily.csv")
    
    txn_sub_cat_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_dc/deb_trn_sub_cat_daily.csv")
    
  mouse_event <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
  deb_sub_cat<- mouse_event  ###  data we will  get  after  clicking  the data 
  print(deb_sub_cat)
  trans_subset <- txn_sub_cat_daily[txn_sub_cat_daily$trans_categories==deb_sub_cat$x,]
  plot_ly(trans_subset, x=~trans_sub_categories, y=~trans_sub_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
    layout(title = "Transasctional details categories",
           xaxis = list(title = 'categories'),
           yaxis = list(title = 'total'))%>%layout(
             paper_bgcolor='#ED75EF')
  
  } else if (input$options=="Monthly"){
    
      #txn_dat_monthy<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_dc/debit_credit_monthly.csv")
      
      #txn_cat_monthly<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_dc/deb_int_ext_monthly.csv")
      
      txn_sub_cat_monthly<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_dc/deb_trn_sub_cat_monthly.csv")
      
      mouse_event <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
      deb_sub_cat<- mouse_event  ###data we will  get  after  clicking  the data 
      print(deb_sub_cat)
      trans_subset <- txn_sub_cat_monthly[txn_sub_cat_monthly$trans_categories==deb_sub_cat$x,]
      plot_ly(trans_subset, x=~trans_sub_categories, y=~trans_sub_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
        layout(title = "Transasctional details categories",
               xaxis = list(title = 'categories'),
               yaxis = list(title = 'total'))%>%layout(
                 paper_bgcolor='#ED75EF')
      
  }else if (input$options=="Weekly"){
    
    
    #txn_dat_weekly<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_dc/debit_credit_weekly.csv")
    
    #txn_cat_weekly<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_dc/deb_int_ext_weekly.csv")
    
    txn_sub_cat_weekly<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_dc/deb_trn_sub_cat_weekly.csv")
    
    mouse_event <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
    deb_sub_cat<- mouse_event  ###  data we will  get  after  clicking  the data 
    print(deb_sub_cat)
    trans_subset <- txn_sub_cat_weekly[txn_sub_cat_weekly$trans_categories==deb_sub_cat$x,]
    plot_ly(trans_subset, x=~trans_sub_categories, y=~trans_sub_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
      layout(title = "Transasctional details categories",
             xaxis = list(title = 'categories'),
             yaxis = list(title = 'total'))%>%layout(
               paper_bgcolor='#ED75EF')
  }
    
})

#####################################first box  plot ends  here####################################

#########################secound box  plot  starts  here  #####################################

output$plot_card_1<-renderPlotly({
  

  if(input$options_cards=="Daily"){
    
    card_txn_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_vc/physical_virtual_daily.csv")
    #card_cat_txn_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_vc/physical_virtual_category_daily.csv")
    #card_sub_cat_txn_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_vc/physical_virtual_sub_cat_daily.csv")
    plot_ly(card_txn_daily, x=card_txn_daily$card_type, y=card_txn_daily$total, type = 'bar',
            marker = list(color = '#12550B',
                          width = 0.1)) %>%layout(
                            paper_bgcolor='rgba(0,0,0,0,0)'
                          )%>% 
      layout(title = "Card details daily",
             xaxis = list(title = 'Card type'),
             yaxis = list(title = 'total'))
    
  }else if (input$options_cards=="Weekly"){
      
    card_txn_week<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_vc/physical_virtual_weekly.csv")
    #card_cat_txn_week<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_vc/physical_virtual_category_weekly.csv")
    #card_sub_cat_txn_week<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_vc/physical_virtual_sub_cat_weekly.csv")
    plot_ly(card_txn_week, x=card_txn_week$card_type, y=card_txn_week$total, type = 'bar',
            marker = list(color = '#12550B',
                          width = 0.1)) %>%layout(
                            paper_bgcolor='rgba(0,0,0,0,0)'
                          )%>%
      layout(title = "Card details weekly",
             xaxis = list(title = 'Card type'),
             yaxis = list(title = 'total'))
    
  }else if (input$options_cards=="Monthly"){
    
    card_txn_month<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_vc/physical_virtual_monthly.csv")
   # card_cat_txn_month<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_vc/physical_virtual_category_monthly.csv")
    #card_sub_cat_txn_month<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_vc/physical_virtual_sub_cat_monthly.csv")
    plot_ly(card_txn_month, x=card_txn_month$card_type, y=card_txn_month$total, type = 'bar',
            marker = list(color = '#12550B',
                          
                          width = 0.1)) %>%layout(
                            paper_bgcolor='rgba(0,0,0,0,0)'
                          )%>%
      layout(title = "Card details Monthly",
             xaxis = list(title = 'Card type'),
             yaxis = list(title = 'total'))
  }
  
  
})    


output$plot_card_2<-renderPlotly({
  
  if(input$options_cards=="Daily"){
    
  card_cat_txn_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_vc/physical_virtual_category_daily.csv")
  mouse_event <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
  card_cat<- mouse_event  ###  data we will  get  after  clicking  the data 
  print(card_cat)             
  card_trans_subset <- card_cat_txn_daily[card_cat_txn_daily$card_type==card_cat$x,]
  plot_ly(card_trans_subset, x=~trans_categories, y=~trans_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
    layout(title = "Card details categories daily",
           xaxis = list(title = 'card categories'),
           yaxis = list(title = 'total'))%>%layout(
             paper_bgcolor='#ED75EF')
    
  } else if (input$options_cards=="Weekly"){
    card_cat_txn_week<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_vc/physical_virtual_category_weekly.csv")
    
    mouse_event <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
    card_cat_week<- mouse_event  ###  data we will  get  after  clicking  the data 
    #print(card_cat_week)
    card_trans_subset_week <-card_cat_txn_week[card_cat_txn_week$card_type==card_cat_week$x,]
    plot_ly(card_trans_subset_week, x=~trans_categories, y=~trans_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
      layout(title = "Card details categories weekly",
             xaxis = list(title = 'card categories'),
             yaxis = list(title = 'total'))%>%layout(
               paper_bgcolor='#ED75EF')
    
  }else if (input$options_cards=="Monthly"){
    #card_cat_txn_week<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_vc/physical_virtual_category_weekly.csv")
    #card_txn_month<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_vc/physical_virtual_monthly.csv")
    card_cat_txn_month<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_vc/physical_virtual_category_monthly.csv")
    
    mouse_event <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
    card_cat_month<- mouse_event  ###  data we will  get  after  clicking  the data 
    print(card_cat_month)
    card_trans_subset_month<-card_cat_txn_month[card_txn_month$card_type==card_cat_month$x,]
    plot_ly(card_trans_subset_month, x=~trans_categories, y=~trans_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
      layout(title = "Card details categories monthly",
             xaxis = list(title = 'card categories'),
             yaxis = list(title = 'total'))%>%layout(
               paper_bgcolor='#ED75EF')
  }
  
  
})


output$plot_card_3<-renderPlotly({
  
  if(input$options_cards=="Daily"){
    
card_sub_cat_txn_daily<-read.csv("F:/analytical_dashboard/dummy_dataset/daily_vc/physical_virtual_sub_cat_daily.csv")

mouse_event_sub <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
card_sub_cat<- mouse_event_sub  ###  data we will  get  after  clicking  the data 
print(card_sub_cat)             
card_sub_subset <-card_sub_cat_txn_daily[card_sub_cat_txn_daily$trans_categories==card_sub_cat$x,]
plot_ly(card_sub_subset, x=~trans_sub_categories, y=~trans_sub_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
  layout(title = "Card details sub categories daily",
         xaxis = list(title = 'card categories'),
         yaxis = list(title = 'total'))%>%layout(
           paper_bgcolor='#ED75EF')

  }else if(input$options_cards=="Weekly"){
    
    card_sub_cat_txn_weekly<-read.csv("F:/analytical_dashboard/dummy_dataset/weekly_vc/physical_virtual_sub_cat_weekly.csv")
    
    mouse_event_sub_week <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
    card_sub_cat_week<- mouse_event_sub_week  ###  data we will  get  after  clicking  the data 
    print(card_sub_cat_week)             
    card_sub_subset_week <-card_sub_cat_txn_weekly[card_sub_cat_txn_weekly$trans_categories==card_sub_cat_week$x,]
    plot_ly(card_sub_subset_week, x=~trans_sub_categories, y=~trans_sub_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
      layout(title = "Card details sub categories weekly",
             xaxis = list(title = 'card categories'),
             yaxis = list(title = 'total'))%>%layout(
               paper_bgcolor='#ED75EF')
    
  }else if(input$options_cards=="Monthly"){
    
    card_sub_cat_txn_monthly<-read.csv("F:/analytical_dashboard/dummy_dataset/monthly_vc/physical_virtual_sub_cat_monthly.csv")
    
    mouse_event_sub_week <- event_data("plotly_click", source = "A", session = shiny::getDefaultReactiveDomain())
    card_sub_cat_week<- mouse_event_sub_week  ###  data we will  get  after  clicking  the data 
    print(card_sub_cat_week)             
    card_sub_subset_week <-card_sub_cat_txn_monthly[card_sub_cat_txn_monthly$trans_categories==card_sub_cat_week$x,]
    plot_ly(card_sub_subset_week, x=~trans_sub_categories, y=~trans_sub_categories_amount, type = "bar")%>%  ###  this  is the final  graph 
      layout(title = "Card details sub categories monthly",
             xaxis = list(title = 'card categories'),
             yaxis = list(title = 'total'))%>%layout(
               paper_bgcolor='#ED75EF')
    
  }
  
  
})
#################################################################################################
  
  }  ###  server code  ends here 


###########################################################################################################
shinyApp(ui = ui, server = server)
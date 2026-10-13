library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(colourpicker)
library(treemapify)

ui<-tagList(tags$head(
  tags$style(HTML("
      body {
        background: #f7f9fc;
      }

      .intro-container {
        background: white;
        padding: 35px;
        margin-top: 40px;
        border-radius: 12px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        max-width: 2000px;
        margin-left: auto;
        margin-right: auto;
      }

      .intro-title {
        font-size: 100px;
        font-weight: 1000;
        text-align: center;
        color: #2c3e50;
        margin-bottom: 10px;
      }

      .intro-title:after {
        content: '';
        display: block;
        width: 80px;
        height: 4px;
        background: #18bc9c;
        margin: 12px auto 20px auto;
        border-radius: 2px;
      }

      .intro-text {
        font-size: 30px;
        line-height: 1.6;
        color: #4a4a4a;
        text-align: center;
        max-width: 2000px;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 20px;
      }
    "))
), themeSelector(),
  navbarPage(title="Introduction to Data Visualization",
               theme = shinytheme("flatly"),
              
               ##Tab 1: Intro
             tabPanel(
               "Introduction",
               div(class = "intro-container",
                   h2("Welcome to the Data Visualization Learning App!", class = "intro-title"),
                   p("This app helps you explore different types of data visualizations interactively.
             Use the tabs above to navigate through discrete, continuous, and categorical data examples.",
                     class = "intro-text"),
                   p("Each section lets you choose a visualization, pick a dataset variable,
             and adjust controls to see how visualizations change dynamically.",
                     class = "intro-text")
               )
             ),
               
               ##Tab 2: Discrete Data Visualization
               tabPanel("Discrete Data Visualization",
                        sidebarLayout(
                          sidebarPanel(h4("Data Selection"),
                                       selectInput("disc_list","Pick dataset",choices=c("A poisson random variable","Waiting Times in Faithful","Number of carburetors in mtcars","Number of lynx trapped each year")),
                                       conditionalPanel(condition = "input.disc_data == 'A poisson random variable' ",
                                                        sliderInput("disc_n", "Number of observations (counts to simulate):",
                                                                    min = 50, max = 2000, value = 500, step = 50),
                                                        sliderInput("disc_lambda", "Poisson mean (lambda):",
                                                                    min = 0.5, max = 20, value = 3, step = 0.5)),
                                       tags$hr(),
                                       
                                       h4("Plot Controls",class="font-weight:40px"),
                                       sliderInput("dot_size", "Dot size:", 1, 5, 2,step=0.5),
                                       sliderInput("dot_trans","Dot transparency",0.1,1,0.5),
                                       sliderInput("line_width", "Line width:", 0.1, 1, 0.5, step = 0.1),
                                       
                                       colourInput("stack_palette", "Choose color palette:", value = "#1FB8AB")),
                          
                          
                          
                          mainPanel( h3("1) Line Plot"),
                                     p("A line plot shows progression of discrete values.") ,
                                     plotlyOutput("disc_line"),
                                     hr(),
                                     h3("2) Dot plot (stripchart-like)"),
                                     p("Dots show each observation — useful for small/medium sample sizes."),
                                     plotlyOutput("disc_dot"),
                                     hr(),
                                     h3("3) Step Plot") ,
                                     p("Shows cumulative counts of each value — useful to read quantiles."),
                                     plotlyOutput("disc_cum"))
                          
                        )
                        
               ),
               
               
               ##Tab 3: Continuous Data Visualization
               tabPanel("Continuous Data Visualization",
                        sidebarLayout(
                          sidebarPanel(h4("Data Selection"),
                                       selectInput("cont_list","Pick dataset",choices=c("A normal random variable","A gamma random variable","Ozone levels in airquality",
                                                                                        "Miles per gallon in mtcars")),
                                       conditionalPanel(condition = "input.cont_list == 'A normal random variable' ",
                                                        sliderInput("cont_mean", "Normal mean:", min = -5, max = 5, value = 0, step = 0.5),
                                                        sliderInput("cont_sd", "Normal SD:", min = 0.1, max = 5, value = 1, step = 0.1),
                                                        sliderInput("cont_nn", "Number of samples:", min = 50, max = 5000, value = 1000, step = 50)),
                                        conditionalPanel(condition = "input.cont_list == 'A gamma random variable' ",
                                                         sliderInput("cont_shape", "Gamma shape:", min = 0.5, max = 10, value = 2, step = 0.1),
                                                         sliderInput("cont_scale", "Gamma scale:", min = 0.1, max = 5, value = 1, step = 0.1),
                                                         sliderInput("cont_ng", "Number of samples:", min = 50, max = 5000, value = 1000, step = 50)),           
                                       tags$hr(),
                                       
                                       h4("Plot Controls"),
                                      # numericInput("cont_bins", "Histogram bin count:", value = 30, min = 5, max = 80, step = 1),
                                       sliderInput("cont_width", "Histogram bin width:", value = 0.5, min = 0.1, max = 20, step = 0.4),
                                       sliderInput("line_width2", "Q-Q Line width:", 0.1, 1, 0.5, step = 0.1),
                                       
                                       #colourInput("out_col","Choose outlier colour",value="#AB20BD"),
                                       colourInput("stack_palette2", "Choose plot color:", value = "#1F5DBA"),
                                       colourInput("line_col", "Choose line color:", value = "#020502")),
                          
                          
                          
                          mainPanel( h3("1) Histogram"),
                                     p("Histograms show the data distribution"),
                                     plotlyOutput("cont_hist"),
                                     hr(),
                                     h3("2) Boxplot"),
                                     p("Boxplots show median, interquartile range and potential outliers."),
                                     plotlyOutput("cont_box"),
                                     hr(),
                                     h3("3) Q-Q plot"),
                                     p("Q-Q (quantile-quantile) plot compares your data quantiles to a theoretical distribution (normal) to check normality."),
                                     plotlyOutput("cont_qq") )
                        ) ),
               
                ##Tab3: Categorical Tab
              tabPanel("Categorical Data Visualization",
                       sidebarLayout(
                         sidebarPanel(h4("Data selection"),
                                      selectInput("cat_list", "Choose variable to visualize:",
                                                  choices = c("A random sample of student numbers in colleges in UTEP","Species in iris",
                                                              "Passenger class in Titanic dataset",
                                                              "Eye color in HairEyeColor")),
                                      conditionalPanel(condition="input.cat_list == 'A random sample of student numbers in colleges in UTEP' ",
                                                       sliderInput("ran_n","Sample size",100,500,200,step=50)),
                                      
                                      tags$hr(),
                                      h4("Plot Controls"),
                                      colourInput("stack_palette3","Choose Plot Colour","#AB20BD")
                                      ),
                           mainPanel(h3("1) Bar Chart"),
                                     p("Bar charts visualise counts or proportions of categories."),
                                     plotlyOutput("cat_bar"),
                                     hr(),
                                     h3("2) Lollipop Chart"),
                                     p("Lollipop charts provide a modern alternative to bar charts."),
                                     plotlyOutput("cat_lol"),
                                     hr(),
                                     h3("3) Tree map"),
                                     p("Treemaps visualize proportions using area."),
                                     plotlyOutput("cat_tree"),
                                     hr(),
                                     h3("4) Pie chart"),
                                     p("Pie charts show composition — good for simple, small number of categories."),
                                     plotlyOutput("cat_pie")
                             
                           )
                         
                       )
                      )



)

)



server<-function(input,output){
 
  ##Discrete server 
  disc_data <- reactive({
    switch(
      input$disc_list,
      
      "A poisson random variable" = {
        set.seed(12)
        data.frame(
          index = 1:input$disc_n,
          value = rpois(input$disc_n, input$disc_lambda)
        )
      },
      "Waiting Times in Faithful" = {
        faithful %>%
          mutate(index = 1:n(), value = waiting)
      },
      "Number of carburetors in mtcars" = {
        mtcars %>% 
          mutate(index = 1:n(), value = carb)
      },
      "Number of lynx trapped each year" = {
        data.frame(
          index = time(lynx),
          value = as.numeric(lynx)
        )
      }
    )
  })
  
  output$disc_line <- renderPlotly({
    df <- disc_data()
    p <- ggplot(disc_data(), aes(index, value)) +
      geom_line(linewidth = input$line_width, color = input$stack_palette) +
      labs(x = "Index", y = "Value")
    ggplotly(p)
  })
  
  
  output$disc_dot <- renderPlotly({
    p <- ggplot(disc_data(), aes(x = value, y = index)) +
      geom_point(size = input$dot_size, alpha = input$dot_trans,
                 color = input$stack_palette)
    ggplotly(p)
  })
  
  output$disc_cum <- renderPlotly({
    df <- disc_data() %>% count(value) %>% arrange(value) %>%
      mutate(cum = cumsum(n))
    p <- ggplot(df, aes(value, cum)) +geom_step(linewidth=input$line_width,color=input$stack_palette) +
      geom_point()
    ggplotly(p)
  })
  
  
  
##Continuous server

  cont_data<-reactive({
    switch(input$cont_list,
           "A normal random variable"={set.seed(12)
             data.frame(value=rnorm(n=input$cont_nn,mean=input$cont_mean,sd=input$cont_sd))},
             
            "A gamma random variable"={set.seed(12)
              data.frame(value=rgamma(n=input$cont_ng,shape = input$cont_shape,scale = input$cont_scale))},
           
           "Ozone levels in airquality"={airquality %>% mutate(value=Ozone) %>% drop_na(value)},
           
           "Miles per gallon in mtcars"={mtcars %>% mutate(value=mpg) })
  })
  
  
  output$cont_hist<-renderPlotly({
    p<-ggplot(cont_data(),aes(value))+
        geom_histogram(binwidth=input$cont_width,fill=input$stack_palette2,colour=input$line_col)+theme_minimal()
        ggplotly(p)})
  
  output$cont_box<-renderPlotly({
    p<-ggplot(cont_data(),aes(y=value))+
      geom_boxplot(fill=input$stack_palette2,colour = input$line_col)+theme_minimal()
    ggplotly(p)})
  
  output$cont_qq <- renderPlotly({
    p <- ggplot(cont_data(), aes(sample=value)) +
      stat_qq(color = input$stack_palette2,) +
      stat_qq_line(color=input$line_col,linewidth=input$linewidth2)+
      labs(x = "Theoretical quantiles", y = "Sample quantiles") +
      theme_minimal()
    ggplotly(p)
  })
  
  ##Categorical server
  cat_data<-reactive({
    switch(input$cat_list,"A random sample of student numbers in colleges in UTEP"={
      set.seed(12)
      data<-data.frame(level=sample(c("Education","Engineering","Health","Liberal Arts","Nursing","Science","Pharmacy","Business","Graduate"),input$ran_n,replace=TRUE))
      data %>% count(level)
    },
           "Species in iris"={iris %>% count(Species) %>% rename(level=Species)},
           "Passenger class in Titanic dataset"={as.data.frame(Titanic)%>% group_by(level=Class) %>% summarise(n=sum(Freq),.groups = "drop")},
           "Eye color in HairEyeColor"={as.data.frame(HairEyeColor)%>% group_by(level=Eye) %>% summarise(n=sum(Freq),.groups = "drop") }
    )
  })

  output$cat_bar<-renderPlotly({
    p<-ggplot(cat_data(),aes(level,n))+geom_col(fill=input$stack_palette3)+theme_minimal()
    ggplotly(p)})
    
    output$cat_lol<-renderPlotly({
      p<-ggplot(cat_data(),aes(n,level))+geom_segment(aes(x=0,xend=n,y=level,yend=level),color="gray")+geom_point(size=4,color=input$stack_palette3)+theme_minimal()
      ggplotly(p) })
    
    output$cat_tree <- renderPlotly({
      df <- cat_data() %>% arrange(desc(n))
      
      plot_ly(data = df,type = "treemap",labels = ~level,parents = NA,  values = ~n,
              textinfo = "label+value+percent entry"
      )
    })
    
    output$cat_pie <- renderPlotly({
      df <- cat_data() %>% arrange(desc(n)) %>% mutate(prop = n / sum(n))
      
      plot_ly(data = df,labels = ~level,values = ~n,type = "pie",textposition = "inside", textinfo = "label+percent" )
    })
    
   
  
}

shinyApp(ui,server)



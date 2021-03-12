library(ggplot2)
library(gganimate)
library(gifski)
library(av)
library(shiny)
library(plotly)
library(dplyr)
library(dygraphs)
library(scales)
library(anytime)
library(forcats)
library(stringr)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(lattice)
library(reprex)
library(shinyjs)
library(shinyalert)

b<-readline(prompt="Enter your data set as a file name: ")
a<-readLines(b)
x<-a
x<-as.numeric(x)
df<-data.frame(x)
y<-abs(x-mean(x))
ef<-data.frame(y)
q<-y^2/max(y)
z<-sum(y^(2))/(max(y)*length(x))
kf<-data.frame(q)
s<-sqrt(sum(y^2)/(length(x)-1))

ui  <- shinyUI(
    
    fluidPage(useShinyjs(),
              useShinyalert(),
              useShinyjs(),
              plotOutput(outputId="plotgraph"),
              tableOutput("contents"),
              sidebarPanel(
                  radioButtons(
                      inputId = "source",
                      inline = FALSE,
                      label = "Select plot number and guess the correct number for each plot in order",
                      choices = c(
                          "Plot #1 (Mean)" = "Mean",
                          "Plot #2 (Mean Deviation)" = "MeanD",
                          "Plot #3 (SRMSD & SD)" = "SD"
                      )
                  ),
                  fluidRow(
                      id="compresspanel",
                      column(10,checkboxGroupInput("extra", "Other Important Buttons:", c("Compress ECDF G to get ECDF H" = "compress")))
                  )
              ),
              sidebarPanel(
                  width=8,
                  fluidRow( 
                      id= "meanslider",
                      column(8, div(id="quiz",sliderInput("wt1","Mean",min=min(x),max=max(x),value=0,step=0.01))),
                      column(2,actionButton(inputId="button1",label="Submit")),
                      column(2,uiOutput("Selected_feedback")),
                      tags$head(tags$style("#Selected_feedback{color: blue}"))
                  ),
                  fluidRow(
                      id= "dslider",
                      column(8,div(id="test",sliderInput("wt2","Mean Deviation",min=round(min(y),2),max=round(max(y),2),value=0,step=0.01))),
                      column(2,actionButton(inputId="button2",label="Submit")),
                      column(2,uiOutput("Submitted_feedback")),
                      tags$head(tags$style("#Submitted_feedback{color: red}"))
                  ),
                  fluidRow(
                      id= "ssmdslider",
                      column(8,div(id="hw",sliderInput("wt4","Scaled Root Mean Squared Deviation (SRMSD)",min=round(min(q),2),max=round(max(q),2),value=0,step=0.01))),
                      column(2,actionButton(inputId="button4",label="Submit")),
                      column(2,uiOutput("Generated_feedback")),
                      tags$head(tags$style("#Generated_feedback{color: green}"))
                  ),
                  fluidRow(
                      id= "sdslider",
                      column(8,div(id="exam",sliderInput("wt3","Geometric Mean between SRMSD and largest Mean Deviation (Standard Deviation)",
                                                         min=round(min(q),2),max=round(max(q),2),value=round(min(q),2)+0.01,step=0.01))),
                      column(2,actionButton(inputId="button3",label="Submit")),
                      column(2,uiOutput("Resulted_feedback")),
                      tags$head(tags$style("#Resulted_feedback{color: purple}"))
                  ),
                  tags$style(
                      '#quiz {
          {color: blue};
          cursor: crosshair;
          color: blue;
          }'
                  ),
                  tags$style(
                      '#test {
          {color: red};
          cursor: crosshair;
          color: red;
          }'
                  ),
                  tags$style(
                      '#exam {
          {color: purple};
          cursor: crosshair;
          color: purple;
          }'
                  ),
                  tags$style(
                      '#hw {
          {color: green};
          cursor: crosshair;
          color: green;
          }'
                  ),
                  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: blue}")),
                  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
                  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: green}")),
                  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: purple}"))
              )
    )
)

server <- shinyServer(function(input, output, session) {
    
    pt1 <- reactive({
        if(input$source != "Mean") return(NULL)
        shinyjs::hide(id="dslider")
        shinyjs::hide(id="sdslider")
        shinyjs::hide(id="ssmdslider")
        
        shinyjs::show(id="meanslider")
        
        shinyjs::hide(id="compresspanel")
        
        ggplot(df,aes(x)) + 
            geom_rect(aes(xmin = input$wt1, ymin = 0, xmax= max(x) ,ymax = 1), fill = "blue") +
            stat_function(fun=ecdf(x),geom="area",fill="blue",xlim=c(min(x),input$wt1))+
            stat_function(fun=ecdf(x),geom="area",fill="light grey",xlim=c(input$wt1,max(x)))+
            geom_hline(yintercept=c(0,1),color="black")+
            geom_vline(xintercept=input$wt1,color="red")+
            labs(y="ECDF F")
    })
    pt2 <- reactive({
        if(input$source != "MeanD") return(NULL)
        if( !(input$wt1>=mean(x)-0.01 & input$wt1<=mean(x)+0.01)) 
        {
            shinyalert("Oops!", "You must select the correct mean first!", type = "error")
            updateRadioButtons(session, "source", selected = character(1))
            return(NULL)
        }
        
        shinyjs::hide(id="meanslider")
        shinyjs::hide(id="sdslider")
        shinyjs::show(id="dslider")
        shinyjs::hide(id="ssmdslider")
        shinyjs::hide(id="compresspanel")
        
        ggplot(ef,aes(y)) +
            geom_rect(aes(xmin = input$wt2, ymin = 0, xmax= max(y) ,ymax = 1), fill = "red") +
            stat_function(fun=ecdf(y),geom="area",fill="red",xlim=c(min(y),input$wt2))+
            stat_function(fun=ecdf(y),geom="area",fill="light grey",xlim=c(input$wt2,max(y)))+
            geom_vline(xintercept=input$wt2,color="blue")+
            geom_hline(yintercept=c(0,1),color="black")+
            labs(x="Mean Deviation",y="ECDF G")
        
    })
    pt3<-reactive({
        if(input$source != "SD") return(NULL)
        if( !(input$wt2>=mean(y)-0.01 & input$wt2<=mean(y)+0.01)) 
        {
            shinyalert("Oops!", "You must select the correct mean deviation first!", type = "error")
            updateRadioButtons(session, "source", selected = character(2))
            return(NULL)
        }
        
        shinyjs::hide(id="meanslider")
        shinyjs::hide(id="dslider")
        
        if(input$wt4>=mean(q)-0.01 & input$wt4<=mean(q)+0.01){
            if(input$button4 %% 2 == 1){
                shinyjs::show(id="sdslider")
                shinyjs::show(id="ssmdslider")
            }else{
                shinyjs::hide(id="sdslider")
                shinyjs::show(id="ssmdslider")
            }
        }else{
            shinyjs::hide(id="sdslider")
            shinyjs::show(id="ssmdslider")
        }
        
        shinyjs::show(id="compresspanel")
        
        if (length(input$extra) ==1)
        {
            if(input$extra == "compress")
            {
                ggplot(kf,aes(q)) + 
                    geom_rect(aes(xmin = input$wt4, ymin = 0, xmax= max(q) ,ymax = 1), fill = "dark green") +
                    stat_function(fun=ecdf(q),geom="area",fill="dark green",xlim=c(min(q),input$wt4))+
                    stat_function(fun=ecdf(q),geom="area",fill="light grey",xlim=c(input$wt4,max(q)))+
                    stat_function(fun=ecdf(y),geom="step",color="brown",xlim=c(0,max(y)))+
                    geom_vline(xintercept=input$wt4,color="orange")+
                    geom_vline(xintercept=input$wt3,color="yellow")+
                    geom_hline(yintercept=c(0,1),color="black")+
                    labs(x="Standard Deviation",y="ECDF H, ECDF G")
            }
            
        }
        else # not selected
        {
            ggplot(ef,aes(y)) +
                stat_function(fun=ecdf(y),geom="step",color="brown",xlim=c(0,max(y)))+
                geom_vline(xintercept=input$wt2,color="blue")+
                geom_hline(yintercept=c(0,1),color="black")+
                labs(x="Mean Deviation",y="ECDF G")
        }
        
    })
    
    observeEvent(input$wt1,{
        output$Selected_feedback <- renderUI(
            if(input$button1 %% 2 == 1){
                if(input$wt1>=mean(x)-0.01 & input$wt1<=mean(x)+0.01){
                    print("Correct Mean")
                }else if(input$wt1>=mean(x)-0.05 & input$wt1<mean(x)){
                    print("You're close, move right")
                }else if(input$wt1<=mean(x)+0.05 & input$wt1>mean(x)){
                    print("You're close, move left")
                }else if(input$wt1<=mean(x)+sd(x) & input$wt1>mean(x)){
                    print("Move left")
                }else if(input$wt1>=mean(x)-sd(x) & input$wt1<mean(x)){
                    print("Move right")
                }else if(input$wt1<mean(x)-sd(x)){
                    print("Too low")
                }else{
                    print("Too high")
                }
            })
    })
    
    observeEvent(input$wt2,{
        output$Submitted_feedback <- renderUI(
            if(input$button2 %% 2 == 1){
                if(input$wt2>=mean(y)-0.01 & input$wt2<=mean(y)+0.01){
                    print("Correct Mean Deviation")
                }else if(input$wt2>=mean(y)-0.05 & input$wt2<mean(y)){
                    print("You're close, move right")
                }else if(input$wt2<=mean(y)+0.05 & input$wt2>mean(y)){
                    print("You're close, move left")
                }else if(input$wt2<=mean(y)+sd(y) & input$wt2>mean(y)){
                    print("Move left")
                }else if(input$wt2>=mean(y)-sd(y) & input$wt2<mean(y)){
                    print("Move right")
                }else if(input$wt2<mean(y)-sd(y)){
                    print("Too low")
                }else{
                    print("Too high")
                }
            })
    })
    observeEvent(input$wt4,{
        output$Generated_feedback <- renderUI(
            if(input$button4 %% 2 == 1){
                if(input$wt4>=mean(q)-0.01 & input$wt4<=mean(q)+0.01){
                    print("Correct Scaled Root Mean Squared Deviation")
                }else if(input$wt4>=mean(q)-0.05 & input$wt4<mean(q)){
                    print("You're close, move right")
                }else if(input$wt4<=mean(q)+0.05 & input$wt4>mean(q)){
                    print("You're close, move left")
                }else if(input$wt4<=mean(q)+sd(q) & input$wt4>mean(q)){
                    print("Move left")
                }else if(input$wt4>=mean(q)-sd(q) & input$wt4<mean(q)){
                    print("Move right")
                }else if(input$wt4<mean(q)-sd(q)){
                    print("Too low")
                }else{
                    print("Too high")
                }
            })
    })
    observeEvent(input$wt3,{
        output$Resulted_feedback <- renderUI(
            if(input$button3 %% 2 == 1){
                if(input$wt3>=s-0.01 & input$wt3<=s+0.01){
                    print("Correct Standard Deviation")
                }else if(input$wt3>=s-0.05 & input$wt3<s){
                    print("You're close, move right")
                }else if(input$wt3<=s+0.05 & input$wt3>s){
                    print("You're close, move left")
                }else if(input$wt3<=s+1 & input$wt3>s){
                    print("Move left")
                }else if(input$wt3>=s-1 & input$wt3<s){
                    print("Move right")
                }else if(input$wt3<s-1){
                    print("Too low")
                }else{
                    print("Too high")
                }
            })
    })
    
    observeEvent(input$source,
                 {
                     if(input$source == "MeanD")
                     {
                         if( !(input$wt1>=mean(x)-0.01 & input$wt1<=mean(x)+0.01)) 
                         {
                             updateRadioButtons(session, "source", selected = character(0))
                             print("trying to shift")
                         }
                         else
                         {
                         }
                         
                     }
                 }
    )
    
    output$plotgraph <- renderPlot({
        ptlist <- list(pt1(),pt2(),pt3())
        wtlist <- c(input$wt1,input$wt2,input$wt3)
        
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete]
        wtlist <- wtlist[to_delete]
        if(length(ptlist)==0) return(NULL)
        grid.arrange(grobs=ptlist,widths=wtlist)
    })
})
shinyApp(ui=ui,server = server)

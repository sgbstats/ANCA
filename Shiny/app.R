
library(shiny)
library(tidyverse)
library(survival)
library(survminer)
library(plotly)
load("Data.Rda")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-size:20px;
        font-weight: bold;
      }
    "))
  ),
  
  # Application title
  titlePanel("ARRS23 Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      h4(tags$b("Kidney function")),
      numericInput("Creatinine", label = h4("Creatinine (\u03BCmol/L)"), value = 250, min=0),
      radioButtons("glomchoose", h4(tags$b("Normal glomeruli")), c("Raw"=1, "Percentage"=2), selected = 1, inline=T),
      conditionalPanel(condition = "input.glomchoose==1",
                       numericInput("Gloms", label = h4("Glomeruli on biopsy"), value = 20, min=1),
                       numericInput("Normal", label = h4("Normal glomeruli on biopsy"), value = 10, min=0)
      ),
      conditionalPanel(condition = "input.glomchoose==2",
                       numericInput("glom2", label = h4("Normal glomeruli percentage"), value =50, min=0,max=100),
      ),
     
      radioButtons("iftachoose", h4(tags$b("IFTA")), c("Semi-quantative"=1, "Percentage"=2), selected = 1, inline=T),
      conditionalPanel(condition = "input.iftachoose==1",
                       selectInput("IFTA", label = h4("Grading"),
                                   choices = list("None or Mild" = 0, "\u2265Mild to Moderate" = 2#, "Mild to Moderate" = 2, "Moderate"=3, "Moderate to Severe"=4, "Severe"=5
                                                  ), 
                                   selected = 0),
      ),
      conditionalPanel(condition = "input.iftachoose==2",
                       #lazy coding here as it was a numeric input
                       selectInput("IFTA2", label = h4("Percentage"), choices=list("<25%"="T0", "\u226525%"="T1"), selected = "T0"),
      ),
      actionButton("go", "Go"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4(tags$b("Risk Category")),
      # tableOutput("m"),
      htmlOutput("riskpoints"),
      htmlOutput("riskgr"),
      h4(tags$b("ESKD-free probability")),
      textOutput("error"),
      htmlOutput("surv12"),
      htmlOutput("surv36"),
      htmlOutput("surv60"),
     # plotlyOutput("plot"), #the idea is that thiswill become a picotgram but I couldn't the pictogram package to work.
      # tableOutput("debug")
      
      #textOutput("text1")
    )
  )
)


server <- function(input, output) {
  #this is the main continuous model. The data sits in the app but there is a formula, see readme
  m0=coxph(Surv(TTEESKDCNSR, ESKD)~I(log(CREATININE))+PERCN+TA, data=Data %>% filter(COHORT=="DEV"))
  colour=function(p)
  {
    c=case_when(p>87~"#4DAF4A",
                p>73~"#FF7F00",
                p>68~"#E41A1C",
                p>0~"#660000")
    return(c)
  }
  
  colour2=function(gr)
  {
    c=case_when(gr=="Low"~"#4DAF4A",
                gr=="Moderate"~"#FF7F00",
                gr=="High"~"#E41A1C",
                gr=="Very High"~"#660000")
    return(c)
  }
  
  # debugging shit
  output$m=renderTable({
    if(input$iftachoose==1)
    {
      ifta=if_else(input$IFTA %in% 0:1, "T0", "T1")
    }else if(input$iftachoose==2)
    {
      ifta=input$IFTA2
    }
    
    nd=data.frame("CREATININE"=input$Creatinine, "PERCN"=100*input$Normal/input$Gloms, "TA"=ifta)
    # s=survfit(m0, newdata = nd)
    data.frame("time"=s$time, "surv"=s$surv)
    # m0$coefficients
    # nd
  })
  
  output$text1=renderText({input$IFTA2})
  #this might be deprecated
  surv <- eventReactive(input$go, {
    validate(
      need(input$Creatinine>0, "Creatinine too low"),
      need(input$Normal<=input$Gloms|input$glomchoose==2, "Check glomeruli inputs"),
      need(input$Normal>=0|input$glomchoose==2, "Check glomeruli inputs"),
      need(input$Gloms>0|input$glomchoose==2, "Check glomeruli inputs"),
      need(input$glom2>=0|input$glomchoose==1, "Check glomeruli inputs"),
      need(input$glom2<=100|input$glomchoose==1, "Check glomeruli inputs"),
      # need(input$IFTA2>=0|input$iftachoose==1, "Check TA/IF inputs"),
      # need(input$IFTA2<=100|input$iftachoose==1, "Check TA/IF inputs")
    )
    if(input$iftachoose==1)
    {
      ifta=if_else(input$IFTA %in% 0:1, "T0", "T1")
    }else if(input$iftachoose==2)
    {
      ifta=input$IFTA2
    }
    if(input$glomchoose==1)
    {
      glom=100*input$Normal/input$Gloms
    }else if(input$glomchoose==2)
    {
      glom=input$glom2
    }
    nd=data.frame("CREATININE"=input$Creatinine, "PERCN"=glom, "TA"=ifta)
    s=survfit(m0, newdata = nd) 
    data.frame("time"=s$time, "surv"=s$surv)
    #nd
    
  })
  
  smoothsurv=eventReactive(input$go,{
    validate(
      need(input$Creatinine>0, ""),
      need(input$Normal<=input$Gloms|input$glomchoose==2, ""),
      need(input$Normal>=0|input$glomchoose==2, ""),
      need(input$Gloms>0|input$glomchoose==2, ""),
      need(input$glom2>=0|input$glomchoose==1, ""),
      need(input$glom2<=100|input$glomchoose==1, ""),
      # need(input$IFTA2>=0|input$iftachoose==1, ""),
      # need(input$IFTA2<=100|input$iftachoose==1, "")
    )
    contnd=data.frame("CREATININE"=1, "PERCN"=0, "TA"="T0")
    xhz2=survfit(m0, newdata = contnd)
    
    bl2=data.frame("time"=xhz2$time, "surv"=xhz2$surv, "model"="KM")%>% 
      filter(time!=0) %>% #using log time with zero would break it
      mutate(logh=log(-log(surv))) 
    
    #survival function
    x=lm(logh~I(time^-0.1)+I(time^0.1*log(time)), data=bl2 %>% filter(time<10)) #for some reason this is being estimated in real time
    logh=predict(x,data.frame("time"=seq(from=0, to=10, by=0.01)))
    if(input$iftachoose==1)
    {
      ifta=if_else(input$IFTA %in% 0:1, "T0", "T1")
    }else if(input$iftachoose==2)
    {
      ifta=input$IFTA2
    }
    
    if(input$glomchoose==1)
    {
      glom=100*input$Normal/input$Gloms
    }else if(input$glomchoose==2)
    {
      glom=input$glom2
    }
    nd=data.frame("CREATININE"=input$Creatinine, "PERCN"=glom, "TA"=ifta)
    PI=predict(m0, newdata = nd , reference = "zero")
    
    S0=exp(-exp(logh))
    S0^exp(PI)
    
    # generateing the data
    data.frame("time"=seq(from=0, to=10, by=0.01),
               surv=S0^exp(PI))
  })
  
  #lots of repetition but this ouput the errors
  output$error=renderText({
    validate(
      need(input$Creatinine>0, "Creatinine too low"),
      need(input$Normal<=input$Gloms, "Check glomeruli inputs"),
      need(input$Normal>=0, "Check glomeruli inputs"),
      need(input$Gloms>0, "Check glomeruli inputs"),
      # need(input$IFTA2>=0|input$iftachoose==1, "Check TA/IF inputs"),
      # need(input$IFTA2<=100|input$iftachoose==1, "Check TA/IF inputs")
    )
  })
  output$surv12=renderText({
    validate(
      #I have done the error checking multipl times to surpress the rrors on output
      need(input$Creatinine>0, ""),
      need(input$Normal<=input$Gloms|input$glomchoose==2, ""),
      need(input$Normal>=0|input$glomchoose==2, ""),
      need(input$Gloms>0|input$glomchoose==2, ""),
      need(input$glom2>=0|input$glomchoose==1, ""),
      need(input$glom2<=100|input$glomchoose==1, ""),
      # need(input$IFTA2>=0|input$iftachoose==1, ""),
      # need(input$IFTA2<=100|input$iftachoose==1, "")
    )
    #gives survival probability
    prob=100*(smoothsurv() %>% slice_min(abs(time-1)))$surv
    
    
    paste("<span style=\"font-size: 20px\">1 year: ","<font color=\"",colour(prob),  "\"><b>", round(prob), "%", "</b></font></span>", sep = "") 
    
  })
  output$surv36=renderText({
    validate(
      need(input$Creatinine>0, ""),
      need(input$Normal<=input$Gloms|input$glomchoose==2, ""),
      need(input$Normal>=0|input$glomchoose==2, ""),
      need(input$Gloms>0|input$glomchoose==2, ""),
      need(input$glom2>=0|input$glomchoose==1, ""),
      need(input$glom2<=100|input$glomchoose==1, ""),
      # need(input$IFTA2>=0|input$iftachoose==1, ""),
      # need(input$IFTA2<=100|input$iftachoose==1, "")
    )
    prob=100*(smoothsurv() %>% slice_min(abs(time-3)))$surv
    
    paste("<span style=\"font-size: 20px\">3 years: ","<font color=\"",colour(prob),  "\"><b>", round(prob), "%", "</b></font></span>", sep = "")
  })
  output$surv60=renderText({
    validate(
      need(input$Creatinine>0, ""),
      need(input$Normal<=input$Gloms|input$glomchoose==2, ""),
      need(input$Normal>=0|input$glomchoose==2, ""),
      need(input$Gloms>0|input$glomchoose==2, ""),
      need(input$glom2>=0|input$glomchoose==1, ""),
      need(input$glom2<=100|input$glomchoose==1, ""),
      # need(input$IFTA2>=0|input$iftachoose==1, ""),
      # need(input$IFTA2<=100|input$iftachoose==1, "")
    )
    prob=100*(smoothsurv() %>% slice_min(abs(time-5)))$surv
    
    paste("<span style=\"font-size: 20px\">5 years: ","<font color=\"",colour(prob),  "\"><b>", round(prob), "%", "</b></font></span>", sep = "")
  })
  
  #risk points
  points=eventReactive(input$go,{
    if(input$iftachoose==1)
    {
      ifta=if_else(input$IFTA %in% 0:1, "T0", "T1")
    }else if(input$iftachoose==2)
    {
      ifta=input$IFTA2
    }
    if(input$glomchoose==1)
    {
      glom=100*input$Normal/input$Gloms
    }else if(input$glomchoose==2)
    {
      glom=input$glom2
    }
    nd=data.frame("CREATININE"=input$Creatinine, "PERCN"=glom, "TA"=ifta) %>% 
      mutate(C=case_when(CREATININE>=450~ 11,
                         CREATININE>=250~4,
                         CREATININE>=0~0), 
             N=case_when(PERCN<10~7,
                         PERCN<=25~4,
                         PERCN<=100~0),
             TA4=if_else(TA=="T1",3,0),
             RRS4=C+N+TA4,
             RRS4GR=factor(case_when(RRS4<=4~"Low",
                                     RRS4<=11~"Moderate",
                                     RRS4<=18~"High",
                                     !is.na(RRS4)~"Very High"), c("Low", "Moderate","High", "Very High"), ordered = T)) %>% 
      dplyr::select(RRS4, RRS4GR)
    
  })
  
  output$riskgr=renderText({
    validate(
      need(input$Creatinine>0, ""),
      need(input$Normal<=input$Gloms|input$glomchoose==2, ""),
      need(input$Normal>=0|input$glomchoose==2, ""),
      need(input$Gloms>0|input$glomchoose==2, ""),
      need(input$glom2>=0|input$glomchoose==1, ""),
      need(input$glom2<=100|input$glomchoose==1, ""),
      # need(input$IFTA2>=0|input$iftachoose==1, ""),
      # need(input$IFTA2<=100|input$iftachoose==1, "")
    )
    paste("<span style=\"font-size: 20px\">Group: ","<font color=\"",colour2((points())$RRS4GR),  "\"><b>", (points())$RRS4GR,  "</b></font></span>", sep = "")
  })
  output$riskpoints=renderText({
    validate(
      need(input$Creatinine>0, ""),
      need(input$Normal<=input$Gloms|input$glomchoose==2, ""),
      need(input$Normal>=0|input$glomchoose==2, ""),
      need(input$Gloms>0|input$glomchoose==2, ""),
      need(input$glom2>=0|input$glomchoose==1, ""),
      need(input$glom2<=100|input$glomchoose==1, ""),
      # need(input$IFTA2>=0|input$iftachoose==1, ""),
      # need(input$IFTA2<=100|input$iftachoose==1, "")
    )
    paste("<span style=\"font-size: 20px\">Points: ","<font color=\"",colour2((points())$RRS4GR),  "\"><b>", (points())$RRS4,  "</b></font></span>", sep = "")
  })
  
  
  output$plot=renderPlotly({
    #ugly plot that needs making nice probably with a pictogram

    z=smoothsurv() %>%
      filter(time<10) %>%
      ggplot(aes(x=time, y=surv,
                 text=paste("Time: ", round(time,1), " years",
                            "<br>Survival: ", round(100*surv), "%",
                            sep=""),
                 group=1
                 ))+
      geom_line(col=colour2((points())$RRS4GR),
                size=1)+
      scale_y_continuous(breaks = seq(0,1,0.1),
                         limits = c(0,1),
                         labels = scales::percent)+
      scale_x_continuous(breaks = seq(0,10,2))+
      theme_bw()+
      theme( panel.grid.minor = element_blank())+
      xlab("Time (years)")+
      ylab("Renal survival")
    # z
   plotly::ggplotly(z, tooltip = "text")
      
  })
  output$debug=renderTable({
  head(smoothsurv())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

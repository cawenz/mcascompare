#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(hrbrthemes)
# Get Data
mcas <- read.csv("https://github.com/cawenz/MA_Statewide_Reports/raw/main/output/MCASnextgen_ByDistrict.csv")%>%
  filter(!grepl("(District)", District)& 
           Subject != "SCI"
           )%>%
  rename("Mean Scale Score"=mean_SS)%>%
  rename("Percent Meeting Exceeding" = ME_per)

district <- mcas %>%
  distinct(District)%>%
  pull(District)

grade <- mcas %>%
  distinct(grade)%>%
  pull(grade)

subject <- mcas %>%
  distinct(Subject)%>%
  pull(Subject)

var <- c("Mean Scale Score", "Percent Meeting Exceeding")


round5 = function(x, accuracy, f=round){f(x/ accuracy)*accuracy}

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Set theme 
    theme = shinytheme("lumen"), 
    # Application title
    titlePanel("Comparing MCAS scores of Massachusetts School Districts"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("var",
                      "Variable:",
                      choices=var,
                      selected=var[1]),
          selectInput("subject", 
                      "Subject:",
                      choices=subject, 
                      selected=subject[1]),
          selectInput("grade",
                      "Grade:",
                      choices=grade,
                      selected=grade[1]),
          selectInput("district",
                      "District:",
                      choices=district,
                      multiple=T,
                      selectize = T, 
                      selected=c("State","Holyoke"))
          
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  wtf<- reactive(mcas %>% 
      filter(
        Subject == input$subject &
        District %in% input$district &
        grade == input$grade)%>%
      mutate(District=factor(District,levels=input$district))%>%
        rename(var=input$var)
    )
  
  # round_any = function(x, accuracy, f=round){f(x/ accuracy)*accuracy}
  # ymini <- round5(min(wtf$mean_SS), 10)
  # ymaxi <- round5(max(mcas$mean_SS), 10)
  
  # plyr::round_any(min(mcas$mean_SS), 10)
  
    output$plot <- renderPlot({
        d <- wtf()
      
      
       p <-  d %>%
        ggplot(aes(x=Year, y=var, color=District, group=District, shape=District))+
        geom_point(size=3)+
        geom_line()+
        theme_ipsum_rc()+
        xlab(NULL)+
        ylab("Mean Scale Score")+
        geom_vline(xintercept = 2020, linetype="dashed", color="grey", size=1.25)+
        scale_x_continuous(breaks=seq(2017,2021,1))+
        # scale_y_continuous(limits=c(460,540), breaks=seq(460,540,20))+
        theme(
          legend.position = "bottom", 
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          panel.grid.minor.x = element_blank(),
          # legend.text = element_text(11),
          axis.text.y  = element_text(size=12), 
          axis.text.x  = element_text(size=12))
    p
    })

}
# Run the application 
shinyApp(ui = ui, server = server)
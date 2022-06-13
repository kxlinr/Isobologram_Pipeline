library(DT)

navbarPage(
  "Isobologram Generator",
  
  
  tabPanel("See Data",
           selectInput("experiment", h4("Select experiment"),
                         choices=c("Nutlin + Radiation" = "data1",
                                   "AMG + Radiation" = "data2",
                                   "JHUEM2 WT" = "data3")),
           selectInput("condition", h4("Select condition"),
                       choices=NULL),
           
           DTOutput("table")),
  
  
  
  tabPanel("Plot Data",
           selectInput("experiment", h4("Select experiment"),
                       choices=c("Nutlin + Radiation" = "data1",
                                 "AMG + Radiation" = "data2",
                                 "JHUEM2 WT" = "data3")),
           selectInput("condition", h4("Select condition"),
                       choices=NULL),
           
           plotOutput("plot"))
)
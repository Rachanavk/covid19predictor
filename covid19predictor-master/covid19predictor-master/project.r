
##########################################################################
# Covid 19 Analysis and Prediction (Karnataka)
##########################################################################
##########################################################################
# Reactive apps using the shiny library 
##########################################################################

# Here we are using the readcsv library to import dataset
# Make sure to set working directory in the "Files" panel
# So that the data is picked up from the correct directory
# Install necessary libraries
# Uncomment and install the following packages if not installed earlier
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("dplyr")
# install.packages("caTools")
# install.packages("pracma")
# Load necessary libraries
library("shiny")
library("shinythemes")
library("dplyr")
library("caTools")
library("pracma")
# Read dataset directly from the url
data<-read.csv("https://api.covid19india.org/csv/latest/states.csv",sep=",",header=TRUE)
#Extract the required data by Filtering
data1<-filter(data,State=="Karnataka")

##########################################################################
# A shiny app is created in 3 steps:
# 1. Create a UI page (What should be displayed to user)
# 2. Create Server / backend logic (What should be executed at backend)
# 3. Run the Shiny app binding the UI page and Server / backend logic
##########################################################################

#########################################################################
# Here is the first step of creating the UI page
# We start by calling the fluidPage function 
# This creates the overall UI for the app

ui <- fluidPage(
# Selecting Theme for UI
theme=shinytheme("slate"),
# Selecting Title for UI
titlePanel("Covid-19 Analysis and prediction"),
# Header and Time
tags$table( style="border: 0px solid white;padding: 1%; width:100%;",
            tags$td(tags$b(tags$h1("KARNATAKA"))),
            tags$td(tags$h3(textOutput("currentTime",container=span),align="right"))),
#Building Sidebar Layout and Pannel
sidebarLayout(
  sidebarPanel(  
    # Displaying necessary text for identification of figures  
       tags$h3("Information about TOTAL number of cases:"),
      tags$h4( textOutput("text1"),#confirmed cases
       textOutput("text2"),#confirmed count
       textOutput("text3"),#recovered cases
       textOutput("text4"),#recovered count
       textOutput("text5"),#deceased cases
       textOutput("text6")),#deceased count
       # Displaying image directly from a link
       tags$img(src="https://upload.wikimedia.org/wikipedia/commons/8/82/SARS-CoV-2_without_background.png",align="center",height="150px",width="150px"),
       # Displaying options for Graph filter
       selectInput("filter","Choose Filter:",list("NONE","Confirmed","Recovered","Deceased"),selected="NONE")
    ),
    # Building Mainpanel
    mainPanel(      
      # Output: Bar Plots with necessary information ---- 
      plotOutput(outputId = "distPlot"),
     tags$b( tags$h3("Prediction :")),
      # Displaying Prediction results with text and figures
      tags$table(
        tags$tr(
        tags$td(
          textOutput("text7"),#confirmed prediction
       textOutput("text8"),#confirmed count
       ),
       tags$td(
       textOutput("text9"),#recovered prediction
        textOutput("text10"),#recovered count
          ),
           tags$td(
       textOutput("text11"),#deceased prediction
       textOutput("text12")#deceased count
         )
      ),
      style="border: 0.5px solid white;padding: 1%; width:100%;"
      )
    )
  )
)
#####################################################################################################
# Here comes the second part!
# Define server logic required to draw a bar plot ----
server <- function(input, output,session) {      
colors = c("red","green","blue")
months = list("March","April","May","June","July","August","September","October","November","December")
regions <- c("Confirmed","Recovered","Deceased")
m<-c(3,4,5,6,7,8,9,10,11,12)
Months<-  filter(data1,Date=="2020-03-31"|Date=="2020-04-30"|Date=="2020-05-31"|Date=="2020-06-30"|Date=="2020-07-31"|Date=="2020-08-31"|Date=="2020-09-30"|Date=="2020-10-31"|Date=="2020-11-30"|Date=="2020-12-31")
val=c(c(Months$Confirmed),c(Months$Recovered),(Months$Deceased))
val1=c(data1$Confirmed)
val2=c(data1$Recovered)
val3=c(data1$Deceased)
Values <- matrix(val, nrow = 3, ncol = nrow(Months), byrow = TRUE)
Values1 <- matrix(val1, nrow = 1, ncol = nrow(data1), byrow = TRUE)
Values2 <- matrix(val2, nrow = 1, ncol = nrow(data1), byrow = TRUE)
Values3 <- matrix(val3, nrow = 1, ncol = nrow(data1), byrow = TRUE)

  output$distPlot <- renderPlot({
# Displaying Graphs based on FILTER
 if(input$filter=="NONE") {
     barplot(Values, bg="transparent",main = "Graph of all cases", names.arg = months[1:nrow(Months)],xlab = "Month", ylab = "Number of people", col = colors)#col.axis="#dddddd", fg="white",col.main="white",col.lab="grey",names.arg = months,
 legend("topleft", regions, cex = 1.5, fill = colors,col="white",box.col="black",bg="white")
   }
    else if(input$filter=="Confirmed") {      
 barplot(Values1, bg="transparent",main = "Graph of Confirmed", names.arg = data1$Date,xlab = "Month", ylab = "Number of people", col = "red",border = "white")#col.axis="#dddddd", fg="white",col.main="white",col.lab="grey",names.arg = months,
 legend("topleft","Confirmed" , cex = 1.5, fill ="red" ,col="white",box.col="black",bg="white")
  }
   else if(input$filter=="Recovered") {      
 barplot(Values2, bg="transparent",main = "Graph of Recovered", names.arg = data1$Date,xlab = "Month", ylab = "Number of people", col = "green",border = "white")#col.axis="#dddddd", fg="white",col.main="white",col.lab="grey",names.arg = months,
 legend("topleft","Recovered", cex = 1.5, fill ="green",col="white",box.col="black",bg="white")
  }    
     else  {      
 barplot(Values3, bg="transparent",main = "Graph of Deceased", names.arg = data1$Date,xlab = "Month", ylab = "Number of people", col = "blue",border = "white")#col.axis="#dddddd", fg="white",col.main="white",col.lab="grey",names.arg = months,
 legend("topleft", "Deceased", cex = 1.5, fill ="blue",col="white",box.col="black",bg="white")  
    }
  })
  # Displaying information regarding total number of cases ---
  output$text1<-renderText({"Confirmed number of cases :"}  )
  output$text2<-renderText({return(max(data1$Confirmed))})
   
  output$text3<-renderText({"Recovered number of cases :"}  )
  output$text4<-renderText({return(max(data1$Recovered))})

  output$text5<-renderText({"Deceased number of cases :"}  )
  output$text6<-renderText({return(max(data1$Deceased))})

 # Displaying information regarding prediction of number of cases ---
  output$text7<-renderText({"Confirmed cases of this month would be :"}  )
  output$text8<-renderText({newtonInterp(m[1:nrow(Months)],Months$Confirmed,xs=m[c(nrow(Months))]+1)})
   
  output$text9<-renderText({"Recovered cases of this month would be :"}  )
  output$text10<-renderText({newtonInterp(m[1:nrow(Months)],Months$Recovered,xs=m[c(nrow(Months))]+1)})

  output$text11<-renderText({"Deceased cases of this month would be :"}  )
  output$text12<-renderText({newtonInterp(m[1:nrow(Months)],Months$Deceased,xs=m[c(nrow(Months))]+1)})

# Formatting and displaying system time
  output$currentTime<-renderText({invalidateLater(as.integer(1000),session)
  format(Sys.time())})
 
}
###########################################################################
# The last but not the least part 
# Combining both server and ui into a Shinyapp
# Create Shiny app ----
shinyApp(ui = ui, server = server)
# Hurray ! 
############################################################################

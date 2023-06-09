#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gganimate)
library(dplyr)
library(tidyverse)
library(lubridate)


# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("2020 Los Angelos Crime"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("linePlot")
#         )
#     )
# )

la_crime <- read.csv("./Crime_Data_from_2020_to_Present.csv",header=TRUE)
# creating premise groupings 

la_crime_df_2020 <- la_crime %>% mutate(Date.Rptd = mdy_hms(Date.Rptd),DATE.OCC = mdy_hms(DATE.OCC), Premise = ifelse(str_detect(Premis.Desc,"APARTMENT|HIGH-RISE"), "APARTMENT", ifelse(str_detect(Premis.Desc,"STORE|BUSINESS|MARKET|MART|SHOP|CENTER|MALL|SALON|STORAGE"), "STORE", ifelse(str_detect(Premis.Desc, "FAMILY|RESIDENCE|HOUSE|RESIDENTIAL"), "HOUSE", ifelse(str_detect(Premis.Desc, "PARKING|GARAGE|DRIVEWAY"), "PARKING LOT/GARAGE/DRIVEWAY", ifelse(str_detect(Premis.Desc,"STREET|SIDEWALK"),"STREET/SIDEWALK",'OTHER'))))),Type = ifelse(str_detect(Crm.Cd.Desc, "ASSAULT|BATTERY"), "ASSAULT", ifelse(str_detect(Crm.Cd.Desc, "THEFT|ROBBERY"), "THEFT", 'OTHER'))) %>% filter(Date.Rptd > ymd_hms("2020-01-01 01:00:00") & Date.Rptd <= ymd_hms("2020-12-31 01:00:00"))

# extracting data with only premise of interest 
la_crime_df <- la_crime_df_2020[la_crime_df_2020$Premise %in% c("APARTMENT","STORE","HOUSE","PARKING LOT/GARAGE/DRIVEWAY","STREET/SIDEWALK"),]

# extracting female and male victims 
la_crime_df <- la_crime_df[la_crime_df$Vict.Sex%in% c("M","F"),]

daily_la_crime_df <- la_crime_df %>% group_by(Date.Rptd) %>% summarize(crime_count = n()) 


ui <- fluidPage(mainPanel(imageOutput("linePlot")))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$linePlot <- renderImage({
        # generate bins based on input$bins from ui.R
      p <- ggplot(data = daily_la_crime_df,aes(x= Date.Rptd, y = crime_count)) + geom_line() + xlab("Date Reported") + ylab("Number of Crime Reported") + labs(title = 'Day: {frame_along}', x = "Date Reported", y = "Number of Crime Reported")  +
        transition_reveal(Date.Rptd) +
        ease_aes('linear') + view_follow(fixed_y=TRUE)
      
      anim_save("outfile.gif", animate(p)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif", contentType = "image/gif")
    },
    deleteFile = TRUE
    )
}




# Run the application 
shinyApp(ui = ui, server = server)

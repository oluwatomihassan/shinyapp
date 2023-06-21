

library(shiny)
library(gganimate)
library(dplyr)
library(tidyverse)
library(lubridate)
library(gifski)
library(magrittr)

la_crime <- read.csv("./Crime_Data_from_2020_to_Present.csv",header=TRUE)

la_crime <- la_crime %>% mutate(Date.Rptd = mdy_hms(Date.Rptd),DATE.OCC = mdy_hms(DATE.OCC)) %>% filter(Date.Rptd > ymd_hms("2020-01-01 01:00:00") & Date.Rptd <= ymd_hms("2020-12-31 01:00:00")) 

daily_la_crime_df <- la_crime %>% filter(Vict.Sex %in% c("M","F")) %>% mutate(Premise = ifelse(str_detect(Premis.Desc,"APARTMENT|HIGH-RISE"), "APARTMENT", ifelse(str_detect(Premis.Desc,"STORE|BUSINESS|MARKET|MART|SHOP|CENTER|MALL|SALON|STORAGE"), "STORE", ifelse(str_detect(Premis.Desc, "FAMILY|RESIDENCE|HOUSE|RESIDENTIAL"), "HOUSE", ifelse(str_detect(Premis.Desc, "PARKING|GARAGE|DRIVEWAY"), "PARKING LOT/GARAGE/DRIVEWAY", ifelse(str_detect(Premis.Desc,"STREET|SIDEWALK"),"STREET/SIDEWALK",'OTHER')))))) %>% group_by(Date.Rptd,AREA.NAME,Premise,Vict.Sex) %>% summarize(crime_count = n()) %>% mutate(`Victim Sex` = ifelse(Vict.Sex == 'M', 'Male', 'Female'))




ui <- fluidPage(titlePanel('Los Angeles California Reported Crimes - 2020'),
                selectInput('area', h4('Select LA Neighborhood Area'), choices = c('77th Street', 'Central', 'Devonshire', 'Foothill', 'Harbor', 'Hollenbeck', 'Hollywood', 'Mission', 'N Hollywood', 'Newton', 'Northeast', 'Olympic', 'Pacific', 'Rampart', 'Southeast', 'Southwest', 'Topanga', 'Van Nuys', 'West LA', 'West Valley', 'Wilshire'), selected = 'Topanga'),mainPanel(imageOutput("linePlot")))

# Define server logic required to draw a timeseries
server <- function(input, output) {
  
  
  toListen <- reactive({
    list(input$area)
  })
  
  observeEvent(toListen(), {
    daily_la_crime = daily_la_crime_df %>% filter(AREA.NAME == input$area)
  
  
  
    output$linePlot <- renderImage({
        # generate animated time series from ui.R
      p <- ggplot(data = daily_la_crime,aes(x= Date.Rptd, y = crime_count, group = `Victim Sex`, color = `Victim Sex` )) + geom_line() + facet_wrap(~~factor(Premise, levels=c('APARTMENT', 'HOUSE', 'STORE', 'STREET/SIDEWALK', 'PARKING LOT/GARAGE/DRIVEWAY','OTHER')), ncol=3) + xlab("Date Reported") + ylab("Number of Daily Reported Crimes") + labs(title = 'Report Date: {frame_along}', x = "Date Reported", y = "Number of Crime Reported") + scale_color_manual(labels = c("Female","Male"),values = c("Female" = "#DC3220", "Male" = "#005AB5")) + theme(panel.spacing = unit(2, "lines"),legend.position="top") + transition_reveal(Date.Rptd) + ease_aes('linear') + view_follow(fixed_y=TRUE)
      
      anim_save("outfile.gif", animate(p, height = 5,
                                       width = 10, units = "in", res = 150, nframes = 100, fps = 5, end_pause = 50)) 
      
      # Return a list containing the filename
      list(src = "outfile.gif", contentType = "image/gif")
    },
    deleteFile = TRUE
    )
})

}


# Run the application 
shinyApp(ui = ui, server = server)

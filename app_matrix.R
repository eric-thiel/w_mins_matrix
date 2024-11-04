library("shiny")   
library(DT)
library(googlesheets4)
library(shinycssloaders)
library(dplyr)
library(rdrop2)
library(readr)
library(vroom)
library(tidyverse)


get_gamelog_data = function()
{
  df = vroom::vroom("https://raw.githubusercontent.com/eric-thiel/w_mins_matrix/refs/heads/master/NBA_final_results.csv")
  return(df)
}


header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

q = get_gamelog_data()

Encoding(q$Team) <- "UTF-8"
q$Team = iconv(q$Team, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
team_abbrevs <- sort(unique(q$Team), decreasing = FALSE)

#team_abbrevs = c("ATL","CHI","CON","DAL","IND","LAS","LVA","MIN",
#                 "NYL","PHO","SEA","WAS")
ui = shinyUI(
  pageWithSidebar(
    headerPanel("WNBA mins & (eventually) usage matrix")
    ,
    sidebarPanel(width=2,
                 wellPanel(
                   selectInput("Teams", label = h3("Team Select"),
                               choices =(team_abbrevs), 
                               hr(), ),
                 )
    )
    ,
    
    mainPanel(
      DT::dataTableOutput("mytable"),
    )
    
  ))


server = shinyServer(
  function(input,output,session){
    
    df = get_gamelog_data()

    output$mytable = DT::renderDataTable({   
      df = subset(df, df$Team == input$Teams)
      df$minutes = round(df$minutes, 1)
      
      j = df %>% select(Name, minutes, matchup, description, Date)

      df_data = j %>%
        spread(Name, minutes)
      
      holder = df_data %>% dplyr::select(matchup, description, Date)
      excluded_vars = c("matchup","description","Date")
      holder123 = select(df_data, -one_of(excluded_vars))
      holder2 <- holder123[,names(sort(colSums(holder123, na.rm = TRUE), decreasing = TRUE))]
      
      df_data = cbind(holder, holder2)
      
      df_data = df_data %>% arrange(Date)
      df_data$Date = NULL
      
      j = df %>% select(Name, is_starter, matchup, description, Date)
      
      reference_df = j %>%
        spread(Name, is_starter)
      holder_asdf = reference_df %>% dplyr::select(matchup, description, Date)
      excluded_vars = c("matchup","description","Date")
      holder_new = select(reference_df, -one_of(excluded_vars))
      holder_new <- holder_new[,names(sort(colSums(holder123, na.rm = TRUE), decreasing = TRUE))]
      reference_df = cbind(holder_asdf, holder_new)
      
      
      reference_df = reference_df %>% arrange(Date)
      reference_df$Date = NULL
      
      
      
      joinerino = cbind(df_data, reference_df)
 
      
      datatable(joinerino)
      
      invisible_start = ncol(df_data)+1
      invisible_end = ncol(joinerino)+1-1
      
      visible_start = 1
      visible_end = ncol(df_data)+1-1
      
      datatable(cbind(df_data,reference_df), selection = "single",class = 'cell-border stripe',
                options=list( autoWidth = TRUE, rownames = FALSE,
                              columnDefs = list(list(visible=FALSE, targets=c(invisible_start:invisible_end))),
                              className = 'dt-center', targets = "_all"))%>%
        formatStyle(visible_start:visible_end, valueColumns=invisible_start:invisible_end,
                    color = JS("value < 0 ? 'red' : value > 0 ? 'blue' : 'black'"))
          
     })
  })


shinyApp(ui = ui, server = server)


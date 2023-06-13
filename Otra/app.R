library(shiny)
library(sparklyr)
library(tidyverse)
library(plotly)
library(labelled)
library(knitr)

bd <-  EH2021_Vivienda %>% select(depto, 
                                  area, 
                                  s07a_01, 
                                  s07a_06, 
                                  s07a_07, 
                                  s07a_08, 
                                  s07a_09, 
                                  s07a_10, 
                                  s07a_13,
                                  s07a_16,
                                  s07a_18,
                                  s07a_21,
                                  s07a_26,
                                  s07a_28
                                  )
dict <- labelled::generate_dictionary(EH2021_Vivienda)
deptos <- as.data.frame(names(val_labels(bd[["depto"]])))

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "superhero", base_font = font_google("Inter"),
                          navbar_bg = "#25443B"),
  titlePanel("Caracteristicas de viviendas en Bolivia, EH 2021"),
  
    sidebarLayout(
      
    sidebarPanel(
      selectInput("variable", "Variable", choices = names(bdf)),
      tableOutput('tblvar')
    ),

  mainPanel(
    textOutput("titulo_graf") %>% tagAppendAttributes(style= 'color: cyan; 
                                                                  font-size: 24px;
                                                                  font-style: bold;'),
    tabsetPanel(
      id = "tabset",
      tabPanel("Porcentaje",
               
               plotlyOutput("torta_nac")), 
      tabPanel("Cantidad",
              
               plotlyOutput("treemp"))
    )
  )
)
)

server <- function(input, output, session) {
  
  thematic::thematic_shiny()

  output$titulo_graf  <- renderText({paste(input$variable, ". ", as.character(var_label(bdf[[input$variable]])))})
  
  output$tblvar <- renderTable({cbind(CODIGO = names(bdf), DESCRIPCION = as.character(var_label(bdf)))},
                               spacing = 'xs')  
  
  output$tblsel <- renderTable({summary(as.factor(bdf[[input$variable]]))})
  
  output$hist <- renderPlot({barplot(summary(as.factor(bdf[[input$variable]])))})
  
  output$treemp <- renderPlotly({
    plot_ly(
      bd,
      type= 'treemap',
      labels = names(val_labels(bd[[input$variable]])),
      parents = "",
      values = summary(as.factor(bd[[input$variable]])),
      textinfo="label+value+percent",
      domain=list(column=0)) %>% 
        layout(paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               title = "")
    
  })

  l <- list(
    orientation = 'v',
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 2)

 
  output$torta_nac <- renderPlotly({
    plot_ly(bdf,
            labels = ~names(val_labels(bdf[[input$variable]])),
            values = ~summary(as.factor(bdf[[input$variable]])),
            type = 'pie') %>%
      layout(legend = l,
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             title = "",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
}

shinyApp(ui, server)
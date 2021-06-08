#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# @kelsey_chalmers
# #RecreationThursday

library(shiny)
library(tidyverse)
library(ggimage)
library(magick)
library(here)
library(plotly)
library(htmlwidgets)

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  range = c(0, 1000)
)

original_pic <- png::readPNG(file.path(here(),"2021_06_hilto/hilto_og.png"))

# UI for plotting and tracing
ui <- fluidPage(

    # Application title
    h1("You did these? Freehand? - M. Scott, The Office.", style = "font-size:20px;"),

    # Show a plot of the generated distribution
    fluidRow(
      column(6,
             p("Trace over the original (click to start trace):"),
             plotlyOutput("artPlot")),
      column(6,
             p("Here is your creation!"),
             plotlyOutput("tracePlot"))),
    
    p("@kelsey_chalmers for #recreationThursday. Original: Alfredo Hilto. Curves and Straight Series. 1948.",style = "font-size:10px;")
    
)

# Server logic required for tracing
server <- function(input, output, session) {
    
    js <- "
    function(el, x){
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      Plotly.plot(id).then(attach);
        function attach() {
          var xaxis = gd._fullLayout.xaxis;
          var yaxis = gd._fullLayout.yaxis;

          var coordinates = [null, null]

          gd.addEventListener('click', function(evt) {
            var bb = evt.target.getBoundingClientRect();
            var x = xaxis.p2d(evt.clientX - bb.left);
            var y = yaxis.p2d(evt.clientY - bb.top);
            var coordinates = [x, y];
            Shiny.setInputValue('clickposition', coordinates);
          });
          gd.addEventListener('mousemove', function(evt) {
            var bb = evt.target.getBoundingClientRect();
            var x = xaxis.p2d(evt.clientX - bb.left);
            var y = yaxis.p2d(evt.clientY - bb.top);
            var coordinates = [x, y];
            Shiny.setInputValue('mouseposition', coordinates);
          });
        };
  }
  "
    
    output$artPlot <- renderPlotly({
        plot_ly(type = "scatter", mode = "markers") %>% 
            layout(
                xaxis = ax,
                yaxis = ax,
                images = list(source = raster2uri(as.raster(original_pic)),
                              x=0,y=0,sizex=1000,sizey=1000,
                              xref = "x",yref="y",
                              xanchor = "left",yanchor = "bottom",
                              sizing = "stretch")
            ) %>%
            onRender(js)
    })
    
    output$tracePlot <- renderPlotly(
      plot_ly(type = "scatter", mode = "markers") %>%
        layout(
          xaxis = ax,
          yaxis = ax
        )
    )
    
    artPlotProxy <- plotlyProxy("artPlot", session)
    tracePlotProxy <- plotlyProxy("tracePlot",session)
    
    followMouse <- reactiveVal(FALSE)
    traceCount <- reactiveVal(0L)
    
    observeEvent(input$clickposition, {
        followMouse(!followMouse())
        
        if(followMouse()){
          plotlyProxyInvoke(artPlotProxy, "addTraces", 
                            list(x = list(input$clickposition[1]), 
                                 y = list(input$clickposition[2])))  
          
          plotlyProxyInvoke(tracePlotProxy, "addTraces", 
                            list(x = list(input$clickposition[1]), 
                                 y = list(input$clickposition[2])))
          traceCount(traceCount()+1)
        }

    })
    
    observe({
        if(followMouse()){
            plotlyProxyInvoke(artPlotProxy, "extendTraces",
                              list(x = list(list(input$mouseposition[1])),
                                   y = list(list(input$mouseposition[2]))), 
                              list(traceCount())) 
            plotlyProxyInvoke(tracePlotProxy, "extendTraces",
                            list(x = list(list(input$mouseposition[1])),
                                 y = list(list(input$mouseposition[2]))), 
                            list(traceCount())) 
        }
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

##### Sources / inspiration ##### 
# https://stackoverflow.com/questions/41701807/way-to-free-hand-draw-shapes-in-shiny
# https://stackoverflow.com/questions/64309573/freehand-drawing-ggplot-how-to-improve-and-or-format-for-plotly
# https://linking.plotly-r.com/embedding-images.html

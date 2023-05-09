library(shiny)
library(qcc)

pca_choices <- c("PC1", "PC2", "PC3", "PC4")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(h1("HW4-PRINCIPLE COMPONENT ANALYSIS")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      h3("choose how many input to do PCA:"),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "points",
                  label = "Number of points",
                  min = 6,
                  max = 150,
                  value = 100),
      br(),
      h3("choose what you wnt to see on 'PCA Result : Plot'"),
      selectInput("xVar", "X Variable", choices = pca_choices),
      selectInput("yVar", "Y Variable", choices = pca_choices, selected = as.factor("PC2"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(
        
        # Output: Histogram ----
        tabPanel("PCA Result: Plot",
                 plotOutput(outputId = "pca_result"),
                 align = "center"
        ),
        
        tabPanel("Pareto Chart",
                 plotOutput(outputId = "pareto"),
                 align = "center"
        )
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$pca_result <- renderPlot({
    
    # x    <- faithful$waiting
    # bins <- seq(min(x), max(x), length.out = input$points + 1)
    # 
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    
    data(iris)
    # log transform
    log.ir <- log(iris[1:input$points, 1:4])
    ir.species <- iris[1:input$points, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    library(ggbiplot)
    xVar_id <- which(pca_choices == input$xVar)
    yVar_id <- which(pca_choices == input$yVar)
    g <- ggbiplot(ir.pca, choices = cbind(xVar_id, yVar_id), obs.scale = 1, var.scale = 1, groups = ir.species)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    g
  }, width = 600, height = 600)
  
  # https://rpubs.com/dnchari/pca
  output$pareto <- renderPlot({
    # log transform
    log.ir <- log(iris[1:input$points, 1:4])
    ir.species <- iris[1:input$points, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    PCA <- ir.pca$sdev^2
    cov <- round(ir.pca$sdev^2/sum(ir.pca$sdev^2)*100, 2)
    cov <- data.frame(c(1:4),cov)
    names(cov)[1] <- 'PCs'
    names(cov)[2] <- 'Variance'
    names(PCA) <- paste0('PC', cov$PCs)
    pareto.chart(PCA)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)




# data(iris)
# # log transform
# log.ir <- log(iris[, 1:4])
# ir.species <- iris[, 5]
# # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
# ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
# library(ggbiplot)
# g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
# print(g)

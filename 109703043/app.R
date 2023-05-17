library(shiny)
library(ggbiplot)
library(qcc)
library(FactoMineR)
library(factoextra)

pca_choices <- c("PC1", "PC2", "PC3", "PC4")
data(iris)

ui <- navbarPage("HW4-Principle Component Analysis",

  tabPanel("PCA",

    sidebarLayout(

      sidebarPanel(
       
        h3("Choose how many input to do PCA:"),

        sliderInput(inputId = "points_pca",
                    label = "Number of points",
                    min = 6,
                    max = 150,
                    value = 100),
        br(),
        h3("Choose what you wnt to see on 'PCA Result : Plot'"),
        selectInput("xVar", "X Variable", choices = pca_choices),
        selectInput("yVar", "Y Variable", choices = pca_choices, selected = as.factor("PC2"))
      ),

      mainPanel(
       
        tabsetPanel(
          
          tabPanel("PCA: Summary",
                   verbatimTextOutput(outputId = "pca_summary", placeholder = TRUE),
                   plotOutput(outputId = "pca_plot")
          )
        )
      )
    )
  ),
  
  tabPanel("CA",

    sidebarLayout(

      sidebarPanel(
       
        h3("Choose how many input to do CA:"),

        sliderInput(inputId = "points_ca",
                    label = "Number of points",
                    min = 6,
                    max = 150,
                    value = 100)
      ),

      mainPanel(
       
        tabsetPanel(
           
          tabPanel("CA: Summary",
                   plotOutput(outputId = "ca_plot"),
                   verbatimTextOutput(outputId = "ca_summary", placeholder = TRUE)
          )
        )
      )
    )
  ),
  
  tabPanel("Raw Data",

    sidebarLayout(

      sidebarPanel(
       
        h3("Choose how many input to summarize raw data:"),

        sliderInput(inputId = "points_raw",
                    label = "Number of points",
                    min = 6,
                    max = 150,
                    value = 100)
      ),

      mainPanel(
       
        tabsetPanel(
  
          tabPanel("Raw Data: Summary",
                   h3("Summary of raw data:"),
                   verbatimTextOutput(outputId = "raw_summary", placeholder = TRUE)
          )
        )
      )
    )
  ),
  
  tabPanel("More...",

    sidebarLayout(

      sidebarPanel(
       
        h3("Choose how many input to use:"),

        sliderInput(inputId = "points_more",
                    label = "Number of points",
                    min = 6,
                    max = 150,
                    value = 100)
      ),

      mainPanel(
       
        tabsetPanel(
           
          tabPanel("Pareto Chart",
                   plotOutput(outputId = "pareto"),
                   align = "center"
          )
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
  output$pca_plot <- renderPlot({
    # log transform
    log.ir <- log(iris[1:input$points_pca, 1:4])
    ir.species <- iris[1:input$points_pca, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    xVar_id <- which(pca_choices == input$xVar)
    yVar_id <- which(pca_choices == input$yVar)
    g <- ggbiplot(ir.pca, choices = cbind(xVar_id, yVar_id), obs.scale = 1, var.scale = 1, groups = ir.species)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    g
  }, height = 480, width = 480)
  
  output$pca_summary <- renderPrint({
    # log transform
    log.ir <- log(iris[1:input$points_pca, 1:4])
    ir.species <- iris[1:input$points_pca, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    summary(ir.pca)
  })
  
  # http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/113-ca-correspondence-analysis-in-r-essentials/
  output$ca_plot <- renderPlot({
    res.ca <- CA(iris[1:input$points_ca, 1:4], graph = FALSE)
    fviz_ca_biplot(res.ca, repel = FALSE)
  })
  
  output$ca_summary <- renderPrint({
    res.ca <- CA(iris[1:input$points_ca, 1:4], graph = FALSE)
    summary(res.ca)
  })
  
  output$raw_summary <- renderPrint({
    summary(iris[1:input$points_raw,])
  })
  
  # https://rpubs.com/dnchari/pca
  output$pareto <- renderPlot({
    # log transform
    log.ir <- log(iris[1:input$points_more, 1:4])
    ir.species <- iris[1:input$points_more, 5]
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

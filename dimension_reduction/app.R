library(shiny)
library(tidyverse)
library(ggfortify)
library(MASS)
library(caret)
library(e1071)

preproc <- function(df) {
    preproc.param <- df %>% preProcess(method = c("center", "scale"))
    df.trans <- preproc.param %>% predict(df)
    
    return(df.trans)
    
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Comparing Dimension Reduction Algorithms"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("pts_per_group",
                        "Number of Points per Group:",
                        min = 1,
                        max = 200,
                        value = 100),
            
            sliderInput("z_sd",
                       "Z Standard Deviation:",
                       min = 0,
                       max = 0.5,
                       value = 0.03),
            
            sliderInput("x_dist",
                        "Distance Between Means in X:",
                        min = 0,
                        max = 1,
                        value = 0.2),
           
             sliderInput("x_sd",
                        "X Standard deviation:",
                        min = 0,
                        max = 1,
                        value = 0.3),
            
            sliderInput("y_dist",
                        "Distance Between Means in Y:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            
            sliderInput("y_sd",
                        "Y Standard deviation:",
                        min = 0,
                        max = 1,
                        value = 0.075),
            

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("raw_plot"),
           plotOutput("PCA_plot"),
           plotOutput("LDA_plot"),
           plotOutput("SVM_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    set.seed(2)
    pal <- "Dark2"
    
    get_labs <- reactive({
        n_per_grp <- input$pts_per_group
        
        labels <- c(rep(x = "Aang", times = n_per_grp),
                 rep(x = "Toph", times = n_per_grp),
                 rep(x = "Zuko", times = n_per_grp))
        
        labels
    })
    
    make_df <- reactive({
        n_per_grp <- input$pts_per_group
        df <- tibble(.rows = 3*n_per_grp)
        
        lab <- get_labs()
        
        x <- c(rnorm(n = n_per_grp, mean = 0.5 - input$x_dist, sd = input$x_sd),
               (rnorm(n = n_per_grp, mean = 0.5, sd = input$x_sd)),
               (rnorm(n = n_per_grp, mean = 0.5 + input$x_dist, sd = input$x_sd)))
        
        y <- c(rnorm(n = n_per_grp, mean = 0.5 - input$y_dist, sd = input$y_sd),
               (rnorm(n = n_per_grp, mean = 0.5, sd = input$y_sd)),
               (rnorm(n = n_per_grp, mean = 0.5 + input$y_dist, sd = input$y_sd)))
        
        z <- c(rnorm(n = 3*n_per_grp, mean = 0.5, sd = input$z_sd))
        
        df <- df %>%
            add_column(lab) %>% 
            add_column(x) %>% 
            add_column(y) %>% 
            add_column(z)
        
        df
    })

    output$raw_plot <- renderPlot({
        
        df <- make_df()
        
        raw_plot <- ggplot(df, mapping = aes(x = x, y = y, color = lab)) +
            geom_point(aes(shape = lab), size = 4, alpha = 0.6) +
            scale_color_brewer(palette =  pal) +
            scale_fill_brewer(palette = pal) +
            labs(title = "Raw Data (from random normal distributions)",
                 color = "Bender",
                 shape = "Bender")
        
        raw_plot
    })
    
    output$PCA_plot <- renderPlot({
    
        df <- make_df()
        lab <- get_labs()
        
        df.trans <- preproc(df)
        
        df.pca <- prcomp(df.trans[,2:4])
        
        ggplot(df.pca, mapping = aes(PC1, PC2, color = lab)) +
            geom_point(aes(color = lab, shape = lab), size = 4, alpha = 0.6) +
            scale_color_brewer(palette =  pal) +
            scale_fill_brewer(palette = pal) +
            labs(title = "Principal Component Analysis",
                 color = "Bender",
                 shape = "Bender")
    })
    
    output$LDA_plot <- renderPlot({
        
        df <- make_df()
        
        df.trans <- preproc(df)
        
        df.lda <- lda(lab ~ .,data = df.trans)
        lda.data <- cbind(df, predict(df.lda)$x)
        
        n_per_grp <- input$pts_per_group
    
        
        ggplot(lda.data, aes(LD1, LD2)) +
            geom_point(aes(color = lab, shape = lab), size = 4, alpha = 0.6) +
            scale_color_brewer(palette =  pal) +
            scale_fill_brewer(palette = pal) +
            labs(title = "Linear Discriminant Analysis",
                 color = "Bender",
                 shape = "Bender")
    })
    
    output$SVM_plot <- renderPlot({
        
        df <- make_df()
        
        df.trans <- preproc(df)
        
        df.lda <- lda(lab ~ .,data = df.trans)
        lda.data <- cbind(df, predict(df.lda)$x)
        
        svm.data <- lda.data %>% dplyr::select(lab, LD1, LD2)
        svm.data$lab <- factor(svm.data$lab)
        
        svm_fit <-  svm.data %>% svm(factor(lab) ~ ., data = ., kernel = "radial", cost = 10, scale = FALSE)
        
        grid <- expand.grid(
            seq(min(svm.data$LD1), max(svm.data$LD1),length.out=200),  
            seq(min(svm.data$LD2), max(svm.data$LD2),length.out=200))
        
        names(grid) <- names(svm.data)[2:3]
        preds <- predict(svm_fit, grid)
        
        df <- data.frame(grid, preds)
        ggplot(df, mapping = aes(x = LD1, y = LD2)) + 
            geom_tile(alpha = 0.25, aes(fill = preds)) +
            geom_point(data = svm.data, size = 4, alpha = 0.6,
                       mapping = aes(color = lab, shape = lab)) +
            scale_color_brewer(palette =  pal) +
            scale_fill_brewer(palette = pal) +
            labs(title = "Support Vector Machine on LDA",
                 subtitle = "Radial Kernel",
                 fill = "SVM Prediction",
                 color = "Bender",
                 shape = "Bender")
            
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

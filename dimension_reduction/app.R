# Author:  Kenneth Schackart
# Date:    13 September 2020
# Email:   schackartk1@gmail.com

library(caret)
library(e1071)
library(ggfortify)
library(MASS)
library(shiny)
library(tidyverse)

preproc <- function(df) {
    #' Center and scale data prior to dimension reduction
    #' 
    #' @description
    #' Returns transformed dataframe same size as df
    
    # Center: subtract mean of predictor's data, scale: divide by std. dev.
    preproc_params <- df %>% preProcess(method = c("center", "scale"))
    df_trans <- preproc_params %>% predict(df)
    
    return(df_trans)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tabsetPanel(
        tabPanel("Raw Data Creation", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         h3("Class Size"),
                         
                         sliderInput("pts_per_group",
                                     "Number of Points per Class:",
                                     min = 1,
                                     max = 200,
                                     value = 100),
                         
                         h3("Z Distribution"),
                         
                         sliderInput("z_sd",
                                     "Standard Deviation:",
                                     min = 0,
                                     max = 1,
                                     value = 0.03),
                         
                         h3("X Distributions"),
                         
                         sliderInput("x_dist",
                                     "Distance Between Means:",
                                     min = 0,
                                     max = 1,
                                     value = 0.2),
                         
                         sliderInput("x_sd",
                                     "Standard deviation:",
                                     min = 0,
                                     max = 1,
                                     value = 0.3),
                         
                         h3("Y Distributions"),
                         
                         sliderInput("y_dist",
                                     "Distance Between Means:",
                                     min = 0,
                                     max = 1,
                                     value = 0.5),
                         
                         sliderInput("y_sd",
                                     "Standard deviation:",
                                     min = 0,
                                     max = 1,
                                     value = 0.075),
                         
                     ),
                     
                     mainPanel(
                         plotOutput("raw_plot")
                     )
                 )
        ),
        tabPanel("Dimension Reduction", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         
                     ),
                     
                     mainPanel(
                         plotOutput("PCA_plot"),
                         plotOutput("LDA_plot")
                     )
                 )
        ),
        tabPanel("SVM Classification", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         
                         selectInput("svm_kernel", label = h3("SVM Kernel"), 
                                     choices = list("Linear" = "linear",
                                                    "Polynomial" = "polynomial",
                                                    "Radial basis" = "radial",
                                                    "Sigmoid" = "sigmoid"),
                                                    
                                     selected = "linear"),
                         sliderInput("grid_res",
                                     label = h5("SVM Boundary Resolution (used for plotting):"),
                                     min = 10,
                                     max = 500,
                                     value = 100),
                     ),
                     
                     mainPanel(
                         plotOutput("SVM_plot")
                     )
                 )
        )
    )
    
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    set.seed(2)
    pal <- "Dark2"
    
    get_labs <- reactive({
        #' Generate label lists based on number of points chosen
        
        n_per_grp <- input$pts_per_group
        
        labels <- c(rep(x = "Aang", times = n_per_grp),
                    rep(x = "Toph", times = n_per_grp),
                    rep(x = "Zuko", times = n_per_grp))
        
        labels
    })
    
    make_df <- reactive({
        #' Generate dataframe based on selected parameters
        
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
        #' Make a plot of the raw data
        
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
        #' Perform PCA and plot the results
        
        df <- make_df()
        lab <- get_labs()
        
        df_trans <- preproc(df)
        
        df_pca <- prcomp(df_trans[,2:4])
        
        ggplot(df_pca, mapping = aes(PC1, PC2, color = lab)) +
            geom_point(aes(color = lab, shape = lab), size = 4, alpha = 0.6) +
            scale_color_brewer(palette =  pal) +
            scale_fill_brewer(palette = pal) +
            labs(title = "Principal Component Analysis",
                 color = "Bender",
                 shape = "Bender")
    })
    
    output$LDA_plot <- renderPlot({
        #' Perform LDA and plot the results
        
        df <- make_df()
        
        df_trans <- preproc(df)
        
        df_lda <- lda(lab ~ .,data = df_trans)
        lda_data <- cbind(df, predict(df_lda)$x)
        
        ggplot(lda_data, aes(LD1, LD2)) +
            geom_point(aes(color = lab, shape = lab), size = 4, alpha = 0.6) +
            scale_color_brewer(palette =  pal) +
            scale_fill_brewer(palette = pal) +
            labs(title = "Linear Discriminant Analysis",
                 color = "Bender",
                 shape = "Bender")
    })
    
    output$SVM_plot <- renderPlot({
        #' Run LDA and generate a SVM classifier, plot it
        
        df <- make_df()
        
        df_trans <- preproc(df)
        
        df_lda <- lda(lab ~ .,data = df_trans)
        lda_data <- cbind(df, predict(df_lda)$x)
        
        svm_data <- lda_data %>% dplyr::select(lab, LD1, LD2)
        svm_data$lab <- factor(svm_data$lab)
        
        svm_fit <-  svm_data %>% svm(factor(lab) ~ ., data = .,
                                     kernel = input$svm_kernel,
                                     scale = FALSE)
        
        # A grid the same size as the data, that will be turned into the SVM regions
        grid <- expand.grid(
            seq(min(svm_data$LD1), max(svm_data$LD1),length.out=input$grid_res),  
            seq(min(svm_data$LD2), max(svm_data$LD2),length.out=input$grid_res))
        names(grid) <- names(svm_data)[2:3]
        
        # Use SVM model to assign predicted class to grid points
        preds <- predict(svm_fit, grid)
        svm_regions <- data.frame(grid, preds)
        
        ggplot(svm_data, mapping = aes(x = LD1, y = LD2)) + 
            geom_tile(data = svm_regions, alpha = 0.25, aes(fill = preds)) +
            geom_point(size = 4, alpha = 0.6,
                       mapping = aes(color = lab, shape = lab)) +
            scale_color_brewer(palette =  pal) +
            scale_fill_brewer(palette = pal) +
            labs(title = "Support Vector Machine on LDA",
                 subtitle = "Radial Kernel",
                 fill = "SVM\nPrediction",
                 color = "Bender",
                 shape = "Bender")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

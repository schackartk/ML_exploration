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
    preproc_params <-
        df %>% preProcess(method = c("center", "scale"))
    df_trans <- preproc_params %>% predict(df)
    
    return(df_trans)
}

# Define the UI of the app

ui <- fluidPage(tabsetPanel(
    tabPanel(
        "Raw Data Creation",
        fluid = TRUE,
        sidebarLayout(
            sidebarPanel(
                h3("Class Size"),
                
                sliderInput(
                    "pts_per_group",
                    "Number of Points per Class:",
                    min = 1,
                    max = 200,
                    value = 100
                ),
                
                h3("Z Distribution"),
                
                sliderInput(
                    "z_sd",
                    "Standard Deviation:",
                    min = 0,
                    max = 1,
                    value = 0.03
                ),
                
                h3("X Distributions"),
                
                sliderInput(
                    "x_dist",
                    "Distance Between Means:",
                    min = 0,
                    max = 1,
                    value = 0.2
                ),
                
                sliderInput(
                    "x_sd",
                    "Standard deviation:",
                    min = 0,
                    max = 1,
                    value = 0.3
                ),
                
                h3("Y Distributions"),
                
                sliderInput(
                    "y_dist",
                    "Distance Between Means:",
                    min = 0,
                    max = 1,
                    value = 0.5
                ),
                
                sliderInput(
                    "y_sd",
                    "Standard deviation:",
                    min = 0,
                    max = 1,
                    value = 0.075
                ),
                
            ),
            
            mainPanel(plotOutput("raw_plot"))
        )
    ),
    tabPanel("Dimension Reduction", fluid = TRUE,
             fluidRow(
                 column(6,
                        plotOutput("PCA_plot")),
                 column(6,
                        plotOutput("LDA_plot"))
             )),
    tabPanel(
        "SVM Classification",
        fluid = TRUE,
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    "dim_reduc",
                    label = h3("Dimension Reduction"),
                    choices = list(
                        "None" = "none",
                        "Linear Discriminant Analysis" = "lda",
                        "Principal Component Analysis" = "pca"
                    ),
                    
                    selected = "lda"
                ),
                
                em(
                    "Note: Plot may not be accurate when choosing 'None'.
                         In this case points are simply projected onto the the plane Z=0.5 for plotting.
                         So SVM bounds shown are for the plane Z=0.5."
                ),
                
                h3("Support Vector Machine"),
                
                selectInput(
                    "svm_kernel",
                    label = h5("SVM Kernel"),
                    choices = list(
                        "Linear" = "linear",
                        "Polynomial" = "polynomial",
                        "Radial basis" = "radial",
                        "Sigmoid" = "sigmoid"
                    ),
                    
                    selected = "linear"
                ),
                
                sliderInput(
                    "grid_res",
                    label = h5("SVM Boundary Resolution (used for plotting):"),
                    min = 10,
                    max = 500,
                    value = 100
                )
            ),
            
            mainPanel(plotOutput("SVM_plot"))
        )
    )
))

# Define server logic to generate responsive content
server <- function(input, output) {
    set.seed(2)
    pal <- c("#82B4E9", "#339966", "#DD3E15")
    plot_title_size <- 20
    
    get_labs <- reactive({
        #' Generate label lists based on number of points chosen
        
        n_per_grp <- input$pts_per_group
        
        labels <- c(
            rep(x = "Toph", times = n_per_grp),
            rep(x= "Aang", times = n_per_grp),
            rep(x = "Zuko", times = n_per_grp)
        )
        
        labels <- factor(labels, levels = c("Aang", "Toph", "Zuko"))
        
        labels
    })
    
    make_df <- reactive({
        #' Generate dataframe based on selected parameters
        
        n_per_grp <- input$pts_per_group
        df <- tibble(.rows = 3 * n_per_grp)
        
        # Get labels, based on the class size chosen
        
        lab <- get_labs()
        
        X <-
            c(rnorm(n = n_per_grp, mean = 0.5 - input$x_dist, sd = input$x_sd),
              rnorm(n = n_per_grp, mean = 0.5, sd = input$x_sd),
              rnorm(n = n_per_grp,mean = 0.5 + input$x_dist, sd = input$x_sd))
        
        Y <-
            c(rnorm(n = n_per_grp, mean = 0.5 - input$y_dist, sd = input$y_sd),
              rnorm(n = n_per_grp, mean = 0.5, sd = input$y_sd),
              rnorm(n = n_per_grp, mean = 0.5 + input$y_dist, sd = input$y_sd))
        
        Z <- c(rnorm(n = 3 * n_per_grp, mean = 0.5, sd = input$z_sd))
        
        df <- df %>%
            add_column(lab) %>%
            add_column(X) %>%
            add_column(Y) %>%
            add_column(Z)
        
        df
    })
    
    output$raw_plot <- renderPlot({
        #' Make a plot of the raw data
        
        df <- make_df()
        
        raw_plot <-
            ggplot(df, mapping = aes(x = X, y = Y, color = lab
            )) +
            geom_point(aes(shape = lab), alpha = 0.6, size = 4) +
            scale_color_manual(values =  pal) +
            scale_fill_manual(values = pal) +
            theme(
                plot.title = element_text(hjust = 0.5, size = plot_title_size),
                plot.subtitle = element_text(hjust = 0.5, size = 0.75 *
                                                 plot_title_size)
            ) +
            labs(
                title = "Raw Data (from random normal distributions)",
                subtitle = "Z-axis not shown",
                color = "Bender",
                shape = "Bender"
            )
        
        raw_plot
    })
    
    output$PCA_plot <- renderPlot({
        #' Perform PCA and plot the results
        
        df <- make_df()
        lab <- get_labs()
        
        df_trans <- preproc(df)
        
        df_pca <- prcomp(df_trans[, 2:4])
        
        ggplot(df_pca, mapping = aes(PC1, PC2, color = lab)) +
            geom_point(aes(color = lab, shape = lab),
                       size = 4,
                       alpha = 0.6) +
            scale_color_manual(values = pal) +
            scale_fill_manual(values = pal) +
            theme(plot.title = element_text(hjust = 0.5, size = plot_title_size)) +
            labs(title = "Principal Component Analysis",
                 color = "Bender",
                 shape = "Bender")
    })
    
    output$LDA_plot <- renderPlot({
        #' Perform LDA and plot the results
        
        df <- make_df()
        
        df_trans <- preproc(df)
        
        df_lda <- lda(lab ~ ., data = df_trans)
        lda_data <- cbind(df, predict(df_lda)$x)
        
        ggplot(lda_data, aes(LD1, LD2)) +
            geom_point(aes(color = lab, shape = lab),
                       size = 4,
                       alpha = 0.6) +
            scale_color_manual(values =  pal) +
            scale_fill_manual(values = pal) +
            theme(plot.title = element_text(hjust = 0.5, size = plot_title_size)) +
            labs(title = "Linear Discriminant Analysis",
                 color = "Bender",
                 shape = "Bender")
    })
    
    output$SVM_plot <- renderPlot({
        #' Run LDA and generate a SVM classifier, plot it
        
        df <- make_df()
        
        df_trans <- preproc(df)
        
        if (input$dim_reduc == "lda") {
            df_lda <- lda(lab ~ ., data = df_trans)
            lda_data <- cbind(df, predict(df_lda)$x)
            
            svm_data <- lda_data %>% dplyr::select(lab, LD1, LD2)
            names(svm_data) <- c("lab", "X1", "X2")
            x_label <- "LD1"
            y_label <- "LD2"
        }
        else if (input$dim_reduc == "pca") {
            df_pca <- prcomp(df_trans[, 2:4])
            df_pca <- as.data.frame.matrix(df_pca$x)
            df_pca$lab <- get_labs()
            
            svm_data <- df_pca %>% dplyr::select(lab, PC1, PC2)
            names(svm_data) <- c("lab", "X1", "X2")
            x_label <- "PC1"
            y_label <- "PC2"
        }
        else {
            svm_data <- df
            svm_data$lab <- factor(svm_data$lab)
            names(svm_data) <- c("lab", "X1", "X2", "X3")
            x_label <- "X"
            y_label <- "Y"
        }
        
        
        svm_fit <-  svm_data %>% svm(
            factor(lab) ~ .,
            data = .,
            kernel = input$svm_kernel,
            scale = FALSE
        )
        
        # A grid same size as data, that will be turned into the SVM regions
        grid <- expand.grid(
            seq(
                min(svm_data$X1),
                max(svm_data$X1),
                length.out = input$grid_res
            ),
            seq(
                min(svm_data$X2),
                max(svm_data$X2),
                length.out = input$grid_res
            )
        )
        names(grid) <- names(svm_data)[2:3]
        
        if (input$dim_reduc == "none") {
            grid <- expand.grid(
                seq(
                    min(svm_data$X1),
                    max(svm_data$X1),
                    length.out = input$grid_res
                ),
                seq(
                    min(svm_data$X2),
                    max(svm_data$X2),
                    length.out = input$grid_res
                )
            )
            
            names(grid) <- names(svm_data)[2:3]
            
            grid$X3 <- rep(0.5, length(grid$X1))
        }
        
        # Use SVM model to assign predicted class to grid points
        preds <- predict(svm_fit, grid)
        svm_regions <- data.frame(grid[1:2], preds)
        
        ggplot(svm_data, mapping = aes(x = X1, y = X2)) +
            geom_tile(data = svm_regions, alpha = 0.25, aes(fill = preds)) +
            geom_point(
                size = 4,
                alpha = 0.6,
                mapping = aes(color = lab, shape = lab)
            ) +
            scale_color_manual(values =  pal) +
            scale_fill_manual(values = pal) +
            theme(plot.title = element_text(hjust = 0.5, size = plot_title_size)) +
            labs(
                title = "Support Vector Machine",
                fill = "SVM\nPrediction",
                x = x_label,
                y = y_label,
                color = "Bender",
                shape = "Bender"
            )
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

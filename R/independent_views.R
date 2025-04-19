#' @importFrom stats cov rnorm sd

#' @noRd
.independent_views <- function(X, y, dim = 2, method, center.scale = TRUE, asp.equal = TRUE, views=1) {

  if (!dim %in% c(2, 3)) {
    stop("Parameter 'dim' must be either 2 or 3. Provided value: ", dim)
  }
  views <- as.integer(views)
  if (!is.integer(views) || views < 1) {
    stop("Parameter 'views' must be a positive integer. Provided value: ", views)
  }

  plots <- list()
  # To store all transformation matrices and projected data
  transformation_matrices <- list()
  projected_data_list <- list()

  # Predefined color palette
  colorway <- my_package_env$palette
  # Adjust the color palette if there are more unique categories than colors
  unique_categories <- unique(as.factor(y))
  if(length(unique_categories) > length(colorway)) {
    colorway <- rep(colorway, length.out = length(unique_categories))
  }
  colorway <- colorway[1:length(unique_categories)]

  x_ax <- list(title = "Dimension 1")
  y_ax <- list(title = "Dimension 2")

  if (asp.equal) {
    # lock y to x with equal scaling
    y_ax$scaleanchor <- "x"
    y_ax$scaleratio <- 1
  }

  X <- as.matrix(X)
  y <- as.factor(y)

  if (method == "pca") {
    num_components_needed <- 2 * dim
    if (ncol(X) < num_components_needed) {
      stop(paste("Not enough dimensions in X. PCA requires at least", num_components_needed, "features for", dim, "D visualization."))
    }
    # First view: Use pca to get PC1 and PC2
    pca_result <- .pca(X, dim, center.scale)
    projected_data <- pca_result$projected_data
    transformation_matrix <- pca_result$transformation_matrix
  } else if(method == "cca"){
    # First view: Use cca to get first two dimensions
    cca_result <- .cca(X, y, dim, center.scale)
    projected_data <- Re(cca_result$projected_data)
    transformation_matrix <- Re(cca_result$transformation_matrix)
  } else if(method == "nn"){
    # First view: Use nn to get first two dimensions
    nn_result <- .nn(X, y, dim, center.scale)
    projected_data <- nn_result$projected_data
    transformation_matrix <- nn_result$transformation_matrix
  } else {
    stop("Unsupported method. Please use 'pca', 'cca' or 'nn.")
  }

  # Store the first view data
  transformation_matrices[[1]] <- transformation_matrix
  projected_data_list[[1]] <- projected_data

  title <- switch(method,
                  'pca' = 'PCA Independent Views',
                  'cca' = 'CCA Independent Views',
                  'nn' = 'Neural Network Independent Views'
  )

  # Extract first dim components
  dim1 <- projected_data[, 1]
  dim2 <- projected_data[, 2]
  if (dim == 3) {
    dim3 <- projected_data[, 3]
  }

  # First view visualization
  plot1 <- if (dim == 2) {
    plot_ly(x = dim1, y = dim2, color = y,
            type = "scatter",
            mode = "markers",
            colors = colorway,
            marker = list(size = 5,
                          opacity = 0.85),
            showlegend = TRUE) %>%
      layout(xaxis = x_ax,
             yaxis = y_ax) %>%
      .plotly_theme()
  } else {
    plot_ly(x = dim1, y = dim2, z = dim3, color = y,
            type = "scatter3d",
            mode = "markers",
            colors = colorway,
            marker = list(size = 4),
            showlegend = TRUE,
            scene="scene1")
  }
  plots[[1]] <- plot1

  # Other visualizations
  m <- nrow(transformation_matrix)

  if (dim == 3){
    views = min(views, 4)
  }
  views = min(views, floor(m/dim))

  # Calculate number of rows and columns for a square grid
  num_rows <- ceiling(sqrt(views))
  num_cols <- floor(sqrt(views))

  for (view_num in 2:views) {
    m <- nrow(transformation_matrix)  # Length of each vector
    # Generate (m-2) vectors of length m from N(0,1)
    #set.seed(123)
    random_matrix <- matrix(rnorm((m-dim) * m), nrow = m, ncol = (m-dim))
    #set.seed(NULL)
    P <- cbind(transformation_matrix, random_matrix)
    Q2 <- .orth(X, P, dim)$Q2
    independent_data <- X %*% Q2

    if (method == "pca") {
      # Compute PCA again on independent data
      pca_result_orth <- .pca(independent_data, dim, center.scale)
      projected_data_orth <- pca_result_orth$projected_data
      transformation_matrix <- pca_result_orth$transformation_matrix
    } else if(method == "cca"){
      # Compute cca again on independent data
      cca_result_orth <- .cca(independent_data, y, dim, center.scale)
      projected_data_orth <- Re(cca_result_orth$projected_data)
      transformation_matrix <- cca_result_orth$transformation_matrix
    } else if(method == "nn"){
      # Compute nn again on independent data
      nn_result_orth <- .nn(independent_data, y, dim, center.scale)
      projected_data_orth <- nn_result_orth$projected_data
      transformation_matrix <- nn_result_orth$transformation_matrix
    }

    # Store the transformation matrix and projected data for this view
    transformation_matrices[[view_num]] <- transformation_matrix
    projected_data_list[[view_num]] <- projected_data_orth

    # Extract next dim components
    dim4 <- projected_data_orth[, 1]
    dim5 <- projected_data_orth[, 2]
    if (dim == 3) {
      dim6 <- projected_data_orth[, 3]
    }

    # Second view visualization
    plot2 <- if (dim == 2) {
      plot_ly(x = dim4, y = dim5, color = y,
              type = "scatter",
              mode = "markers",
              colors = colorway,
              marker = list(size = 5, opacity = 0.85),
              showlegend = FALSE) %>%
        layout(xaxis = x_ax,
               yaxis = y_ax) %>%
        .plotly_theme()
    } else {
      plot_ly(x = dim4, y = dim5, z = dim6, color = y,
              type = "scatter3d",
              mode = "markers",
              colors = colorway,
              marker = list(size = 4),
              showlegend = FALSE,
              scene = paste0("scene", view_num)
        )
    }
    plots[[view_num]] <- plot2
  }

  y_scene <- c(0,1)
  if (dim == 3 && views > 2){
    y_scene <- c(0.5, 1)
  }
  # Final layout with two independent 3D scenes
  options(warn = -1)
  final_plot <- if (dim == 3)  {subplot(plots, nrows = num_rows, margin = 0.05,
                                        shareX = FALSE, shareY = FALSE,
                                        titleX = TRUE, titleY = TRUE  ) %>%
      layout(
        title = list(text = title, y = 0.98),
        scene = list(xaxis=list(title = "Dimension 1",
                                gridcolor='white',
                                zerolinecolor='white',
                                showbackground=TRUE,
                                backgroundcolor='rgb(240, 240,240)'),
                     yaxis=list(title = "Dimension 2",
                                gridcolor='white',
                                zerolinecolor='white',
                                showbackground=TRUE,
                                backgroundcolor='rgb(240, 240,240)'),
                     zaxis=list(title = "Dimension 3",
                                gridcolor='white',
                                zerolinecolor='white',
                                showbackground=TRUE,
                                backgroundcolor='rgb(240, 240,240)'),
                    domain=list(x=c(0,0.5),y=y_scene)),
        scene2 = list(xaxis=list(title = "Dimension 1",
                                 gridcolor='white',
                                 zerolinecolor='white',
                                 showbackground=TRUE,
                                 backgroundcolor='rgb(240, 240,240)'),
                      yaxis=list(title = "Dimension 2",
                                 gridcolor='white',
                                 zerolinecolor='white',
                                 showbackground=TRUE,
                                 backgroundcolor='rgb(240, 240,240)'),
                      zaxis=list(title = "Dimension 3",
                                 gridcolor='white',
                                 zerolinecolor='white',
                                 showbackground=TRUE,
                                 backgroundcolor='rgb(240, 240,240)'),
                      domain=list(x=c(0.5,1),y=y_scene)),
        scene3 = list(xaxis=list(title = "Dimension 1",
                                 gridcolor='white',
                                 zerolinecolor='white',
                                 showbackground=TRUE,
                                 backgroundcolor='rgb(240, 240,240)'),
                      yaxis=list(title = "Dimension 2",
                                 gridcolor='white',
                                 zerolinecolor='white',
                                 showbackground=TRUE,
                                 backgroundcolor='rgb(240, 240,240)'),
                      zaxis=list(title = "Dimension 3",
                                 gridcolor='white',
                                 zerolinecolor='white',
                                 showbackground=TRUE,
                                 backgroundcolor='rgb(240, 240,240)'),
                      domain=list(x=c(0,0.5),y=c(0, 0.5))),
        scene4 = list(xaxis=list(title = "Dimension 1",
                                 gridcolor='white',
                                 zerolinecolor='white',
                                 showbackground=TRUE,
                                 backgroundcolor='rgb(240, 240,240)'),
                      yaxis=list(title = "Dimension 2",
                                 gridcolor='white',
                                 zerolinecolor='white',
                                 showbackground=TRUE,
                                 backgroundcolor='rgb(240, 240,240)'),
                      zaxis=list(title = "Dimension 3",
                                 gridcolor='white',
                                 zerolinecolor='white',
                                 showbackground=TRUE,
                                 backgroundcolor='rgb(240, 240,240)'),
                      domain=list(x=c(0.5,1),y=c(0, 0.5))))
  } else{
  final_plot <-
    subplot(plots, nrows = num_rows, margin = 0.05,
            shareX = TRUE, shareY = TRUE,
            titleX = TRUE, titleY=TRUE) %>%
    layout(
      title = list(text = title, y = 0.98))
  }
  print(final_plot)

  # Return all transformation matrices and projected data
  return(invisible(list(transformation_matrices = transformation_matrices, projected_data = projected_data_list)))
}

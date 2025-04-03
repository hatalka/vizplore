#' @title Visualization of Independent Views
#'
#' @description Creates 2 independent 2D views of the multidimensional data using the specified dimensionality reduction method, with subsequent views based on orthogonal projections to explore independent aspects of the data.
#'
#' @param X A matrix (n x m) representing the input features, where n is the number of samples and m is the number of features.
#' @param y A vector of length n representing the categories or labels corresponding to the input data.
#' @param dim Integer indicating the desired dimensionality of the visualization: 2 for 2D, 3 for 3D. Default is 2.
#' @param method A character string specifying the reduction method to use ("pca", "nn", "catcca").
#' @param center.scale A logical (boolean) value indicating whether the data should be centered and scaled before processing.
#'
#' @return A grid of plotly subplots representing the independent views.
#' @examples
#' if (!require("ContaminatedMixt")) {
#'      install.packages("ContaminatedMixt")}
#' library("ContaminatedMixt")
#' data(wine)
#' X <- wine[,-1]
#' y <- wine[,1]
#' independent_views(X, y, method = 'pca')
#' @export
#' @import plotly
#' @importFrom stats cov rnorm sd

#' @export
independent_views <- function(X, y, dim = 2, method = "all", center.scale = TRUE) {

  if (!dim %in% c(2, 3)) {
    stop("Parameter 'dim' must be either 2 or 3. Provided value: ", dim)
  }

  plots <- list()

  # Predefined color palette
  colorway <- my_package_env$palette
  # Adjust the color palette if there are more unique categories than colors
  unique_categories <- unique(as.factor(y))
  if(length(unique_categories) > length(colorway)) {
    colorway <- rep(colorway, length.out = length(unique_categories))
  }
  colorway <- colorway[1:length(unique_categories)]

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
  } else if(method == "catcca"){
    # First view: Use catcca to get first two dimensions
    catcca_result <- .catcca(X, y, dim, center.scale)
    projected_data <- Re(catcca_result$projected_data)
    transformation_matrix <- Re(catcca_result$transformation_matrix)
  } else if(method == "nn"){
    # First view: Use nn to get first two dimensions
    nn_result <- .nn(X, y, dim, center.scale)
    projected_data <- nn_result$projected_data
    transformation_matrix <- nn_result$transformation_matrix
  } else {
    stop("Unsupported method. Please use 'pca', 'catcca' or 'nn.")
  }

  title <- switch(method,
                  'pca' = 'PCA Independent Views',
                  'catcca' = 'catCCA Independent Views',
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
      layout(xaxis = list(title="Dimension 1"),
             yaxis = list(title="Dimension 2")) %>%
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

  # Second visualization
  m <- nrow(transformation_matrix)  # Length of each vector

  # Generate (m-2) vectors of length m from N(0,1)
  random_matrix <- matrix(rnorm((m-dim) * m), nrow = m, ncol = (m-dim))
  P <- cbind(transformation_matrix, random_matrix)
  Q2 <- .orth(X, P, dim)$Q2
  independent_data <- X %*% Q2

  if (method == "pca") {
    # Compute PCA again on independent data
    pca_result_orth <- .pca(independent_data, dim, center.scale)
    projected_data_orth <- pca_result_orth$projected_data
  } else if(method == "catcca"){
    # Compute catcca again on independent data
    catcca_result_orth <- .catcca(independent_data, y, dim, center.scale)
    projected_data_orth <- Re(catcca_result_orth$projected_data)
  } else if(method == "nn"){
    # Compute nn again on independent data
    nn_result_orth <- .nn(independent_data, y, dim, center.scale)
    projected_data_orth <- nn_result_orth$projected_data
  }

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
      layout(xaxis = list(title="Dimension 1"),
             yaxis = list(title="Dimension 2")) %>%
      .plotly_theme()
  } else {
    plot_ly(x = dim4, y = dim5, z = dim6, color = y,
            type = "scatter3d",
            mode = "markers",
            colors = colorway,
            marker = list(size = 4),
            showlegend = FALSE,
            scene = "scene2"
      )
  }
  plots[[2]] <- plot2

  # Final layout with two independent 3D scenes
  options(warn = -1)
  final_plot <- if (dim == 3)  {subplot(plots, nrows = 1, margin = 0.05,
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
                                backgroundcolor='rgb(240, 240,240)')),
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
                                 backgroundcolor='rgb(240, 240,240)')))
  } else{
  final_plot <-
    subplot(plots, nrows = 1, margin = 0.05,
            shareX = FALSE, shareY = FALSE,
            titleX = TRUE, titleY=TRUE) %>%
    layout(
      title = list(text = title, y = 0.98))
  }
  print(final_plot)
}

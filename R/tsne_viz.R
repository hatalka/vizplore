#' @title t-SNE Visualization
#'
#' @description Creates a 2D visualization of multidimensional data using
#' t-distributed Stochastic Neighbor Embedding.
#'
#' @param X A matrix representing the input features (quantitative variables).
#' @param y A vector representing the categories corresponding to the input data.
#' @param dim Integer indicating the desired dimensionality of the visualization: 2 for 2D, 3 for 3D. Default is 2.
#' @param center.scale A logical (boolean) value indicating whether the data should be centered and scaled before processing.
#'
#' @return A list containing:
#' \describe{
#'   \item{projected_data}{The projected data onto the canonical components.}
#'   \item{transformation_matrix}{The eigenvectors corresponding to the canonical components.}
#' }
#' Additionally, a Plotly plot representing the data points in the reduced feature space,
#' with points colored by their categories is displayed.
#'
#' @examples
#' data(iris)
#' iris <- unique(iris)
#' X <- iris[,-5]
#' y <- iris[,5]
#' tsne_viz(X, y)          # Default 2D visualization
#' tsne_viz(X, y, dim = 3) # 3D visualization
#'
#' @export
#' @importFrom plotly plot_ly layout
#' @import Rtsne
#'
#' @note This function uses the `Rtsne` function from the `Rtsne` package for dimensionality
#' reduction. The `Rtsne` package was developed by and is maintained by others.

#' @export
tsne_viz <- function(X, y, dim = 2, center.scale = TRUE) {
  # Validate the 'dim' parameter
  if (!dim %in% c(2, 3)) {
    stop("Parameter 'dim' must be either 2 or 3. Provided value: ", dim)
  }

  # Predefined color palette
  colorway <- my_package_env$palette
  y_factor <- as.factor(y)
  # Adjust the color palette if there are more unique categories than colors
  unique_categories <- unique(y_factor)
  if(length(unique_categories) > length(colorway)) {
    colorway <- rep(colorway, length.out = length(unique_categories))
  }
  colorway <- colorway[1:length(unique_categories)]

  # Call the internal neural network function to perform dimensionality reduction
  tsne_result <- .tsne(X, dim, center.scale)
  reduced_data <- tsne_result$projected_data

  # Prepare data for visualization based on the desired dimension
  if (dim == 2) {
    plot_data <- data.frame(
      Dim1 = reduced_data[, 1],
      Dim2 = reduced_data[, 2],
      Category = y_factor
    )

    p <- plot_ly(
      data = plot_data,
      x = ~Dim1,
      y = ~Dim2,
      color = ~Category,
      type = 'scatter',
      mode = 'markers',
      colors = colorway,
      marker = list(size = 5, opacity = 0.85)
    ) %>%
      layout(
        title = list(text = "2D Projection Using t-SNE", y = 0.99),
        xaxis = list(title = "Dimension 1"),
        yaxis = list(title = "Dimension 2")
      )

  } else if (dim == 3) {
    if (ncol(reduced_data) < 3) {
      stop("Insufficient number of dimensions in reduced data for 3D visualization.")
    }

    plot_data <- data.frame(
      Dim1 = reduced_data[, 1],
      Dim2 = reduced_data[, 2],
      Dim3 = reduced_data[, 3],
      Category = y_factor
    )

    p <- plot_ly(
      data = plot_data,
      x = ~Dim1,
      y = ~Dim2,
      z = ~Dim3,
      color = ~Category,
      type = 'scatter3d',
      mode = 'markers',
      colors = colorway,
      marker = list(size = 4)
    ) %>%
      layout(
        title = list(text = "3D Projection Using t-SNE", y = 0.98),
        scene = list(
          xaxis = list(title = "Dimension 1"),
          yaxis = list(title = "Dimension 2"),
          zaxis = list(title = "Dimension 3")
        )
      )
  }

  # Apply the global Plotly theme and display the plot
  print(.plotly_theme(p))

  return(invisible(list(reduced_data = tsne_result$reduced_data)))
}

#' @noRd
.tsne <- function(X, dim = 2, center.scale = TRUE) {
  # Ensure the input data are in matrix format
  X <- as.matrix(X)
  # Standardizing the data (mean = 0, standard deviation = 1)
  if(center.scale == TRUE){
    # Standardizing the data (mean = 0, standard deviation = 1)
    X <- .center.and.scale(X)
  }

  # Check if the number of columns is sufficient for the requested components
  if(ncol(X) < dim) {
    stop("The number of columns in X is less than the requested dimension.")
  }

  projected_data <- Rtsne(X, dims = dim)$Y

  return(invisible(list(projected_data = projected_data)))
}

#' @title PCA Visualization
#'
#' @name pca_viz
#'
#' @description Creates a 2D (or 3D) visualization of multidimensional labeled data using Principal Component Analysis (PCA).
#'
#' @param X A matrix representing the input features (quantitative variables).
#' @param y A vector representing the categories corresponding to the input data.
#' @param dim Integer indicating the desired dimensionality of the visualization: 2 for 2D, 3 for 3D. Default is 2.
#' @param center.scale A logical (boolean) value indicating whether the data should be centered and scaled before processing.
#' @param asp.equal A logical (boolean) value, relevant only for 2D visualization, indicating whether the aspect ratio on both axes should be the same.
#' @param views An integer specifying the number of independent views. For 3D visualization, the maximum is 4. Subsequent views are based on orthogonal projections to capture different perspectives of the data.
#'
#' @return
#' \describe{
#'   \item{projected_data}{The data projected onto the principal components.}
#'   \item{transformation_matrix}{The eigenvectors corresponding to the selected principal components.}
#' }
#' Additionally, a plotly object representing the PCA visualization is displayed.
#'
#' @examples
#' data(iris)
#' X <- iris[,-5]
#' y <- iris[,5]
#' pca_viz(X, y, views = 2)     # 2D visualization
#' pca_viz(X, y, dim = 3) # 3D visualization
#' @export
#' @importFrom plotly plot_ly layout

#' @export
pca_viz <- function(X, y, dim = 2, center.scale = TRUE, asp.equal = TRUE, views = 1) {
  # Validate the 'dim' parameter
  if (!dim %in% c(2, 3)) {
    stop("Parameter 'dim' must be either 2 or 3. Provided value: ", dim)
  }

  # Ensure input data are matrices and vectors
  X <- as.matrix(X)
  y <- as.factor(y)

  # Predefined color palette
  colorway <- my_package_env$palette
  # Adjust the color palette if there are more unique categories than colors
  unique_categories <- unique(y)
  if(length(unique_categories) > length(colorway)) {
    colorway <- rep(colorway, length.out = length(unique_categories))
  }
  colorway <- colorway[1:length(unique_categories)]

  # Check if the number of columns is sufficient for the requested components
  if(ncol(X) < dim) {
    stop("The number of columns in X is less than the requested number of principal components. Please ensure that X has at least ", dim, " columns.")
  }
  views <- as.integer(views)
  if (!is.integer(views) || views < 1) {
    stop("Parameter 'views' must be a positive integer. Provided value: ", views)
  }

  if(center.scale == TRUE){
    # Standardizing the data (mean = 0, standard deviation = 1)
    X <- .center.and.scale(X)
  }

  m <- ncol(X)

  if (dim == 3){
    views = min(views, 4)
  }
  views = min(views, floor(m/dim))

  if (views == 1){
    # Perform PCA using the internal function
    pca_result <- .pca(X, dim)
    projected_data <- pca_result$projected_data
    transformation_matrix <- pca_result$transformation_matrix

    # Prepare data for visualization
    plot_data <- data.frame(PC1 = projected_data[, 1],
                            PC2 = projected_data[, 2],
                            Category = y)

    x_ax <- list(title = "PC1")
    y_ax <- list(title = "PC2")

    if (asp.equal) {
      # lock y to x with equal scaling
      y_ax$scaleanchor <- "x"
      y_ax$scaleratio <- 1
    }

    # Create 2D or 3D plot based on the desired dimensionality
    if (dim == 2) {
      p <- plot_ly(data = plot_data,
                   x = ~PC1,
                   y = ~PC2,
                   color = ~Category,
                   type = 'scatter',
                   mode = 'markers',
                   colors = colorway[1:length(unique(y))],
                   marker = list(size = 5, opacity = 0.85)) %>%
        layout(title = list(text = '2D Projection Using PCA',y=0.98),
               xaxis = x_ax,
               yaxis = y_ax)
    } else if (dim == 3) {
      # Append the third principal component to the plot data
      plot_data$PC3 <- projected_data[, 3]
      p <- plot_ly(data = plot_data,
                   x = ~PC1,
                   y = ~PC2,
                   z = ~PC3,
                   color = ~Category,
                   type = 'scatter3d',
                   mode = 'markers',
                   colors = colorway[1:length(unique(y))],
                   marker = list(size = 4)) %>%
        layout(title = list(text = '3D Projection Using PCA',y=0.98),
               scene = list(
                 xaxis = list(title = 'PC1'),
                 yaxis = list(title = 'PC2'),
                 zaxis = list(title = 'PC3')
               ))
    } else {
      stop("Parameter 'dim' must be either 2 or 3. Provided value: ", dim)
    }

    # Apply the global plotly theme
    print(.plotly_theme(p))
  } else {
    iv <- .independent_views(X,y, dim, method = 'pca', center.scale, asp.equal, views)
    projected_data <- iv$projected_data
    transformation_matrix <- iv$ transformation_matrices
  }
  # Return PCA results invisibly
  return(invisible(list(projected_data = projected_data, transformation_matrix = transformation_matrix)))
}

#' @noRd
# Internal PCA function
.pca <- function(X, dim=2, center.scale = TRUE) {
  # Ensure input data are matrices
  X <- as.matrix(X)
  num_components <- min(dim, ncol(X))

  # Check if the number of columns is sufficient for the requested components
  if(ncol(X) < dim) {
    stop("The number of columns in X is less than the requested number of principal components.")
  }

  if(center.scale == TRUE){
    # Standardizing the data (mean = 0, standard deviation = 1)
    X <- .center.and.scale(X)
  }

  # Perform PCA using eigen decomposition of the covariance matrix
  cov_matrix <- cov(X)

  # If the covariance matrix is near-singular, use SVD for stability
  if(ncol(X) > 1 && abs(det(cov_matrix)) < .Machine$double.eps) {
    message("Covariance matrix is near-singular.")
  }
  eigen_decomp <- eigen(cov_matrix)
  # Extract the required number of principal components
  transformation_matrix <- eigen_decomp$vectors[, 1:num_components]

  # Project the data onto the principal components
  projected_data <- X %*% transformation_matrix

  return(list(projected_data = projected_data, transformation_matrix = transformation_matrix))
}

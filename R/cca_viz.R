#' @title CCA Visualization
#'
#' @description Creates a 2D (or 3D) visualization of multidimensional labeled data using
#' categorical canonical correlation analysis.
#'
#' @param X A matrix representing the input features (quantitative variables).
#' @param y A vector representing the categories corresponding to the input data.
#' @param center A logical value indicating whether to center the quantitative data.
#' @param dim Integer indicating the desired dimensionality of the visualization: 2 for 2D, 3 for 3D. Default is 2.
#' @param center.scale A logical (boolean) value indicating whether the data should be centered and scaled before processing.
#' @param asp.equal A logical (boolean) value, relevant only for 2D visualization, indicating whether the aspect ratio on both axes should be the same.
#' @param views An integer specifying the number of complementary projections. For 3D visualization, the maximum is 4. Subsequent views are based on orthogonal projections to capture different perspectives of the data.
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
#' X <- iris[,-5]
#' y <- iris[,5]
#' cca_viz(X, y)          # Default 2D visualization
#' cca_viz(X, y, dim = 3) # 3D visualization
#'
#' @export
#' @importFrom plotly plot_ly layout

#' @export
cca_viz <- function(X, y, dim = 2, center.scale = TRUE, asp.equal = TRUE, views = 1) {
  # Validate the 'dim' parameter
  if (!dim %in% c(2, 3)) {
    stop("Parameter 'dim' must be either 2 or 3. Provided value: ", dim)
  }

  y_factor <- as.factor(y)

  # Predefined color palette
  colorway <- my_package_env$palette

  # Adjust the color palette if there are more unique categories than colors
  unique_categories <- unique(y_factor)
  if(length(unique_categories) > length(colorway)) {
    colorway <- rep(colorway, length.out = length(unique_categories))
  }
  colorway <- colorway[1:length(unique_categories)]

  views <- as.integer(views)
  if (!is.integer(views) || views < 1) {
    stop("Parameter 'views' must be a positive integer. Provided value: ", views)
  }
  m <- ncol(X)

  if (dim == 3){
    views = min(views, 4)
  }
  views = min(views, floor(m/dim))

  if (views == 1){
    # Perform canonical correlation calculation
    cca_result <- .cca(X, y, dim, center.scale)
    projected_data <- cca_result$projected_data
    transformation_matrix <- Re(cca_result$transformation_matrix)

    # Prepare data for visualization
    plot_data <- data.frame(Dim1 = Re(projected_data)[, 1], Dim2 = Re(projected_data)[, 2], Category = y_factor)

    x_ax <- list(title = "Dimension 1")
    y_ax <- list(title = "Dimension 2")

    if (asp.equal) {
      # lock y to x with equal scaling
      y_ax$scaleanchor <- "x"
      y_ax$scaleratio <- 1
    }
    # Create 2D or 3D plot based on the desired dimensionality
    if (dim == 2) {
      p <- plot_ly(data = plot_data, x = ~Dim1, y = ~Dim2, color = ~Category, type = 'scatter',
                   mode = 'markers',
                   marker = list(size = 5, opacity = 0.85),
                   colors = colorway[1:length(unique(y_factor))]) %>%
        layout(title = list(text = '2D Projection Using CCA', y = 0.98),
               xaxis = x_ax, yaxis = y_ax)
    } else if (dim == 3) {
      # Ensure that we have at least 3 components for 3D visualization
      if (ncol(projected_data) < 3) {
        stop("Insufficient number of components for 3D visualization. Please ensure that X has at least 3 columns.")
      }
      plot_data$Dim3 <- Re(projected_data)[, 3]
      p <- plot_ly(data = plot_data, x = ~Dim1, y = ~Dim2, z = ~Dim3, color = ~Category,
                   type = 'scatter3d', mode = 'markers',
                   marker = list(size = 4),
                   colors = colorway[1:length(unique(y))]) %>%
        layout(title = list(text = '3D Projection Using CCA', y=0.98),
               scene = list(xaxis = list(title = 'Dimension 1'),
                            yaxis = list(title = 'Dimension 2'),
                            zaxis = list(title = 'Dimension 3')))
    } else {
      stop("Parameter 'dim' must be either 2 or 3. Provided value: ", dim)
    }

    # Apply the global Plotly theme and display the plot
    print(.plotly_theme(p))
  } else {
    iv <- .independent_views(X, y, dim, method = 'cca', center.scale, asp.equal, views)
    projected_data <- iv$projected_data
    transformation_matrix <- iv$transformation_matrices
  }
  return(invisible(list(projected_data = projected_data, transformation_matrix = transformation_matrix)))
}

#' @noRd
.cca <- function(X, y, dim=2, center.scale = TRUE) {
  # Ensure input data are matrices and vectors
  X <- as.matrix(X)  # Quantitative variables
  y <- as.factor(y)    # Categorical variable

  # Standardizing the data (mean = 0, standard deviation = 1)
  if(center.scale == TRUE){
    # Standardizing the data (mean = 0, standard deviation = 1)
    X <- .center.and.scale(X)
  }

  k <- length(unique(y))  # Number of categories

  # Create matrix for simplex representation
  W <- eigen(2 * diag(k) - 1)$vectors
  V <- W[, 1:(k - 1)]

  Z <- matrix(0, nrow = length(y), ncol = k - 1)

  # Assign vectors to each category
  cat.index <- 1
  for (i in unique(y)) {
    # Find the indices of all instances of the current category
    indices <- which(y == i)
    for (j in indices) {
      Z[j, ] <- V[cat.index, ]
    }
    cat.index <- cat.index + 1
  }

  # Covariance matrices
  S11 <- cov(X)
  S22 <- cov(Z)
  S12 <- cov(X, Z)
  S21 <- t(S12)

  # Eigen decomposition for dimensionality reduction
  H1 <- solve(S11) %*% S12 %*% solve(S22) %*% S21
  eigen_values <- eigen(H1)$values
  eigen_vectors <- eigen(H1)$vectors

  # Select the top 'dim' eigenvectors based on the eigenvalues
  transformation_matrix <- eigen_vectors[, order(eigen_values, decreasing = TRUE)[1:dim]]
  # Find orthogonal basis in reduced space
  orthogonalization <- .orth(X, transformation_matrix, dim)
  # Projected data in lower dimension
  projected_data <- orthogonalization$projected_data
  transformation_matrix <- orthogonalization$Q1

  return(invisible(list(projected_data = projected_data, transformation_matrix = transformation_matrix)))
}

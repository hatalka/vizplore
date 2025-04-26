#' @title Neural Network Dimensionality Reduction Visualization
#'
#' @description Creates a 2D (or 3D) visualization of multidimensional labeled data using a simple
#' neural network.
#'
#' @param X A numeric matrix (n x m) representing the input features, where n is the number of samples and m is the number of features.
#' @param y A vector of length n representing the categories or labels corresponding to the input data.
#' @param dim Integer indicating the desired dimensionality of the visualization: 2 for 2D, 3 for 3D. Default is 2.
#' @param center.scale A logical (boolean) value indicating whether the data should be centered and scaled before processing.
#' @param asp.equal A logical (boolean) value, relevant only for 2D visualization, indicating whether the aspect ratio on both axes should be the same.
#' @param views An integer specifying the number of independent views. For 3D visualization, the maximum is 4. Subsequent views are based on orthogonal projections to capture different perspectives of the data.
#'
#' @return A list containing:
#' \describe{
#'   \item{projected_data}{The data projected onto the lower-dimensional space using the neural network.}
#'   \item{transformation_matrix}{The weight matrix of the neural network's first layer used for dimensionality reduction.}
#' }
#' Additionally, a Plotly plot representing the data points in the reduced feature space,
#' with points colored by their categories is displayed.
#'
#' @examples
#' data(iris)
#' X <- iris[,-5]
#' y <- iris[,5]
#' nn_viz(X, y)          # Default 2D visualization
#' nn_viz(X, y, dim = 3) # 3D visualization
#'
#' @export
#' @import keras3
#' @import plotly
#' @importFrom reshape2 melt

#' @export
nn_viz <- function(X, y, dim = 2, center.scale = TRUE, asp.equal = TRUE, views = 1) {
  # Validate the 'dim' parameter
  if (!dim %in% c(2, 3)) {
    stop("Parameter 'dim' must be either 2 or 3. Provided value: ", dim)
  }

  X <- as.matrix(X)
  # Check if the number of columns is sufficient for the requested components
  if(ncol(X) <= dim) {
    stop("The number of columns in X is not sufficient for the requested dimension.")
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
    # Call the internal neural network function to perform dimensionality reduction
    nn_result <- .nn(X, y, dim, center.scale)
    projected_data <- nn_result$projected_data

    x_ax <- list(title = "Dimension 1")
    y_ax <- list(title = "Dimension 2")

    if (asp.equal) {
      # lock y to x with equal scaling
      y_ax$scaleanchor <- "x"
      y_ax$scaleratio <- 1
    }

    # Prepare data for visualization based on the desired dimension
    if (dim == 2) {
      plot_data <- data.frame(
        Dim1 = projected_data[, 1],
        Dim2 = projected_data[, 2],
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
          title = list(text = "2D Projection Using Neural Network", y = 0.98),
          xaxis = x_ax,
          yaxis = y_ax
        )

    } else if (dim == 3) {
      if (ncol(projected_data) < 3) {
        stop("Insufficient number of dimensions in reduced data for 3D visualization.")
      }

      plot_data <- data.frame(
        Dim1 = projected_data[, 1],
        Dim2 = projected_data[, 2],
        Dim3 = projected_data[, 3],
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
          title = list(text = "3D Projection Using Neural Network", y = 0.98),
          scene = list(
            xaxis = list(title = "Dimension 1"),
            yaxis = list(title = "Dimension 2"),
            zaxis = list(title = "Dimension 3")
          )
        )
    }

    # Apply the global Plotly theme and display the plot
    print(.plotly_theme(p))
  } else {
    iv <- .independent_views(X,y, dim, method = 'nn', center.scale, asp.equal, views)
    projected_data <- iv$projected_data
    transformation_matrix <- iv$ transformation_matrices
  }

  return(invisible(list(projected_data = projected_data,
              transformation_matrix = transformation_matrix)))
}

#' @noRd
.nn <- function(X, y, dim = 2, center.scale = TRUE) {
  # Ensure the input data are in matrix format
  X <- as.matrix(X)
  # Standardizing the data (mean = 0, standard deviation = 1)
  if(center.scale == TRUE){
    # Standardizing the data (mean = 0, standard deviation = 1)
    X <- .center.and.scale(X)
  }

  # Check if the number of columns is sufficient for the requested components
  if(ncol(X) <= dim) {
    stop("The number of columns in X is less than the requested dimension.")
  }

  # Convert labels to factor and then to a numeric (zero-indexed) format for categorical conversion
  y_factor <- as.factor(y)
  y_numeric <- as.numeric(y_factor) - 1
  # One-hot encoded matrix
  y_categorical <- to_categorical(y_numeric)

  # Suppress TensorFlow oneDNN warnings
  options(warn = -1)

  # Simple neural network model with a linear dimensionality reduction layer
  inputs <- layer_input(shape = c(ncol(X)))
  outputs <- inputs %>%
    layer_dense(units = dim, activation = 'linear', name = 'dimensionality_reduction') %>%
    layer_dense(units = length(unique(y_factor)), activation = 'softmax', name = 'output_layer')
  model <- keras_model(inputs = inputs, outputs = outputs)

  # Compile the model
  model %>% compile(
    optimizer = optimizer_adam(),
    loss = 'categorical_crossentropy'
  )

  # Model training
  model %>% fit(
    X, y_categorical,
    epochs = 10,
    batch_size = 32,
    validation_split = 0,
    verbose = 0
  )

  # Extract the weight matrix from the first layer
  transformation_matrix <- get_weights(model$get_layer('dimensionality_reduction'))[[1]]
  # Find orthogonal basis in reduced space
  orthogonalization <- .orth(X, transformation_matrix, dim)
  # Projected data in lower dimension
  projected_data <- orthogonalization$projected_data
  transformation_matrix <- orthogonalization$Q1

  return(invisible(list(projected_data = projected_data,
                        transformation_matrix = transformation_matrix)))
}

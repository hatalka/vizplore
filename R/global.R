#' @noRd
#' @title Global package environment
#' @description Creates an environment for storing global variables used across the package
my_package_env <- new.env()

#' @noRd
#' @title Global Color Palette
#' @description Defines a global color palette for consistent usage across visualizations
my_package_env$palette <- c(
  '#AB63FA', '#FFA15A', '#19D3F3', '#FF6692', '#B6E880', '#FF97FF', '#FECB52', '#636EFA', '#EF553B', '#00CC96',
  '#D62728', '#9467BD', '#8C564B', '#E377C2', '#7F7F7F', '#BCBD22', '#17BECF', '#1F77B4', '#FFBB78', '#98DF8A'
)

#' @noRd
#' @title Orthonormal projection
#' @description Computes the orthonormal projection of the data into a subspace and its orthogonal complement.
#' @param data Matrix or data frame of data to be projected.
#' @param matrix Matrix whose columns span the subspace and orthogonal complement.
#' @param dim Integer, the dimensionality of the subspace for projection.
#' @return A list containing:
#'   - `projected_data`: The data projected into the subspace.
#'   - `orth_data`: The data projected into the orthogonal complement.
#'   - `Q1`: The orthonormal basis for the subspace.
#'   - `Q2`: The orthonormal basis for the orthogonal complement.
.orth <- function(data, matrix, dim) {
  P <- matrix
  Q <- qr.Q(qr(P), complete = TRUE)  # Extract orthonormal basis for P's column space
  Q1 <- Q[, 1:dim]    # Basis for the subspace spanned by first "dim" columns
  Q2 <- Q[, (dim+1):nrow(P)] # Basis for the orthogonal complement (m x (m-2))

  projected_data <- data %*% Q1
  # Project data into the orthogonal complement
  orth_data <- data %*% Q2

  return(invisible(list(projected_data = projected_data, orth_data=orth_data, Q1=Q1, Q2=Q2)))
}


#' @noRd
#' @title Centering and Scaling of Data
#' @description Standardizes the input data by centering to mean 0 and scaling to unit variance.
#' @param X Numeric matrix or data frame to be standardized.
#' @return The standardized matrix or data frame with columns scaled to unit variance.
.center.and.scale <- function(X){
  # Standardizing the data (mean = 0, standard deviation = 1)
  X <- scale(X, center = TRUE, scale=FALSE)
  # Calculate the standard deviation of each column
  column_sds <- apply(X, 2, sd)
  # Normalize only the columns with non-zero standard deviation
  for (i in 1:ncol(X)) {
    if (column_sds[i] != 0) {
      X[, i] <- X[, i] / column_sds[i]
    }
  }
  return(X)
}

#' @noRd
#' @title Plotly Theme
#' @description Applies a consistent theme to Plotly visualizations, including background colors and axis settings.
#' @param plot A Plotly plot object to which the theme will be applied.
#' @return A Plotly plot with the applied theme.
#' @import plotly
.plotly_theme <- function(plot) {
  plot %>%
    layout(
      plot_bgcolor = 'rgb(240, 240, 240)',
      paper_bgcolor = 'rgb(255, 255, 255)',
      xaxis = list(gridcolor = 'white', zerolinecolor = 'white', showline = FALSE, showgrid = TRUE),
      yaxis = list(gridcolor = 'white', zerolinecolor = 'white', showline = FALSE, showgrid = TRUE),
      scene = list(
        xaxis = list(backgroundcolor = 'rgb(230, 230, 230)', gridcolor = 'white', zerolinecolor = 'white', showline = FALSE, showgrid = TRUE, showbackground = TRUE),
        yaxis = list(backgroundcolor = 'rgb(230, 230, 230)', gridcolor = 'white', zerolinecolor = 'white', showline = FALSE, showgrid = TRUE, showbackground = TRUE),
        zaxis = list(backgroundcolor = 'rgb(230, 230, 230)', gridcolor = 'white', zerolinecolor = 'white', showline = FALSE, showgrid = TRUE, showbackground = TRUE)
      )
    )
}

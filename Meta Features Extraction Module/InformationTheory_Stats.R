# Required libraries
library(infotheo)

# Generic function handler for the 'infotheo' methods
infotheo <- function(...) {
  UseMethod("infotheo")
}

# Default method for handling non-formula data
infotheo.default <- function(x, y, features="all", summary=c("mean", "sd"),
                             transform=TRUE, ...) {
  # Ensure x is a data frame
  if(!is.data.frame(x)) {
    stop("Data argument must be a data.frame.")
  }
  
  # If y is a data frame, use the first column
  if(is.data.frame(y)) {
    y <- y[, 1]
  }
  y <- as.factor(y)
  
  # Ensure the minority class has at least two examples
  if(min(table(y)) < 2) {
    stop("Number of examples in the minority class should be >= 2.")
  }
  
  # Ensure x and y have the same number of rows
  if(nrow(x) != length(y)) {
    stop("X and Y must have the same number of rows.")
  }
  
  # Feature selection control
  if(features[1] == "all") {
    features <- ls.infotheo()
  }
  features <- match.arg(features, ls.infotheo(), several.ok = TRUE)
  
  # Standardize column names to valid variable names
  colnames(x) <- make.names(colnames(x))
  
  # Set default summary if none provided
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }
  
  # Discretization of numeric variables
  if (transform) {
    x.dis <- categorize(x)
  } else {
    x.dis <- x[, !sapply(x, is.numeric), drop=FALSE]
    if (length(x.dis) == 0) {
      return(
        sapply(features, function(f) {
          post.processing(NA, summary, f %in% ls.infotheo.multiples(), ...)
        }, simplify=FALSE)
      )
    }
  }
  
  # Remove constant attributes from consideration
  x.dis <- x.dis[, sapply(x.dis, nlevels) > 1, drop=FALSE]
  
  # Additional computations such as entropy, mutual information
  extra <- list(
    y.entropy = entropy(y),
    y.log = log2(nlevels(y)),
    x.entropy = sapply(x.dis, entropy),
    x.log = sapply(sapply(x.dis, nlevels), log2),
    mutinf = sapply(x.dis, mutinf, y=y)
  )
  
  # Apply feature-specific functions
  sapply(features, function(f) {
    fn <- get(paste0("m", f))
    measure <- do.call(fn, c(list(x=x.dis, y=y, extra=extra), list(...)))
    post.processing(measure, summary, f %in% ls.infotheo.multiples(), ...)
  }, simplify=FALSE)
}

# Method for handling formula data
infotheo.formula <- function(formula, data, features="all",
                             summary=c("mean", "sd"), 
                             transform=TRUE, ...) {
  # Ensure input is a formula
  if(!inherits(formula, "formula")) {
    stop("Method is only for formula data.")
  }
  
  # Ensure data is a data frame
  if(!is.data.frame(data)) {
    stop("Data argument must be a data.frame.")
  }
  
  # Convert formula to model frame
  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL
  
  # Apply default method on model frame
  infotheo.default(modFrame[, -1], modFrame[, 1], features, summary, 
                   transform, ...)
}

# List available information theoretical meta-features
ls.infotheo <- function() {
  c("attrConc", "attrEnt", "classConc", "classEnt", "eqNumAttr", "jointEnt", 
    "mutInf", "nsRatio")
}

# List features that can return multiple values
ls.infotheo.multiples <- function() {
  c("attrConc", "attrEnt", "classConc", "jointEnt", "mutInf")
}

# Define feature calculation functions
m.attrConc <- function(x, ...) {
  if (ncol(x) == 1) return(NA)
  comb <- expand.grid(i=seq(ncol(x)), j=seq(ncol(x)))
  comb <- comb[comb$i != comb$j, ]
  
  mapply(function(i, j) {
    concentration.coefficient(x[, i], x[, j])
  }, i=comb$i, j=comb$j)
}

m.attrEnt <- function(extra, ...) {
  extra$x.entropy
}

m.classConc <- function(x, y, ...) {
  apply(x, 2, concentration.coefficient, y)
}

m.classEnt <- function(extra, ...) {
  extra$y.entropy
}

m.eqNumAttr <- function(extra, ...) {
  extra$y.entropy / mean(extra$mutinf)
}

m.jointEnt <- function(x, y, ...) {
  joint.data <- sapply(as.data.frame(sapply(x, paste, y)), as.factor)
  sapply(as.data.frame(joint.data), entropy)
}

m.mutInf <- function(extra, ...) {
  extra$mutinf
}

m.nsRatio <- function(extra, ...) {
  mean(extra$mutinf)
}

# Mutual information calculation
mutinf <- function(x, y) {
  entropy(x) + entropy(y) - entropy(paste(x, y))
}

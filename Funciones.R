media_estimada <- function(tiempo, estado, variable = NULL){
  
  # Si no se especifica una variable, calcular la media para todos los datos
  if (is.null(variable)) {
    
    a <- survfit(Surv(tiempo, estado) ~ 1)
    media <- 1 * (a$time[1] - 0)
    
    for (i in 2:length(a$time)) {
      media <- media + a$surv[i-1] * (a$time[i] - a$time[i-1])
    }
    
    paste("Media: ", round(media, 2))
    
    # Si se especifica una variable, calcular la media para cada grupo
  } else {
    
    # Separación en grupos
    grupos <- split(seq_along(tiempo), variable)
    resultados <- numeric(length(grupos))
    
    for (i in seq_along(grupos)) {
      
      grupo <- grupos[[i]]
      a <- survfit(Surv(tiempo[grupo], estado[grupo]) ~ 1)
      media <- 1 * (a$time[1] - 0)
      
      for (j in 2:length(a$time)) {
        media <- media + a$surv[j-1] * (a$time[j] - a$time[j-1])
      }
      
      resultados[i] <- media
    }
    
    # Formateo de los resultados
    output <- cbind(
      names(grupos),
      round(resultados, 3)
    )
    
    colnames(output) <- c(
      gsub(".+\\$", "", deparse(substitute(variable))),
      "Media truncada"
    )
    
    output
  }
}



is.formula <- function (x) inherits(x, "formula")

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

product_names <- function() {
  function(x) {
    #cat(" in product_breaks\n")
    #browser()
    unique(x)
  }
}

product_breaks <- function() {
  function(x) {
    #cat(" in product_breaks\n")
    #browser()
    unique(x)
  }
}

product_labels <- function() {
  function(x) {
    #cat(" in product_labels\n")
    #browser()
    
    unique(x)
  }
}

is.waive <- getFromNamespace("is.waive", "ggplot2")




## copied from ggplot2
with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}

set_sec_axis <- function(sec.axis, scale) {
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) abort("Secondary axes must be specified using 'sec_axis()'")
    scale$secondary.axis <- sec.axis
  }
  return(scale)
}





scale_y_productlist_yo = function (name = ggplot2::waiver(), breaks = product_breaks(), 
                                   minor_breaks = NULL, labels = product_labels(), limits = NULL, 
                                   expand = ggplot2::waiver(), oob = scales:::censor, na.value = NA_real_, 
                                   trans = "identity", position = "left", sec.axis = ggplot2::waiver()) 
{
  sc <- ggplot2::continuous_scale(c("y", "ymin", "ymax", "yend", 
                                    "yintercept", "ymin_final", "ymax_final", "ylower", "ymiddle", 
                                    "yupper"), "position_c", identity, name = name, breaks = breaks, 
                                  minor_breaks = minor_breaks, labels = labels, limits = limits, 
                                  expand = expand, oob = oob, na.value = na.value, trans = trans, 
                                  guide = ggplot2::waiver(), position = position, super = ScaleContinuousProduct)

    sc$secondary.axis <- sec.axis

  sc
}
  
  
  
  
  
  
  
  
  

scale_x_productlist_yo = function (name = ggplot2::waiver(), breaks = product_breaks(), 
          minor_breaks = NULL, labels = product_labels(), limits = NULL, 
          expand = ggplot2::waiver(), oob = scales:::censor, na.value = NA_real_, 
          transform = "identity", position = "bottom", sec.axis = ggplot2::waiver()) 
{
  sc <- ggplot2::continuous_scale(c("x", "xmin", "xmax", "xend", 
                                    "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", 
                                    "xupper"), "position_c", identity, name = name, breaks = breaks, 
                                  minor_breaks = minor_breaks, labels = labels, limits = limits, 
                                  expand = expand, oob = oob, na.value = na.value, transform = transform, 
                                  guide = ggplot2::waiver(), position = position, super = ScaleContinuousProduct)
  sc$secondary.axis <- sec.axis
  sc
}






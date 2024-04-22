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
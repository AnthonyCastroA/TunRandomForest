#' @title Tunning para escoger los hiperparámetros óptimos (mtry, ntree y nodezise) de un modelo Random Forest

#' @description Escoge los hiperparámetros óptimos para un modelo random forest.

#' @param df_base      #data frame con los predictores y variable respuesta
#' @param y            # nombre de la variable respuesta
#' @param ntree        # número de árboles creados en el modelo randomForest
#' @param mtry
#' @param mtry_optimo
#' @param mtry_optimo.2
#' @param size
#' @param nodesize
#' @param nodesize_optimo
#' @param n_arbol_optimo
#' @param size


#'
#' @return
#'
# '@examples
#'
#' @export

TunRandomForest<- function (df_base, y, mtry_optimo.2, nodesize_optimo, n_arbol_optimo, importance= T)
{

  ## Tuneamos el mtry
  rf_mtry <- function(df_base, y= "Respuesta", ntree = 500){

    predictores_max <- ncol(df_base) - 1
    predictores   <- rep(NA, predictores_max)
    oob_mse         <- rep(NA, predictores_max)
    for (i in 1:predictores_max) {
      set.seed(666)
      ecuacion <- formula(paste(y,"~ ."))
      modelo.rf <- randomForest(formula = ecuacion, data = df_base, mtry = i, ntree = ntree)
      predictores[i] <- i
      oob_mse[i] <- tail(modelo.rf$mse, n = 1)
    }
    results <- data_frame(predictores, oob_mse)
    return(results)
  }


  set.seed(666)

  mtry_hiperparametro <-  rf_mtry(df_base = Dbase.01, y = "Respuesta")
  mtry_optimo<- which.min(mtry_hiperparametro$oob_mse)

  # Para saber el hiperparámetro mtry que se escogió
  ggplot(data = mtry_hiperparametro, aes(x = "", y = oob_mse)) +
    scale_x_continuous(breaks = mtry_hiperparametro$predictores) +
    geom_line() +
    geom_point() +
    geom_point(data = mtry_hiperparametro %>% arrange(oob_mse) %>% head(1),
               color = "red") +
    labs(title = "Evolución del out-of-bag-error vs mtry",
         x = "N° de predictores empleados") +
    theme_bw()


  ## Tuneamos nodesize
  rf_nodesize <- function(df_base, y= "Respuesta", size = NULL, ntree = 500){
    if (is.null(size)){
      size <- seq(from = 1, to = nrow(df_base), by = 2)
    }
    oob_mse <- rep(NA, length(size))
    for (i in seq_along(size)) {
      set.seed(123)
      ecuacion <- formula(paste(y,"~ ."))
      modelo.rf <- randomForest(formula = ecuacion, data = df, mtry = mtry_optimo, ntree = ntree,
                                nodesize = i)
      oob_mse[i] <- tail(modelo.rf$mse, n = 1)
    }
    results <- data_frame(size, oob_mse)
    return(results)
  }

  set.seed(666)

  nodesize_hiperparametro <-  rf_nodesize(df = Dbase.01, y = "Respuesta",
                                          size = c(5:I(nrow(dbase.01)/3)))
  nodesize_optimo<- which.min(nodesize_hiperparametro$oob_mse)

  ggplot(data = nodesize_hiperparametro, aes(x = size, y = oob_mse)) +
    scale_x_continuous(breaks = nodesize_hiperparametro$size) +
    geom_line() +
    geom_point() +
    geom_point(data = nodesize_hiperparametro %>% arrange(oob_mse) %>% head(1),
               color = "red") +
    labs(title = "Evolución del out-of-bag-error vs nodesize",
         x = "N° de observaciones en nodos terminales") +
    theme_bw()

  # Veamos el número de árboles óptimo
  modelo.rf<- randomForest( Respuesta ~ .
                            , mtry = mtry_optimo
                            , ntree = 500
                            , nodesize = nodesize_optimo
                            , data = Dbase.01
  )

  oob_mse <- data.frame(oob_mse = modelo.rf$mse,
                        arboles = seq_along(modelo.rf$mse))

  n_arbol_optimo<- which.min(oob_$oob_mse)
  ggplot(data = oob_mse, aes(x = arboles, y = oob_mse )) +
    geom_line() +
    labs(title = "Evolución del out-of-bag-error vs número árboles",
         x = "N° árboles") +
    theme_bw()

  # Tuneo final del mtry

  oob.err=double(ncol(Dbase.01)-1)
  test.err=double(ncol(Dbase.01)-1)

  for(mtry in 1:(ncol(Dbase.01)-1)) {
    modelo.rf=randomForest
    ( Respuesta ~ .
      , mtry = mtry
      , ntree = n_arbol_optimo
      , nodesize = nodesize_optimo
      , data = Dbase.01
    )
    oob.err[mtry] = rf$mse[n_arbol_optimo]

    Respuesta.Pred<-predict(modelo.rf,Dbase.02)
    test.err[mtry]= with(Dbase.02, mean( (Respuesta- Respuesta.Pred)^2)) #Mean Squared Test Error

    cat(mtry," ") #printing the output to the console
  }

  errores<- data.frame(oob.err, test.err)

  mtry_optimo.2 <- which.min(errores$oob.err)

  modelo.rf.2<- randomForest( Respuesta ~ .
                              , mtry = mtry_optimo.2
                              , ntree = n_arbol_optimo
                              , nodesize = nodesize_optimo
                              , data = Dbase.01
                              , importance= T
  )

  importancia_pred <- as.data.frame(modelo.rf.2$importance, scale = TRUE)
  importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
  p1 <- ggplot(data = importancia_pred, aes(x = reorder(variable, `%IncMSE`),
                                            y = `%IncMSE`,
                                            fill = `%IncMSE`)) +
    labs(x = "variable", title = "Reducción de MSE") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")

  p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, IncNodePurity),
                                            y = IncNodePurity,
                                            fill = IncNodePurity)) +
    labs(x = "variable", title = "Reducción de pureza") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
  ggarrange(p1, p2)

}






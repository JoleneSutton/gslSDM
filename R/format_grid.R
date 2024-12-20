#' Format prediction grid using information from a model and the data to which the model was fitted to
#'
#' Adds parameters to a prediction grid if they are part of a model's formula.
#' Added parameters include year (number), fyear (factor), month (number), month (factor), depth_scaled (number), and cfvn (factor).
#' For year and month, the grid is repeated length(unique(x))
#' @param grid A data frame with columns for spatial coordinates and depth. Each pair of coordinates only occurs once.
#' @param model A saved model
#' @param dat.model The data that the model was fitted to
#' @importFrom stringr str_pad
#' @importFrom stats sd
#' @return Returns a data frame that can be used to make model predictions, e.g., predict(model, newdata=format_grid(grid,model,dat.model))
#' @export
format_grid<-function(grid,model,dat.model){

  # add year
  if(length(grep('year',model$formula))>0||length(grep('year',model$time))>0){
    var<-rep(unique(dat.model$year),each=nrow(grid))
    grid<-grid[rep(seq_len(nrow(grid)), length(unique(dat.model$year))), ]
    grid<-cbind(grid,var)
    names(grid)[ncol(grid)]<-'fyear'
    grid$fyear<-as.character(grid$fyear)
    grid$year<-as.numeric(as.character(grid$fyear))
  }

  # add fmonth
  if(length(grep('month',model$formula))>0||length(grep('month',model$time))>0){
    #grid$month<-match(grid$Month, month.abb)
    #grid$fmonth<-as.character(as.numeric(grid$month))
    var<-rep(unique(dat.model$fmonth),each=nrow(grid))
    grid<-grid[rep(seq_len(nrow(grid)), length(unique(dat.model$fmonth))), ]
    grid<-cbind(grid,var)
    names(grid)[ncol(grid)]<-'fmonth'
    grid$month<-as.numeric(as.character(grid$fmonth))

    if(length(grep("^0",levels(dat.model$fmonth)))>0){#need to pad with zero?
      grid$fmonth<-stringr::str_pad(grid$month, 2, pad = "0")
    }
    grid$fmonth<-factor(grid$fmonth,levels=levels(dat.model$fmonth))
  }

  # add depth_scaled
  grid$depth_scaled <- (grid$depth - mean(dat.model$depth)) / stats::sd(dat.model$depth)


  # add cfvn
  if(length(grep('cfvn',model$formula))>0){
    vessel<-names(sort(table(dat.model$cfvn),decreasing=T)[1])
    grid$cfvn<-factor(vessel)
  }
  return(grid)
  }

#' Generate and plot randomized quantile residuals from sdmTMB models
#' 
#' Combines three plots, 1) residuals plotted by X and Y coordinates, 2) histogram of residuals, and 3) a normalized Q-Q plot.
#' Results are saved to the working directory. CAUTION: See sdmTMB documentation about limitations of using randomized quantile residuals!!
#' @param model An sdmTMB model. Assumes model was built from a data set in which spatial coordinates were named 'X' and 'Y'
#' @param structure Whether the model as a one-part ("one") structure or a two-part ("hurdle") structure. TARGET FOR FUTURE UPDATES
#' @import ggplot2
#' @import grid
#' @importFrom cowplot plot_grid
#' @importFrom stats predict residuals

#' @return Returns a ggplot object or list of ggplot objects
#' @keywords sdmTMB
#' @export
plot_diagnostics_sdmTMB<-function(model,structure){
  writeLines('\nCAUTION: See sdmTMB documentation for limitations of randomized quantile residuals!!\n')
  
  if(is.null(structure)){
    structure='one'
  }
  
  MOD<-model
  
  writeLines("Generating model predictions")
  
  pred<-stats::predict(MOD,type="response")
  
  # for non-hurdles:
  if(structure=='one'){
    
    pred$res<-stats::residuals(MOD) #randomized quantile residuals
    p1<-ggplot(data=pred, aes(X, Y, col = res)) + 
      scale_colour_gradient2() +
      geom_point() +ggtitle('Residuals Spatial')
    p2<-ggplot(data=pred,aes(res))+geom_histogram(binwidth = diff(range(pred$res))/30)+theme_bw()+ggtitle('Residuals Histogram')
    p3<-ggplot(data=pred, aes(sample = res))+ stat_qq() + stat_qq_line()+ggtitle('Normal Q-Q')
    

    plots<-plot_grid(p2,p3,nrow=2)
    p5<-plot_grid(p1,plots)
    
    return(p5)
  }
  
  # for hurdles:
  if(structure=='hurdle'){

    pred$res.mod1<-stats::residuals(MOD,model=1)
    suppressWarnings(pred$res.mod2<-stats::residuals(MOD,model=2))
    
    #binomial
    p1a<-ggplot(data=pred, aes(X, Y, col = res.mod1)) + 
      scale_colour_gradient2(name='res') +
      geom_point() +ggtitle('Residuals Spatial')
    p2a<-ggplot(data=pred,aes(res.mod1))+geom_histogram(binwidth = diff(range(pred$res.mod1))/30)+theme_bw()+ggtitle('Residuals Histogram')
    p3a<-ggplot(data=pred, aes(sample = res.mod1))+ stat_qq() + stat_qq_line()+ggtitle('Normal Q-Q')
    
    plotsa<-cowplot::plot_grid(p2a,p3a,nrow=2)
    p5a<-cowplot::plot_grid(p1a,plotsa)
    
    # pos. catch
    p1b<-ggplot(data=pred, aes(X, Y, col = res.mod2)) + 
      scale_colour_gradient2(name='res') +
      geom_point() +ggtitle('Residuals Spatial')
    p2b<-ggplot(data=pred,aes(res.mod2))+geom_histogram(binwidth = diff(range(pred$res.mod2,finite=1))/30)+theme_bw()+ggtitle('Residuals Histogram')
    p3b<-ggplot(data=pred, aes(sample = res.mod2))+ stat_qq() + stat_qq_line()+ggtitle('Normal Q-Q')
    
    suppressWarnings(plotsb<-plot_grid(p2b,p3b,nrow=2))
    p5b<-plot_grid(p1b,plotsb)

    return(list(p5a,p5b))
  }
  
  
}

######
# F7. Heatmapfunciton()
######

## ---------------------------------------------------------------------------------------------------------------------------------
# Draw heatmap of tables with proportion of students who obtained correct recommendations (File #4b)
heatmapfunction <- function(x, xlab, ylab, min, marg = FALSE){
  as.matrix(x)
  if(!marg){
    x <- x[-9,-9]
  }
  x <- melt(x)
  x$value <- as.numeric(as.character(x$value))
  
  return(
    ggplot(x, aes(Var2, Var1))+
      geom_tile(aes(fill=value))+
      geom_text(aes(fill = x$value, label = round(x$value,2)))+
      xlab(xlab)+
      ylab(ylab)+
      labs(fill = "proportion of students")+
      theme_classic()+
      scale_fill_gradientn(limits = c(min, 1), colours = c("#132B43", "#56B1F7"))
  )
}

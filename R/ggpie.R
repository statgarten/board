#' @export
#' @import ggplot2
#' @importFrom scales percent
#'
#'
ggpie <- function(i){
  i <- as.data.frame(table(i))
  i[,1] <- as.character(i[,1])
  colnames(i) <- c('variable', 'value')

  if(nrow(i) > 6){
    i <- i[order(i$value,decreasing = TRUE), ]


    i[7,] <- c('Etc', sum(i[7:nrow(i), 2]))
    i <- i[1:7,]
  }
  i[,1] <- as.factor(i[,1])
  i[,2] <- as.numeric(i[,2])
  i[,2] <- round( 100*i[,2]/sum(i[,2]) ) # percentage

  g <-
    ggplot(i, aes(x="", y = value, fill = variable)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme(
      axis.text.x=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold"),
      legend.position = "bottom"
    ) +
    geom_text(
      aes(
        label = scales::percent(value/100)
      ),
      size = 5,
      position = position_stack(vjust = 0.5)
    ) +
    ggtitle('most distribution')
  return(g)
}

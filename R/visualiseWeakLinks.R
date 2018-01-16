visualiseWeakLinks <- function(paramList){
  weakLinks <- paramList$weakLinks
  data <- paramList$source

  for (i in 1:length(weakLinks)){

    z <- data[[weakLinks[[i]]]]
    lineChart <- ggplot(z$x)+
        geom_line(aes(y = z$x, x = 1:length(z$x)))+
        xlab("Time")+
        ylab(z$description)+
        ggtitle(z$description, subtitle = paste(z$period, z$type, i, sep = " : "))+
        theme_light()

    plotName <- paste("Object:", weakLinks[[i]], z$description, "line chart", sep = " ")
    pathName <- paste("outputs/", plotName, ".jpeg", sep = "")
    ggsave(pathName, lineChart)


    acf <- ggAcf(z$x)+
        ggtitle(z$description, subtitle = paste(z$period, z$type, i, sep = " : "))+
        theme_light()
    plotName <- paste("Object:", weakLinks[[i]], z$description, "ACF", sep = " ")
    pathName <- paste("outputs/", plotName, ".jpeg", sep = "")
    ggsave(pathName, acf)


    pacf <- ggPacf(z$x) +
        ggtitle(z$description, subtitle = paste(z$period, z$type, i, sep = " : "))+
        theme_light()
    plotName <- paste("Object:", weakLinks[[i]], z$description, "PACF", sep = " ")
    pathName <- paste("outputs/", plotName, ".jpeg", sep = "")
    ggsave(pathName, pacf)


  }



}

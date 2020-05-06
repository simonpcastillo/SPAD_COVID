#' Plot global dynamic and beta distribution estimates relatioships
#' @author Simon P Castillo \email{spcastil@@uc.cl}.
#' @description This function plots the relations between global dynamic indicators (#SARS-CoV-2(+) and #Countries) and beta distribution estimates relatioships $\hat{\alpha}$ and $\hat{\beta}$
#' @param CPAD: dataframe. The dataframe returned by \code{\link{analyse_CPAD}}.
#' @param saveplots: \code{TRUE} or \code{FALSE}. Save the plots in your \code{wd}.Default \code{TRUE}.
#' @param saveplots.ext: character. The extension for the saved figures admitted by \code{\link[ggplot2]{ggsave}} (e.g., \code{".png"}, \code{".svg"}).Default \code{".png"}.
#' @return This function return to your global environment an object list named \code{plotrelations} with the plot(s) created. Also, if \code{saveplot} is \code{TRUE}, a folder named \code{betaplots} in the folder \code{plots} is created in your \code{wd}.
#' @examples plot_relations(CPAD, saveplot=TRUE, saveplot.ext= ".png")
#'
#'
#'
plot_relations <- function(CPAD, saveplot=TRUE, saveplot.ext= ".png"){
  pacman::p_load(ggplot2,lubridate, viridis, rlist, patchwork)
  plotrelations <- list()
  df4 = CPAD
  breaksdate = seq(range(CPAD$day)[1],range(CPAD$day)[2], length.out = 5) #c(min(df2b$date),as.Date("2020-02-28"),as.Date("2020-02-29"),as.Date("2020-04-14"),max(df2b$date))#

   FF = ggplot(df4, aes((alfa), (NcasesPos),colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.3)+
    labs(x= expression(paste("",alpha, "")), y= expression("#SARS-CoV-2(+)"), title = "A. ") +
    theme_minimal() +
    #scale_y_continuous(limits = c(-10000,3000000), expand  = c(-10000, 3000000))+
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")
  G = ggplot(df4, aes((beta),countries,colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.3)+
    labs(x= expression(paste("",beta, "")), y= expression("#Countries"), title= "D. ") +
    theme_minimal() +
    #scale_y_continuous(limits = c(-0.1,170), expand  = c(-0.1, NA))+
    #guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")



  FFinset = ggplot(df4, aes((alfa), NcasesPos,colour=as.numeric(day))) +
    geom_path(size=1, colour="gray60")+
    labs(x= expression(paste(alpha)), y= expression(paste(""))) +
    theme_minimal() +
    theme(rect = element_rect(fill = "transparent"),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))


  FF2 = FF
  #+ annotation_custom(grob=ggplotGrob(FFinset),ymin = 1000000, ymax=2500000, xmin= sum(range(log(df4$alfa)))/2, xmax=Inf)

  Ginset=ggplot(df4, aes((beta),countries,colour=as.numeric(day))) +
    geom_path(size=1, colour="gray60")+
    labs(x= expression(paste(beta)), y= expression(paste(""))) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0, 3000000, 6000000))+
    theme(rect = element_rect(fill = "transparent"),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    #scale_x_log10(labels=trans_format('log10',math_format(10^.x)))+
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))


  G2 = G
  #+ annotation_custom(grob=ggplotGrob(Ginset), ymin = 90, ymax=175, xmin= sum(range(log(df4$beta)))/2, xmax=Inf)



  ###############

  HH = ggplot(df4, aes((alfa), (countries),colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.3)+
    labs(x= expression(paste("",alpha, "")), y= expression("#Countries"), title = "C. ") +
    theme_minimal() +
    #scale_y_continuous(limits = c(-10000,3000000), expand  = c(-10000, 3000000))+
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")

  I = ggplot(df4, aes((beta),NcasesPos,colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.3)+
    labs(x= expression(paste("",beta, "")), y= expression("#SARS-CoV-2(+) "), title= "B. ") +
    theme_minimal() +
    #scale_y_continuous(limits = c(-0.1,170), expand  = c(-0.1, NA))+
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")



  HHinset = ggplot(df4, aes((alfa), countries,colour=as.numeric(day))) +
    geom_path(size=1, colour="gray60")+
    labs(x= expression(paste(alpha)), y= expression(paste(""))) +
    theme_minimal() +
    theme(rect = element_rect(fill = "transparent"),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))


  HH2 = HH #+ annotation_custom(grob=ggplotGrob(HHinset),ymin = 90, ymax=170, xmin= sum(range(log(df4$alfa)))/2, xmax=Inf)

  Iinset=ggplot(df4, aes((beta),NcasesPos,colour=as.numeric(day))) +
    geom_path(size=1, colour="gray60")+
    labs(x= expression(paste(beta)), y= expression(paste(""))) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0, 3000000, 6000000))+
    theme(rect = element_rect(fill = "transparent"),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    #scale_x_log10(labels=trans_format('log10',math_format(10^.x)))+
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))


  I2 = I #+ annotation_custom(grob=ggplotGrob(Iinset), ymin = 1000000, ymax=2500000, xmin= sum(range(log(df4$beta)))/2, xmax=Inf)


  plotrelations <<- list.append(plotrelations, alphaSARS= FF2,
                                betaCountries=G2,
                                alphaCountries = HH2,
                                betaSARS = I2)

  print((FF2/HH2)|(I2/G2))

  if(saveplot==TRUE){
    dir.create("plots")
    dir.create("plots/betaplots")
    ggsave(plot = ((FF2/HH2)|(I2/G2)), filename = paste("plots/betaplots/relations", saveplot.ext), width = 25, height = 12, units = "cm")
  }

}#ElFin

#' Plot beta distribution estimates evolution
#' @author Simon P Castillo \email{spcastil@@uc.cl}.
#' @description This function plots the temporal evolution of the estimated parameters of beta distribution $\hat{\alpha}$ and $\hat{\beta}$
#' @param CPAD: dataframe. The dataframe returned by \code{\link{analyse_CPAD}}.
#' @param saveplots: \code{TRUE} or \code{FALSE}. Save the plots in your \code{wd}.Default \code{TRUE}.
#' @param saveplots.ext: character. The extension for the saved figures admitted by \code{\link[ggplot2]{ggsave}} (e.g., \code{".png"}, \code{".svg"}).Default \code{".png"}.
#' @return This function return to your global environment an object list named \code{plotbetatraj} with the plot(s) created. Also, if \code{saveplot} is \code{TRUE}, a folder named \code{betaplots} in the folder \code{plots} is created in your \code{wd}.
#' @examples plot_betatraj(CPAD, saveplot=TRUE, saveplot.ext= ".png")
#'
#'
#'
plot_betatraj <- function(CPAD, saveplot=TRUE, saveplot.ext= ".png"){
  pacman::p_load(ggplot2,lubridate, viridis, rlist, patchwork)
  plotbetatraj <- list()
  df4 = CPAD
  breaksdate = seq(range(CPAD$day)[1],range(CPAD$day)[2], length.out = 5) #c(min(df2b$date),as.Date("2020-02-28"),as.Date("2020-02-29"),as.Date("2020-04-14"),max(df2b$date))#

  A =ggplot(df4, aes((alfa), (beta),colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    geom_path(size=1.5, alpha=0.3)+
    labs(x= expression(paste("",alpha, "")), y= expression(paste("",beta, ""))) +
    theme_minimal() +
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")

  B=ggplot(df4, aes(day, (alfa), colour=as.numeric(day))) + #log(alfa)
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.6)+
    labs(x= "", y= expression(paste("",alpha, ""))) +
    theme_minimal() +
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")
  C=ggplot(df4, aes(day, (beta), colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.3)+
    labs(x= "", y= expression(paste("",beta, ""))) +
    theme_minimal() +
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")

  D=ggplot(df4, aes(day, (E_beta), colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.3)+
    labs(x= "", y= expression("E|prop ab|")) +
    theme_minimal() +
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")

  E=ggplot(df4, aes(day, (V_beta), colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.3)+
    labs(x= "", y= expression("Var|prop ab|")) +
    theme_minimal() +
    #guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")


  plotbetatraj <<- list.append(plotbetatraj, alphabeta= A, alphatime=B, betatime=C, expectancytime=D, vartime=E)

  print(A|(B/C)|(D/E))


  if(saveplot==TRUE){
    dir.create("plots")
    dir.create("plots/betaplots")
    ggsave(plot = A|(B/C)|(D/E), filename = paste("plots/betaplots/betatraj", saveplot.ext), width = 24, height = 15, units = "cm")
  }

}#ElFin

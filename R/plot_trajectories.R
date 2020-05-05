#' Plot SARS-CoV-2(+) trajectories
#' @author Simon P Castillo \email{spcastil@@uc.cl}.
#' @description This function plots the temporal tendencies in #SARS-CoV-2(+) and its proportional abundance each log-transformed data and insets with corresponding non-transformed data.
#' @param inc.df: dataframe. The incidence dataframe.
#' @param propab.df: dataframe. The proportional abundance dataframe.
#' @param plot.inc: \code{TRUE} or \code{FALSE}. Plot incidence trajectory. Default \code{TRUE}.
#' @param plot.propab: \code{TRUE} or \code{FALSE}. Plot proportional abundance trajectory. Default \code{TRUE}.
#' @param saveplots: \code{TRUE} or \code{FALSE}. Save the plots in your \code{wd}.Default \code{TRUE}.
#' @param saveplots.ext: character. The extension for the saved figures admitted by \code{\link[ggplot2]{ggsave}} (e.g., \code{".png"}, \code{".svg"}).Default \code{".png"}.
#' @examples plot_trajectories(inc.df=inc_df, propab.df=propab_df, plot.inc=TRUE, plot.propab=TRUE, saveplots= TRUE, saveplots.ext= ".png")
#' @return This function returns to your global environment an object list named \code{plotTraj} with the plot(s) created. Also, if \code{saveplots} is \code{TRUE}, a folder named \code{plots} is created in your \code{wd}.
#'
#'
#'
plot_trajectories <- function(inc.df, propab.df, plot.inc=TRUE, plot.propab=TRUE, saveplots= TRUE, saveplots.ext= ".png"){

  pacman::p_load(ggplot2,lubridate, viridis, rlist)

  if(plot.inc == FALSE & plot.propab== FALSE)stop("plot.inc or plot.propab must be TRUE")

  plotTraj=list()
  df2b <- inc.df
  df2 <- propab.df
  breaksdate = seq(range(df2b$date)[1],range(df2b$date)[2], length.out = 5) #c(min(df2b$date),as.Date("2020-02-28"),as.Date("2020-02-29"),as.Date("2020-04-14"),max(df2b$date))#

  if(saveplots==TRUE){dir.create("plots")}

  if(plot.inc==TRUE){
  popMain  = ggplot(df2b, aes(date, (Incidence),colour=Country, group= Country)) +
    #geom_point(aes(group = seq_along(Day)), size=2)+
    geom_path(size=0.5)+
    labs(x= "", y= expression("Number of SARS-Cov-2(+)"), title = "A.") +
    theme_minimal() +
    guides(colour=FALSE)+
    theme(plot.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_d()

  Number_W  = popMain
  #+ annotation_custom(grob=ggplotGrob(popInset),ymin = 10.4, ymax=20, xmin= -Inf, xmax=median(df2b$date))

  plotTraj <- list.append(plotTraj, incidence=Number_W)
  print(Number_W)

  if(saveplots==TRUE){
  ggsave(plot = Number_W, filename = paste0("plots/traj_incidence",saveplots.ext) , width = 20, height = 10, units = "cm")
  }
}
  #---------------------------------------------------------------------------------
  # proportional abundance plots
  #---------------------------------------------------------------------------------

if(plot.propab==TRUE){
  popMain_p = ggplot(df2, aes(date, (propAb),colour=Country, group= Country)) +
    #geom_point(aes(group = seq_along(Day)), size=2)+
    geom_path(size=0.5)+
    labs(x= "", y= expression("Prop abundance SARS-Cov-2(+)"), title = "B.") +
    theme_minimal() +
    guides(colour=FALSE)+
    theme(plot.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_d()
    #scale_colour_viridis_c(breaks = as.numeric(breaksdate),
     #                      labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
      #                     name = "Day",
       #                    option = "plasma")

  Prp_W  = popMain_p


  plotTraj <- list.append(plotTraj, propab=Prp_W)
  print(Prp_W)

  if(saveplots==TRUE){
  ggsave(plot = Prp_W, filename = paste0("plots/traj_propAb",saveplots.ext) , width = 20, height = 10, units = "cm")
  }
}
plotTraj <<- plotTraj
}#ElFin

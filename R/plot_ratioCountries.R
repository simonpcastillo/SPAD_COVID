#' Plot the fraction of countries with active SARS-CoV-2 vs sum #SARS-CoV-2, E|#SARS-CoV-2|, and E|prop ab|
#' @author Simon P Castillo \email{spcastil@@uc.cl}.
#' @description This function plots the relations between the fraction of countries with active SARS-CoV-2 vs sum #SARS-CoV-2, E|#SARS-CoV-2|, and E|prop ab|.
#' @param CPAD: dataframe. The dataframe returned by \code{\link{analyse_CPAD}}.
#' @param saveplots: \code{TRUE} or \code{FALSE}. Save the plots in your \code{wd}.Default \code{TRUE}.
#' @param saveplots.ext: character. The extension for the saved figures admitted by \code{\link[ggplot2]{ggsave}} (e.g., \code{".png"}, \code{".svg"}).Default \code{".png"}.
#' @return This function return to your global environment an object list named \code{plotratioCountries} with the plot(s) created. Also, if \code{saveplot} is \code{TRUE}, a folder named \code{betaplots} in the folder \code{plots} is created in your \code{wd}.
#' @examples plot_ratioCountries(CPAD, saveplot=TRUE, saveplot.ext= ".png")
#'
#'
#'
plot_ratioCountries <- function(CPAD, saveplot=TRUE, saveplot.ext= ".png"){
  pacman::p_load(ggplot2,lubridate, viridis, rlist, patchwork)
  plotratioCountries <- list()
  df4 = CPAD
  breaksdate = seq(range(CPAD$day)[1],range(CPAD$day)[2], length.out = 5) #c(min(df2b$date),as.Date("2020-02-28"),as.Date("2020-02-29"),as.Date("2020-04-14"),max(df2b$date))#
  H = ggplot(df4, aes(ratio_countries, log(MRcasesPos), colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.3)+
    labs(x= expression("Ratio countries"), y= expression("ln(E|prop ab|)"), title = "A. ") +
    theme_minimal() +
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")

  I = ggplot(df4, aes(ratio_countries, log(NcasesPos), colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.3)+
    labs(x= expression("Ratio countries"), y= expression("ln(#SARSCoV-2(+))"), title="B. ") +
    theme_minimal() +
    #guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")

  J = ggplot(df4, aes(ratio_countries, log(McasesPos), colour=as.numeric(day))) +
    geom_point(aes(group = seq_along(day)), size=2)+
    #geom_path(size=1.5, alpha=0.3)+
    labs(x= expression("Ratio countries"), y= expression("ln(E|#SARSCoV-2(+)|)"), title = "C. ") +
    theme_minimal() +
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                           labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                           name = "Day",
                           option = "plasma")


  plotratioCountries <<- list.append(plotratioCountries, countriesMRCases= H, countriesNCases=I)

  print(H/I/J)

  if(saveplot==TRUE){
    dir.create("plots")
    dir.create("plots/betaplots")
    ggsave(plot = H/I/J, filename = paste("plots/betaplots/plotratioCountries", saveplot.ext), width = 25, height = 18, units = "cm")
  }

}#ElFin

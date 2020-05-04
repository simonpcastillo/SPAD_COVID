#' Animated beta distribution estimates evolution
#' @author Simon P Castillo \email{spcastil@@uc.cl}.
#' @description This function plots the animated temporal evolution of the estimated parameters of beta distribution $\hat{\alpha}$ and $\hat{\beta}$
#' @param CPAD: dataframe. The dataframe returned by \code{\link{analyse_CPAD}}.
#' @param logScale: \code{TRUE} or \code{FALSE}. Log-transformed estimates.Default \code{TRUE}.
#' @param saveAnim: \code{TRUE} or \code{FALSE}. Save the animation.Default \code{FALSE}.
#' @return This function returns to your Viewer panel the animated temporal evolution of $\hat{\alpha}$ and $\hat{\beta}$ .
#' Also, if \code{saveAnim} is \code{TRUE}, a folder named \code{GIFs} is created in your \code{wd} with the animation in \code{gif} format.
#' @examples plot_AnimatedBetaEst(CPAD, logScale=TRUE, saveAnim=FALSE)
#'
#'
#'
plot_AnimatedBetaEst <- function(CPAD,logScale=TRUE, saveAnim=FALSE){

  pacman::p_load(ggplot2,lubridate, viridis, rlist, patchwork, gganimate)

  df4 = CPAD
  breaksdate = seq(range(CPAD$day)[1],range(CPAD$day)[2], length.out = 5) #c(min(df2b$date),as.Date("2020-02-28"),as.Date("2020-02-29"),as.Date("2020-04-14"),max(df2b$date))#

  if(logScale == TRUE){
  space = ggplot(df4, aes(log(alfa), log(beta),colour=as.numeric(day),
                         label = datetext)) +
    geom_point(aes(group = seq_along(day)), size=2)+
    geom_text(aes(-1, 8), size = 12, hjust = 0, color = "gray70") +
    geom_path(size=1.5, alpha=0.3)+
    theme_minimal() +
    guides(colour=FALSE)+
    theme(axis.title=element_text(size=12,face="bold"))+
    scale_colour_viridis_c(breaks = as.numeric(seq(range(df4$day)[1],range(df4$day)[2], length.out = 5)),
                           labels = paste0(day(seq(range(df4$day)[1],range(df4$day)[2], length.out = 5)), "-", month(seq(range(df4$day)[1],range(df4$day)[2], length.out = 5), label = TRUE)),
                           name = "Day",
                           option = "plasma") +
    labs(x= expression(paste("ln(",alpha, ")")), y= expression(paste("ln(",beta, ")"))) +
    transition_reveal(as.Date(df4$datetext, "%d %b"))+
    enter_fade()+
    exit_fade()
  }

  if(logScale == FALSE){
    space= ggplot(df4, aes((alfa), (beta),colour=as.numeric(day),
                           label = datetext)) +
      geom_point(aes(group = seq_along(day)), size=2)+
      geom_text(aes(-1, 8), size = 12, hjust = 0, color = "gray70") +
      geom_path(size=1.5, alpha=0.3)+
      theme_minimal() +
      guides(colour=FALSE)+
      theme(axis.title=element_text(size=12,face="bold"))+
      scale_colour_viridis_c(breaks = as.numeric(seq(range(df4$day)[1],range(df4$day)[2], length.out = 5)),
                             labels = paste0(day(seq(range(df4$day)[1],range(df4$day)[2], length.out = 5)), "-", month(seq(range(df4$day)[1],range(df4$day)[2], length.out = 5), label = TRUE)),
                             name = "Day",
                             option = "plasma") +
      labs(x= expression(paste("(",alpha, ")")), y= expression(paste("(",beta, ")"))) +
      transition_reveal(as.Date(df4$datetext, "%d %b"))+
      enter_fade()+
      exit_fade()
  }
  #space
  #space2=animate(space, nframes = length(unique(CPAD$day))*3)
  animate(space, nframes = length(unique(CPAD$day))*3)
  if(saveAnim==TRUE){
  dir.create("GIFs")
  anim_save(animation = animate(space, nframes = length(unique(CPAD$day))*3), filename = "GIFs/plot_AnimatedBeta.gif")
  }

}

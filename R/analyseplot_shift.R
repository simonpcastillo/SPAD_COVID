#' Estimate a single trajectory shift in the beta distribution estimates evolution
#' @author Simon P Castillo \email{spcastil@@uc.cl}.
#' @description This function calculates and plots a single shift in the temporal evolution of the estimated parameters of beta distribution $\hat{\alpha}$ and $\hat{\beta}$
#' @param CPAD: dataframe. The dataframe returned by \code{\link{analyse_CPAD}}.
#' @param saveplot: \code{TRUE} or \code{FALSE}. Save the plots in your \code{wd}.Default \code{TRUE}.
#' @param saveplot.ext: character. The extension for the saved figures admitted by \code{\link[ggplot2]{ggsave}} (e.g., \code{".png"}, \code{".svg"}).Default \code{".png"}.
#' @return This function returns to your global environment an object list named \code{plotbetatraj} with the plot(s) created. Also, if \code{saveplot} is \code{TRUE}, a folder named \code{betaplots} in the folder \code{plots} is created in your \code{wd}.
#' @return Also, this function returns a numeric object \code{plotShift} to your Global Environment, it is the esitmated date of the single transition.
#' @details The estimation of the single transition in the temporal trajectories of $\hat{\alpha}$ and $\hat{\beta}$ is performed via minimisaiton of the residual squared sum (RSS) (Bai. 1994).
#' @references J. M. Box-Steffensmeier, J. R. Freeman, M. P. Hitt, J. C. W. Pevehouse, Time Series Analysis for the Social Sciences (Cambridge University Press, 2014).
#' @references J. Bai, Least Squares Estimation of a Shift in Linear Processes. Journal of Time Series Analysis. 15, 453–472 (1994).
#' @examples analyseplot_shift(CPAD, saveplot=TRUE, saveplot.ext= ".png")
#'
#'
#'
analyseplot_shift <- function(CPAD,saveplot=TRUE, saveplot.ext= ".png"){

  pacman::p_load(ggplot2,lubridate, viridis, rlist, patchwork)

  plotShift <- list()
  df4 = CPAD
  breaksdate = seq(range(CPAD$day)[1],range(CPAD$day)[2], length.out = 5) #c(min(df2b$date),as.Date("2020-02-28"),as.Date("2020-02-29"),as.Date("2020-04-14"),max(df2b$date))#
  n = nrow(df4)

      y <- log(df4$beta)
      U <- data.frame(tau=(1:(n-1)),RSS=0)
      for (tau in (1:(n-1))) {
        m1 <- mean(y[1:tau])
        m2 <- mean(y[(tau+1):n])
        m <- c(rep(m1,tau),rep(m2,(n-tau)))
        e <- y - m
        U[tau,2] <- sum(e^2)
      }
      tau.est <- which.min(U$RSS)
      U.min.beta <- U[tau.est,2]
      U$date = df4$day[-length(df4$day)]
      date.est = U[U$tau==tau.est,]$date
      dateshiftbeta <<- date.est
      t.test(y[1:tau.est],y[(tau.est+1):n],var.equal = T)


      tau_beta=ggplot(U, aes(date, RSS, colour=as.numeric(date))) +
        geom_point(aes(group = seq_along(date)), size=2)+
        #geom_path(size=1.5, alpha=1)+
        labs(x= "", y= expression(paste("RSS-", beta)), title= "B.") +
        #geom_point(aes(x=date.est,y=U.min.beta), colour="red", size=2)+
        geom_vline(xintercept = date.est, colour="red", size=1, lty=2)+
        theme_minimal() +
        #guides(colour=FALSE)+
        theme(axis.title=element_text(size=12,face="bold"))+
        scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                               labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                               name = "Day",
                               option = "plasma")

      y <- log(df4$alfa)
      U <- data.frame(tau=(1:(n-1)),RSS=0)
      for (tau in (1:(n-1))) {
        m1 <- mean(y[1:tau])
        m2 <- mean(y[(tau+1):n])
        m <- c(rep(m1,tau),rep(m2,(n-tau)))
        e <- y - m
        U[tau,2] <- sum(e^2)
      }
      tau.est <- which.min(U$RSS)
      U.min.alfa <- U[tau.est,2]
      U$date = df4$day[-length(df4$day)]
      date.est = U[U$tau==tau.est,]$date
      dateshiftalfa <<- date.est

      tau_alfa  = ggplot(U, aes(date, RSS, colour=as.numeric(date))) +
        geom_point(aes(group = seq_along(date)), size=2)+
        #geom_path(size=1.5, alpha=1)+
        labs(x= "", y= expression(paste("RSS-", alpha)), title="A.") +
        # geom_point(aes(x=date.est,y=U.min.alfa), colour="red", size=2)+
        geom_vline(xintercept = date.est, colour="red", size=1, lty=2)+
        theme_minimal() +
        guides(colour=FALSE)+
        theme(axis.title=element_text(size=12,face="bold"))+
        scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                               labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                               name = "Day",
                               option = "plasma")


      D=ggplot(df4, aes(day, log(E_beta), colour=as.numeric(day))) +
        geom_point(aes(group = seq_along(day)), size=2)+
        #geom_path(size=1.5, alpha=0.3)+
        labs(x= "", y= expression("ln(E|prop ab|)"), title="C.") +
        theme_minimal() +
        guides(colour=FALSE)+
        theme(axis.title=element_text(size=12,face="bold"))+
        #geom_vline(xintercept = date.est, colour="red", size=1, lty=2)+
        scale_colour_viridis_c(breaks = as.numeric(breaksdate),
                               labels = paste0(day(breaksdate), "-", month(breaksdate, label = TRUE)),
                               name = "Day",
                               option = "plasma")
plotShift <<- list.append(plotShift, alpha= tau_alfa, beta= tau_beta, ExpectancyPropAb= D )
print(tau_alfa/tau_beta/D)
if(saveplot==TRUE){
  dir.create("plots")
  dir.create("plots/betaplots")
ggsave(plot = tau_alfa/tau_beta/D,filename = paste("plots/betaplots/threshold", saveplot.ext), width = 12, height = 18, units = "cm")
}

}

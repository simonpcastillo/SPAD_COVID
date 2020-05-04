#' Analyse CPAD
#' @description  This function analyses the coronavirus proportional abundance distribution (CPAD)
#' @author Simon P Castillo \email{spcastil@@uc.cl}.
#' @param propab.matrix: dataframe. The global active cases/country population (proprtional abundance) by country and day.
#' @param propab.df: dataframe. The proportional abundance dataframe of htree columns: \code{Country}, \code{Day}, and \code{propAb}.
#' @param sumCases: numeric vector. The daily sum of active SARS-CoV-2 cases; its length must be equal that the temporal slices present in \code{propab.df}.
#' @param avCases: numeric vector. The daily average of active SARS-CoV-2 cases; its length must be equal that the temporal slices present in \code{propab.df}.
#' @param saveplots: \code{TRUE} or \code{FALSE}. Save plots related to de distributions in your \code{wd}.Default \code{TRUE}.
#' @return The function returns the \code{CPAD} dataframe to your Global Environment with the corresponding statistics for each day. If \code{saveplots} is \code{TRUE}, the \code{plots} folder is created in your \code{wd}, containing different folder with plots.
#' @examples analyse_CPAD(propab.matrix=propab_matrix, propab.df=propab_df, sumCases=sumCases, avCases=avCases, saveplots= TRUE)
#'
#'
#'
analyse_CPAD <-function(propab.matrix, propab.df, sumCases,avCases ,saveplots= TRUE){

  pacman::p_load(ggplot2,lubridate, viridis, rlist, foreach, parallel, fitdistrplus, goftest)

  if(length(as.character(unique(propab.df$Day))) != length(sumCases))stop("Need a sumCases vector of the same length of temporal slices present in propab.df")
  if(length(as.character(unique(propab.df$Day))) != length(avCases))stop("Need a avCases vector of the same length of temporal slices present in propab.df")
  if(saveplots==TRUE){
    dir.create("plots")
    dir.create("plots/kurtosis")
    dir.create("plots/CDF")
    dir.create("plots/beta_distro")
    dir.create("plots/lognormal_distro")
    dir.create("plots/betahistograms")
  }
  #---------------------------------------------------------------------------------

  df1 <- propab.matrix
  df2 <- propab.df
  dates = unique(df2$Day)
  dates =as.character(unique(df2$Day))

  dat = as.Date(dates, "%m/%d/%y")
  estimates = data.frame()
  PVAL=data.frame()
  S= data.frame()
  mean_propAb= numeric()

  #for (i in 1:length(dates)) {
  foreach (i=1:length(dates)) %do% {
   # print(paste0(round(i/length(dates)*100), "%"))
    #tryCatch({
    q = df1[,as.character(dates[i])]
    q2 = q[q>0]
    a = fitdist(q2, "beta", method="mle", optim.method = "Nelder-Mead")
    a2 =fitdist(q2, "lnorm", method="mle", optim.method = "Nelder-Mead")
    c=gofstat(a)
    vm=cvm.test(q2, "pbeta", shape1 = a$estimate[1], shape2 = a$estimate[2])
    ad=ad.test(q2, "pbeta", shape1 = a$estimate[1], shape2 = a$estimate[2])
    vm2=cvm.test(q2, "plnorm", a2$estimate[1],a2$estimate[2])
    ad2=ad.test(q2, "plnorm", a2$estimate[1],a2$estimate[2])
    #ksbeta=ks.test(x, y)
    #chibeta=chisq.test(x,y)
    #pvaldf= data.frame(dates[i], kstest= ksbeta$p.value, chisq= chibeta$p.value, adtest=ad$p.value, vmtest= vm$p.value)
    #PVAL= rbind(PVAL, pvaldf)
    mean_propAb[i] = mean(q2)
    b1g <- bootdist(a, niter = 100)
    l= as.data.frame(summary(b1g)$CI)
    l$days = dates[i]
    l$param = c("alpha", "beta")
    l$fitted = c(a$estimate[1], a$estimate[2])
    l$Countries = rep(length(q2),2)
    l$test_beta = c("CvM", "AD")
    l$test_lnorm = c("CvM", "AD")
    l$pval_beta = c(vm$p.value, ad$p.value)
    l$pval_lnorm = c(vm2$p.value, ad2$p.value)
    l$param_lnorm = c("mulog", "sdlog")
    l$fitted_lnorm = c(a2$estimate[1], a2$estimate[2])
    a = fitdist(q2, "beta", method="mle", optim.method = "Nelder-Mead")

    bb2= pbeta((q2), shape1=a$estimate[1], shape2 = a$estimate[2])
    s=(cor.test(sort(bb2),sort(q2)))
    s1 = data.frame(f= dates[i], cor= s$estimate,pval= s$p.value)
    S= rbind(S, s1)

    estimates = rbind(estimates, l)


  if(saveplots== TRUE){
    png(paste0("plots/kurtosis/",dat[i],".png"))
    descdist(q2, discrete = FALSE, boot = 1000, obs.col = "blue", boot.col = "yellow")
    dev.off()

    png(paste0("plots/CDF/",dat[i], ".png"))
    cdfcomp(list(a,a2), xlab = "Proportional abundance SARS-Cov-2(+)", lwd=2,main = paste0("Date ", dat[i]), fitcol = c("blue", "red"))
    dev.off()

    png(paste0("plots/beta_distro/",dat[i],".png"))
    plot(a, col="gray80")
    dev.off()

    png(paste0("plots/lognormal_distro/",dat[i],".png"))
    plot(a2)
    dev.off()

    png(paste0("plots/betahistograms/",dat[i],".png"))
    x <- q2#mtcars$mpg
    h<-hist(x, col="gray85", main = paste0("Date ", dat[i]) )
    xfit<-seq(min(x),max(x),length=length(x))
    yfit<-dbeta(xfit,shape1=a$estimate[1],shape2= a$estimate[2], ncp = 0)
    yfit <- yfit*diff(h$mids[1:2])*length(x)
    lines(xfit[-1], yfit[-1], col="blue", lwd=2)
    dev.off()
    }

    # }, error = function(e) paste("Error"))
  }

  #---------------------------------------------------------------------------------
  # data of statistics and estimates
  #---------------------------------------------------------------------------------

  df4 = data.frame(day = unique(estimates$days),
                   countries = estimates[estimates$param == "alpha",]$Countries,
                   ratio_countries = estimates[estimates$param == "alpha",]$Countries/max(estimates$Countries),
                   NcasesPos= sumCases,
                   McasesPos = avCases,
                   MRcasesPos = mean_propAb,
                   mu =  estimates[estimates$param_lnorm == "mulog",]$fitted_lnorm,
                   sd = estimates[estimates$param_lnorm == "sdlog",]$fitted_lnorm,
                   #mu_star =  exp(estimates[estimates$param_lnorm == "mulog",]$fitted_lnorm),
                   #sd_star = exp(estimates[estimates$param_lnorm == "sdlog",]$fitted_lnorm),
                   CvM_lnorm = estimates[estimates$test_lnorm == "CvM",]$pval_lnorm,
                   #AD_lnorm = estimates[estimates$test_lnorm == "AD",]$pval_lnorm,
                   alfa =  estimates[estimates$param == "alpha",]$fitted,
                   beta= estimates[estimates$param == "beta",]$fitted,
                   CvM_beta = estimates[estimates$test_beta == "CvM",]$pval_beta
                   #AD_beta = estimates[estimates$test_beta == "AD",]$pval_beta
  )
  df4$E_beta = df4$alfa/(df4$alfa+df4$beta)
  df4$V_beta = df4$alfa*df4$beta/((df4$alfa+df4$beta+1)*(df4$alfa+df4$beta)^2)

  df4$day = as.Date(df4$day, "%m/%d/%y")
  df4$pearson = S$cor
  df4$pearson_p = S$pval
  df4$datetext = format(df4$day, "%d %b")


  CPAD <<- df4

}#ElFin

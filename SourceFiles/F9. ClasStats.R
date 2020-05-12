######
# F9. Class_stats
#####

## ---------------------------------------------------------------------------------------------------------------------------------
# obtain classification statistics (o.a. Gamma + Cramer's V + mean SE )
Class_stats <- function(dat){
    classification_stats <- matrix(ncol=7, nrow=iter)
    colnames(classification_stats) <- c("phi", "Cramers V", "Kendall Tau B", "Goodman Kruskall Gamma", "ID", "mean SE", "mean SE EPST")
    tab <- table(dat$toets_advies, dat$design_advies)
      phi <- Phi(tab)
      V <- cramerV(tab)
      KTB <- KendallTauB(tab)
      GKG <- GoodmanKruskalGamma(tab)
      ID <- Theta_comp(dat$EPST_est, dat$theta_estimate)
      SE <- mean(dat$SE_test)
      SE_EPST <- mean(dat$EPST_SE)
    classification_stats <- c(phi, V, KTB, GKG, ID, SE, SE_EPST)
  return(classification_stats)}

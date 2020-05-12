Data_simulation1 <- function(seed, n, mu, sd)
{
  require("tidyverse")
  set.seed(seed)
  ## ---------------------------------------------------------------------------------------------------------------------------------
  ## TRUTH
  ## ---------------------------------------------------------------------------------------------------------------------------------
  
  # Draw theta values (the true ability of students)
  thetas <- as.matrix(rnorm(n, mu, sd))
  id <- 1:n               
  theta <- cbind(thetas, id)
  
  # Assign each student with a true advice, based on percentages from CBS data
  theta <- theta[order(theta[,1]),]
  per_group <- round((tabelx[,3]/100*n))
  d <- n - sum(per_group)
  ware_advies <-
    rep(
      c(1, 2, 3, 4, 5, 6 ,7, 8, 9),
      c((per_group[1] + d),
        per_group[2],
        per_group[3],
        per_group[4],
        per_group[5],
        per_group[6],
        per_group[7],
        per_group[8],
        per_group[9]
      )
    )
  theta <- as.data.frame(cbind(theta, ware_advies))

  ## ---------------------------------------------------------------------------------------------------------------------------------
  ## EPST
  ## ---------------------------------------------------------------------------------------------------------------------------------
  ## What advice would these students get if they made the MST test as currently used (1-2-3 design)?

  # Specify itembank (difficulty is matched to group that will take the module)
  it.MST <-
    rbind(
      mstR::genDichoMatrix(55, model = "1PL", bPrior = c("norm", 0, 1), seed = seed),
      mstR::genDichoMatrix(55, model = "1PL", bPrior = c("norm", M_L5, Var_L5), seed = seed),
      mstR::genDichoMatrix(55, model = "1PL", bPrior = c("norm", M_U5, Var_U5), seed = seed),
      mstR::genDichoMatrix(55, model = "1PL", bPrior = c("norm", M_L3, Var_L3), seed = seed),
      mstR::genDichoMatrix(55, model = "1PL", bPrior = c("norm", M_M3, Var_M3), seed = seed),
      mstR::genDichoMatrix(55, model = "1PL", bPrior = c("norm", M_U3, Var_U3), seed = seed)
      )

  it.MST <- as.matrix(it.MST)

  ## analyze
  theta_test <- matrix()
  SE_test <- matrix()
  mod2 <- matrix()
  mod3 <- matrix()
  for (j in 1:nrow(theta)) {
    x <- mstR::randomMST(trueTheta = theta[j,1], itemBank = it.MST, modules = modules_123, transMatrix = trans_123, start = list(fixModule = 1, seed = (seed*j)),
                         genSeed = (seed*j), test = list(method = "WL"), final = list(method = "WL"))
    theta_test[j] <- x$thFinal #save final theta estimate
    SE_test[j] <- x$seFinal #save SE
    mod2[j] <- x$selected.modules[2]
    mod3[j] <- x$selected.modules[3]
  }


  theta <- cbind(theta, theta_test, SE_test, mod2, mod3)

  # What advice would a student receive based on the test?
  theta <- theta[order(theta[,4]),]
  per_group <- round(tabelx[, 3] / 100 * n)
  d <- n - sum(per_group)
  toets_advies <-
    rep(
      c(1, 2, 3, 4, 5, 6 ,7, 8, 9),
      c((per_group[1] + d),
        per_group[2],
        per_group[3],
        per_group[4],
        per_group[5],
        per_group[6],
        per_group[7],
        per_group[8],
        per_group[9]
      )
    )

    theta <- cbind(theta, toets_advies)

  ## ---------------------------------------------------------------------------------------------------------------------------------
  ## TEACHER
  ## ---------------------------------------------------------------------------------------------------------------------------------

  docent_advies <- matrix(ncol=1, nrow=n)
  docent_advies[theta[,8]==1,] <- sample(c(1, 2, 3, 5, 6 ,7, 8, 9), size = nrow(theta[theta[,8] ==1,]), replace = TRUE, prob = cond.prob.bb[,1])
  docent_advies[theta[,8]==2,] <- sample(c(1, 2, 3, 5, 6 ,7, 8, 9), size = nrow(theta[theta[,8] ==2,]), replace = TRUE, prob = cond.prob.bb_kb[,1])
  docent_advies[theta[,8]==3,] <- sample(c(1, 2, 3, 5, 6 ,7, 8, 9), size = nrow(theta[theta[,8] ==3,]), replace = TRUE, prob = cond.prob.kb[,1])
  docent_advies[theta[,8]==5,] <- sample(c(1, 2, 3, 5, 6 ,7, 8, 9), size = nrow(theta[theta[,8] ==5,]), replace = TRUE, prob = cond.prob.gt[,1])
  docent_advies[theta[,8]==6,] <- sample(c(1, 2, 3, 5, 6 ,7, 8, 9), size = nrow(theta[theta[,8] ==6,]), replace = TRUE, prob = cond.prob.gt_havo[,1])
  docent_advies[theta[,8]==7,] <- sample(c(1, 2, 3, 5, 6 ,7, 8, 9), size = nrow(theta[theta[,8] ==7,]), replace = TRUE, prob = cond.prob.havo[,1])
  docent_advies[theta[,8]==8,] <- sample(c(1, 2, 3, 5, 6 ,7, 8, 9), size = nrow(theta[theta[,8] ==8,]), replace = TRUE, prob = cond.prob.havo_vwo[,1])
  docent_advies[theta[,8]==9,] <- sample(c(1, 2, 3, 5, 6 ,7, 8, 9), size = nrow(theta[theta[,8] ==9,]), replace = TRUE, prob = cond.prob.vwo[,1])

  theta <- cbind(theta, docent_advies)
  theta <- as.data.frame(theta[order(theta[,2]),])

  names(theta) <- c("theta", "id", "plaatsing", "EPST_est", "EPST_SE", "module2", "module3", "toets_advies", "docent_advies")
  theta$dif <- abs(theta$toets_advies - theta$docent_advies)
  theta <- theta %>%
    mutate(true_dif = abs(plaatsing - docent_advies))

  return(theta)
}

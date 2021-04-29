# Functions from Renkewitz & Keiner 2019
# List of functions:
#   renk_no_bias()
#   renk_100prcnt_bias()
#   renk_optional_stopping()
#   renk_two_step_bias()
#   renk_90prcnt_bias()

renk_no_bias <- function(m, k, n_min, n_max, m1g, Tau, sd1, m2, sd2) {
  Matrix <- matrix(nrow = (k*m), ncol = 24)
  count = 0
  for (b in 1:m)
  {
    i = 1

    while (i<=k)
    {
      a = i
      count = count+1

      n   <- round((runif(1, min = n_min, max = n_max)), digits = 0)
      m1  <- rnorm(1, m1g, Tau)
      V1  <- rnorm(n, m1, sd1)
      V2  <- rnorm(n, m2, sd2)
      MW1 <- mean(V1)
      SD1 <- sd(V1)
      MW2 <- mean(V2)
      SD2 <- sd(V2)
      N   <- n+n

      t.Wert        <- t.test(V1, V2, var.equal = TRUE) $statistic
      p.Wert_2tail  <- t.test(V1, V2, var.equal = TRUE) $p.value
      p.Wert_1tail  <- t.test(V1, V2, var.equal = TRUE, alternative = "greater")$p.value
      df            <- t.test(V1, V2, var.equal = TRUE) $parameter
      r             <- sqrt((t.Wert^2)/((t.Wert^2)+df))



      if(MW2>MW1){r <- ((-1)*r)}

      fisherz    <- (0.5*(log((1+r)/(1-r))))
      sefisherz  <- sqrt(1/(N-3))
      varfisherz <- (1/(N-3))
      d          <- (t.Wert*(sqrt((2*n)/(n*n))))
      var_d      <- ((2*n/(n*n)) + ((d^2)/(2*(n+n))))
      d_se       <-  sqrt(var_d)

      power = power.t.test(n, m1g, d_se, sig.level = p.Wert_2tail,
                           power = NULL,
                           type = "two.sample",
                           alternative = "two.sided",
                           strict = FALSE, tol = .Machine$double.eps^0.25)
      power = power$power

      J          <- (1-(3/((4*(2*n-2))-1)))
      g          <- d*J
      var_g      <- ((J^2)*var_d)

      Vector_Sa  <- c(b, a, n, N, MW1, SD1, MW2, SD2, t.Wert, p.Wert_2tail, p.Wert_1tail,
                      df, r, fisherz, sefisherz, varfisherz, d, var_d, d_se, g, var_g, count, m1, power)
      colnames(Matrix)<-c("b", "a", "n", "N", "MW1", "SD1", "MW2", "SD2", "t.Wert", "p.Wert_2tail",
                          "p.Wert_1tail", "df", "r", "fisherz", "sefisherz", "varfisherz", "d",
                          "var_d", "se", "g", "var_g", "count", "m1", "power")
      c <- (b-1)*k+a
      Matrix[c,] <- Vector_Sa
      i <- i+1
    }
  }
  return(Matrix)
}

renk_100prcnt_bias <- function(m, k, n_min, n_max, m1g, Tau, sd1, m2, sd2, sigS = 0.05, AsigS = 1, nsigS = 0.1, AnsigS_1 = 0.2, AnsigS_2 = 0) {
  Matrix <- matrix(nrow = (k*m), ncol = 24)
  count = 0

  for (b in 1:m)
  {
    i = 1

    while (i<=k)
    {
      a = i
      count = count+1

      n   <- round((runif(1, min = n_min, max = n_max)), digits = 0)
      m1  <- rnorm(1, m1g, Tau)
      V1  <- rnorm(n, m1, sd1)
      V2  <- rnorm(n, m2, sd2)

      p.Wert_1tail  <- t.test(V1, V2, var.equal = TRUE, alternative = "greater")$p.value

      if (sigS < p.Wert_1tail) {i = i-1}
      else
      {
        MW1 <- mean(V1)
        SD1 <- sd(V1)
        MW2 <- mean(V2)
        SD2 <- sd(V2)
        N   <- n+n

        t.Wert        <- t.test(V1, V2, var.equal = TRUE) $statistic
        p.Wert_2tail  <- t.test(V1, V2, var.equal = TRUE) $p.value
        df            <- t.test(V1, V2, var.equal = TRUE) $parameter
        r             <- sqrt((t.Wert^2)/((t.Wert^2)+df))

        fisherz    <- (0.5*(log((1+r)/(1-r))))
        sefisherz  <- sqrt(1/(N-3))
        varfisherz <- (1/(N-3))
        d          <- (t.Wert*(sqrt((2*n)/(n*n))))
        var_d      <- ((2*n/(n*n)) + ((d^2)/(2*(n+n))))
        d_se       <-  sqrt(var_d)

        power = power.t.test(n, m1g, d_se, sig.level = 0.05,
                             power = NULL,
                             type = "two.sample",
                             alternative = "two.sided",
                             strict = FALSE, tol = .Machine$double.eps^0.25)
        power = power$power

        J          <- (1-(3/((4*(2*n-2))-1)))
        g          <- d*J
        var_g      <- ((J^2)*var_d)

        Vector_Sa  <- c(b, a, n, N, MW1, SD1, MW2, SD2, t.Wert, p.Wert_2tail, p.Wert_1tail,
                        df, r, fisherz, sefisherz, varfisherz, d, var_d, d_se, g, var_g, count, m1, power)
        colnames(Matrix)<-c("b", "a", "n", "N", "MW1", "SD1", "MW2", "SD2", "t.Wert", "p.Wert_2tail",
                            "p.Wert_1tail", "df", "r", "fisherz", "sefisherz", "varfisherz", "d",
                            "var_d", "se", "g", "var_g", "count", "m1", "power")

        c <- (b-1)*k+a
        Matrix[c,] <- Vector_Sa
        count = 0
      }

      i = i+1
      print(b)
    }
  }
  return(Matrix)
}

renk_optional_stopping <- function(m, k, n, nn, n_max, m1g, Tau, sd1, m2, sd2, sigS = 0.05) {
  Matrix <- matrix(nrow = (k*m), ncol = 24)
  count = 0


  for (b in 1:m)
  {
    i = 1

    while (i<=k)
    {
      a = i
      count = count+1

      z = n

      m1  <- rnorm(1, m1g, Tau)
      V1  <- rnorm(n, m1, sd1)
      V2  <- rnorm(n, m2, sd2)

      p.Wert_1tail  <- t.test(V1, V2, var.equal = TRUE, alternative = "greater")$p.value

      if (sigS<=p.Wert_1tail)
      {

        while(sigS<p.Wert_1tail)
        {
          if (n<n_max)

          {
            V1<-c(V1, (rnorm(nn, m1, sd1)))
            V2<-c(V2, (rnorm(nn, m2, sd2)))
            p.Wert_1tail  <- t.test(V1, V2, var.equal = TRUE, alternative = "greater")$p.value
            n = n+nn
          }
          else
          {
            p.Wert_1tail = 0
            i = i-1
          }
        }
      }

      if (p.Wert_1tail>0)
      {
        MW1 <- mean(V1)
        SD1 <- sd(V1)
        MW2 <- mean(V2)
        SD2 <- sd(V2)
        N   <- n+n

        t.Wert        <- t.test(V1, V2, var.equal = TRUE) $statistic
        p.Wert_2tail  <- t.test(V1, V2, var.equal = TRUE) $p.value
        df            <- t.test(V1, V2, var.equal = TRUE) $parameter
        r             <- sqrt((t.Wert^2)/((t.Wert^2)+df))

        fisherz    <- (0.5*(log((1+r)/(1-r))))
        sefisherz  <- sqrt(1/(N-3))
        varfisherz <- (1/(N-3))
        d          <- (t.Wert*(sqrt((2*n)/(n*n))))
        var_d      <- ((2*n/(n*n)) + ((d^2)/(2*(n+n))))
        d_se       <-  sqrt(var_d)

        power = power.t.test(n, m1g, d_se, sig.level = 0.05,
                             power = NULL,
                             type = "two.sample",
                             alternative = "two.sided",
                             strict = FALSE, tol = .Machine$double.eps^0.25)
        power = power$power

        J          <- (1-(3/((4*(2*n-2))-1)))
        g          <- d*J
        var_g      <- ((J^2)*var_d)

        Vector_Sa  <- c(b, a, n, N, MW1, SD1, MW2, SD2, t.Wert, p.Wert_2tail, p.Wert_1tail,
                        df, r, fisherz, sefisherz, varfisherz, d, var_d, d_se, g, var_g, count, m1, power)
        colnames(Matrix)<-c("b", "a", "n", "N", "MW1", "SD1", "MW2", "SD2", "t.Wert", "p.Wert_2tail",
                            "p.Wert_1tail", "df", "r", "fisherz", "sefisherz", "varfisherz", "d",
                            "var_d", "se", "g", "var_g", "count", "m1", "power")

        c <- (b-1)*k+a
        Matrix[c,] <- Vector_Sa
        count = 0
      }
      i <- i+1
      n = z
    }
  }
  return(Matrix)
}

renk_two_step_bias <- function(m, k, n_min, n_max, m1g, Tau, sd1, m2, sd2, sigS = 0.05, AsigS=1, nsigS=0.1, AnsigS_1 = 0.2, AnsigS_2 = 0) {
  Matrix <- matrix(nrow = (k*m), ncol = 24)
  count = 0

  for (b in 1:m)
  {
    i = 1

    while (i<=k)
    {

      a = i
      count = count+1

      n   <- round((runif(1, min = n_min, max = n_max)), digits = 0)
      m1  <- rnorm(1, m1g, Tau)
      V1  <- rnorm(n, m1, sd1)
      V2  <- rnorm(n, m2, sd2)
      MW1 <- mean(V1)
      SD1 <- sd(V1)
      MW2 <- mean(V2)
      SD2 <- sd(V2)
      N   <- n+n

      t.Wert        <- t.test(V1, V2, var.equal = TRUE) $statistic
      p.Wert_2tail  <- t.test(V1, V2, var.equal = TRUE) $p.value
      p.Wert_1tail  <- t.test(V1, V2, var.equal = TRUE, alternative = "greater")$p.value
      df            <- t.test(V1, V2, var.equal = TRUE) $parameter
      r             <- sqrt((t.Wert^2)/((t.Wert^2)+df))

      if(MW2>MW1){r <- ((-1)*r)}

      fisherz    <- (0.5*(log((1+r)/(1-r))))
      sefisherz  <- sqrt(1/(N-3))
      varfisherz <- (1/(N-3))
      d          <- (t.Wert*(sqrt((2*n)/(n*n))))
      var_d      <- ((2*n/(n*n)) + ((d^2)/(2*(n+n))))
      d_se       <-  sqrt(var_d)

      power = power.t.test(n, m1g, d_se, sig.level = 0.05,
                           power = NULL,
                           type = "two.sample",
                           alternative = "two.sided",
                           strict = FALSE, tol = .Machine$double.eps^0.25)
      power = power$power

      J          <- (1-(3/((4*(2*n-2))-1)))
      g          <- d*J
      var_g      <- ((J^2)*var_d)

      if (p.Wert_1tail < sigS){sel_p = AsigS} else
      {if(p.Wert_1tail >= nsigS){sel_p = AnsigS_2} else
      {sel_p = AnsigS_1}}

      Cha <- runif(1, min = 0,max = 1)

      if(sel_p>Cha) {selected = 1} else {selected = 0}

      Vector_Sa  <- c(b, a, n, N, MW1, SD1, MW2, SD2, t.Wert, p.Wert_2tail, p.Wert_1tail,
                      df, r, fisherz, sefisherz, varfisherz, d, var_d, d_se, g, var_g, count, m1, power)
      colnames(Matrix)<-c("b", "a", "n", "N", "MW1", "SD1", "MW2", "SD2", "t.Wert", "p.Wert_2tail",
                          "p.Wert_1tail", "df", "r", "fisherz", "sefisherz", "varfisherz", "d",
                          "var_d", "se", "g", "var_g", "count", "m1", "power")

      c <- (b-1)*k+a
      Matrix[c,] <- Vector_Sa

      if(selected == 1){i <- i+1}
    }
  }
  return(Matrix)
}


renk_90prcnt_bias <- function(m, k, n_min, n_max, m1g, Tau, sd1, m2, sd2, sigS = 0.05, Asig=0.9) {
  Matrix <- matrix(nrow = (k*m), ncol = 25)
  count = 0

  for (b in 1:m)
  {
    i = 1

    while (i<=k)
    {
      Cha <- runif(1, min = 0, max = 1)

      if (Cha<Asig)
      {
        a = i

        n   <- round((runif(1, min = n_min, max = n_max)), digits = 0)
        m1  <- rnorm(1, m1g, Tau)
        V1  <- rnorm(n, m1, sd1)
        V2  <- rnorm(n, m2, sd2)

        p.Wert_1tail  <- t.test(V1, V2, var.equal = TRUE, alternative = "greater")$p.value

        if (sigS<p.Wert_1tail) {i = i-1}
        else
        {
          MW1 <- mean(V1)
          SD1 <- sd(V1)
          MW2 <- mean(V2)
          SD2 <- sd(V2)
          N   <- n+n

          p.Wert_2tail  <- t.test(V1, V2, var.equal = TRUE) $p.value
          t.Wert        <- t.test(V1, V2, var.equal = TRUE) $statistic
          df            <- t.test(V1, V2, var.equal = TRUE) $parameter
          r             <- sqrt((t.Wert^2)/((t.Wert^2)+df))


          fisherz    <- (0.5*(log((1+r)/(1-r))))
          sefisherz  <- sqrt(1/(N-3))
          varfisherz <- (1/(N-3))
          d          <- (t.Wert*(sqrt((2*n)/(n*n))))
          var_d      <- ((2*n/(n*n)) + ((d^2)/(2*(n+n))))
          d_se       <-  sqrt(var_d)

          power = power.t.test(n, m1g, d_se, sig.level = 0.05,
                               power = NULL,
                               type = "two.sample",
                               alternative = "two.sided",
                               strict = FALSE, tol = .Machine$double.eps^0.25)
          power = power$power

          J          <- (1-(3/((4*(2*n-2))-1)))
          g          <- d*J
          var_g      <- ((J^2)*var_d)

          Bias = 1

          Vector_Sa  <- c(b, a, n, N, MW1, SD1, MW2, SD2, t.Wert, p.Wert_2tail, p.Wert_1tail,
                          df, r, fisherz, sefisherz, varfisherz, d, var_d, d_se, g, var_g, m1,
                          Bias, Cha, power)
          colnames(Matrix)<-c("b", "a", "n", "N", "MW1", "SD1", "MW2", "SD2", "t.Wert", "p.Wert_2tail",
                              "p.Wert_1tail", "df", "r", "fisherz", "sefisherz", "varfisherz", "d",
                              "var_d", "se", "g", "var_g", "m1", "Bias", "Cha", "power")

          c <- (b-1)*k+a
          Matrix[c,] <- Vector_Sa

        }
        i <- i+1
      }
      else
      {
        a = i

        n   <- round((runif(1, min = n_min, max = n_max)), digits = 0)
        m1  <- rnorm(1, m1g, Tau)
        V1  <- rnorm(n, m1, sd1)
        V2  <- rnorm(n, m2, sd2)

        MW1 <- mean(V1)
        SD1 <- sd(V1)
        MW2 <- mean(V2)
        SD2 <- sd(V2)
        N   <- n+n

        p.Wert_1tail  <- t.test(V1, V2, var.equal = TRUE, alternative = "greater")$p.value
        p.Wert_2tail  <- t.test(V1, V2, var.equal = TRUE) $p.value
        t.Wert        <- t.test(V1, V2, var.equal = TRUE) $statistic
        df            <- t.test(V1, V2, var.equal = TRUE) $parameter
        r             <- sqrt((t.Wert^2)/((t.Wert^2)+df))

        if(MW2>MW1){r <- ((-1)*r)}


        fisherz    <- (0.5*(log((1+r)/(1-r))))
        sefisherz  <- sqrt(1/(N-3))
        varfisherz <- (1/(N-3))
        d          <- (t.Wert*(sqrt((2*n)/(n*n))))
        var_d      <- ((2*n/(n*n)) + ((d^2)/(2*(n+n))))
        d_se       <-  sqrt(var_d)

        power = power.t.test(n, m1g, d_se, sig.level = 0.05,
                             power = NULL,
                             type = "two.sample",
                             alternative = "two.sided",
                             strict = FALSE, tol = .Machine$double.eps^0.25)
        power = power$power

        J          <- (1-(3/((4*(2*n-2))-1)))
        g          <- d*J
        var_g      <- ((J^2)*var_d)

        Bias = 0

        Vector_Sa  <- c(b, a, n, N, MW1, SD1, MW2, SD2, t.Wert, p.Wert_2tail, p.Wert_1tail,
                        df, r, fisherz, sefisherz, varfisherz, d, var_d, d_se, g, var_g, m1,
                        Bias, Cha, power)
        colnames(Matrix)<-c("b", "a", "n", "N", "MW1", "SD1", "MW2", "SD2", "t.Wert", "p.Wert_2tail",
                            "p.Wert_1tail", "df", "r", "fisherz", "sefisherz", "varfisherz", "d",
                            "var_d", "se", "g", "var_g", "m1", "Bias", "Cha", "power")

        c = (b-1)*k+a
        Matrix[c,] <- Vector_Sa

        i <- i+1
      }
    }
  }
  return(Matrix)
}


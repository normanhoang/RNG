
random2 <- function(reps, seed){

  IM1 <- 2147483563
  IM2 <- 2147483399
  AM <- 1.0 / IM1
  IMM1 <- IM1 - 1
  IA1 <- 40014
  IA2 <- 40692
  IQ1 <- 53668
  IQ2 <- 52774
  IR1 <- 12211
  IR2 <- 3791
  NTAB <- 32
  NDIV <- 1 + as.integer(IMM1/NTAB)
  EPS <- 1.2e-7
  RNMX <- 1.0 - EPS
  
  idum2 <- 123456789
  
  numran <- reps
  ran1 <- 0
  
  iy <- 0
  iv <- rep(0, NTAB)
  random <- rep(0, numran)
  
  icount <- 1
  
  idum <- -seed
  
  for (icount in 1:numran){
    if (idum <= 0){
      idum <- max(-idum, 1)
      for (j in seq(NTAB+8, 1, -1)){
        k = as.integer(idum/IQ1)
        idum <- IA1*(idum-k*IQ1) - IR1*k
        if (idum < 0)
          idum <- idum + IM1
        if (j <= NTAB)
          iv[j] <- idum
      }
      iy <- iv[1]
    }
    k <- as.integer(idum/IQ1)
    idum <- IA1*(idum-k*IQ1)-IR1*k
    if (idum < 0)
      idum <- idum + IM1
    
    k <- as.integer(idum2/IQ2)
    idum2 <- IA2*(idum2 - k * IQ2)-k*IR2
    if (idum2 < 0)
      idum2 <- idum2 + IM2
    
    j <- 1 + as.integer(iy/NDIV)
    iy <- iv[j] - idum2
    iv[j] <- idum
    if (iy < 1)
      iy <- iy + IMM1
    ran2 <- min(AM*iy, RNMX)
    random[icount] <- ran2
  }
  random
}

test <- random2(10000, 1)
hist(test)
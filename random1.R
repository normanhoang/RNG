
IA <- 16807
IM <- 2147483647
AM <- 1.0 / IM
IQ <- 127773
IR <- 2836
NTAB <- 32
NDIV <- as.integer(1 + (IM-1)/NTAB)
EPS <- 1.2e-7
RNMX <- 1.0 - EPS

idum2 <- 123456789
idum <- 100
numran <- 100
ran1 <- 0

iy <- 1
iv <- rep(0, NTAB)
random <- rep(0, numran)
icount <- 1

idum <- -1

for (irange in 1:numran){
  if (idum <= 0 || iy == 0){
    idum <- max(-idum, 1)
    
    #j = NTAB + 8 , 1, -1
    for (j in seq(NTAB+8, 1, -1)){
      k = as.integer(idum/IQ)
      idum <- IA*(idum-k*IQ) - IR*k
      if (idum < 0)
        idum <- idum + IM
      if (j <= NTAB)
        iv[j] <- idum
    }
    iy <- iv[1]
  }
  k <- as.integer(idum/IQ)
  idum <- IA*(idum-k*IQ)-IR*k
  if (idum < 0)
    idum <- idum + IM
  j <- 1 + as.integer(iy/NDIV)
  iy <- iv[j]
  iv[j] <- idum
  ran1 <- min(AM*iy, RNMX)
  random[irange] <- ran1 
  print(j)
}
random
hist(random)
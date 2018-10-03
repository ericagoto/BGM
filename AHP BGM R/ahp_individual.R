# data = row with the values for the matrix
# n= number of parameters
# MM = main matrix 
# MP = priority matrix (weight)
# Sum = sum of each column of the MM
# MN = normalized matrix
# AX = MP*MM
# Lamb_Max
# CI = critical index
# RI = random index
# CR = consistency ratio
# return: CR, MP


#'  AHP
#' 
#' Function computes weight and consistancy ratio
#' @param data vector to build the matrix
#' @param n number of criteria
#' @param RI random index
#' @param CI critical index
#' @param Lamb_Max
#' @return CR, MP Critical Ratio and Priority matrix (weight)

AHP <- function (data, n, MM, MN, MP, AX, Lamb_Max, CI, CR, RI) {
  
  MM <- matrix(data, nrow=n, ncol=n, byrow = TRUE)
  
  #Sum <- colSums (MM)
  MN <- sweep(MM, FUN="/", MARGIN=2, STATS = colSums(MM))
  
  #Matrix Priority (MP)
  MP <- (rowSums(MN))/n 
  MP <- matrix (MP, nrow=n, ncol=1)
  
  #AX, Lambida_max, CR
  AX <- (MM)%*%(MP)
  
  Lamb_Max  <- (AX/MP)
  Lamb_Max <- (sum(Lamb_Max))/n
  
  CI = (Lamb_Max-n)/(n-1)
  
  # RI 
  if (n==2) {RI <- 0}
  if (n==3) {RI <- 0.58}
  if (n==4) { RI <- 0.9}
  if (n==5) {RI <- 1.12}
  if (n==6) {RI <- 1.24}
  if (n==7) {RI <- 1.32}
  if (n==8) {RI <- 1.41}
  if (n==9) {RI <- 1.45}
  if (n==10) {RI <- 1.51}
  if (n >10) {RI <- NA}
  
  
  CR=CI/RI
  
  
  return (list(CR, MP))
}
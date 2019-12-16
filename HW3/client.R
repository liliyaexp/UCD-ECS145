#!/usr/bin/env Rscript
source("ftp2.R")
obj <- c(1,2,3)
obj2 <- matrix(1:9, nrow = 3, ncol = 3)
obj3 <- function(x){x*3}
obj4 <- data.frame(matrix(1:9, nrow = 3, ncol = 3))
otpClient("localhost",6000)

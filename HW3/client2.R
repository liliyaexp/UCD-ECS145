#!/usr/bin/env Rscript
source("ftp2.R")
obj4 <- data.frame(matrix(1:9, nrow = 3, ncol = 3))
obj5 <- c(1,2,3)
obj6 <- matrix(1:9, nrow = 3, ncol = 3)
obj7 <- function(x){x*3}
otpClient("localhost",6000)
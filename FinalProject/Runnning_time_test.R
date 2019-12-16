library(microbenchmark)
library(ggplot2)
library(gradDescent)
data("gradDescentRData")

#data
test_data <- gradDescentRData[[3]]
data("gradDescentRData")

##########test1##############
## test original GD function on dataset with different sizes and fixed number of iteration
original_result_1 <- data.frame()
data_size <- c(500,1000,1500)
iteration <- 1e5
for (i in data_size){
  result <- summary(microbenchmark(GD(test_data[1:i,], maxIter = iteration,alpha = 0.0001),times =1,unit= "s"))
  result["data_size"] <- i
  result["version"] <- "Original"
  original_result_1 <- rbind(original_result_1,result)
}
detach("package:gradDescent", unload=TRUE)

## test modified GD function on dataset with different sizes and fixed number of iteration for run of GD
library(gradDescent.modified)
modified_result_1 <- data.frame()
iteration <- 1e5
for (i in data_size){
  result <- summary(microbenchmark(GD(test_data[1:i,], maxIter = iteration,alpha = 0.0001),times =1,unit= "s"))
  result["data_size"] <- i
  result["version"] <- "Modified"
  modified_result_1 <- rbind(modified_result_1,result)
}
detach("package:gradDescent.modified", unload=TRUE)
#plot result1
test1<-rbind(original_result_1,modified_result_1)
ggplot(test1)+geom_line(aes(x=data_size,y=mean,color=version))+
  geom_point(aes(x=data_size,y=mean,color=version))+
  theme_classic()+
  ggtitle('Modified GD vs. Original GD\n on Different Data Sizes')+
  xlab("Data Size")+ylab("Time(s)")+
  theme(axis.text=element_text(size=10),
        plot.title = element_text(size = 12, hjust=0.5, face = 'bold'),
        legend.position="right")
ggsave("test1.png",plot = last_plot(),width = 120, 
       height = 90,units = "mm",dpi = 300)


##########test2##############
## test original GD function on dataset with same sizes and different number of iteration
library(gradDescent)
original_result_2 <- data.frame()
iteration <- c(1e3,1e4,1e5)
for (i in iteration){
  result <- summary(microbenchmark(GD(test_data, maxIter = i,alpha = 0.0001),times =1,unit= "s"))
  result["iterations"] <- i
  result["version"] <- "Original"
  original_result_2 <- rbind(original_result_2,result)
}
detach("package:gradDescent", unload=TRUE)

## test modified GD function on dataset with same sizes and different number of iteration
library(gradDescent.modified)
modified_result_2 <- data.frame()
iteration <- c(1e3,1e4,1e5)
for (i in iteration){
  result <- summary(microbenchmark(GD(test_data, maxIter = i,alpha = 0.0001),times =10,unit= "s"))
  result["iterations"] <- i
  result["version"] <- "Modified"
  modified_result_2 <- rbind(modified_result_2,result)
}
#plot result2
test2<-rbind(original_result_2,modified_result_2)
ggplot(test2)+geom_line(aes(x=iterations,y=mean,color=version))+
  geom_point(aes(x=iterations,y=mean,color=version))+
  theme_classic()+
  ggtitle('Modified GD vs. Original GD\n on Different Number of Iterations')+
  xlab("Iterations")+ylab("Time(s)")+
  theme(axis.text=element_text(size=10),
        plot.title = element_text(size = 12, hjust=0.5, face = 'bold'),
        legend.position="right")

ggsave("test2.png",plot = last_plot(),width = 120, 
       height = 90,units = "mm",dpi = 300)
detach("package:graDescent.modified", unload=TRUE)
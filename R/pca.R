#pca function
PCA <- function(a , b)
{
  #install.packages('devtoools')
  library(devtools)
  #install_github("vqv/ggbiplot")
  library(ggbiplot)
  #reading genedata csv
  data <-read.csv(a)
  #checking for missing vaues , any character values in the dataset
  data[,15] = as.numeric(data[,15])
  data[,4] = as.numeric(data[,4])
  x <- na.omit(data)
  #applying pca
  data.pca <- prcomp(x[,c(3:32)], center = TRUE,scale. = TRUE)
  summary(data.pca)
  str(data.pca)
  #reading the metadata
  metadata <- read.csv(b)
  #plot1 contains graph where x axis has index and y axis has time
  plot1 = plot(metadata$Time)
  #plot2 containd graph of pca
  plot2 = ggbiplot(data.pca)
  return(plot2)
}
#calling the files
PCA("C:/Users/shrey/Documents/Assignment-gene_data(1).csv" , "C:/Users/shrey/Documents/Assignment-Meta_data sheet.csv")


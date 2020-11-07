data <- read.csv2("dane.csv", header = TRUE, sep = ";")

len <- length(data$wzrost)

for ( i in 1 : len) {
  
  print(data$waga[i]/(data$wzrost[i]/100)^2)
  
}



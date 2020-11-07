#SPRAWDZENIE POPRAWNOSCI ZADAN

funkcja_zad1(12,2)
funkcja_zad2(90,120)
funkcja_zad3('waga','wzrost')
funkcja_zad4(5)
funkcja_zad5("C:/zadaniar","140_temperature","mean",8) #ODP: 14.6
funkcja_zad5("C:/zadaniar","140_temperature","median",8) #ODP: 15
funkcja_zad5("C:/zadaniar","140_temperature","min",8) #ODP: -1
funkcja_zad5("C:/zadaniar","140_temperature","max",8) #ODP: 37


######################################################################

# DEFINICJA ZADANIE 1
funkcja_zad1 <- function(a,b) 
{
  if (a %% b == 0) {
    cat(a, "jest podzielne przez",b)
  }
  
  else {
    cat(a, "nie jest podzielne przez",b)
  }
  
}

# DEFINICJA ZADANIE 2

funkcja_zad2 <- function(a,b) {
  v = 2*a*b/(a+b)
  cat("Średnia prędkość pociągu wynosi",v,"km/h")
}

# DEFINICJA ZADANIE 3

funkcja_zad3 <- function(x,y) 
{
  df<-read.csv("dane.csv",header = TRUE,sep=";",dec=",")
  
  result = cor(df[[x]], df[[y]], method = "pearson")
  print(result)
}

#Wynik oznacza bardzo silna korelacje czyli zależnosc pomiedzy wiekiem a waga.
#Mozemy zobaczyc to na wykresie.

library("ggpubr")
ggscatter(df, x = "waga", y = "wzrost", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "waga w kg", ylab = "wzrost w cm)")



# DEFINICJA ZADANIE 4

funkcja_zad4 <- function(ile=1){
  print("Podaj nazwe kolumny")
  headSplit <- strsplit(readline(), " ")
  header <-headSplit[[1]]
  countColumns<-length(header)
  df <- data.frame(matrix(vector(),nrow = ile,ncol= countColumns ))
  colnames(df) <- header
  for( i in 1 : ile){
    r=0
    while(r==0){
      print("wartosc wiersza")
      rowSplit <- strsplit(readline(), " ")
      row <-rowSplit[[1]]
      if(length(row)==countColumns){
        df[i,] = row
        r=1
      }else{
        print(sprintf("Liczba elementów w wierszu musi byc równa %s",countColumns))
      }
    }
    View(df)
  }
 
}

# DEFINICJA ZADANIE 5

funkcja_zad5 <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=2){ 
  ResultVector <- c()
  for( i in 1 : DlaIluPlikow){
    ResultVector <- c(ResultVector, na.omit(read.csv(file.path(sciezka,list.files(sciezka)[i]))[ ,c(paste0("X",nazwaKolumny))]))
  }
  if(jakaFunkcja=="mean"){
    return(mean(ResultVector))
  }
  if(jakaFunkcja=="median"){
    return(median(ResultVector))
  }
  if(jakaFunkcja=="min"){
    return(min(ResultVector))
  }
  if(jakaFunkcja=="max"){
    return(max(ResultVector))
  }
  
}











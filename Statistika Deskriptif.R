ringkasan <- function(x){
  #main blok
  ringkas <- function(x){
    n <- function(x){
      n <- 0
      for (i in x){
        n <- n+1
      }
      return(n)
    }
    
    jumlah <- function(x){
      total <- 0
      for (i in x){
        total <- total + i
      }
      return(total)
    }
    
    rata_rata <- function(x){
      n <- 0
      jumlah <- 0
      for(i in x){
        n <- n + 1
        jumlah <- jumlah + i
      }
      return(jumlah/n)
    }
    
    urut <- function(x){
      n <- n(x)
      for (i in 1:(n-1)){
        for (j in 1:(n-i)){
          if (x[j] > x[j+1]){
            nilai <- x[j]
            x[j] <- x[j+1]
            x[j+1] <- nilai
          }
        }
      }
      return(x)
    }
    
    quartil <- function(x){
      n <- n(x)
      urut <- urut(x)
      
      pq1 <- 0.25 * (n +1)
      pq2 <- 0.75 * (n +1)
      
      lq1 <- floor(pq1)
      lq2 <- floor(pq2)
      
      q1 <- urut[lq1] + (pq1 - lq1) * (urut[lq1 + 1] - urut[lq1])
      q2 <- urut[lq2] + (pq2 - lq2) * (urut[lq2 + 1] - urut[lq2])
      
      return(data.frame(Q1 = q1, Q3 = q2))
    }
    quartil <- quartil(x)
    
    std <- function(x){
      rata <- rata_rata(x)
      selisih <- (x - rata)^2
      ratakuadrat <- rata_rata(selisih)
      std <- sqrt(ratakuadrat)
      return(std)
    }
    
    ragam <- function(x){
      rata <- rata_rata(x)
      selisih <- (x - rata)^2
      ratakuadrat <- rata_rata(selisih)
      return(ratakuadrat)
    }
    return(data.frame(min = urut(x)[1],
                      Q1 = quartil$Q1,
                      mean = rata_rata(x),
                      Q3 = quartil$Q3,
                      total = jumlah(x),
                      varians = ragam(x),
                      std = std(x),
                      max = urut(x)[n(x)]))
  }
  #end main blok
  if (is.data.frame(x) | (is.matrix(x))){
    datas <- as.matrix(x)
    hasil <- data.frame()
    for (i in range(1,ncol(datas))){
      a <- datas[,i]
      nilai <- ringkas(a)
      hasil <- rbind(hasil, nilai)
    }
    return(cbind(variabel = colnames(x),hasil))
  }
  #end percabangan pertama
  else if (length(x) > 1){
    hasil <- ringkas(x)
    return(hasil)
  }
  else{
    return('tipe data bukan data frame dan matriks')
  }
}
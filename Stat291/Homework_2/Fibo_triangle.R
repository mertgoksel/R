fibo <- function(x){
  result <- vector()
  for(i in 1:x){
    if(i == 1 | i == 2){
      result[i] <- 1
    } else {
      result[i] <- result[i-1]+result[i-2]
    }
  }
  if(x == 1){
    return(c(1))
  }else{
    return(tail(result, length(result)-1))
  }
}
fibo_triangle <- function(g){
  for(i in 1:g){
    row <- c(fibo(i+1))
    row <- c(row,tail(rev(row), length(row)-1))
    print(row)
  }
}
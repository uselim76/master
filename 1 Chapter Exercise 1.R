CoinTosses <- function (n){
  return (sample(c(0,1), n, replace = TRUE))
}

UpdatedCoinTosses <- function (n){
  vector=sample(c(0,1), n, replace = TRUE)
  result=c()
  if (n<=100){
    #print(paste0("Number of Proportion of heads minus 0.5 is ", sum(vector)/n-0.5))
    result=c(result,sum(vector)/n-0.5)
  }
  else{
    number_of_hundred=floor(n/100)
    for (x in 0:(number_of_hundred-1)){
       temp=vector[1:((x+1)*100)]
       #print(paste0("Number of Proportion of heads minus 0.5 is ", sum(temp)/100/(x+1)-0.5))
       result=c(result,sum(temp)/100/(x+1)-0.5)
    }
    #print(paste0("Number of Proportion of heads minus 0.5 is ", sum(vector)/n-0.5))
    result=c(result,sum(vector)/n-0.5)
  }
  return(result)
}
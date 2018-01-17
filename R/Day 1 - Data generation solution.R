# Day 1 - Data generation solution

PD<-dlnorm(seq(0.05,3,by = 0.05), meanlog = 0, sdlog = 1, log = FALSE)/6


generate_account<-function(id,PD){
  
  id<-paste("account",id,sep = "-")
  segment<-sample(LETTERS[1:3],1)
  start_balance<-runif(1,5000,6000)
  interest_rate<-runif(1,0.05,0.20)/12
  n<-60
  
  pmt<-start_balance*(interest_rate*(1+interest_rate)^n)/((1+interest_rate)^n-1)
  
  for(i in 1:n){
    if(i==1){
      balance_contractual<-start_balance*(1+interest_rate)-pmt
      
      made_payment<-rbinom(1,1,1-PD[i])
      balance_actual<-start_balance*(1+interest_rate)-pmt*made_payment[i]
      
    }else{
      balance_contractual<-c(balance_contractual,balance_contractual[i-1]*(1+interest_rate)-pmt)
      
      made_payment<-c(made_payment,rbinom(1,1,1-PD[i]))
      balance_actual<-c(balance_actual,balance_actual[i-1]*(1+interest_rate)-pmt*made_payment[i])
    }
  }
  
  
  data<-data.frame(
    id=id,
    segment=segment,
    start_balance=start_balance,
    interest_rate=interest_rate,
    n=n,
    age=1:n,
    pmt=pmt,
    balance_contractual=balance_contractual,
    made_payment=made_payment,
    balance_actual=balance_actual,
    arrears=balance_contractual-balance_actual,
    cd_bucket=-ceiling((balance_contractual-balance_actual)/pmt)
  )
}



account<-lapply(
  1:10000,
  generate_account,
  PD=PD
)

data<-dplyr::bind_rows(account)


# Export
data%>%rio::export("Data/tranition_data.csv")

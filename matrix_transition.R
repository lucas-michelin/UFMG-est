P = function(a,A){
a = 10
A = 22
t = a+A+1 #0..
D = round(dbinom(1:t,31,0.72),3)

qA = sum(D[A:33])
qa = sum(D[(a+1):33])
  
m = matrix(numeric(t^2),ncol=t,nrow = t)

for(i in 1:t){
if( i <= a){
  x = qA
  x1 = c(x,D[(A-1):1])
} else{
  x = qa
  x1 = c(x,D[(a-1):1])
}
n = t - length(x1)
x1 = c(x1,numeric(n))
  m[i,] = x1
}
return(m)
}

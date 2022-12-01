set.seed(1987) ##Guarntees the random output is the same
n<-15
a<-LETTERS[sample(n,replace=TRUE)]
b<-rpois(n,1)
c<-rep(TRUE,n)
d<-rbinom(n,1,.25)

#list, matrix,dataframe
list(c(a,b,c,d))
matrix(c(a,b,c,d), ncol = 4)
data.frame(matrix(c(a,b,c,d), ncol = 4))
cbind(a,b,c,d)




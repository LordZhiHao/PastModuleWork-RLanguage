#Install Packages and load libraries
install.packages("lpSolve")
install.packages("linprog")
install.packages("lpSolveAPI")
library(lpSolve)
library(linprog)
library(lpSolveAPI)

#Q1
#solveLP( cvec, bvec, Amat, maximum = FALSE,
#        const.dir)
#https://www.rdocumentation.org/packages/linprog/versions/0.9-4/topics/solveLP
Amat <- rbind(c(10,24,1),c(25,35,1),c( 3,40,1),c(10,30,1),c(30,20,1),c(18,20, 1),c(2,28,-1),c(5,21,-1),c(4,30,-1),c( 0,33,-1))
objcoef <- c(0,0,1)
Rhs <- c(rep(100,10))
dir <- c(">=",">=",">=",">=",">=",">=","<=","<=","<=","<=")
lpsol <- solveLP(objcoef,Rhs,Amat,maximum=FALSE,const.dir=dir)

x <- Amat[,1]
y <- Amat[,2]
z <- Amat[,3]
cust <- c("Cust1","Cust2","Cust3","Cust4","Cust5","Cust6","Cust7","Cust8","Cust9","Cust10")
plot(y~x,col=ifelse(z < 0,'red','blue'),pch=ifelse(z <0,19,12), xlab ="Credit card balance ($100)",ylab="Age (years)", main = "Small credit scoring dataset")
text(x,y,labels=cust,adj=c(0.5,-0.5))
lpsol$solution
slope = -lpsol$solution[1]/lpsol$solution[2]
intercept = 100/lpsol$solution[2]
abline(intercept,slope)

#Q2
Amat <- rbind(c(10,24,1),c(25,35,1),c( 3,40,1),c(10,30,1),c(30,20,1),c(18,20, 1),c(2,28,-1),c(5,21,-1),c(4,30,-1),c( 10,33,-1))
ypred <- Amat[,1]*lpsol$solution[1] + Amat[,2]*lpsol$solution[2]
slope = -lpsol$solution[1]/lpsol$solution[2]
intercept = 100/lpsol$solution[2]
abline(intercept,slope)

label <-c("default","default","default","default","default","default",
          "not default","not default","not default","not default")
i = 1
while (i <= 10) {
  if (ypred[i] >= 100 && label[i] == "default") { cat("Correct default prediction Cust", i,"\n\n")}
  else if (ypred[i] < 100 && label[i] == "not default") { cat("Correct not default prediction Cust", i,"\n\n")}
  else if (ypred[i] > 100 && label[i] == "not default") { 
    cat("Incorrect default prediction Cust ", i,"\n")  
    cat("Incorrect default prediction, dev = ", ypred[i]-100,"\n\n")}
  else if (ypred[i] <  100 && label[i] == "default") {
    cat("Incorrect non default prediction Cust ", i,"\n")  
    cat("Incorrect non default prediction, dev =  ",100- ypred[i], "\n\n")}
  i = i + 1}

##Q3
Anew <- rbind(c(10,24,1,0),c(25,35,1,0),c(3,40,1,0),c(10,30,1,0),c(30,20,1,0),c(18,20,1,0),
              + c(2,28,0,-1),c(5,21,0,-1),c(4,30,0,1),c(10,33,0,-1))
cnew <- c(0,0,2,1)
x <- Anew[,1]
y <- Anew[,2]
z <- Anew[,3]

lpsol <-  solveLP(cnew,Rhs,Anew,
                  maximum=FALSE,const.dir=dir)

slope = -lpsol$solution[1]/lpsol$solution[2]
intercept = 100/lpsol$solution[2]
plot(y~x,col=ifelse(z == 0,'red','blue'),
     pch=ifelse(z==0,19,12), 
     xlab ="Credit card balance ($100)",
     ylab="Age (years)", 
     main = "Small credit scoring dataset")
text(x,y,labels=cust,adj=c(0.5,-0.5))
abline(intercept,slope)



##Qn 3b
#make.lp(nrow = 0, ncol = 0, verbose = "neutral")
#Arguments
#nrow a nonnegative integer value specifying the number of constaints in the linear program.
#ncol a nonnegative integer value specifying the number of decision variables in the linear program
lps.model <- make.lp(10,4)
set.objfn(lps.model,c(0,0,2,1))
add.constraint(lps.model, c(10,24,1,0), ">=", 100)
add.constraint(lps.model, c(25,35,1,0), ">=", 100)
add.constraint(lps.model, c( 3,40,1,0), ">=", 100)
add.constraint(lps.model, c(10,30,1,0), ">=", 100)
add.constraint(lps.model, c(30,20,1,0), ">=", 100)
add.constraint(lps.model, c(18,20, 1,0), ">=", 100)
add.constraint(lps.model, c(2,28,0,-1), "<=", 100)
add.constraint(lps.model, c(5,21,0,-1), "<=", 100)
add.constraint(lps.model, c(4,30,0,-1), "<=", 100)
add.constraint(lps.model, c(10,33,0,-1), "<=", 100)
solve(lps.model)
sol <-  get.variables(lps.model)
sol


##Q3c
lps.model <- make.lp(10,4)
set.objfn(lps.model,c(0,0,1,2))
add.constraint(lps.model, c(10,24,1,0), ">=", 100)
add.constraint(lps.model, c(25,35,1,0), ">=", 100)
add.constraint(lps.model, c( 3,40,1,0), ">=", 100)
add.constraint(lps.model, c(10,30,1,0), ">=", 100)
add.constraint(lps.model, c(30,20,1,0), ">=", 100)
add.constraint(lps.model, c(18,20, 1,0), ">=", 100)
add.constraint(lps.model, c(2,28,0,-1), "<=", 100)
add.constraint(lps.model, c(5,21,0,-1), "<=", 100)
add.constraint(lps.model, c(4,30,0,-1), "<=", 100)
add.constraint(lps.model, c(10,33,0,-1), "<=", 100)
solve(lps.model)
sol <-  get.variables(lps.model)
sol

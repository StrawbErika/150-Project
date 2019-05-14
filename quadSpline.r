QuadraticSplineInterpolation <- function(intX,listXY){
    xList = listXY[[1]]
    yList = listXY[[2]]
    n = length(listXY[[1]])
    ultimateList = c(unlist(cOne(xList,yList,n)),unlist(cTwo(xList,yList,n)),unlist(cThree(xList,yList,n)))
    print(ultimateList)
}

cOne <- function(xList, yList, n){
    cOneList = vector()
    i = 3
    while(i != n+1){
        strFunction0 = paste( xList[i-1]^2,"a",i-2,"+",xList[i-1],"b",i-2,"+ c",i-2,"=",yList[i-1] , sep = "")
        strFunction1 = paste(xList[i-1]^2,"a",i-1, "+",xList[i-1],"b",i-1,"+ c",i-1,"=",yList[i-1],  sep = "")
        cOneList = c(cOneList, strFunction0)
        cOneList = c(cOneList, strFunction1)
        i = i + 1
    }
    return(cOneList)
}

cTwo <- function(xList, yList, n){
    cTwoList = vector()
    strFunction0 = paste( xList[1]^2,"a1","+",xList[1],"b1 + c1 =",yList[1] , sep = "")
    strFunction1 = paste(xList[n]^2,"a",n-1, "+",xList[n],"b",n-1,"+ c",n-1,"=",yList[n],  sep = "")
    cTwoList = c(cTwoList, strFunction0)
    cTwoList = c(cTwoList, strFunction1)
    return(cTwoList)
}

cThree <- function(xList, yList, n){
    cThreeList = vector()
    i = 3
    while(i != n+1){
        strFunction0 = paste( 2*xList[i-1],"a",i-2,"+","b",i-2,"=", 2*xList[i-1],"a",i-1,"+ b",i-1, sep = "")
        cThreeList = c(cThreeList, strFunction0)
        i = i + 1
    }
    return(cThreeList)
}
x = c(3,4.5,7,9)
y = c(2.5, 1, 2.5, 0.5)

QuadraticSplineInterpolation(2, list(x,y))
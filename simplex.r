#Simplex

simplex <- function(xList, yList, available, variables){
    updated = fixLists(xList,yList,available)
    initialMatrix = makeMatrix(updated$xList,updated$yList,updated$available, variables)
    print(initialMatrix)
    print(returnBasicSolution(initialMatrix))
    while(!checkDone(initialMatrix)){
        pivotCol = returnPivotColIndex(initialMatrix)
        pivotRow = returnPivotElementIndex(initialMatrix, pivotCol)
        initialMatrix = normalize(initialMatrix, pivotRow, pivotCol)
        initialMatrix = perColumnIteration(initialMatrix, pivotRow, pivotCol)
        print(initialMatrix)
        print(returnBasicSolution(initialMatrix))
    }
    return(initialMatrix)
}

makeMatrix <- function(xList, yList, available, variables){
    print(length(xList))
    n = length(xList)
    masterList = vector() 
    k = 1
    while(k!=n+1){
        i = 1
        if(k==n){
            temp = c(-(xList[k]),-(yList[k]))
        }else{    
            temp = c(xList[k],yList[k])
        }
        while(i!=n+1){
            if(i==k){
                temp = c(temp, 1)
            }else{
                temp = c(temp, 0)
            }
            if(i==n){
                temp = c(temp, available[k])
            }
            i = i + 1
        }
        masterList = c(masterList, temp)
        k = k + 1
    }
    final = matrix(masterList,ncol=variables+1, byrow=TRUE)
    return(final)
}

fixLists <- function(xList,yList, available){
    n = length(xList)
    i= 1
    if(length(available)==n-1){
        available = c(available,0)
    }else{
        while(i!=n+1){
            if(i==n){
                tempX = xList[n] 
                tempY = yList[n] 
                aX = xList[n-1]
                aY = yList[n-1]
                xList[n-1] = 1
                xList[n] = 0
                yList[n-1] = 0
                yList[n] = 1
                xList = c(xList,tempX)
                yList = c(yList,tempY)
                available = c(available, aX,aY,0)
            }
            i = i + 1
        }
    }
    return(list(xList=xList, yList=yList, available=available))
}

checkDone <- function(init){
    isDone = TRUE
    bottomRow = init[nrow(init),]
    i = 1
    while(i!=length(bottomRow)){
        if(bottomRow[i]<0){
            isDone = FALSE
        }
        i = i+1
    }
    return(isDone)
}

returnPivotColIndex <- function(init){
    bottomRow = init[nrow(init),]
    largestMagnitude = abs(bottomRow[1])
    pivotCol = 1
    i = 2
    while(i!=length(bottomRow)){
        if(bottomRow[i]<0){
            if(abs(bottomRow[i]) > largestMagnitude){
                largestMagnitude = abs(bottomRow[i])
                pivotCol = i
            }
        }
        i = i + 1
    }
    return(pivotCol)
}
returnPivotElementIndex <- function(init, index){
    pivotCol = init[,index]  
    bList = init[,ncol(init)]
    max = 10000
    pivotIndex = 0
    i = 1
    while(i!=nrow(init)){
        if(pivotCol[i]>0){
            testRatio = bList[i]/pivotCol[i]
            if(testRatio < max){
                max = testRatio
                pivotIndex = i
            }
        }
        i = i + 1
    } 
    return(pivotIndex) 
}

normalize <- function(init, pivotRow, pivotCol){
    normalizingValue = init[pivotRow, pivotCol]
    init[pivotRow,] = init[pivotRow,]/normalizingValue
    return(init)
}

perColumnIteration <- function(init, pivotRow, pivotCol){
    rowCount = nrow(init)
    row = 1
    while(row != rowCount){
        currentPivotRow = init[pivotRow,]
        pivotElement = currentPivotRow[pivotCol]
        eliminatedList = returnVEliminated(init, pivotRow, pivotCol)
        vEliminated = eliminatedList[1]
        vector = subtractingVector(vEliminated/pivotElement, currentPivotRow)       
        updatedRowIndex = eliminatedList[2]
        init = updateRow(init, updatedRowIndex, vector)
        row = row + 1
    }
    return(init)
}

returnVEliminated <- function(init, pivotRow, pivotCol){ 
    if(pivotRow != 1){
        i = 1
    }else{
        i = 2
    }
    value = 0 
    row = 0
    while(value == 0){
        if(i < nrow(init)+1){
            if(i!=pivotRow){
                value = init[i, pivotCol]
                row = i
            }
        }else{
            break
        }
        i = i + 1
    }
    return(c(value,row))
}

subtractingVector <- function(multiplier, row){
    return(multiplier*row)
}

updateRow <- function(matrix, updateRow, subtractVector){ 
    matrix[updateRow,] = matrix[updateRow,]-subtractVector
    return(matrix)
}

returnBasicSolution <- function(init){
    basicSolution = vector()
    colCount = ncol(init)
    rowCount = nrow(init)
    i = 1
    while(i!=colCount+1){
        k = 1
        isOne = FALSE
        while(k!=rowCount+1){
            if(init[k,i] == 1){
                if(checkZeroes(init[,i], rowCount)){
                    basicSolution = c(basicSolution, init[k, colCount])
                }else{
                    basicSolution = c(basicSolution, 0)
                }
                isOne = TRUE
            }
            if(!isOne){
                if(k==rowCount){
                    basicSolution = c(basicSolution, 0)
                }
            }
            k = k + 1
        }
        i= i+1
    }
    return(basicSolution[-(length(basicSolution))])

}

checkZeroes <- function(col, rowNum){
    isAnswer = TRUE
    cnt = 0
    i = 1
    while(i<length(col)+1){
        if(col[i] == 0){
            cnt = cnt + 1
        }
        i = i+1
    }
    if(cnt!=rowNum-1){
        isAnswer = FALSE
    }
    return(isAnswer)
}

#Variables is X, Y, Z, + slack variables
# xList = c(7,10,9,150)
# yList = c(11,8,6,175)
# available = c(77,80)
# init = simplex(xList,yList, available, 7)

# xList = c(4,2,70)
# yList = c(3,1,50)
# available = c(240,100)

# init = simplex(xList,yList, available, 5)
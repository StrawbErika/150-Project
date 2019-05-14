#read.csv(path) (returns in list matrix liek how u see at excel, omits first row tho, so start at 2nd)



# Erika Louise A. Nepomuceno
# CMSC 150 Exer_05 regression

PolynomialRegression <- function(orderOfPoly,listXY){
    n = length(listXY[[1]])
    augcoeffmatrix = getAugCoeff(n, orderOfPoly, listXY)
    unknowns = GaussJordan(augcoeffmatrix)
    polynomial_string = turnIntoPolyString(unknowns)
    polynomial_function <- eval(parse(text=polynomial_string))
    return(list(augcoeffmatrix=augcoeffmatrix, unknowns=unknowns, polynomial_string= polynomial_string, polynomial_function=polynomial_function))
}

getSumm <- function(listNum, exponent){ #gets summation including when it has exponent
    ans = 0
    for (num in listNum){
        ans = num^exponent + ans
    }
    return(ans)
}
multGetSumm <- function(listXY, exponent, n){ #get x*y summation with exponent
    ans = 0
    i = 1
    xList = listXY[[1]]
    yList = listXY[[2]]
    while(n+1 != i){
        xy = xList[i]^ exponent * yList[i]
        ans = xy + ans
        i = i + 1
    }
    return(ans)
}

getAugCoeff <- function(n, orderOfPoly, listXY){ #get augcoeff by getting per equation
    cnt = 0
    eqs = orderOfPoly+1
    augcoeff = vector()
    while(eqs > cnt){ #get per eq using makeList
        augcoeff = c(augcoeff, makeList(listXY, eqs, cnt, n))
        cnt = cnt + 1
    }    
    m = matrix(augcoeff, nrow = eqs, byrow = TRUE)
    return(m)
}

makeList <- function(listXY, eqs, index, n){ #make into per equation list
    i = 0
    row = vector()
    exp = index
    while(eqs+1 > i){
        if(index == 0 && i == 0){ #if first row, first column, #of datapoints
            row = c(row, n)
        }else if(i == eqs){
            row = c(row, multGetSumm(listXY, index, n)) #last column is the RHS which is the xy
        }else{
            row = c(row, getSumm(listXY[[1]], exp)) # get summation of x which is the first list in listXY
        }
        i = i + 1
        exp = exp + 1
    }
    return(row)
}

turnIntoPolyString <- function(unknowns){ #turn it into a polynomial string
    cnt = 0
    i = 1
    while(cnt != length(unknowns)){
        if(cnt == 0){
            pString = paste("function(x)", unknowns[i],  sep = " ")
        }else{
            pString = paste(pString, " + ", unknowns[i], " * x^", cnt,  sep = "")
        }
        cnt = cnt + 1
        i = i + 1
    }
    return(pString)
}

# ---------------------------------------------------------------


GaussJordan <- function(augcoeff){ #solves for unknown
    rowNum = nrow(augcoeff)
    colNum = ncol(augcoeff)
    cnt = 1
    while(cnt < colNum){
        biggestRow = findBiggestXRow(augcoeff, rowNum, cnt)
        augcoeff = pivot(augcoeff, biggestRow, cnt)
        augcoeff = normalization(augcoeff, cnt) #Normalizes augcoeff 
        augcoeff = perColumnIteration(augcoeff,cnt, 2)
        cnt = cnt+1
    }
    return(turnIntoList(augcoeff, colNum))
}

turnIntoList <- function(matrix, colNum){ #changes row into list
    array = vector()
    i = 1
    while(i != colNum){
        array = c(array, matrix[i, colNum])
        i = i+1
    }
    return(array)
}

normalization <- function(matrix, colNum){ #divides m[colNum, colNum] to the whole row where it is found
    normalizingValue = matrix[colNum, colNum]
    matrix[colNum,] = matrix[colNum,]/normalizingValue
    return(matrix)
}

perColumnIteration <- function(matrix, count, type){
    rowNum = nrow(matrix)
    if(type == 1){ #if gaussian, does not count matrix[prevColNum,] 
        i = count #count is current colNum
        row = count
    }else{ #if gaussjordan, counts every row
        i = 0
        row = 1
    }
    while(row != rowNum){
        currentPivotRow = matrix[count,]
        pivotElement = currentPivotRow[count]
        eliminatedList = returnVEliminated(matrix, i, count, type)
        if(type != 1){ #for gaussjordan, must eliminate everything except for diagonal
            i = i + 1
        }
        vEliminated = eliminatedList[1]
        vector = subtractingVector(vEliminated/pivotElement, currentPivotRow)       
        updatedRowIndex = eliminatedList[2]
        matrix = updateRow(matrix, updatedRowIndex, vector)
        row = row + 1
    }
    return(matrix)
}

updateRow <- function(matrix, updateRow, subtractVector){ 
    matrix[updateRow,] = matrix[updateRow,]-subtractVector
    return(matrix)
}

returnVEliminated <- function(matrix, pivotRow, pivotCol, type){ 
    value = 0 
    while(value == 0){
        pivotRow = pivotRow + 1
        if(pivotRow != pivotCol){ #for gauss jordan, if main diagonal, do not eliminate
            value = matrix[pivotRow, pivotCol]
        }
    }
    return(c(value,pivotRow))
}

findBiggestXRow <- function(matrix, rowNum, colNum){
    cnt = colNum
    biggest = 0
    biggestRowX = 0
    while(cnt < rowNum+1){
        if(cnt == colNum){
          biggestRowX = cnt
          biggest = abs(matrix[cnt,colNum])
        }else{
          if(abs(matrix[cnt,colNum]) > biggest){ #gives out biggerRow with biggerElementMagnitude
              biggest = abs(matrix[cnt,colNum])
              biggestRowX = cnt
          }
        }
    cnt = cnt + 1        
    }
    return(biggestRowX)
}

pivot <- function(matrix, source, dest){
    tempMatrix = matrix
    tempMatrix[source,]=matrix[dest,]
    tempMatrix[dest,]=matrix[source,]
    return(tempMatrix)
}

subtractingVector <- function(multiplier, row){
    return(multiplier*row)
}


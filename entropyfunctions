nw = setwd("C:/Users/nacampa/Documents")
cwdfle= paste(nw,"/entropy-test-file.csv",sep = "")
entropytest = read.csv(file=cwdfle, head=TRUE)
entropytest

# Function to remove missing values
convertvec <- function(vec) {
  n = length(vec)
  mvec <- is.na(vec)
  tvec <- vector()
  idx = 0
  for (i in 1:n) {
    if (mvec[i] == FALSE) {
      idx = idx + 1
      tvec[idx] = vec[i]
    }
  }
  return(tvec)
}

# Create a function Entropy that accepts an input vector with data type
# factor, charactor or numeric.

calcEntropy <- function(vec) {
  # Remove missing values
  vec         <- sort(vec)
  n           <- length(vec)
  probvec     <-  vector()
  Entropyvec  <- vector()
  totalcnt = 0
  if (is.factor(vec)) {
    category = levels(factor(vec))
    nocategories = length(category)
    # Convert vector elements to character for the comparison
    # since category is data type factor
    for (i in 1:nocategories) {
      for (j in 1:n) {  
        if (category[i] == as.character(vec[j])) {
          totalcnt = totalcnt + 1
        }
      }
      probvec[i] = totalcnt
      totalcnt =  0
    }
    for (i in 1:nocategories) {
      probvec[i] = probvec[i] / n
      Entropyvec[i] = ((probvec[i]) * log2(probvec[i])) 
    }
    Entropyvec <- convertvec(Entropyvec)
    return(sum(Entropyvec))
    
  }
  if (is.numeric(vec)) {   
    category = vec[1]
    categorycnt = 1
    for (i in 1:n) {
      if (vec[i] == category) {
        totalcnt = totalcnt + 1
      } else {
        probvec[categorycnt] = totalcnt
        categorycnt = categorycnt + 1
        category = vec[i]
        totalcnt = 1
      }
    } 
    # get last category
    probvec[categorycnt] = totalcnt
    for (i in 1:categorycnt) {
      probvec[i] = probvec[i] / n
      Entropyvec[i] = ((probvec[i]) * log2(probvec[i])) 
    }
    Entropyvec <- convertvec(Entropyvec)
    return(sum(Entropyvec)*-1)
  }      
  if (is.character(vec)) {
    category = vec[1]
    categorycnt = 1
    for (i in 1:n) {
      if (vec[i] == category) {
        totalcnt = totalcnt + 1
      } else {
        probvec[categorycnt] = totalcnt
        categorycnt = categorycnt + 1
        category = vec[i]
        totalcnt=1
      }
    } 
    probvec[categorycnt] = totalcnt
    for (i in 1:categorycnt) {
      probvec[i] = probvec[i] / n
      Entropyvec[i] = ((probvec[i]) * log2(probvec[i])) 
    }
    Entropyvec <- convertvec(Entropyvec)
    return(sum(Entropyvec)*-1)  
  }
}

# 2) Create a function infogain that takes two vectors -
#    The target vector d and the attribute vector a with which to partition the data
#    return the information gain for the attribute
#    where n <- length of the target vector d, n(j) is the the size of the partition
#    of vector d according to the jth value of vector a and E(d(j)) is the entropy
#    of partition of vector d according to the jth value of vector a  
#    Once working remane variables or define

infogain <- function(d,a) {
  n = length(d)
  targetPartitionVec <- vector()
  targetEntropyVec   <- vector()
  totalAttrCnt   = 0   
  totalTargetCnt  = 0 
  targetEntropy   = 0
  totalPartitionEntropy = 0  
  if (is.factor(a)) {
    category = levels(factor(a))
    nocategories = length(category)
    # Convert vector elements to character for the comparison
    # since category is data type factor
    for (i in 1:nocategories) {
      for (j in 1:n) {  
        if (category[i] == as.character(a[j])) {
          # We are searching the entire attribute on each category
          # and updating targetPartionVec with values of vector d (target vector) 
          totalAttrCnt = totalAttrCnt + 1
          targetPartitionVec[totalAttrCnt] = d[j]
        }
      }
      # Now call calcEntropy for each partion 
      targetEntropyVec = calcEntropy(targetPartitionVec)
      totalPartitionEntropy =  totalPartitionEntropy + (totalAttrCnt/n * targetEntropyVec)
      totalAttrCnt = 0 
      targetPartitionVec <- vector()
      targetEntropyVec  <- vector()
    }
    targetEntropy = calcEntropy(d)
    ig <- targetEntropy -  totalPartitionEntropy
    return(ig)
  } 
  if ((is.character(a)) || (is.numeric(a))) {
    f = factor(a) 
    category = levels(factor(f))
    nocategories = length(category)
    # Convert vector elements to character for the comparison
    # since category is data type factor
    for (i in 1:nocategories) { 
      for (j in 1:n) {
        if (category[i] == as.character(a[j])) {
          # We are searching the entire attribute on each category
          # and updating targetPartionVec with values of vector d (target vector) 
          totalAttrCnt = totalAttrCnt + 1
          targetPartitionVec[totalAttrCnt] = d[j]
        }
      }
      # Now call calcEntropy for each partion 
      targetEntropyVec = calcEntropy(targetPartitionVec)
      totalPartitionEntropy =  totalPartitionEntropy + (totalAttrCnt/n * targetEntropyVec)
      totalAttrCnt = 0 
      targetPartitionVec <- vector()
      targetEntropyVec  <- vector()     
    }
    targetEntropy = calcEntropy(d)
    ig <- targetEntropy -  totalPartitionEntropy
    return(ig)
  }
  
}


decide <- function(df, target) {
  
  igvec   <- vector()     
  igcnt   = 0
  maxig   = 0
  maxindx = 0
  
  if ((ncol(df) <= 0) || (ncol(df) < target)) {
    return("Error - Target not in range of DataFrame ")
  } else {
    d <- df[,target]
    n = ncol(df)
    for (i in 1:n) {
      if (i != target) {
        a <- df[,i]
        igcnt = igcnt + 1
        igvec[igcnt] <- infogain(d,a)
        if (igvec[igcnt] > maxig) {
          maxig = igvec[igcnt]
          maxindx = i
        }
      }
    }
    return(list(Max = maxindx, Gains = igvec))
  }
}

###############################################################################################################
#    deklaracia tried
###############################################################################################################
#intefrace pre Modely
ITreeNode <- setRefClass("ITreeNode", 
                        methods = list(
                          predictOne = function(x) {
                            print("!!!")
                          },
                          predict = function(data) {
                            pocetPozorovani = nrow(data)
                            vysledok = 1:pocetPozorovani
                            for (i in 1:pocetPozorovani) {
                              vysledok[i] = predictOne(data[i,]) 
                            }
                            return(vysledok)
                          },
                          printModel = function(m = ""){
                            print("!!!")
                          },
                          variableImportance = function(data, YParam,lossFun = sse){
                            print("!!!")
                          }
                        )
)
###############################################################################################################
#tato instancia bude vytvorena iba v listoch stromu
TreeNode <- setRefClass("TreeNode",
                        contains = "ITreeNode",
                        fields = c(
                          value = "numeric"
                        ),
                        methods = list(
                          predictOne = function(x) {
                            return(value)
                          },
                          printModel = function(m = ""){
                            print(paste(m,value))
                          },
                          variableImportance = function(data, YParam,lossFun = sse){
                            l = c()
                            
                            sendData = 0
                            sendData$value = lossFun(data[,YParam],mean(data[,YParam]))
                            
                            sendData$array = l
                            return(sendData)
                          }
                        )
)
###############################################################################################################
#predok pre vetvenie , nikdy nebude v liste
TreeNodeCompare <- setRefClass("TreeNodeCompare", 
                               contains = "TreeNode",
                               fields = c(
                                 param = "character",
                                 less = "TreeNode",
                                 equalesMore = "TreeNode"
                               ),
                               methods = list(
                                 printModel = function(m = ""){
                                   print(paste(m,param, getCompareValue()))
                                   less$printModel(paste(m," |"))
                                   equalesMore$printModel(paste(m,"  "))
                                 },
                                 getCompareValue = function(){return(NULL)},
                                 variableImportance = function(data, YParam,lossFun = sse){
                                   l = c()
                                   l2 = 0
                                   l3 = 0
                                   if(is.numeric(getCompareValue())){
                                     l2 = less$variableImportance(data[data[param] < compareValue,], YParam,lossFun)
                                     l3 = equalesMore$variableImportance(data[data[param] >= compareValue,], YParam,lossFun)
                                   }else{
                                     l2 = less$variableImportance(data[data[param] != compareValue,], YParam,lossFun)
                                     l3 = equalesMore$variableImportance(data[data[param] == compareValue,], YParam,lossFun)
                                   }
                                   
                                   sendData = 0
                                   sendData$value = lossFun(data[,YParam],mean(data[,YParam]))
                                   
                                   l[param] = sendData$value - l2$value - l3$value
                                   
                                   
                                   for (i in names(l2$array)) {
                                     if(!is.na(l[i])){
                                       l[i] = l[i] + l2$array[i]
                                     }else{
                                       l[i] = l2$array[i]
                                     }
                                   }
                                   for (i in names(l3$array)) {
                                     if(!is.na(l[i])){
                                       l[i] = l[i] + l3$array[i]
                                     }else{
                                       l[i] = l3$array[i]
                                     }
                                   }
                                   
                                   
                                   sendData$array = l
                                   return(sendData)
                                 }
                               )
)
###############################################################################################################
#vetvenie na zaklade numerickej hodnoty
TreeNodeNumeric <- setRefClass("TreeNodeNumeric", 
                               contains = "TreeNodeCompare",
                               fields = c(
                                 compareValue = "numeric"
                               ),
                               methods = list(
                                 predictOne = function(x) {
                                   if(x[param] < compareValue){
                                     return(less$predict(x))
                                   }else{
                                     return(equalesMore$predict(x))
                                   }
                                 },
                                 getCompareValue = function(){return(compareValue)}
                               )
)
#vetvenie podla klasifikacie (character, factor)
TreeNodeClassification <- setRefClass("TreeNodeClassification", 
                                      contains = "TreeNodeCompare",
                                      fields = c(
                                        compareValue = "character"
                                      ),
                                      methods = list(
                                        predictOne = function(x) {
                                          if(x[param] != compareValue){
                                            return(less$predict(x))
                                          }else{
                                            return(equalesMore$predict(x))
                                          }
                                        },
                                        getCompareValue = function(){return(compareValue)}
                                      )
)
TreeNodeClassificationF <- setRefClass("TreeNodeClassificationF", 
                                      contains = "TreeNodeCompare",
                                      fields = c(
                                        compareValue = "factor"
                                      ),
                                      methods = list(
                                        predictOne = function(x) {
                                          
                                          if(x[1,param] != compareValue){
                                            return(less$predict(x))
                                          }else{
                                            return(equalesMore$predict(x))
                                          }
                                        },
                                        getCompareValue = function(){return(compareValue)}
                                      )
)
###############################################################################################################
###############################################################################################################
#lesy
TreeNodeForest<- setRefClass("TreeNodeForest", 
                             contains = "ITreeNode",
                             fields = c(
                               n = "numeric",
                               trees = "list"
                             ),
                             methods = list(
                               predict = function(data) {
                                 pom = 0;
                                 for (i in 1:n) {
                                   pom = pom + (trees[[i]])$predict(data)
                                 }
                                 return(pom/n)
                               },
                               printModel = function(m = "") {
                                 for (i in 1:n) {
                                   print(i)
                                   (trees[[i]])$printModel()
                                 }
                               }
                             )
)

TreeNodeRandomForest<- setRefClass("TreeNodeRandomForest", 
                             contains = "TreeNodeForest",
                             fields = c(
                               errors = "vector"
                             ),
                             methods = list(
                               predict = function(data) {
                                 pom = 0;
                                 for (i in 1:n) {
                                   pom = pom + (trees[[i]])$predict(data) * errors[i]
                                 }
                                 return(pom)
                               },
                               printModel = function(m = "") {
                                 for (i in 1:n) {
                                   print(paste(i,errors[i]))
                                   (trees[[i]])$printModel()
                                 }
                               }
                             )
)

TreeNodeGradBoostForest<- setRefClass("TreeNodeGradBoostForest", 
                                   contains = "TreeNodeForest",
                                   fields = c(
                                     value = "numeric",
                                     learningValue = "numeric"
                                   ),
                                   methods = list(
                                     predict = function(data) {
                                       pom = value;
                                       for (i in 1:n) {
                                         pom = pom + (trees[[i]])$predict(data) * learningValue
                                       }
                                       return(pom)
                                     },
                                     printModel = function(m = "") {
                                       print(value)
                                       for (i in 1:n) {
                                         print(i)
                                         (trees[[i]])$printModel()
                                       }
                                     }
                                   )
)

###############################################################################################################
#    deklaracia funkcii
###############################################################################################################

mse <-function(a,b){
  return(sum((a-b)**2)/length(a))
}
sse <-function(a,b){
  return(sum((a-b)**2))
}
###############################################################################################################

#filtrovanie len potrebnych dat
filterData <- function(formula, data){
  data2 = model.frame(formula,data)
  
  for (i in 1:ncol(data2)) {
    c = data2[1,i]
    if(!is.numeric(c)&&!is.character(c)&&!is.factor(c)){
      stop("Bad columns!!!")
    } 
  }
  return(data2)
}

filterData = cmpfun(filterData)

###############################################################################################################
#rekurzivne vytvaranie rozdeleni
createTreeRec<-function(trainData, lossFun = sse, err = 0.5, maxK = 100, minGroupe = 1, penalty = 0,pomerPremenych =1){
  pocetPremenych= ncol(trainData) -1
  pocetPozorovani= nrow(trainData)
  
  premenne = sample(1:(pocetPremenych), max(1,pocetPremenych*pomerPremenych))
  
  YVector = trainData[,1]
  
  value = .Internal(mean(YVector))
  
  #podmienky pre ukoncenie vytvarania dalsieho vetvenia
  if(err >= lossFun(YVector, value) || pocetPozorovani <= minGroupe*2 || maxK == 0){
    return(TreeNode$new(value = value))
  }

  #matica indexov, zoradenie podla parametra
  indexMatrix = matrix(nrow = pocetPremenych, ncol = pocetPozorovani)

  #hladanie najlepsieho rozdelenia
  min = Inf
  index =0
  separate = 0
  pomValue = Inf
  pomFilter = trainData[,2]==trainData[1,2]
  for (i in premenne) {
    if(!is.numeric(trainData[,i+1])){
      grups = 1:1
      best = 0
      if(is.factor(trainData[,i+1])){
        grups = levels(trainData[,i+1])
      }else{
        grups = unique(trainData[,i+1])
      }
      max = 0
      pomM = 0
      for (j in grups) {
        pomFilter = trainData[,i+1]==j
        pomM = .Internal(mean(YVector[pomFilter]))
        
        if(!is.na(pomM)){# v pripade factora to filtruje aj podla skupin, ktore sa tu uznenachadzaju
          if(max < pomM){
            max = pomM
            best = j
          }
        }

      }
      
      pomFilter = trainData[,i+1]==best
      prvaPolovica = YVector[pomFilter]
      druhaPolovica = YVector[!pomFilter]
      mean1 = max
      mean2 = (value * pocetPozorovani - mean1*length(prvaPolovica))/length(druhaPolovica)
      pomValue = lossFun(prvaPolovica,mean1) + lossFun(druhaPolovica,mean2)
      
      if(min > pomValue ){
        min = pomValue
        index = i
        separate = match(best,trainData[,i+1])
      }
    }else{
      indexMatrix[i,] = order(trainData[,i+1])
      for (j in (minGroupe):(pocetPozorovani-minGroupe)) {
        if(trainData[indexMatrix[i,j],i+1]==trainData[indexMatrix[i,j+1],i+1]){
          
        }else{
          prvaPolovica = YVector[indexMatrix[i,1:j]]
          druhaPolovica = YVector[indexMatrix[i,(j+1):pocetPozorovani]]
          mean1 = .Internal(mean(prvaPolovica))
          mean2 = (value * pocetPozorovani - mean1*length(prvaPolovica))/length(druhaPolovica)
          pomValue = lossFun(prvaPolovica,mean1) + lossFun(druhaPolovica, mean2)
          
          if(min > pomValue ){
            min = pomValue
            index = i
            separate = j
          }
        }

      }
    }
  }

  #ukoncenie vetvenia(toto vetvenie by nevytvorilo lepsi vysledok)
  if(min + penalty >= lossFun(trainData[,1], value)){
    return(TreeNode$new(value = value))
  }

  #nastavenie vetvenia
  if(is.character(trainData[,index+1])){
    node = TreeNodeClassification$new(value = value)
    node$param = colnames(trainData)[index+1]
    node$compareValue = trainData[separate,node$param]
    
    node$less = createTreeRec(trainData[trainData[,node$param]!=node$compareValue,], lossFun, err, maxK -1,minGroupe,penalty,pomerPremenych)
    node$equalesMore = createTreeRec(trainData[trainData[,node$param]==node$compareValue,], lossFun, err,maxK -1,minGroupe,penalty,pomerPremenych)
  }else if(is.factor(trainData[,index+1])){
    node = TreeNodeClassificationF$new(value = value)
    node$param = colnames(trainData)[index+1]
    node$compareValue = trainData[separate,node$param]
    
    node$less = createTreeRec(trainData[trainData[,node$param]!=node$compareValue,], lossFun, err, maxK -1,minGroupe,penalty,pomerPremenych)
    node$equalesMore = createTreeRec(trainData[trainData[,node$param]==node$compareValue,], lossFun, err,maxK -1,minGroupe,penalty,pomerPremenych)
  }else{
    node = TreeNodeNumeric$new(value = value)
    node$param = colnames(trainData)[index+1]
    node$compareValue = (trainData[indexMatrix[index, separate],node$param]+trainData[indexMatrix[index, separate+1],node$param])/2
    
    node$less = createTreeRec(trainData[indexMatrix[index, 1:separate],], lossFun, err, maxK -1,minGroupe,penalty,pomerPremenych)
    node$equalesMore = createTreeRec(trainData[indexMatrix[index, (separate+1):pocetPozorovani],], lossFun, err,maxK -1,minGroupe,penalty,pomerPremenych)
  }
  
  return(node)
}


#'      Funkcia, ktora vytvory regresny model 
#' @param formula - zapisuje na v tvare "Y ~ X1 + X2 + ...",kde Y je zavisla premenna, a X1 ,X2 ,... su nezavysle premenne
#' @param data - trenovacie data
#' @param fun - funkcia, ktora pocita chybu, ktorej sa model dopusta pri rozdeleni dat
#' @param err - akceptovana chyby, ak je hodnota z fun nemcia, dane data sa dalej nerozdeluju
#' @param maxK - maximalna hlbka stromu
#' @param minGroupe - minimalna velkost mnoziny rozdelenych dat (aby nedoslo k pretrenovaniu)
#' zdroj : https://www.youtube.com/watch?v=g9c66TUylZ4&t=1111s
#' 
createTree <- function(formula, data, lossFun = sse, err = 0.5, maxK = 100, minGroupe = 1, penalty = 0,pomerPremenych = 1){
  
  #filtrovanie len potrebnych dat
  data2 = filterData(formula, data)
  
  return(createTreeRec(data2, lossFun, err,maxK ,minGroupe, penalty,pomerPremenych))
}

#install.packages("compiler", lib="G:/RLib")
library(compiler,lib.loc ="G:/RLib")

createTreeRec = cmpfun(createTreeRec)
createTree = cmpfun(createTree)
###############################################################################################################
createForest <- function(formula, data, lossFun = sse, err = 0.5, maxK = 100, minGroupe = 1, penalty = 0,n = 10, groupePomer = 0.8){
  
  #filtrovanie len potrebnych dat
  data2 = filterData(formula, data)
  
  forest = TreeNodeRandomForest$new(n = n, errors = 1:n)
  lengthD = nrow(data2)
  riadky = 1:(lengthD*groupePomer)#round(runif(lengthD*0.8,1,lengthD))
  test = data2[1:(lengthD*(1-groupePomer)),]
  sum = 0
  for (i in 1:n) {
    riadky = sample(1:lengthD, lengthD*groupePomer, replace=T)
    
    forest$trees[[i]] = createTreeRec(data2[riadky,], lossFun, err,maxK ,minGroupe,penalty,groupePomer)
    
    test = data2[!(1:lengthD %in% riadky),]

    forest$errors[i] = mse((forest$trees[[i]])$predict(test),test[,1])
    sum = sum + forest$errors[i]
  }
  
  forest$errors = ((sum-forest$errors)/sum)/(n-1)
  
  return(forest)
}

createForest = cmpfun(createForest)


createForestADABoost <- function(formula, data, lossFun = sse, err = 0.5, maxK = 1, minGroupe = 1, penalty = 0,n = 10){
  
  #filtrovanie len potrebnych dat
  data2 = filterData(formula, data)
  
  forest = TreeNodeForest$new(n = n)
  lengthD = nrow(data2)
  riadky = 1:(lengthD)#round(runif(lengthD*0.8,1,lengthD))

  p =  1:lengthD * 0 + 0.1
  f = log(p/(1-p))/2
  
  weightss = exp(f)
  weightss = weightss / sum(weightss)
  
  pred = 1:lengthD * 0
  
  prav = weightss
  for (i in 1:(lengthD-1)) {
    prav[i+1] = prav[i+1] + prav[i]
  }

  eps  = 1e-9
  for (i in 1:n) {
    
    riadky = sapply(runif(lengthD), FUN = function(x){return(Position(function(xx) xx> x,prav))})
    
    forest$trees[[i]] = createTreeRec(data2[riadky,], lossFun, err,maxK ,minGroupe,penalty)
    
    pred = (pred * (i-1) + (forest$trees[[i]])$predict(data2))/i
    
    
    p = abs(pred - data2[,1])/data2[,1] + eps
    p = 1-(p / (max(p)+eps))
    
    f = log(p/(1-p))/2
    
    weightss = weightss*exp(f)
    weightss = weightss / sum(weightss)
    
    prav = weightss
    for (i in 1:(lengthD-1)) {
      prav[i+1] = prav[i+1] + prav[i]
    }
  }
  
  return(forest)
}

createForestADABoost = cmpfun(createForestADABoost)


createForestGradienBoost <- function(formula, data, lossFun = sse, err = 0.5, maxK = 1, minGroupe = 1, penalty = 0,
                                     nIter = 10, learningValue = 0.1, gradienFunction = function(y,f) y-f ){
  
  #filtrovanie len potrebnych dat
  data2 = filterData(formula, data)
  
  y = data2[,1]
  
  forest = TreeNodeGradBoostForest$new(n = nIter, value = mean(y), learningValue = learningValue)
  
  lengthD = nrow(data2)
  pred = 1:lengthD * 0 + forest$value
  
  for (i in 1:nIter) {
    
    #nastavi vystupnu premennu na rezidua
    data2[,1] = gradienFunction(y,pred)
    
    forest$trees[[i]] = createTreeRec(data2, lossFun, err,maxK ,minGroupe,penalty)
    
    u = (forest$trees[[i]])$predict(data2)
    
    pred = pred + learningValue * u
    #print(paste(i,sum((y - pred)**2)))
  }
  
  return(forest)
}

createForestGradienBoost = cmpfun(createForestGradienBoost)


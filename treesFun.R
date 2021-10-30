###############################################################################################################
#    deklaracia tried
###############################################################################################################
#tato instancia bude vytvorena iba v listoch stromu
TreeNode <- setRefClass("TreeNode", 
                        fields = c(
                          value = "numeric"
                        ),
                        methods = list(
                          predictOne = function(x) {
                            return(value)
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
                            print(paste(m,value))
                          }
                        )
)

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
                                 getCompareValue = function(){return(NULL)}
                               )
)
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

###############################################################################################################
#    deklaracia funkcii
###############################################################################################################

mse <-function(a,b){
  return(sum((a-b)**2)/length(a))
}
sse <-function(a,b){
  return(sum((a-b)**2))
}

#rekurzivne vytvaranie rozdeleni
createTreeRec<-function(trainData, fun = sse, err = 0.5, maxK = 100, minGroupe = 1){
  pocetPremenych= ncol(trainData) -1
  pocetPozorovani= nrow(trainData)
  
  YVector = trainData[,1]
  
  value = .Internal(mean(YVector))
  
  #podmienky pre ukoncenie vytvarania dalsieho vetvenia
  if(err >= fun(YVector, value) || pocetPozorovani <= minGroupe*2 || maxK == 0){
    return(TreeNode$new(value = value))
  }

  #matica indexov, zoradenie podla parametra
  indexMatrix = matrix(nrow = pocetPremenych, ncol = pocetPozorovani)
  
  #hladanie najlepsieho rozdelenia
  min = Inf
  index =0
  separate = 0
  pomValue = Inf;
  for (i in 1:pocetPremenych) {
    if(is.character(trainData[,i+1])){
      bol = data.frame(1)
      for (j in (1):(pocetPozorovani)) {
        if(is.null(bol[1,trainData[j,i+1]])){
          pomFilter = trainData[,i+1]==trainData[j,i+1];
          prvaPolovica = YVector[pomFilter]
          druhaPolovica = YVector[!pomFilter]
          mean1 = .Internal(mean(prvaPolovica))
          mean2 = (value * pocetPozorovani - mean1*length(prvaPolovica))/length(druhaPolovica)
          bol[1,trainData[j,i+1]] = 1
          pomValue = fun(prvaPolovica,mean1) + fun(druhaPolovica,mean2)
          
          if(min > pomValue ){
            min = pomValue
            index = i
            separate = j
          }
        }
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
          pomValue = fun(prvaPolovica,mean1) + fun(druhaPolovica, mean2)
          
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
  if(min >= fun(trainData[,1], value)){
    return(TreeNode$new(value = value))
  }
  
  #nastavenie vetvenia
  if(is.character(trainData[,index+1])){
    node = TreeNodeClassification$new(value = value)
    node$param = colnames(trainData)[index+1]
    node$compareValue = trainData[separate,node$param]
    
    node$less = createTreeRec(trainData[trainData[,node$param]!=node$compareValue,], fun, err, maxK -1,minGroupe)
    node$equalesMore = createTreeRec(trainData[trainData[,node$param]==node$compareValue,], fun, err,maxK -1,minGroupe)
  }else{
    node = TreeNodeNumeric$new(value = value)
    node$param = colnames(trainData)[index+1]
    node$compareValue = (trainData[indexMatrix[index, separate],node$param]+trainData[indexMatrix[index, separate+1],node$param])/2
    
    node$less = createTreeRec(trainData[indexMatrix[index, 1:separate],], fun, err, maxK -1,minGroupe)
    node$equalesMore = createTreeRec(trainData[indexMatrix[index, (separate+1):pocetPozorovani],], fun, err,maxK -1,minGroupe)
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
createTree <- function(formula, data, fun = sse, err = 0.5, maxK = 100, minGroupe = 1){
  
  #filtrovanie len potrebnych dat
  data2 = model.frame(formula,data)
  
  for (i in 1:ncol(data2)) {
    c = data2[1,i]
    if(!is.numeric(c)&&!is.character(c)&&!is.factor(c)){
      stop("Bad columns!!!")
    } 
  }
  
  return(createTreeRec(data2, fun, err,maxK ,minGroupe))
}

#install.packages("compiler", lib="G:/RLib")
#library(compiler,lib.loc ="G:/RLib")

createTreeRec = cmpfun(createTreeRec)
createTree = cmpfun(createTree)


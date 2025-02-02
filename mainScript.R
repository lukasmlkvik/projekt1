
###############################################################################################################
#    testovanie modelu na datach
###############################################################################################################

#nacitanie a uprava modelu
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
titanic[1,]
titanic$age=strtoi(titanic$age)
titanic = titanic[!is.na(titanic$age),]
titanic$fare=strtoi(titanic$fare)
titanic[is.na(titanic$fare),"fare"] = 0
titanic$pclass = paste(titanic$pclass)

#rozdelenie na trenovacie a testovacie data
titanicRand = sample(titanic)
train = titanicRand[1:700,]
test = titanicRand[701:900,]

#vyhtorenie modelu a testovanie rychlosti
zaciatok = Sys.time()
model = createTree(survived ~ sex + age + pclass + fare, train,fun=sse,maxK=10,minGroupe = 20)
Sys.time() - zaciatok

model$printModel()

#vytvorenie predikcii na testovacich datach
vysledok = model$predict(test)

#kedze zavizla premenna, ktoru chceme zistit je bud 0 alebo 1, tak vysledok predikcie sa zaokruhli
pravdepodobnost = 0.5
vysledok[vysledok<pravdepodobnost] = 0
vysledok[vysledok>=pravdepodobnost] = 1

#kontrola presnosti modelu na testovacich datach
table(vysledok,test$survived)

#install.packages("bench", lib="G:/RLib")
#library(bench,lib.loc ="G:/RLib")
model = createForest(survived ~ sex + age + pclass + fare, train,lossFun=sse,maxK=10,minGroupe = 10,n=10, groupePomer=0.8)
model = createTree(survived ~ sex + age + pclass + fare, train,lossFun=sse,maxK=10,minGroupe = 10)
model = createForestADABoost(survived ~ sex + age + pclass + fare, train,lossFun=sse,maxK=1,minGroupe = 10,n=10)
model = createForestGradienBoost(survived ~ sex + age + pclass + fare, train,lossFun=sse,maxK=5,minGroupe = 20,n=10, learningValue = 0.1)
model = createForestGradienBoost(survived ~ sex + age + pclass + fare, train,lossFun=function(y,f)sum(abs(y - f)),maxK=5,minGroupe = 20,
                                 n=10, learningValue = 0.1, gradienFunction = function(y,f)sign(y - f))

model =  createForest(survived ~ ., train,lossFun=sse,maxK=2,minGroupe = 10,n=10, groupePomer=0.5)

model$predict(test) - model$predict2(test)

model$printModel()
model$variableImportance(train,"survived",sse)

system.time(
  replicate(n,createTree(survived ~ sex + age + pclass + fare, train,lossFun=sse,maxK=10,minGroupe = 20))
)


zaciatok = Sys.time()
model = createForest(survived ~ sex + age + pclass + fare, train,lossFun=sse,maxK=10,minGroupe = 10,n=20)
Sys.time() - zaciatok
model


path <- 'G:/projekt1/projekt1/winequality-red.csv'
wine <-read.csv(path,sep = ";")
wine
head(wine)
model = createTree(quality ~ ., wine,lossFun=sse,maxK=10,minGroupe = 20)
model = createForest(quality ~ ., wine,lossFun=sse,maxK=10,minGroupe = 20,n=20)
vysledok = round(model$predict(wine))
vysledok = round(model$predict2(wine))

table(vysledok,wine$quality)


install.packages("gbm", lib="G:/RLib")
library(gbm,lib.loc ="G:/RLib")



titanic <-read.csv(path)
titanic[1,]
titanic$age=strtoi(titanic$age)
titanic = titanic[!is.na(titanic$age),]
titanic$fare=strtoi(titanic$fare)
titanic[is.na(titanic$fare),"fare"] = 0
titanic$pclass = as.factor(titanic$pclass)
titanic$sex = as.factor(titanic$sex)

#rozdelenie na trenovacie a testovacie data
titanicRand = sample(titanic)
train = titanicRand[1:700,]
test = titanicRand[701:900,]

a = gbm(survived ~ sex + age + pclass + fare,data =train, n.trees = 100)

summary(a)
vysledok = predict(a, newdata = test,n.trees = 10)

model = createForestGradienBoost(survived ~ sex + age + pclass + fare, train,lossFun=sse,maxK=1,minGroupe = 20,n=10, learningValue = 0.1)



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
vysledok  = model$predict(test)
#kedze zavizla premenna, ktoru chceme zistit je bud 0 alebo 1, tak vysledok predikcie sa zaokruhli
pravdepodobnost = 0.5
vysledok[vysledok<pravdepodobnost] = 0
vysledok[vysledok>=pravdepodobnost] = 1

#kontrola presnosti modelu na testovacich datach
table(vysledok,test$survived)

#install.packages("bench", lib="G:/RLib")
#library(bench,lib.loc ="G:/RLib")
model =  createForest(survived ~ sex + age + pclass + fare, train,fun=sse,maxK=10,minGroupe = 10,n=10)

system.time(
  replicate(n,createTree(survived ~ sex + age + pclass + fare, train,fun=sse,maxK=10,minGroupe = 20))
)

zaciatok = Sys.time()
model = createForest(survived ~ sex + age + pclass + fare, train,fun=sse,maxK=10,minGroupe = 10,n=20)
Sys.time() - zaciatok
model


path <- 'G:/projekt1/projekt1/winequality-red.csv'
wine <-read.csv(path,sep = ";")
wine
head(wine)
model = createTree(quality ~ ., wine,fun=sse,maxK=10,minGroupe = 20)
model = createForest(quality ~ ., wine,fun=sse,maxK=10,minGroupe = 20,n=20)
vysledok = round(model$predict(wine))

table(vysledok,wine$quality)


## Load data and delete description data
data = read.csv('SEAHI.csv',header=T)
colnames(data)[1] = 'Duration'
data = data[-c(6,14,17)]
data[c('Asian', 'Black', 'White', 'Other','D12_1','D12_2','D12_3','D13_1','D13_2','D13_3')][is.na(data[c('Asian', 'Black', 'White', 'Other','D12_1','D12_2','D12_3','D13_1','D13_2','D13_3')])] = 0
survey1 = data[23:101]
s1noNA = survey1[rowSums(is.na(survey1)) == 0,]

## Slice data of 33 variables selected by EFA
EFA = data[, c('seahi6_b', 'seahi7_b' , 'seahi8_b' , 'seahi9_b' , 'seahi10_b' , 'seahi11_b' , 'seahiaction10_b' , 'seahiaction11_b' , 
               'seahiaction12_b' , 'seahiaction19_b' , 'seahiaction20_b' , 'seahiaction21_b' , 'seahiaction22_b' , 'chatc101_b' , 
               'chatc102_b' , 'chatc103_b' , 'chatc118_b' , 'chatc1211_b' , 'chatc1215_b' , 'chatc1216_b' , 'chatc1217_b' , 'chatc13a7_b' ,
               'chatc13a10_b' , 'chatc13a13_b' , 'chatc13a15_b' , 'chatc14a2_b' , 'chatc14a7_b' , 'chatc15a1_b' , 'chatc15a6_b' , 'chatc15a9_b'
               , 'chatc16a4_b' , 'chatc16a5_b' , 'chatc16a6_b')]
EFA.noNA = EFA[rowSums(is.na(EFA)) == 0, ]

##Random forest clustering
library(randomForest)
rf.fit = randomForest(EFA.noNA, proximity = T, oob.prox = T)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster7 = cutree(hclust.rf, k=6)
g1 = EFA.noNA[rf.cluster7 == 1,]
g2 = EFA.noNA[rf.cluster7 == 2,]
g3 = EFA.noNA[rf.cluster7 == 3,]
g4 = EFA.noNA[rf.cluster7 == 4,]
g5 = EFA.noNA[rf.cluster7 == 5,]
g6 = EFA.noNA[rf.cluster7 == 6,]

l1 = lapply(g1, function(x) as.data.frame(table(x)))
l2 = lapply(g2, function(x) as.data.frame(table(x)))
l3 = lapply(g3, function(x) as.data.frame(table(x)))
l4 = lapply(g4, function(x) as.data.frame(table(x)))
l5 = lapply(g5, function(x) as.data.frame(table(x)))
l6 = lapply(g6, function(x) as.data.frame(table(x)))

rf6 = list()
names = c('seahi6_b', 'seahi7_b' , 'seahi8_b' , 'seahi9_b' , 'seahi10_b' , 'seahi11_b' , 'seahiaction10_b' , 'seahiaction11_b' , 
          'seahiaction12_b' , 'seahiaction19_b' , 'seahiaction20_b' , 'seahiaction21_b' , 'seahiaction22_b' , 'chatc101_b' , 
          'chatc102_b' , 'chatc103_b' , 'chatc118_b' , 'chatc1211_b' , 'chatc1215_b' , 'chatc1216_b' , 'chatc1217_b' , 'chatc13a7_b' ,
          'chatc13a10_b' , 'chatc13a13_b' , 'chatc13a15_b' , 'chatc14a2_b' , 'chatc14a7_b' , 'chatc15a1_b' , 'chatc15a6_b' , 'chatc15a9_b'
          , 'chatc16a4_b' , 'chatc16a5_b' , 'chatc16a6_b')
for (i in 1:33) {
  colnames(l1[[i]]) = c('x', 1)
  colnames(l2[[i]]) = c('x', 2)
  colnames(l3[[i]]) = c('x', 3)
  colnames(l4[[i]]) = c('x', 4)
  colnames(l5[[i]]) = c('x', 5)
  colnames(l6[[i]]) = c('x', 6)
  a = merge(l1[[i]], l2[[i]], all = T, by = 'x')
  a = merge(a, l3[[i]], all = T, by = 'x')
  a = merge(a, l4[[i]], all = T, by = 'x')
  a = merge(a, l5[[i]], all = T, by = 'x')
  a = merge(a, l6[[i]], all = T, by = 'x')
  a[,1] = as.numeric(as.character(a[,1]))
  rf6[[names[i]]] = a[order(a[,'x']),]
}

##Kmeans with cluster 7
km.fit = kmeans(EFA.noNA, 7)
kcluster = km.fit$cluster
k1 = EFA.noNA[kcluster == 1,]
k2 = EFA.noNA[kcluster == 2,]
k3 = EFA.noNA[kcluster == 3,]
k4 = EFA.noNA[kcluster == 4,]
k5 = EFA.noNA[kcluster == 5,]
k6 = EFA.noNA[kcluster == 6,]
k7 = EFA.noNA[kcluster == 7,]

l1 = lapply(k1, function(x) as.data.frame(table(x)))
l2 = lapply(k2, function(x) as.data.frame(table(x)))
l3 = lapply(k3, function(x) as.data.frame(table(x)))
l4 = lapply(k4, function(x) as.data.frame(table(x)))
l5 = lapply(k5, function(x) as.data.frame(table(x)))
l6 = lapply(k6, function(x) as.data.frame(table(x)))
l7 = lapply(k7, function(x) as.data.frame(table(x)))

kmeans7 = list()
names = c('seahi6_b', 'seahi7_b' , 'seahi8_b' , 'seahi9_b' , 'seahi10_b' , 'seahi11_b' , 'seahiaction10_b' , 'seahiaction11_b' , 
          'seahiaction12_b' , 'seahiaction19_b' , 'seahiaction20_b' , 'seahiaction21_b' , 'seahiaction22_b' , 'chatc101_b' , 
          'chatc102_b' , 'chatc103_b' , 'chatc118_b' , 'chatc1211_b' , 'chatc1215_b' , 'chatc1216_b' , 'chatc1217_b' , 'chatc13a7_b' ,
          'chatc13a10_b' , 'chatc13a13_b' , 'chatc13a15_b' , 'chatc14a2_b' , 'chatc14a7_b' , 'chatc15a1_b' , 'chatc15a6_b' , 'chatc15a9_b'
          , 'chatc16a4_b' , 'chatc16a5_b' , 'chatc16a6_b')
for (i in 1:33) {
  colnames(l1[[i]]) = c('x', 1)
  colnames(l2[[i]]) = c('x', 2)
  colnames(l3[[i]]) = c('x', 3)
  colnames(l4[[i]]) = c('x', 4)
  colnames(l5[[i]]) = c('x', 5)
  colnames(l6[[i]]) = c('x', 6)
  colnames(l7[[i]]) = c('x', 7)
  a = merge(l1[[i]], l2[[i]], all = T, by = 'x')
  a = merge(a, l3[[i]], all = T, by = 'x')
  a = merge(a, l4[[i]], all = T, by = 'x')
  a = merge(a, l5[[i]], all = T, by = 'x')
  a = merge(a, l6[[i]], all = T, by = 'x')
  a = merge(a, l7[[i]], all = T, by = 'x')
  a[,1] = as.numeric(as.character(a[,1]))
  kmeans7[[names[i]]] = a[order(a[,'x']),]
}

##Kmeans with cluster 6
km.fit = kmeans(EFA.noNA, 6)
kcluster = km.fit$cluster
k1 = EFA.noNA[kcluster == 1,]
k2 = EFA.noNA[kcluster == 2,]
k3 = EFA.noNA[kcluster == 3,]
k4 = EFA.noNA[kcluster == 4,]
k5 = EFA.noNA[kcluster == 5,]
k6 = EFA.noNA[kcluster == 6,]

l1 = lapply(k1, function(x) as.data.frame(table(x)))
l2 = lapply(k2, function(x) as.data.frame(table(x)))
l3 = lapply(k3, function(x) as.data.frame(table(x)))
l4 = lapply(k4, function(x) as.data.frame(table(x)))
l5 = lapply(k5, function(x) as.data.frame(table(x)))
l6 = lapply(k6, function(x) as.data.frame(table(x)))

kmeans6 = list()
names = c('seahi6_b', 'seahi7_b' , 'seahi8_b' , 'seahi9_b' , 'seahi10_b' , 'seahi11_b' , 'seahiaction10_b' , 'seahiaction11_b' , 
          'seahiaction12_b' , 'seahiaction19_b' , 'seahiaction20_b' , 'seahiaction21_b' , 'seahiaction22_b' , 'chatc101_b' , 
          'chatc102_b' , 'chatc103_b' , 'chatc118_b' , 'chatc1211_b' , 'chatc1215_b' , 'chatc1216_b' , 'chatc1217_b' , 'chatc13a7_b' ,
          'chatc13a10_b' , 'chatc13a13_b' , 'chatc13a15_b' , 'chatc14a2_b' , 'chatc14a7_b' , 'chatc15a1_b' , 'chatc15a6_b' , 'chatc15a9_b'
          , 'chatc16a4_b' , 'chatc16a5_b' , 'chatc16a6_b')
for (i in 1:33) {
  colnames(l1[[i]]) = c('x', 1)
  colnames(l2[[i]]) = c('x', 2)
  colnames(l3[[i]]) = c('x', 3)
  colnames(l4[[i]]) = c('x', 4)
  colnames(l5[[i]]) = c('x', 5)
  colnames(l6[[i]]) = c('x', 6)
  a = merge(l1[[i]], l2[[i]], all = T, by = 'x')
  a = merge(a, l3[[i]], all = T, by = 'x')
  a = merge(a, l4[[i]], all = T, by = 'x')
  a = merge(a, l5[[i]], all = T, by = 'x')
  a = merge(a, l6[[i]], all = T, by = 'x')
  a[,1] = as.numeric(as.character(a[,1]))
  kmeans6[[names[i]]] = a[order(a[,'x']),]
}

##Kmeans with cluster 5
km.fit_5 = kmeans(EFA.noNA,5 )
kcluster_5 = km.fit_5$cluster
k5_1 = EFA.noNA[kcluster_5 == 1,]
k5_2 = EFA.noNA[kcluster_5 == 2,]
k5_3 = EFA.noNA[kcluster_5 == 3,]
k5_4 = EFA.noNA[kcluster_5 == 4,]
k5_5 = EFA.noNA[kcluster_5 == 5,]


l5_1 = lapply(k5_1, function(x) as.data.frame(table(x)))
l5_2 = lapply(k5_2, function(x) as.data.frame(table(x)))
l5_3 = lapply(k5_3, function(x) as.data.frame(table(x)))
l5_4 = lapply(k5_4, function(x) as.data.frame(table(x)))
l5_5 = lapply(k5_5, function(x) as.data.frame(table(x)))
kmeans5 = list()
for (i in 1:33) {
  colnames(l5_1[[i]]) = c('x', 1)
  colnames(l5_2[[i]]) = c('x', 2)
  colnames(l5_3[[i]]) = c('x', 3)
  colnames(l5_4[[i]]) = c('x', 4)
  colnames(l5_5[[i]]) = c('x', 5)
  a = merge(l5_1[[i]], l5_2[[i]], all = T, by = 'x')
  a = merge(a, l5_3[[i]], all = T, by = 'x')
  a = merge(a, l5_4[[i]], all = T, by = 'x')
  a = merge(a, l5_5[[i]], all = T, by = 'x')
  a[,1] = as.numeric(as.character(a[,1]))
  kmeans5[[names[i]]] = a[order(a[,'x']),]
}

##Kmeans with cluster 4
km.fit_4 = kmeans(EFA.noNA,4)
kmeansAIC(kmeans(EFA.noNA,4))
kcluster_4 = km.fit_4$cluster
k4_1 = EFA.noNA[kcluster_4 == 1,]
k4_2 = EFA.noNA[kcluster_4 == 2,]
k4_3 = EFA.noNA[kcluster_4 == 3,]
k4_4 = EFA.noNA[kcluster_4 == 4,]


l4_1 = lapply(k4_1, function(x) as.data.frame(table(x)))
l4_2 = lapply(k4_2, function(x) as.data.frame(table(x)))
l4_3 = lapply(k4_3, function(x) as.data.frame(table(x)))
l4_4 = lapply(k4_4, function(x) as.data.frame(table(x)))
kmeans4 = list()
for (i in 1:33) {
  colnames(l4_1[[i]]) = c('x', 1)
  colnames(l4_2[[i]]) = c('x', 2)
  colnames(l4_3[[i]]) = c('x', 3)
  colnames(l4_4[[i]]) = c('x', 4)
  a = merge(l4_1[[i]], l4_2[[i]], all = T, by = 'x')
  a = merge(a, l4_3[[i]], all = T, by = 'x')
  a = merge(a, l4_4[[i]], all = T, by = 'x')
  a[,1] = as.numeric(as.character(a[,1]))
  kmeans4[[names[i]]] = a[order(a[,'x']),]
}

##Calculate Gmax for LCA
library(LCAvarsel)
maxG(EFA.noNA, 1:10)

##Function to calculate AIC and BIC for kmeans
kmeansAIC = function(fit){
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + 2*m*k)
}

kmeansBIC = function(fit){
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + log(n)*m*k)
}

bic = c()
for (i in 1:20){
  temp = kmeansBIC(kmeans(EFA.noNA,i))
  bic = c(bic, temp)
}

##Plot bic for kmeans with clusters from 1:20
library(ggplot2)
data = data.frame(x = 1:20, bic = bic)
ggplot(data = data, aes(x = x, y = bic)) +   geom_point()+  geom_line(color = 'blue')


##Draw boxplot for raw data
data= data.frame(qs = factor(rep(colnames(survey1)[1:5], each=514)), 
                 rating = c(as.matrix(survey1[,1:5])))
ggplot(data = data, aes(x = qs, y = rating, fill = qs))+geom_boxplot()

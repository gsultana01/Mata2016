install.packages("vegan")
library(vegan)
Mata=as.data.frame(dataset_Mata_et_al_2016)
Mata = Mata[Mata$`Wingspan (mm)`!="na", ]
#this creates the linear matrix 
reads = table(Mata$Sample, Mata$Species)


for(i in 1:nrow(Mata)) {
  reads[as.character(Mata[i, "Sample"]), Mata[i, "Species"]] = Mata[i, "No. Reads"]
}

reads = sqrt(reads)
plot(hclust(dist(reads)), cex = 0.5)

p = princomp(reads)

males = array(data = 0, dim = length(unique(Mata$Sample)), dimnames = list(as.character(unique(Mata$Sample))))
adults = array(data = 0, dim = length(unique(Mata$Sample)), dimnames = list(as.character(unique(Mata$Sample))))

for(i in 1:nrow(Mata)) {
  if(Mata[i, "Sex"] == "M")
    males[as.character(Mata[i, "Sample"])] = 1
  if(Mata[i, "Age"] == "Ad")
    adults[as.character(Mata[i, "Sample"])] = 1
}

plot(p$scores, col=hsv(h = males/2))

table(Mata$Sample)
table(Mata$Species)

p = princomp(reads)
#PCA

k = cmdscale(vegdist(reads))
#principal coordinate analysis 
plot(k, pch = 19, col=hsv(h = males/2))
summary(glm(males ~ k[,1], family = "binomial"))
#this was a binomial regression 
model = (glm(males ~ k[,1], family = "binomial"))
1-(model$deviance/model$null.deviance)
plot(k[,1], males)
m = reads
m[m>0] = 1
#this turned the data into a binary 
k = cmdscale(vegdist(m))
plot(k, pch = 19, col=hsv(h = males/2))
#plotted the binary data
model = (glm(males ~ k[,1], family = "binomial"))
1-(model$deviance/model$null.deviance)
#this is a pseudo R^2

nrow(Mata)
sum(Mata$`Wingspan (mm)`=="na")
t = table(Mata$Species, Mata$`Wingspan (mm)`)
dim(t)
wingspan = array(dim=ncol(reads), dimnames = list(colnames(reads)))
for(i in 1:nrow(Mata))
  wingspan[Mata$Species[i]] = Mata$`Wingspan (mm)`[i]
wingspan = as.numeric(wingspan)

k = cmdscale(vegdist(t(reads)), k = 4)
plot(k, cex = wingspan/40)
plot(k[,4], wingspan)                 

summary(lm(wingspan ~ poly(k[,1], 2)))
#there is a non random pattern --> bats eat smaller bugs preferentially 

dim(m)
length(wingspan)
wm = wingspan*m
av = rowSums(wm)/rowSums(m)
model = (glm(males ~ as.numeric(av), family = "binomial"))
plot(av, males)
summary(model)
#males and females eat the same sized bugs

model = (glm(adults ~ as.numeric(av), family = "binomial"))
summary(model)
plot(av, adults)
1-(model$deviance/model$null.deviance)
#adults eat larger insects than juveniles 
# Praktikum APG
# ANALISIS KORESPONDENSI

#Load Packages
library(ca)
library(Hmisc)

data= read.delim("clipboard")
data

#Summary data
describe(data)
head(data)

#Preparation data
data.ca = matrix(data[,3], byrow = T, ncol = 5, nrow = 10,
                 dimnames = list(Usia = c("15- 19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49","50-54","55-59","60+"),
                                 Status = c("Bekerja", "TPB", "SKLH", "IRT", "DLL")))
data.ca

#Data independent test
chisq.test(data.ca)

#Hypotesist
# Look for row profile and column profile
rowtot = apply(data.ca, 1, sum)
coltot = apply(data.ca, 2, sum)
rowtot
coltot

#proportion
rowprof = sweep(data.ca, 1, rowtot, "/")
colprof = sweep(data.ca, 2, coltot, "/")
rowprof
colprof

#Correspondence Analysis
Sector.Ekonomi.ca = ca(data.ca)
Sector.Ekonomi.ca
summary(Sector.Ekonomi.ca)

#Row and Column Coordinates
Sector.Ekonomi.ca$rowcoord   #row 
Sector.Ekonomi.ca$colcoord   #Column

#The plot
plot(Sector.Ekonomi.ca, what = c("all", "all"), mass = TRUE,
     contrib = "relative", main =  "Title")



#__________________________________________________________________
#BIPLOT

install.packages("roxygen2")
install.packages("devtools")
install.packages("caret")
install.packages("ggplot2")
dirPkgs <- "D:/ggbiplot-master"
setwd(dirPkgs)
getwd() 
install_github("vqv/ggbiplot")
library(roxygen2)
library(devtools)
roxygenize(clean = TRUE) 
sum(lengths(sapply(list.files("R", full.names = TRUE), tools::showNonASCIIfile))) 
build_manual() 
check()
build()
install()

#Loads Packages
library(devtools)
library(ggbiplot)
library(Hmisc)
library(caret)

#Load data from clipboard
data7 = read.delim("clipboard")
data7

#Check Data type, data view, summary data 
str(data7)
head(data7)
summary(data7)

#data Normalitation 
data.normal = scale(data7[,-1])

#Partial Component Analysis
data.pca = prcomp(data.normal, center = FALSE)
data.pca
summary(data.pca)


#Plot biplot
biplot(data.pca, scale = 0, cex = 0.7)
g = ggbiplot(data.pca, obs.scale = 1, var.scale = 1, labels = (data7[,1]),
             ellipse = TRUE,
             circle = TRUE)
#g = g + scale_color_discrete(name = '')
#g = g + theme(legend.direction = 'horizontal',
#legend.position = 'top')
print(g)



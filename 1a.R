
library("arules")
library("arulesViz")
#import data
data.src <- read.csv("C:/Users/PC/Desktop/DATA/GroceriesInitial.csv",header=TRUE,sep=",")
str(data.src)

# transforming of data
product_names <- levels(unlist(data.src[,4:35]))
blank <- which(product_names == "") #row 102
#product_names <- product_names[-c(blank)]

#products that we need
citrus_fruit<- which(product_names == "citrus fruit")
tropical_fruit<- which(product_names == "tropical fruit")
whole_milk<-which(product_names == "whole milk")
other_vegetables<-which(product_names == "other vegetables")
rolls_buns<-which(product_names == "rolls/buns")
chocolate<-which(product_names == "chocolate")
bottled_water<-which(product_names == "bottled water")
yogurt<-which(product_names == "yogurt")
sausage<-which(product_names == "sausage")
root_vegetables<-which(product_names == "root vegetables")
pastry <- which(product_names == "pastry")  
soda <- which(product_names == "soda") 
cream <- which(product_names == "cream") 

#a vector with the products that we need
vectorofproducts=c(citrus_fruit,tropical_fruit,whole_milk,other_vegetables,rolls_buns,chocolate,bottled_water,yogurt,sausage,root_vegetables,pastry,soda,cream)
#the products tha we need
product_names<-product_names[vectorofproducts]

#c(1,2,3) %in% c(3,4,5,8) in:binary operator
#%in% returns a table with true or false
#binary table without headers
products <- as.data.frame(t(apply(data.src[,4:35],1, function(x) 
(product_names) %in% as.character(unlist(x)))))

#here we define the headers and create a table without the qualitative basket values
names(products) <- product_names
groceries <- cbind(data.src[,1:3],products)

# Discretizing
data_discrete <- groceries
cut_points <- quantile(data_discrete$basket_value
                       , probs = c(0,0.33, 0.66,1)
                       , na.rm = TRUE
                       ,names = FALSE)

data_discrete$basket_value_bin <- cut(data_discrete$basket_value
                                      ,breaks = cut_points,labels=c("Low","Medium","High"),include.lowest = TRUE) 
table(data_discrete$basket_value_bin)

#
#below, we produce the binary form of the new discredited variables
binarize <-function(data_columns,extra_columns=NULL){
  
  column_names <- levels(unlist(data_columns))
  cat(column_names)
  blank <- which(column_names == "") # Remove blank columns
  if (length(blank) !=0)
    column_names <- column_names[-c(blank)]
  
  binary_result <- as.data.frame(t(apply(data_columns,1
  ,function(x) column_names %in% as.character(unlist(x)))))
  names(binary_result) <- column_names
  if (is.null(extra_columns)==FALSE)
    binary_result<- cbind(extra_columns,binary_result)
  return(binary_result)
}

# Converting basket_value_bin to binary format 
data_discrete <- binarize(as.data.frame(data_discrete$basket_value_bin),data_discrete) 

data_discrete <- 
data_discrete[,-c(which(colnames(data_discrete)=="basket_value_bin"))] # Remove the non-binary column
View(data_discrete)

############EXERCISE 2############

#PART A)

#classification of rules by their support, first we define the min number of rules minlen
#after we can make a graphical reproduction
rules <- apriori(data_discrete[,4:ncol(data_discrete)] 
                 ,parameter = list(minlen=2, supp=0.001, conf=1) 
                 ,control = list(verbose=FALSE))
plot(rules, method="grouped") DynamicNetPlot(rules)

#rules <- apriori(data_discrete[,4:ncol(data_discrete)] ,parameter = list(minlen=2,  supp=0.01, conf=1) ,control = list(verbose=FALSE)) 
rules_sorted <- sort(rules, by="support")
inspect(rules.sorted)
#rules <- apriori(data_discrete[,4:ncol(data_discrete)] ,parameter = list(minlen=2, supp=0.005, conf=1) ,control = list(verbose=FALSE)) 
rules_sorted <- sort(rules, by="support")
inspect(rules.sorted)
#plot(rules, method="graph", control=list(reorder=TRUE))

#PART B)
rules <- apriori(data_discrete[,4:16],parameter = list(minlen=2, supp=0.001))
rules_sorted <- sort(rules, by="confidence",decreasing=TRUE)
inspect(rules_sorted[1:20])#displays the top 20 rules


plot(rules)
lot(rules, method="grouped")
plot(rules, method="graph", control=list(type="items"))

#PART C)
rules <- apriori(data_discrete[,4:ncol(data_discrete)],parameter = list(minlen=2, supp=0.01))
rules_sorted <- sort(rules, by="conf",decreasing=TRUE)
inspect(rules_sorted[1:20])


plot(rules, method="grouped")
plot(rules, method="graph", control=list(type="items"))















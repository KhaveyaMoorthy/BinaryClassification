entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}


IG_cat<-function(data,feature,target){
  #Strip out rows where feature is NA
  data<-data[!is.na(data[,feature]),] 
  #use dplyr to compute e and p for each value of the feature
  dd_data <- data %>% group_by_at(feature) %>% summarise(e=entropy(get(target)), 
                                                         n=length(get(target))
  )
  
  #compute entropy for the parent
  e0<-entropy(data[,target])
  #calculate p for each value of feature
  dd_data$p<-dd_data$n/nrow(data)
  #compute IG
  IG<-e0-sum(dd_data$p*dd_data$e)
  
  return(IG)
}
train<-read.csv("training.csv",header = T,sep=";")
names(train)
IG_cat(train, "variable1", "classLabel")
IG_cat(train, "variable2", "classLabel")
IG_cat(train, "variable3", "classLabel")
IG_cat(train, "variable4", "classLabel")
IG_cat(train, "variable5", "classLabel")
IG_cat(train, "variable6", "classLabel")
IG_cat(train, "variable7", "classLabel")
IG_cat(train, "variable8", "classLabel")
IG_cat(train, "variable9", "classLabel")
IG_cat(train, "variable10", "classLabel")
IG_cat(train, "variable11", "classLabel")
IG_cat(train, "variable12", "classLabel")
IG_cat(train, "variable13", "classLabel")
IG_cat(train, "variable14", "classLabel")
IG_cat(train, "variable15", "classLabel")
IG_cat(train, "variable17", "classLabel")
IG_cat(train, "variable18", "classLabel")
IG_cat(train, "variable19", "classLabel")


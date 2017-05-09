#PCA alone + Random Forest

library(caret)
library(stats)

train <- read.csv("C:/Users/renuka/Desktop/Desktop/R working dir/train.csv")

train$id=factor(train$id)
train$species=factor(train$species)


#normalize the data for continuous features between 0 and 1

shape <- train[ , grepl( "shape" , names( train ) ) ]
margin <- train[ , grepl( "margin" , names( train ) ) ]
texture <- train[ , grepl( "texture" , names( train ) ) ]

normalize <- function(x) {
  
  return ((x - min(x)) / (max(x) - min(x)))
  
}

normalized_shape <- as.data.frame(lapply(shape, normalize))
normalized_margin <- as.data.frame(lapply(margin, normalize))
normalized_texture <- as.data.frame(lapply(texture, normalize))

train_data <- data.frame(train$id,normalized_margin,normalized_shape,normalized_texture,train$species)

names(train_data)[1] <- paste("id")
names(train_data)[194] <- paste("species")

train <- train_data [,-1]


shape <- train[ , grepl( "shape" , names( train ) ) ]
margin <- train[ , grepl( "margin" , names( train ) ) ]
texture <- train[ , grepl( "texture" , names( train ) ) ]


pca_margin<-princomp(margin,cor=TRUE,score=TRUE)
pca_shape<-princomp(shape,cor=TRUE,score=TRUE)
pca_texture<-princomp(texture,cor=TRUE,score=TRUE)

train_pca <- data.frame(train$species,pca_margin$scores[,1:25],pca_shape$scores[,1:10],pca_texture$scores[,1:25])

colnames(train_pca)<-c('species','Com1','Com2','Com3','Com4','Com5','Com6','Com7','Com8','Com9','Com10','Com11','Com12',
                       'Com13','Com14','Com15','Com16','Com17','Com18','Com19','Com20','Com21','Com22','Com23','Com24',
                       'Com25','Com26','Com27','Com28','Com29','Com30','Com31','Com32','Com33','Com34','Com35','Com36',
                       'Com37','Com38','Com39','Com40','Com41','Com42','Com43','Com44','Com45','Com46','Com47','Com48',
                       'Com49','Com50','Com51','Com52','Com53','Com54','Com55','Com56','Com57','Com58','Com59','Com60')


randomForest_train <- train_pca

library(randomForest)

rf.model <- randomForest(species ~ Com1+Com2+Com3+Com4+Com5+Com6+Com7+Com8+Com9+Com10+Com11+Com12+Com13+Com14+Com15+Com16+Com17+Com18+Com19+Com20+Com21+Com22+Com23+Com24+Com25+Com26+Com27+Com28+Com29+Com30+Com31+Com32+Com33+Com34+Com35+Com36+Com37+Com38+Com39+Com40+Com41+Com42+Com43+Com44+Com45+Com46+Com47+Com48+Com49+Com50+Com51+Com52+Com53+Com54+Com55+Com56+Com57+Com58+Com59+Com60 ,  data=randomForest_train, ntree = 500, importance=TRUE, proximity=TRUE)






test <- read.csv("C:/Users/renuka/Desktop/Desktop/R working dir/test.csv")

shape <- test[ , grepl( "shape" , names( test ) ) ]
margin <- test[ , grepl( "margin" , names( test ) ) ]
texture <- test[ , grepl( "texture" , names( test ) ) ]

#normalized_shape <- as.data.frame(lapply(shape, normalize))
#normalized_margin <- as.data.frame(lapply(margin, normalize))
#normalized_texture <- as.data.frame(lapply(texture, normalize))


#ann_test <- data.frame(normalized_margin[,1:25],normalized_shape[,1:10],normalized_texture[,1:25])
rf_test <- data.frame(margin[,1:25],shape[,1:10],texture[,1:25])

colnames(rf_test)<-c('Com1','Com2','Com3','Com4','Com5','Com6','Com7','Com8','Com9','Com10','Com11','Com12',
                     'Com13','Com14','Com15','Com16','Com17','Com18','Com19','Com20','Com21','Com22','Com23','Com24',
                     'Com25','Com26','Com27','Com28','Com29','Com30','Com31','Com32','Com33','Com34','Com35','Com36',
                     'Com37','Com38','Com39','Com40','Com41','Com42','Com43','Com44','Com45','Com46','Com47','Com48',
                     'Com49','Com50','Com51','Com52','Com53','Com54','Com55','Com56','Com57','Com58','Com59','Com60')



rf.pred <- predict(rf.model, rf_test,type="prob")


result <- data.frame (test$id,rf.pred)

library(xlsx)
write.xlsx(x=result,file= "C:/Users/renuka/Desktop/Desktop/R working dir/randomForestPCA.xlsx", row.names = FALSE)
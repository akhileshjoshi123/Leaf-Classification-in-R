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

train_data <- train_data [,-1]

#Non - Zero Variance
nzv <- nearZeroVar(train_data)


filteredData_nzv <- train_data[, -nzv]



train_corr <- cor(filteredData_nzv[,-ncol(filteredData_nzv)])



high_Correlation <- findCorrelation(train_corr, cutoff = .75)
filteredData_HighCorr <- filteredData_nzv[,-high_Correlation]

train <- filteredData_HighCorr
shape <- train[ , grepl( "shape" , names( train ) ) ]
margin <- train[ , grepl( "margin" , names( train ) ) ]
texture <- train[ , grepl( "texture" , names( train ) ) ]


pca_margin<-princomp(margin,cor=TRUE,score=TRUE)
pca_shape<-princomp(shape,cor=TRUE,score=TRUE)
pca_texture<-princomp(texture,cor=TRUE,score=TRUE)

train_pca <- data.frame(train$species,pca_margin$scores[,1:25],pca_shape$scores,pca_texture$scores[,1:25])

colnames(train_pca)<-c('species','Com1','Com2','Com3','Com4','Com5','Com6','Com7','Com8','Com9','Com10','Com11','Com12',
                       'Com13','Com14','Com15','Com16','Com17','Com18','Com19','Com20','Com21','Com22','Com23','Com24',
                       'Com25','Com26','Com27','Com28','Com29','Com30','Com31','Com32','Com33','Com34','Com35','Com36',
                       'Com37','Com38','Com39','Com40','Com41','Com42','Com43','Com44','Com45','Com46','Com47','Com48',
                       'Com49','Com50','Com51','Com52')
randomForest_train <- train_pca

library(randomForest)

rf.model <- randomForest(species ~ Com1+Com2+Com3+Com4+Com5+Com6+Com7+Com8+Com9+Com10+Com11+Com12+Com13+Com14+Com15+Com16+Com17+Com18+Com19+Com20+Com21+Com22+Com23+Com24+Com25+Com26+Com27+Com28+Com29+Com30+Com31+Com32+Com33+Com34+Com35+Com36+Com37+Com38+Com39+Com40+Com41+Com42+Com43+Com44+Com45+Com46+Com47+Com48+Com49+Com50+Com51+Com52 ,  data=randomForest_train, ntree = 500, importance=TRUE, proximity=TRUE)



test <- read.csv("C:/Users/renuka/Desktop/Desktop/R working dir/test.csv")



shape <- test[ , grepl( "shape" , names( test ) ) ]
margin <- test[ , grepl( "margin" , names( test ) ) ]
texture <- test[ , grepl( "texture" , names( test ) ) ]

#normalized_shape <- as.data.frame(lapply(shape, normalize))
#normalized_margin <- as.data.frame(lapply(margin, normalize))
#normalized_texture <- as.data.frame(lapply(texture, normalize))


#test_withoutID <- data.frame(normalized_margin,normalized_shape,normalized_texture)
test_withoutID <- data.frame(margin,shape,texture)

new_test <- data.frame(test_withoutID$margin1 , test_withoutID$margin3 , test_withoutID$margin4 , test_withoutID$margin7 , test_withoutID$margin8 , test_withoutID$margin9 , 
                       test_withoutID$margin10 , test_withoutID$margin11 , test_withoutID$margin12 , test_withoutID$margin13 , test_withoutID$margin14 , test_withoutID$margin17 , 
                       test_withoutID$margin18 , test_withoutID$margin19 , test_withoutID$margin20 , test_withoutID$margin24 , test_withoutID$margin26 , test_withoutID$margin28 , 
                       test_withoutID$margin29 , test_withoutID$margin30 , test_withoutID$margin32 , test_withoutID$margin33 , test_withoutID$margin34 , test_withoutID$margin35 , 
                       test_withoutID$margin36 , test_withoutID$margin37 , test_withoutID$margin38 , test_withoutID$margin39 , test_withoutID$margin41 , test_withoutID$margin42 , 
                       test_withoutID$margin43 , test_withoutID$margin44 , test_withoutID$margin45 , test_withoutID$margin47 , test_withoutID$margin48 , test_withoutID$margin50 , 
                       test_withoutID$margin51 , test_withoutID$margin52 , test_withoutID$margin53 , test_withoutID$margin54 , test_withoutID$margin55 , test_withoutID$margin57 , 
                       test_withoutID$margin59 , test_withoutID$margin60 , test_withoutID$margin61 , test_withoutID$margin63 , test_withoutID$shape1 , test_withoutID$shape49 , 
                       test_withoutID$texture1 , test_withoutID$texture3 , test_withoutID$texture4 , test_withoutID$texture5 , test_withoutID$texture6 , test_withoutID$texture7 , 
                       test_withoutID$texture8 , test_withoutID$texture9 , test_withoutID$texture10 , test_withoutID$texture11 , test_withoutID$texture12 , 
                       test_withoutID$texture13 , test_withoutID$texture14 , test_withoutID$texture17 , test_withoutID$texture18 , test_withoutID$texture19 , 
                       test_withoutID$texture20 , test_withoutID$texture22 , test_withoutID$texture23 , test_withoutID$texture24 , test_withoutID$texture25 , 
                       test_withoutID$texture26 , test_withoutID$texture27 , test_withoutID$texture28 , test_withoutID$texture29 , test_withoutID$texture31 , 
                       test_withoutID$texture33 , test_withoutID$texture34 , test_withoutID$texture37 , test_withoutID$texture38 , test_withoutID$texture39 , 
                       test_withoutID$texture40 , test_withoutID$texture41 , test_withoutID$texture42 , test_withoutID$texture43 , test_withoutID$texture45 , 
                       test_withoutID$texture46 , test_withoutID$texture47 , test_withoutID$texture48 , test_withoutID$texture49 , test_withoutID$texture50 , 
                       test_withoutID$texture51 , test_withoutID$texture52 , test_withoutID$texture53 , test_withoutID$texture54 , test_withoutID$texture55 , 
                       test_withoutID$texture57 , test_withoutID$texture58 , test_withoutID$texture59 , test_withoutID$texture60 , test_withoutID$texture62 , 
                       test_withoutID$texture63 , test_withoutID$texture64)

shape <- new_test[ , grepl( "shape" , names( new_test ) ) ]
margin <- new_test[ , grepl( "margin" , names( new_test ) ) ]
texture <- new_test[ , grepl( "texture" , names( new_test ) ) ]



rf_test <- data.frame(margin[,1:25],shape,texture[,1:25])



colnames(rf_test)<-c('Com1','Com2','Com3','Com4','Com5','Com6','Com7','Com8','Com9','Com10','Com11','Com12',
                      'Com13','Com14','Com15','Com16','Com17','Com18','Com19','Com20','Com21','Com22','Com23','Com24',
                      'Com25','Com26','Com27','Com28','Com29','Com30','Com31','Com32','Com33','Com34','Com35','Com36',
                      'Com37','Com38','Com39','Com40','Com41','Com42','Com43','Com44','Com45','Com46','Com47','Com48',
                      'Com49','Com50','Com51','Com52')
rf.pred <- predict(rf.model, rf_test,type="prob")


result <- data.frame (test$id,rf.pred)

library(xlsx)
write.xlsx(x=result,file= "C:/Users/renuka/Desktop/Desktop/R working dir/rfresult1.xlsx", row.names = FALSE)

#PCA alone

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



m <- model.matrix( 
  ~ species -1 , 
  data = train_pca )

m <-data.frame(m)
train_pca <- train_pca [,-1]
ann_train <- data.frame(train_pca,m)


library(neuralnet)

nn <- neuralnet(speciesAcer_Capillipes + speciesAcer_Circinatum + speciesAcer_Mono + 
                  speciesAcer_Opalus + speciesAcer_Palmatum + speciesAcer_Pictum + 
                  speciesAcer_Platanoids + speciesAcer_Rubrum + speciesAcer_Rufinerve + 
                  speciesAcer_Saccharinum + speciesAlnus_Cordata + speciesAlnus_Maximowiczii + 
                  speciesAlnus_Rubra + speciesAlnus_Sieboldiana + speciesAlnus_Viridis + 
                  speciesArundinaria_Simonii + speciesBetula_Austrosinensis + 
                  speciesBetula_Pendula + speciesCallicarpa_Bodinieri + speciesCastanea_Sativa + 
                  speciesCeltis_Koraiensis + speciesCercis_Siliquastrum + speciesCornus_Chinensis + 
                  speciesCornus_Controversa + speciesCornus_Macrophylla + speciesCotinus_Coggygria + 
                  speciesCrataegus_Monogyna + speciesCytisus_Battandieri + 
                  speciesEucalyptus_Glaucescens + speciesEucalyptus_Neglecta + 
                  speciesEucalyptus_Urnigera + speciesFagus_Sylvatica + speciesGinkgo_Biloba + 
                  speciesIlex_Aquifolium + speciesIlex_Cornuta + speciesLiquidambar_Styraciflua + 
                  speciesLiriodendron_Tulipifera + speciesLithocarpus_Cleistocarpus + 
                  speciesLithocarpus_Edulis + speciesMagnolia_Heptapeta + speciesMagnolia_Salicifolia + 
                  speciesMorus_Nigra + speciesOlea_Europaea + speciesPhildelphus + 
                  speciesPopulus_Adenopoda + speciesPopulus_Grandidentata + 
                  speciesPopulus_Nigra + speciesPrunus_Avium + speciesPrunus_X_Shmittii + 
                  speciesPterocarya_Stenoptera + speciesQuercus_Afares + speciesQuercus_Agrifolia + 
                  speciesQuercus_Alnifolia + speciesQuercus_Brantii + speciesQuercus_Canariensis + 
                  speciesQuercus_Castaneifolia + speciesQuercus_Cerris + speciesQuercus_Chrysolepis + 
                  speciesQuercus_Coccifera + speciesQuercus_Coccinea + speciesQuercus_Crassifolia + 
                  speciesQuercus_Crassipes + speciesQuercus_Dolicholepis + 
                  speciesQuercus_Ellipsoidalis + speciesQuercus_Greggii + speciesQuercus_Hartwissiana + 
                  speciesQuercus_Ilex + speciesQuercus_Imbricaria + speciesQuercus_Infectoria_sub + 
                  speciesQuercus_Kewensis + speciesQuercus_Nigra + speciesQuercus_Palustris + 
                  speciesQuercus_Phellos + speciesQuercus_Phillyraeoides + 
                  speciesQuercus_Pontica + speciesQuercus_Pubescens + speciesQuercus_Pyrenaica + 
                  speciesQuercus_Rhysophylla + speciesQuercus_Rubra + speciesQuercus_Semecarpifolia + 
                  speciesQuercus_Shumardii + speciesQuercus_Suber + speciesQuercus_Texana + 
                  speciesQuercus_Trojana + speciesQuercus_Variabilis + speciesQuercus_Vulcanica + 
                  speciesQuercus_x_Hispanica + speciesQuercus_x_Turneri + speciesRhododendron_x_Russellianum + 
                  speciesSalix_Fragilis + speciesSalix_Intergra + speciesSorbus_Aria + 
                  speciesTilia_Oliveri + speciesTilia_Platyphyllos + speciesTilia_Tomentosa + 
                  speciesUlmus_Bergmanniana + speciesViburnum_Tinus + speciesViburnum_x_Rhytidophylloides + 
                  speciesZelkova_Serrata ~ Com1+Com2+Com3+Com4+Com5+Com6+Com7+Com8+Com9+Com10+Com11+Com12 +
                  Com13+Com14+Com15+Com16+Com17+Com18+Com19+Com20+Com21+Com22+Com23+Com24 +
                  Com25+Com26+Com27+Com28+Com29+Com30+Com31+Com32+Com33+Com34+Com35+Com36 +
                  Com37+Com38+Com39+Com40+Com41+Com42+Com43+Com44+Com45+Com46+Com47+Com48+
                  Com49+Com50+Com51+Com52+Com53+Com54+Com55+Com56+Com57+Com58+Com59+Com60 ,  data=ann_train, hidden=15 , err.fct="sse",linear.output = FALSE )


test <- read.csv("C:/Users/renuka/Desktop/Desktop/R working dir/test.csv")

shape <- test[ , grepl( "shape" , names( test ) ) ]
margin <- test[ , grepl( "margin" , names( test ) ) ]
texture <- test[ , grepl( "texture" , names( test ) ) ]

#normalized_shape <- as.data.frame(lapply(shape, normalize))
#normalized_margin <- as.data.frame(lapply(margin, normalize))
#normalized_texture <- as.data.frame(lapply(texture, normalize))


#ann_test <- data.frame(normalized_margin[,1:25],normalized_shape[,1:10],normalized_texture[,1:25])
ann_test <- data.frame(margin[,1:25],shape[,1:10],texture[,1:25])

colnames(ann_test)<-c('Com1','Com2','Com3','Com4','Com5','Com6','Com7','Com8','Com9','Com10','Com11','Com12',
                      'Com13','Com14','Com15','Com16','Com17','Com18','Com19','Com20','Com21','Com22','Com23','Com24',
                      'Com25','Com26','Com27','Com28','Com29','Com30','Com31','Com32','Com33','Com34','Com35','Com36',
                      'Com37','Com38','Com39','Com40','Com41','Com42','Com43','Com44','Com45','Com46','Com47','Com48',
                      'Com49','Com50','Com51','Com52','Com53','Com54','Com55','Com56','Com57','Com58','Com59','Com60')


compute(nn, ann_test)$net.result
result <- data.frame (test$id,compute(nn, ann_test)$net.result)

library(xlsx)
write.xlsx(x=result,file= "C:/Users/renuka/Desktop/Desktop/R working dir/result6.xlsx", row.names = FALSE)

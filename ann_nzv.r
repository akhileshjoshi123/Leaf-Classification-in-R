
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


m <- model.matrix( 
  ~ species -1 , 
  data = filteredData_HighCorr )

m <-data.frame(m)

ann_train <- data.frame(filteredData_HighCorr[,-ncol(filteredData_HighCorr)],m)

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
                  speciesZelkova_Serrata ~ margin1 + margin3 + margin4 + margin7 + margin8 + margin9 + 
                  margin10 + margin11 + margin12 + margin13 + margin14 + margin17 + 
                  margin18 + margin19 + margin20 + margin24 + margin26 + margin28 + 
                  margin29 + margin30 + margin32 + margin33 + margin34 + margin35 + 
                  margin36 + margin37 + margin38 + margin39 + margin41 + margin42 + 
                  margin43 + margin44 + margin45 + margin47 + margin48 + margin50 + 
                  margin51 + margin52 + margin53 + margin54 + margin55 + margin57 + 
                  margin59 + margin60 + margin61 + margin63 + shape1 + shape49 + 
                  texture1 + texture3 + texture4 + texture5 + texture6 + texture7 + 
                  texture8 + texture9 + texture10 + texture11 + texture12 + 
                  texture13 + texture14 + texture17 + texture18 + texture19 + 
                  texture20 + texture22 + texture23 + texture24 + texture25 + 
                  texture26 + texture27 + texture28 + texture29 + texture31 + 
                  texture33 + texture34 + texture37 + texture38 + texture39 + 
                  texture40 + texture41 + texture42 + texture43 + texture45 + 
                  texture46 + texture47 + texture48 + texture49 + texture50 + 
                  texture51 + texture52 + texture53 + texture54 + texture55 + 
                  texture57 + texture58 + texture59 + texture60 + texture62 + 
                  texture63 + texture64 ,  data=ann_train, hidden=c(6,8,6) , err.fct="sse",linear.output = FALSE )







test <- read.csv("C:/Users/renuka/Desktop/Desktop/R working dir/test.csv")



#normalize the data for continuous features between 0 and 1

shape <- test[ , grepl( "shape" , names( test ) ) ]
margin <- test[ , grepl( "margin" , names( test ) ) ]
texture <- test[ , grepl( "texture" , names( test ) ) ]

normalized_shape <- as.data.frame(lapply(shape, normalize))
normalized_margin <- as.data.frame(lapply(margin, normalize))
normalized_texture <- as.data.frame(lapply(texture, normalize))


test_withoutID <- data.frame(normalized_margin,normalized_shape,normalized_texture)


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

compute(nn, new_test)$net.result
result <- data.frame (test$id,compute(nn, new_test)$net.result)

library(xlsx)
write.xlsx(x=result,file= "C:/Users/renuka/Desktop/Desktop/R working dir/result.xlsx", row.names = FALSE)
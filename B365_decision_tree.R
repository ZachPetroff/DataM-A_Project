#B365 Final Project
#Zach Petroff, Ben Rieke, Connor Smith

#decision tree classifier

#libraries used
library(rpart)

#load data
training_data = read.csv("training_diabetes_data.csv")
validation_data = read.csv("validation_diabetes_data.csv")
head(training_data)

#classifier implementation
fit = rpart(class ~ Gender + Polyuria + Polydipsia + sudden.weight.loss + partial.paresis,
            data = training_data, method = "class", parms = list(split = 'gini'))
predict_training = predict(fit,training_data,type = 'class')
class = training_data$class
n = length(predict_training)
true_positives = 0
false_positives = 0
true_negatives = 0
false_negatives = 0
for(i in 1:n){
  if(predict_training[i] == 1 && class[i] == 1){
    true_positives = true_positives + 1
  }
  if(predict_training[i] == 1 && class[i] == 0){
    false_positives = false_positives + 1
  }
  if(predict_training[i] == 0 && class[i] == 0){
    true_negatives = true_negatives + 1
  }
  if(predict_training[i] == 0 && class[i] == 1){
    false_negatives = false_negatives + 1
  }
}
accuracy_rate = (true_negatives + true_positives)/
  (true_positives + true_negatives + false_positives + false_negatives)
cat("accuracy rate for training data = ",accuracy_rate) # -> 0.9026128
precision_rate = true_positives/(true_positives + false_positives)
cat("precision rate for training data = ",precision_rate) # -> 0.8932384
recall_rate = true_positives/(true_positives + false_negatives)
cat("recall rate for training data = ",recall_rate) # -> 0.9580153
specificity_rate = true_negatives/(true_negatives + false_positives)
cat("specificity rate for training data = ",specificity_rate) # -> 0.8113208

class_validation = validation_data$class
predict_validation = predict(fit,validation_data,type = 'class')
n = length(predict_validation)
true_positives = 0
false_positives = 0
true_negatives = 0
false_negatives = 0
for(i in 1:n){
  if(predict_validation[i] == 1 && class_validation[i] == 1){
    true_positives = true_positives + 1
  }
  if(predict_validation[i] == 1 && class_validation[i] == 0){
    false_positives = false_positives + 1
  }
  if(predict_validation[i] == 0 && class_validation[i] == 0){
    true_negatives = true_negatives + 1
  }
  if(predict_validation[i] == 0 && class_validation[i] == 1){
    false_negatives = false_negatives + 1
  }
}
accuracy_rate = (true_negatives + true_positives)/
  (true_positives + true_negatives + false_positives + false_negatives)
cat("accuracy rate for validation data = ",accuracy_rate) # -> 0.8585859
precision_rate = true_positives/(true_positives + false_positives)
cat("precision rate for validation data = ",precision_rate) # -> 0.8235294
recall_rate = true_positives/(true_positives + false_negatives)
cat("recall rate for validation data = ",recall_rate) # -> 0.9655172
specificity_rate = true_negatives/(true_negatives + false_positives)
cat("specificity rate for validation data = ",specificity_rate) # -> 0.7073171

#Analysis of tree classifier
printcp(fit)
#Classification tree:
#  rpart(formula = class ~ Gender + Polyuria + Polydipsia + sudden.weight.loss + 
#          partial.paresis, data = training_data, method = "class", 
#        parms = list(split = "gini"))
#
#Variables actually used in tree construction:
#  [1] Gender     Polydipsia Polyuria  

#Root node error: 159/421 = 0.37767

#n= 421 

#   CP        nsplit rel error  xerror  xstd
#1 0.528302      0   1.00000 1.00000 0.062562
#2 0.138365      1   0.47170 0.50314 0.050627
#3 0.075472      2   0.33333 0.38365 0.045423
#4 0.010000      3   0.25786 0.25786 0.038260

#best alpha (where xerror is minimized is at CP = 0.01)
alpha = 0.01

#tree classifier (post-prune)
fit_optimal = prune(fit,cp = alpha)
predict_training = predict(fit_optimal,training_data,type = 'class')
class = training_data$class
n = length(predict_training)
true_positives = 0
false_positives = 0
true_negatives = 0
false_negatives = 0
for(i in 1:n){
  if(predict_training[i] == 1 && class[i] == 1){
    true_positives = true_positives + 1
  }
  if(predict_training[i] == 1 && class[i] == 0){
    false_positives = false_positives + 1
  }
  if(predict_training[i] == 0 && class[i] == 0){
    true_negatives = true_negatives + 1
  }
  if(predict_training[i] == 0 && class[i] == 1){
    false_negatives = false_negatives + 1
  }
}
accuracy_rate = (true_negatives + true_positives)/
  (true_positives + true_negatives + false_positives + false_negatives)
cat("accuracy rate for training data = ",accuracy_rate) # -> 0.9026128
precision_rate = true_positives/(true_positives + false_positives)
cat("precision rate for training data = ",precision_rate) # -> 0.8932384
recall_rate = true_positives/(true_positives + false_negatives)
cat("recall rate for training data = ",recall_rate) # -> 0.9580153
specificity_rate = true_negatives/(true_negatives + false_positives)
cat("specificity rate for training data = ",specificity_rate) # -> 0.8113208

class_validation = validation_data$class
predict_validation = predict(fit_optimal,validation_data,type = 'class')
n = length(predict_validation)
true_positives = 0
false_positives = 0
true_negatives = 0
false_negatives = 0
for(i in 1:n){
  if(predict_validation[i] == 1 && class_validation[i] == 1){
    true_positives = true_positives + 1
  }
  if(predict_validation[i] == 1 && class_validation[i] == 0){
    false_positives = false_positives + 1
  }
  if(predict_validation[i] == 0 && class_validation[i] == 0){
    true_negatives = true_negatives + 1
  }
  if(predict_validation[i] == 0 && class_validation[i] == 1){
    false_negatives = false_negatives + 1
  }
}
accuracy_rate = (true_negatives + true_positives)/
  (true_positives + true_negatives + false_positives + false_negatives)
cat("accuracy rate for validation data = ",accuracy_rate) # -> 0.8585859
precision_rate = true_positives/(true_positives + false_positives)
cat("precision rate for validation data = ",precision_rate) # -> 0.8235294
recall_rate = true_positives/(true_positives + false_negatives)
cat("recall rate for validation data = ",recall_rate) # -> 0.9655172
specificity_rate = true_negatives/(true_negatives + false_positives)
cat("specificity rate for validation data = ",specificity_rate) # -> 0.7073171

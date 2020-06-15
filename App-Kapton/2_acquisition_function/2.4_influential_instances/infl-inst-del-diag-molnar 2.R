#==============================================================================
# Preparing dataset for cervical cancer classification
#==============================================================================
# Source:
# http://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29
# Paper: http://www.inescporto.pt/~jsc/publications/conferences/2017KelwinIBPRIA.pdf
library(e1071)
library(rpart)
library(partykit)

get.cervical.data = function(data_dir){
  cervical = read.csv(sprintf('%s/risk_factors_cervical_cancer.csv', data_dir), na.strings = c('?'), stringsAsFactors = FALSE)
  cervical = select(cervical, -Citology, -Schiller, -Hinselmann)
  cervical$Biopsy = factor(cervical$Biopsy, levels = c(0, 1), labels=c('Healthy', 'Cancer'))
  
  ## subset variables to the ones that should be used in the book
  cervical = dplyr::select(cervical, Age, Number.of.sexual.partners, First.sexual.intercourse,
                           Num.of.pregnancies, Smokes, Smokes..years., Hormonal.Contraceptives, Hormonal.Contraceptives..years.,
                           IUD, IUD..years., STDs, STDs..number., STDs..Number.of.diagnosis, STDs..Time.since.first.diagnosis,
                           STDs..Time.since.last.diagnosis, Biopsy)
  
  # NA imputation
  imputer = mlr::imputeMode()
  
  
  cervical_impute = mlr::impute(cervical, classes = list(numeric = imputeMode()))
  cervical = cervical_impute$data
  #cervical = relevel(cervical, "Healthy")
  cervical
}

get.cervical.task = function(data_dir){
  cervical = get.cervical.data(data_dir)
  mlr::makeClassifTask(id='cervical', data = cervical, target = 'Biopsy')
}
# Deletion diagnostics example
influence.v = function(predicted, predicted.without) {
  predicted - predicted.without
}

# smaller cervical data to increase speed
cervical.new = cervical[1:100,]

influence.matrix = matrix(NA, ncol = nrow(cervical.new), nrow = nrow(cervical.new))

lrn = makeLearner("classif.svm", predict.type = "prob")

tsk = makeReg(data = cervical.new, target = "Biopsy")

mod = train(lrn, tsk)

predicted.orig = getPredictionProbabilities(predict(mod, newdata = cervical.new))

cs = lapply(1:nrow(cervical.new), function(to.remove.index) {
  mod.2 = train(lrn, tsk, subset = setdiff(1:nrow(cervical.new), to.remove.index))
  predict.removed = getPredictionProbabilities(predict(mod.2, newdata = cervical.new))
  influence.v(predicted.orig, predict.removed)
}
)

# Column: Removed instance, row: influenced instance
influence.df = data.frame(cs)
influence.df = as.matrix(influence.df)
diag(influence.df) = NA
save(influence.df, predicted.orig, file = influence.matrix.filename)

load(influence.matrix.filename)
df = data.frame(influence = colMeans(abs(influence.df), na.rm = TRUE), id = 1:nrow(cervical.new))
df = df[order(df$influence, decreasing = TRUE),]

df.cervical  = cbind(df, cervical.new)
ct = rpart(abs(influence) ~ . -id, data = df.cervical, control = rpart.control(maxdepth = 2))
ct = as.party(ct)
plot(ct, inner_panel = node_inner(ct, pval = FALSE, id=FALSE), terminal_panel=node_boxplot(ct,id=FALSE))

# --> for our analysis better
# This first influence analysis revealed the *overall* most influential instance.
# Now we select one of the instances, namely the `r i`-th instance, for which we want to explain the prediction by finding the most influential training data instances.
# It is like a counterfactual question:
#   How would the prediction for instance  i change if we omit instance i from the training process?
#   We repeat this removal for all instances.

i = which(predicted.orig == max(predicted.orig))
obs = influence.df[i,]
cervical.200 = cervical.new
cervical.200$influence = unlist(obs)
#cervical.200 = na.omit(cervical.200)
worst.case.index = which(abs(cervical.200$influence) == max(abs(cervical.200$influence), na.rm = TRUE))
ct = rpart(abs(influence) ~ ., data = cervical.200, control = rpart.control(maxdepth = 2))
ct = as.party(ct)
plot(ct, inner_panel = node_inner(ct, pval = FALSE, id=FALSE), terminal_panel=node_boxplot(ct,id=FALSE))


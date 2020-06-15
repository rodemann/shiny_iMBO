create_predict_acqfun = function(model, control, points, models, control, par.set, designs, iter, progress, 
                                 attributes = FALSE){
  function(newdata){
    pred = acq(points = as.data.frame(newdata), 
               models = model, 
               par.set = par.set, 
               control = control,
               designs = designs, 
               iter = iter, 
               progress = progress
    )
    data.frame(pred, check.names = FALSE)
  }
}


X = res.mbo.mr$seen.points[[1]]
prediction.function = create_predict_acqfun(res.mbo = res.mbo.mr, iter.mbo = 3)
prediction.function(res.mbo.mr$seen.points[[2]])

###################
# or we can even extract it from mbo (sorry, that's not nice code...)
acq = res.mbo.ei$control$infill.crit$fun
acq.fun = function(df, dob) {
  acq(as.data.frame(df), 
      models = list(res.mbo$models[[dob + 1]]), 
      par.set = ps, 
      control = res.mbo$final.opt.state$opt.problem$control,
      designs = list(res.mbo.df[res.mbo.df$dob <= dob, ]), 
      iter = NULL, 
      progress = NULL)
}

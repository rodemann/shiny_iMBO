# Infl with other objfun 

#Ex.1 optimizing a simple sin(x) with mbo / EI
# WORKS!
set.seed(1)

obj.fun = makeSingleObjectiveFunction(
  name = "Sine",
  fn = function(x) sin(x),
  par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1),
  global.opt.value = -1
)

ctrl = makeMBOControl(propose.points = 1)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(),
                           opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(6L, getParamSet(obj.fun), fun = lhs::maximinLHS)
res = mbo(obj.fun, 
          design = design, 
          learner = lrn,
          control = ctrl,
          show.info = FALSE)
debug(inflInst)
undebug(inflInst)
inflInst.sin = inflInst(res, iter = 9, interest = "acquisition", parallel = FALSE, plot = FALSE)

# Ex.2 optimizing branin in 2D with mbo / EI #####
# WOKRS!!!!!!
set.seed(1)
configureMlr(show.learner.output = FALSE)

obj.fun = makeBraninFunction()

ctrl = makeMBOControl(propose.points = 1L, store.model.at = 1:11)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(),
                           opt = "focussearch", opt.focussearch.points = 2000L)

lrn = makeMBOLearner(ctrl, obj.fun)
design = generateDesign(10L, getParamSet(obj.fun), fun = lhs::maximinLHS)

res.brain = mbo(obj.fun, 
                design = design, 
                learner = lrn, 
                control = ctrl,
                show.info = TRUE)
debug(inflInst)
undebug(inflInst)
inflInst.brain = inflInst(res.brain, iter = 2, interest = "surrogate", parallel = FALSE, plot = FALSE)

getParam = function(par.set) {
  checkmate::assertClass(par.set, "ParamSet")
  pars = par.set$pars
  p = list()
  for (i in seq_along(pars)) {
    if (pars[[i]][["len"]] == 1) {
      p[[i]] = pars[[i]][["id"]]
    } else {
      p[[i]] = sapply(1:pars[[i]][["len"]], function(x) paste0(pars[[i]][["id"]], x))
    }
  }
  p = unlist(p)
  p
}

debug(getParam)
undebug(getParam)

# Ex.3 optimizing a simple noisy sin(x) with mbo / EI
# WORKS!

configureMlr(show.learner.output = FALSE)

# function with noise
obj.fun = makeSingleObjectiveFunction(
  name = "Some noisy function",
  fn = function(x) sin(x) + rnorm(1, 0, 0.1),
  par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1L),
  noisy = TRUE,
  global.opt.value = -1,
  fn.mean = function(x) sin(x)
)

ctrl = makeMBOControl(
  propose.points = 1L,
  final.method = "best.predicted",
  final.evals = 10L
)
ctrl = setMBOControlTermination(ctrl, iters = 5L)

ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(),
                           opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(6L, getParamSet(obj.fun), fun = lhs::maximinLHS)

res.noise = mbo(obj.fun, 
                design = design, 
                learner = lrn,
                control = ctrl,
                show.info = TRUE)

debug(inflInst)
undebug(inflInst)
inflInst.noise = inflInst(res.noise, iter = 2, interest = "acquisition", parallel = FALSE, plot = FALSE)


# Ex. 4 optimizing 1D fun with 3 categorical level and
### noisy outout with random forest
# WORKS!!!
set.seed(1)
configureMlr(show.learner.output = FALSE)

obj.fun = makeSingleObjectiveFunction(
  name = "Mixed decision space function",
  fn = function(x) {
    if (x$foo == "a") {
      return(5 + x$bar^2 + rnorm(1))
    } else if (x$foo == "b") {
      return(4 + x$bar^2 + rnorm(1, sd = 0.5))
    } else {
      return(3 + x$bar^2 + rnorm(1, sd = 1))
    }
  },
  par.set = makeParamSet(
    makeDiscreteParam("foo", values = letters[1:3]),
    makeNumericParam("bar", lower = -5, upper = 5)
  ),
  has.simple.signature = FALSE, # function expects a named list of parameter values
  noisy = TRUE
)

ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 10L)

# we can basically do an exhaustive search in 3 values
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(),
                           opt.restarts = 1L, opt.focussearch.points = 3L, opt.focussearch.maxit = 1L)

design = generateDesign(20L, getParamSet(obj.fun), fun = lhs::maximinLHS)

lrn = makeMBOLearner(ctrl, obj.fun)
res.nrf = mbo(obj.fun, 
              design = design, 
              learner = lrn, 
              control = ctrl,
              show.info = FALSE)

debug(inflInst)
undebug(inflInst)
inflInst.noiserf = inflInst(res.nrf, iter = 2, interest = "acquisition", parallel = FALSE, plot = FALSE)

# Ex.5 optimizing mixed space function
# WORKS!!!
set.seed(1)
configureMlr(show.learner.output = FALSE)

obj.fun = makeSingleObjectiveFunction(
  name = "Mixed functions",
  fn = function(x) {
    if (x$cat == "a")
      x$num^2
    else
      x$num^2 + 3
  },
  par.set = makeParamSet(
    makeDiscreteParam("cat", values = c("a", "b")),
    makeNumericParam("num", lower = -5, upper = 5)
  ),
  has.simple.signature = FALSE,
  global.opt.value = -1
)

ctrl = makeMBOControl(propose.points = 1L)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(),
                           opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(4L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = mbo(obj.fun, 
          design = design, 
          learner = lrn,
          control = ctrl, 
          show.info = TRUE)

debug(inflInst)
undebug(inflInst)
inflInst.run = inflInst(run, iter = 2, interest = "acquisition", parallel = FALSE, plot = FALSE)

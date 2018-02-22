## Test environments

* Windows 10 with the OpenBUGS installed

## R CMD check results
There were no Errors or WARNINGs.
There were 2 NOTE: Details are below

There are ::: calls to the package's namespace in its code. A package
  almost never needs to use ::: for its own objects:
  'add.half.2arm' 'add.half.3arm' 'cont.trans' 'cont.trans.2arm'
  'gen.y' 'gen.y.2arm' 'mul.model' 'sortbyna' 'sortbyna.2arm'
  'tx.check' 'uni.model'

checking R code for possible problems ... NOTE
cont.trans: no visible global function definition for 'glm'
cont.trans: no visible global function definition for 'binomial'
cont.trans.2arm: no visible global function definition for 'glm'
cont.trans.2arm: no visible global function definition for 'binomial'
Undefined global functions or variables:
  binomial glm
Consider adding
  importFrom("stats", "binomial", "glm")
  

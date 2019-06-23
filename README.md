# RAutoWEKA: A R/Auto-WEKA Interface

> Many different machine learning algorithms exist that can easily be used off the shelf, many of these methods are implemented in the open source [WEKA package](http://www.cs.waikato.ac.nz/ml/weka/). However, each of these algorithms have their own hyperparameters that can drastically change their performance, and there are a staggeringly large number of possible alternatives overall.
> 
> [Auto-WEKA](https://www.cs.ubc.ca/labs/beta/Projects/autoweka/) considers the problem of simultaneously selecting a learning algorithm and setting its hyperparameters, going beyond previous methods that address these issues in isolation. Auto-WEKA does this using a fully automated approach, leveraging recent innovations in Bayesian optimization.

This implementation interfaces Auto-WEKA to R by providing a package. It is partially based on the [RWeka package](https://cran.r-project.org/package=RWeka).

## Development

RAutoWEKA depends on [Auto-WEKA](https://www.cs.ubc.ca/labs/beta/Projects/autoweka/), hence, dependencies have to be resolved before development. Using the provided Makefile this should be done automatically:

```console
$ cd java
$ make
$ cd ..
```

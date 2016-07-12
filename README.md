# pse
[![Build Status](https://travis-ci.org/andrechalom/pse.svg?branch=master)](https://travis-ci.org/andrechalom/pse)

R package for exploration of parameter spaces

This package aims to provide a simple and robust approach to the exploration of parameter spaces of
computational models, focused around the Latin Hypercube Sampling. 

The theoretical revision provided in [this arXiv paper] (http://arxiv.org/abs/1210.6278) 
and the examples provided in the package focus on ecological models, with emphasis in structured 
population growth models. However, the functions provided should be adequate to other areas of knowledge as well.

We also propose a new methodology based on the likelihood statistical paradigm, called
PLUE - Profiled Likelihood Uncertainty Estimation. The theoretical background for it can be found
in [this arXiv paper] (http://arxiv.org/abs/1508.03354).

The package is available on CRAN at http://cran.r-project.org/web/packages/pse/ and can be installed with
```R
install.packages("pse")
``` 

This package was developed as part of my Master's degree program, which was funded by 
[CAPES] (http://www.capes.gov.br/) (2012-2014).

More detailed information can be found [here] (http://github.com/andrechalom/pse-theory).

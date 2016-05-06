# clyamar

Simple forest learning algorithm for Clojure.

## Installation

1. Make sure you have [Leiningen](http://leiningen.org/) installed  

2. Clone project from GitHub.

    `git clone https://github.com/xhresko/clyamar.git`

3. Create uberjar for standalone usage.

    `cd clyamar`

    `lein uberjar`


## Usage

Run the app with args

    $ java -jar clyamar-0.2.0-standalone.jar [options] action
    

## Options

_--input-file_ - file with samples (in LightSVM format) 

_--model-file_ - file for model saving/loading 

_--depth_ -  tree depth

_--granularity_ - number of thresholds used for feature while creating a split
 
_--alpha_ - learning rate

_--trees_ - number of trees in the forest


## Actions

_train_ - create model on given labeled dataset

_eval_ - evaluate given model on given labeled dataset

_predict_ - print predictions for given dataset
 

## Examples

Download dataset for regression task from Eunite 2001 competition.

`$ wget https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/regression/eunite2001 -O /tmp/eunite2001.libsvm`

Start learning using Leiningen (while inside the project directory). 

`$ lein run train -i /tmp/eunite2001.libsvm`

### Bugs

- problems with some LightSVM data format "implementations"
- different results of MSE for evaluation and training
 

## License

Copyright © 2016 

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

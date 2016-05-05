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

    $ java -jar clyamar-0.1.0-standalone.jar train-file depth granularity alpha trees
    

## Options

**train-file** - file with training samples (in LightSVM format)

**depth** - depth of trees

**granularity** - number of thresholds used for feature while creating a split
 
**alpha** - learning rate

**trees** - number of trees in the forest

## Examples

Download dataset for regression task from Eunite 2001 competition.

`$ wget https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/regression/eunite2001 -O /tmp/eunite2001.libsvm`

Start learning using Leiningen (while inside the project directory). 

`$ lein run /tmp/eunite2001.libsvm 3 20 0.15 30`

### Bugs

- problems with some LightSVM data format "implementations"
 

## License

Copyright © 2016 

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

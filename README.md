# Privacy Preserving Data Stream Perturbation

This repository contains a Accuracy Privacy Optimizaion Framework (APOF) for data stream mining environment.
This framework has three main modules, Accuracy, Privacy and Data Fitting.

## Quickstart

If you have Docker installed, you can run the experiments contained
within this codebase by executing `make jupyter`, opening the returned
URL in a web browser, and executing the contents of the provided
Jupyter notebooks (This has only been tested on an Ubuntu 16.04 host
running Docker 17.05.0-ce).

You will need to run the notebooks in the "dataset-construction"
sub-folder to create the datasets. Then dataset will be created and stored in the Dataset folder.

After that you can run jupyter source files for each dataset in "notebooks folder". Your results will be saved to sub-folders with dataset names under "workspace" folder.

Source codes are located in "src" folder.

Note that the results of each experiment are saved to disk to prevent the
need to re-execute the experiments when re-viewing an experiment's
results.

## Dependencies

* Java (>= 1.8.0)
* Leiningen (>= 2.0)

## Running Tests

`make run-tests`

Tests can also be run repeatedly from a Clojure REPL:

1. `lein repl`
2. `(use 'midje.repl)`
3. `(autotest)`

## Further Usage

See Makefile commands

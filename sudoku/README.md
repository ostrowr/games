Sudoku Solver
=============

A performant Sudoku solver written in Go.

To build, run `go build` from the base directory. To test, `go test` from the sudoku/ directory. To benchmark on a list of minimal Sudokus from http://staffhome.ecm.uwa.edu.au/~00013890/sudoku17, run `go test -bench=.`

Example:
    go build -o solve
    ./solve 800000000003600000070090200050007000000045700000100030001000068008500010090000400

    -------------
    |812|753|649|
    |943|682|175|
    |675|491|283|
    -------------
    |154|237|896|
    |369|845|721|
    |287|169|534|
    -------------
    |521|974|368|
    |438|526|917|
    |796|318|452|
    -------------


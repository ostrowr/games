package main

import "./sudoku"
import "fmt"
import "os"

func main() {
    hints := os.Args[1]
    b := sudoku.Board()
    b.Load(hints)
    solution := b.Solve()
    fmt.Println(solution)
}
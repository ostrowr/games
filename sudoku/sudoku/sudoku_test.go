package sudoku

import "testing"
import "net/http"
import "io/ioutil"
import "regexp"

var boards []string

func init() {
    resp, _ := http.Get("http://staffhome.ecm.uwa.edu.au/~00013890/sudoku17")
    defer resp.Body.Close()
    body, _ := ioutil.ReadAll(resp.Body)
    boards = regexp.MustCompile("\n").Split(string(body), -1)[1:]
}

func BenchmarkBoard(b *testing.B) {
    for i := 0; i < b.N; i++ {
        Board()
    }
}

func BenchmarkSolve(b *testing.B) {
    board := Board()
    for i := 1; i < b.N; i++ {
        board.Load(boards[len(boards) % i])
        board.Solve()
    }
}

func TestSolve(t *testing.T) {
    clues := Board()
    solution := Board()
    clues.Load("800000000003600000070090200050007000000045700000100030001000068008500010090000400")
    solution.Load("812753649943682175675491283154237896369845721287169534521974368438526917796318452")
    solved := clues.Solve()
    for row := 0; row < 9; row++ {
        for col := 0; col < 9; col++ {
            if solved.At(row, col) != solution.At(row, col) {
                t.Error("Expected solution\n", solution, "got\n", solved)
            }
        }
    }
}


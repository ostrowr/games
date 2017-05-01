package sudoku

import "strconv"
import "strings"

type cell uint
type board [][]cell

const FULL_CELL = cell((1<<1)|(1<<2)|(1<<3)|(1<<4)|(1<<5)|(1<<6)|(1<<7)|(1<<8)|(1<<9))

func (c cell) countPossibilities() uint {
	if c == 0 {
		return 0
	}
	count := uint(0)
	for c > 0 {
		count += uint(c & 1);
		c >>= 1
	}
	return count
}


func (c cell) possibilities() []uint {
	p := make([]uint, 0)
	if c == 0 {
		return p
	}
	for i := uint(1); i < 10; i++ {
		if ((1 << i) & c) != 0 {
			p = append(p, i)
		}
	}
	return p
}

func (c cell) uniqueValue() uint {
	possibility_count := c.countPossibilities()
	if possibility_count != 1 {
		return 0
	}
	for i := uint(1); i < 10; i++ {
		if c & (1<<i) > 0 {
			return i
		}
	}
	return 0
}

func remainingMoves(seen cell) cell {
	if seen & FULL_CELL != FULL_CELL {
		seen_count := seen.countPossibilities()
		if seen_count < 8 {
			return 0
		}
		if seen_count == 8 {
			for i := cell(1); i < 10; i++ {
				if (1 << i) & seen == 0 {
					return 1<<i
				}
			}
		}
	}
	return FULL_CELL
}

func getLegalMoves(b *board, row int, col int) cell {
	var rowCellsSeen cell
	var colCellsSeen cell
	var boxCellsSeen cell
	var allowed cell
	allowed = FULL_CELL
	boxRow, boxCol := 3*(row/3), 3*(col/3)
	for i := 0; i < 9; i++ {
		if row != i {
			c := (*b)[i][col]
			rowCellsSeen |= c
			if c.countPossibilities() == 1 {
				allowed &= (FULL_CELL & (^c))
			}
		}
		if col != i {
			c := (*b)[row][i]
			colCellsSeen |= c
			if c.countPossibilities() == 1 {
				allowed &= (FULL_CELL & (^c))
			}
		}
		if !(boxRow == row && boxCol == col) {
			c := (*b)[boxRow][boxCol]
			boxCellsSeen |= c
			if c.countPossibilities() == 1 {
				allowed &= (FULL_CELL & (^c))
			}
		}
		if (i+1)%3 == 0 {
			boxRow += 1
			boxCol -= 2
		} else {
			boxCol++
		}
	}
	colRemaining := remainingMoves(colCellsSeen)
	rowRemaining := remainingMoves(rowCellsSeen)
	boxRemaining := remainingMoves(boxCellsSeen)
	intersection := boxRemaining & rowRemaining & colRemaining & allowed
	return intersection
}

func (b *board) solveStep() bool {
	progress := false
	for i := 0; i < 9; i++ {
		for j := 0; j < 9; j++ {
			possibility_count := (*b)[i][j].countPossibilities()
			if possibility_count > 1 {
				prev_len := possibility_count
				(*b)[i][j] = getLegalMoves(b, i, j)
				if (*b)[i][j].countPossibilities() < prev_len {
					progress = true
				}
			}
		}
	}
	return progress
}

func (b *board) complete() bool {
	for i := 0; i < 9; i++ {
		for j := 0; j < 9; j++ {
			if (*b)[i][j].countPossibilities() != 1 {
				return false
			}
		}
	}
	return true
}

func (b *board) impossible() bool {
	for i := 0; i < 9; i++ {
		for j := 0; j < 9; j++ {
			if (*b)[i][j].countPossibilities() == 0 {
				return true
			}
		}
	}
	return false
}

func Board() board {
	b := make(board, 0)
	for i := 0; i < 9; i++ {
		b = append(b, make([]cell, 9))
		for j := 0; j < 9; j++ {
			b[i][j] = FULL_CELL
		}
	}
	return b
}

func (b board) String() string {
	if b == nil {
		return "Impossible"
	}
	s := "-------------\n"
	for i := 0; i < 9; i++ {
		for j := 0; j < 9; j++ {
			if j%3 == 0 {
				s += "|"
			}
			s += strconv.Itoa(int(b[i][j].uniqueValue()))
		}
		s += "|"
		s += "\n"
		if (i+1)%3 == 0 {
			s += "-------------\n"
		}
	}
	return s
}

func (b *board) Load(input string) {
	input = strings.Join(strings.Fields(input), "")
	if len(input) != 9*9 {
		panic("Attempted to load board of wrong size.")
	}
	for i := 0; i < 9; i++ {
		for j := 0; j < 9; j++ {
			val := input[i*9+j] - '0'
			if val > 0 {
				b.Assign(i, j, uint(val))
			} else {
				(*b)[i][j] = FULL_CELL
			}
		}
	}
}

func (b *board) Assign(row int, col int, val uint) {
	(*b)[row][col] = cell(1<<val)
}

func (b *board) At(row int, col int) uint {
	return (*b)[row][col].uniqueValue()
}



func (b *board) Copy() board {
	b1 := Board()
	for i := 0; i < 9; i++ {
		for j := 0; j < 9; j++ {
			b1[i][j] = (*b)[i][j]
		}
	}
	return b1
}

func (b *board) chooseBestPosition() (int, int) {
	bestI := 0
	bestJ := 0
	minLen := uint(10)
	for i := 0; i < 9; i++ {
		for j := 0; j < 9; j++ {
			num_possibilities := (*b)[i][j].countPossibilities()
			if num_possibilities == 2 {
				return i, j
			}
			if num_possibilities > 2 && num_possibilities < minLen {
				bestI = i
				bestJ = j
				minLen = num_possibilities
			}
		}
	}
	return bestI, bestJ
}

func (b board) Solve() board {
	b1 := b.Copy()
	for b1.solveStep() {
	}
	if b1.impossible() {
		return nil
	}
	if b1.complete() {
		return b1
	}

	row, col := b1.chooseBestPosition()
	possibilities := b1[row][col].possibilities()
	for _, c := range possibilities {
		b1.Assign(row, col, c)
		solved := b1.Solve()
		if solved != nil {
			return solved
		}

	}
	return nil
}

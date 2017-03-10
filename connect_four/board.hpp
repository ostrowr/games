#include <iostream>
#include <cassert>
#include <set>
#include <algorithm>

#define NUM_ROWS 6 // must be >= 4
#define NUM_COLS 7 // must be >= 4

typedef uint64_t board_t;

void print_board(board_t white, board_t black);
bool board_wins(board_t board);
bool move(board_t &white, board_t &black, int column, bool black_move);
void standardize_reflection(board_t &white, board_t &black);

// reversed_row[i] is the value of the reversed 7-bit binary value of i
const static int reversed_row[] = {0, 64, 32, 96, 16, 80, 48, 112, 8, 72, 40, 104, 24, 
							88, 56, 120, 4, 68, 36, 100, 20, 84, 52, 116, 12, 
							76, 44, 108, 28, 92, 60, 124, 2, 66, 34, 98, 18, 
							82, 50, 114, 10, 74, 42, 106, 26, 90, 58, 122, 6, 
							70, 38, 102, 22, 86, 54, 118, 14, 78, 46, 110, 30, 
							94, 62, 126, 1, 65, 33, 97, 17, 81, 49, 113, 9, 73, 
							41, 105, 25, 89, 57, 121, 5, 69, 37, 101, 21, 85, 
							53, 117, 13, 77, 45, 109, 29, 93, 61, 125, 3, 67, 
							35, 99, 19, 83, 51, 115, 11, 75, 43, 107, 27, 91, 
							59, 123, 7, 71, 39, 103, 23, 87, 55, 119, 15, 79, 
							47, 111, 31, 95, 63};
#include <iostream>
#include <cassert>
#include <set>
#include <algorithm>


#define NUM_ROWS 6 // must be >= 4
#define NUM_COLS 7 // must be >= 4

using namespace std;

typedef uint64_t board_t;

void print_board(board_t white, board_t black);

/* check to see if a board (representing a single player's moves)
 * has 4 in a row.
 */
bool board_wins(board_t board) {
	board_t row_check = (1 << 4) - 1; // all on the same row
	board_t col_check = 1 | (1 << NUM_COLS) | (1 << (2 * NUM_COLS)) | (1 << (3 * NUM_COLS)); // all on the same col
	board_t decreasing_diag = 1 | (1 << (NUM_COLS + 1)) | (1 << (2 * (NUM_COLS + 1))) | (1 << (3 * (NUM_COLS + 1))); // on the same decreasing diagonal
	board_t increasing_diag = 1 | (1 << (NUM_COLS - 1)) | (1 << (2 * (NUM_COLS - 1))) | (1 << (3 * (NUM_COLS - 1))); // on the same increasing diagonal
	for (int test = 0; test < NUM_ROWS * NUM_COLS; test++) {
		// if the current test square isn't filled, nothing
		// anchored there can be 4 in a row
		if (!(board & ((board_t) 1 << test))) continue;
		int row = NUM_ROWS - (test / NUM_COLS) - 1;
		int col = NUM_COLS - (test % NUM_COLS) - 1;
		bool winner = false;
		if (row >= 3 && col < NUM_COLS - 3){
			if (((increasing_diag << test) & board) == (increasing_diag << test)){
				return true;
			}
		}
		if (row >= 3 && col >= 3) {
			if (((decreasing_diag << test) & board) == (decreasing_diag << test)) {
				return true;
			}
		}
		if (col >= 3) {
			if (((row_check << test) & board) == (row_check << test)) {
				return true;
			}
		}
		if (row >= 3) {
			if (((col_check << test) & board) == (col_check << test)) {
				return true;
			}
		}
	}
	return false;
}


bool move(board_t &white, board_t &black, int column, bool black_move) {
	assert(column < NUM_COLS);
	board_t board = white | black;
	board_t col_mask = 1 << (NUM_COLS - column - 1);
	board_t full_row = (1 << NUM_COLS) - 1;
	while (board & col_mask & full_row) {
		// the column in this row is full, try the next one up
		full_row <<= NUM_COLS;
		col_mask <<= NUM_COLS;
	}
	if (full_row > ((board_t) 1 << (NUM_COLS * NUM_ROWS))) {
		// the column in every row was full
		return false;
	}
	if (black_move) {
		black = black | (col_mask & full_row);
	}
	else {
		white = white | (col_mask & full_row);
	}
	return true;
}

void print_board(board_t white, board_t black) {
	int mask_shift = NUM_ROWS * NUM_COLS - 1;
	while (mask_shift >= 0) {
		int w = !!(white & ((board_t) 1 << mask_shift));
		int b = !!(black & ((board_t) 1 << mask_shift));
		cout << w + 2 * b;
		if (!(mask_shift % NUM_COLS)) cout << endl;
		mask_shift--;
	}
	cout << endl;
}

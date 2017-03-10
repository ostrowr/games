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


// TODO caching and symmetry

int move_score(board_t white, board_t black, int proposed_move, bool black_made_move) {
	// print_board(white, black);
	// cout << proposed_move << " " << black_made_move << " ";
	bool legal_move = move(white, black, proposed_move, black_made_move);
	// cout << legal_move << endl << endl;
	if (!legal_move) {
		return 0;
	}
	if (black_made_move && board_wins(black)) {
		cout << "black wins board";
		return -1;
	}
	else if (!black_made_move && board_wins(white)) {
		return 1;
	}
	set<int> moves;
	for (int move_col = 0; move_col < NUM_COLS; move_col++) {
		board_t white_cpy = white;
		board_t black_cpy = black;
		int score = move_score(white_cpy, black_cpy, move_col, !black_made_move);
		if (score == -1 && black_made_move) return -1;
		if (score == 1 && !black_made_move) return 1;
		moves.insert(score);
	}

	auto white_can_win = moves.find(1) != moves.end();
	auto black_can_win = moves.find(-1) != moves.end();
	auto can_draw = moves.find(0) != moves.end();

	// if (black_can_win && black_made_move) {
	// 	return -1;
	// }
	// if (white_can_win && !black_made_move) {
	// 	return 1;
	// }
	if (can_draw) {
		return 0;
	}
	if (black_made_move) {
		return 1;
	}
	return -1;
}


int main(int argc, char *argv[]) {
	board_t white = 0;
	board_t black = 0;
	move(white, black, 1, 0);
	move(white, black, 2, 0);
	move(white, black, 3, 0);
	cout << move_score(white, black, 0, 2) << endl;
}

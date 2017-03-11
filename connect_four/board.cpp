#include "board.hpp"

using namespace std;


Board::Board(board_t white, board_t black, bool white_moved_last) {
	this->white = white;
	this->black = black;
	this->white_moved_last = white_moved_last;
}

static inline board_t no_last_columns_mask() {
	//return ~242412012423LL;
	board_t mask = 0;
	board_t row_mask = (1 << 3) - 1;
	for (int i = 0; i < NUM_ROWS; i++){
		mask |= row_mask << (i * NUM_COLS);
	}
	return ~mask;
}


int Board::wins() const {
	board_t board = this->white_moved_last ? this->white : this->black;
	int winner = this->white_moved_last ? 1 : -1;
	// horizontal
	board_t consec = board & (board << 1);
	consec = consec & (consec << 2);
	// to prevent wrap, this last surviving piece
	// can't be in any of the first three columns
	if (consec & no_last_columns_mask()) return winner;

	// vertical
	consec = board & (board << NUM_COLS);
	if (consec & (consec << (2 * NUM_COLS))) return winner;
	// the board_t should be zeroed out above 2**42, so 
	// wrap shouldn't be an issue.

	// diagonal with negative slope
	consec = board & (board << (NUM_COLS + 1));
	consec = consec & (consec << (2 * (NUM_COLS + 1)));
	if (consec & no_last_columns_mask()) return winner;

	// diagonal with positive slope
	consec = board & (board >> (NUM_COLS - 1));
	consec = consec & (consec >> (2 * (NUM_COLS - 1)));
	if (consec & no_last_columns_mask()) return winner;

	return 0;
}

bool Board::move(size_t column) {
	assert(column < NUM_COLS);
	board_t board = this->white | this->black;
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
	if (this->white_moved_last) {
		this->black = this->black | (col_mask & full_row);
	}
	else {
		this->white = this->white | (col_mask & full_row);
	}
	this->white_moved_last = !(this->white_moved_last);
	// assert(this->wins() == this->board_wins());
	return true;
}

void Board::standardize_reflection() {
	board_t white_reflected = this->reflect_board(this->white);
	board_t black_reflected = this->reflect_board(this->black);
	if ((this->white | this->black) > (white_reflected | black_reflected)) {
		this->white = white_reflected;
		this->black = black_reflected;
	}
}

board_t Board::reflect_board(board_t board) {
	// boards reflected across the center line are
	// functionally identical. Return the smaller of the two
	// possible reflections.
	board_t reflected = 0;
	board_t full_row = (1 << NUM_COLS) - 1;
	int offset = 0;
	while (full_row <= ((board_t) 1 << (NUM_COLS * NUM_ROWS))) {
		reflected |= this->reversed_row[(full_row & board) >> offset] << offset;
		full_row <<= NUM_COLS;
		offset += NUM_COLS;
	}
	return min(board, reflected);
}

void Board::process_moves(vector<size_t> moves) {
	for (size_t column : moves) {
		this->move(column);
		if (this->wins()) {
			return;
		}
	}
}

bool Board::is_black_turn() const {
	return this->white_moved_last;
}

bool Board::operator<(const Board& other) const {
	if (this->white < other.white){
		return true;
	}
	if (this->white == other.white && this->black < other.black) {
		return true;
	}
	return false;
}

ostream& operator<<(ostream& os, const Board& b) {
	int mask_shift = NUM_ROWS * NUM_COLS - 1;
	while (mask_shift >= 0) {
		int wh = !!(b.white & ((board_t) 1 << mask_shift));
		int bl = !!(b.black & ((board_t) 1 << mask_shift));
		if (wh) os << 'W';
		else if (bl) os << 'B';
		else os << ' ';
		os << '|';
		if (!(mask_shift % NUM_COLS)) os << endl;
		mask_shift--;
	}
	os << "==============" << endl;
	if (b.wins()) {
		if (b.white_moved_last) {
			os << "White wins." << endl;
		}
		else {
			os << "Black wins." << endl;
		}
	}
	else {
		if (b.white_moved_last) {
			os << "Black's move." << endl;
		}
		else {
			os << "White's move." << endl;
		}
	}
	return os;
}

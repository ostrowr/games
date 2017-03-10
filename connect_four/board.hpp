#include <iostream>
#include <cassert>
#include <set>
#include <algorithm>

#define NUM_ROWS 6 // must be >= 4
#define NUM_COLS 7 // must be >= 4

typedef uint64_t board_t;

// class Board {
// private:
// 	board_t white;
// 	board_t black;

// public:
// 	Board(board_t white, board_t black); 
// 	void print_board();
// 	void board_wins();
// 	bool move(int column);
// 	bool last_move_was_white;
// };

void print_board(board_t white, board_t black);
bool board_wins(board_t board);
bool move(board_t &white, board_t &black, int column, bool black_move);
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
void print_board(board_t white, board_t black);
#include "board.hpp"

using namespace std;

// int move_score(board_t white, board_t black, int proposed_move, bool black_made_move) {
// 	// print_board(white, black);
// 	// cout << proposed_move << " " << black_made_move << " ";
// 	bool legal_move = move(white, black, proposed_move, black_made_move);
// 	// cout << legal_move << endl << endl;
// 	if (!legal_move) {
// 		return 0;
// 	}
// 	if (black_made_move && board_wins(black)) {
// 		cout << "black wins board";
// 		return -1;
// 	}
// 	else if (!black_made_move && board_wins(white)) {
// 		return 1;
// 	}
// 	set<int> moves;
// 	for (int move_col = 0; move_col < NUM_COLS; move_col++) {
// 		board_t white_cpy = white;
// 		board_t black_cpy = black;
// 		int score = move_score(white_cpy, black_cpy, move_col, !black_made_move);
// 		if (score == -1 && black_made_move) return -1;
// 		if (score == 1 && !black_made_move) return 1;
// 		moves.insert(score);
// 	}

// 	auto white_can_win = moves.find(1) != moves.end();
// 	auto black_can_win = moves.find(-1) != moves.end();
// 	auto can_draw = moves.find(0) != moves.end();

// 	// if (black_can_win && black_made_move) {
// 	// 	return -1;
// 	// }
// 	// if (white_can_win && !black_made_move) {
// 	// 	return 1;
// 	// }
// 	if (can_draw) {
// 		return 0;
// 	}
// 	if (black_made_move) {
// 		return 1;
// 	}
// 	return -1;
// }


void print_usage() {
	cout << "Usage:" << endl;
	cout << "\t./solve white black" << endl << endl;
	cout << "\t<white> and <black> are integers representing" << endl;
	cout << "\tthe positions of tokens of their respective colors." << endl << endl;
	cout << "\tFor example, the board" << endl; 
	cout << "\t\t0000000\n\t\t0000000\n\t\t1100101\n\t\t0100100\n\t\t1101001\n\t\t0010010" << endl;
	cout << "\tis represented by the integer 212415634" << endl;
	cout << "\t(from binary 1100101010010011010010010010)." << endl; 
	exit(1);
}

int main(int argc, char *argv[]) {
	if (argc != 3) print_usage();
	board_t white = stoll(argv[1]);
	board_t black = stoll(argv[2]);
	standardize_reflection(white, black);
	// print_board(white, black);
	// cout << move_score(white, black, 0, 2) << endl;
}
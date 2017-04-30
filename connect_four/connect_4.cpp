#include "board.hpp"
#include <map>

#define ILLEGAL_MOVE -1
using namespace std;

map<Board, int> solved_boards;
int counts;

int move_score(Board b, size_t proposed_move, size_t turns = 0, int alpha = -1, int beta = 1) {
	// TODO could only insert/retrieve properly reflected board
	// (but may be slower)
	counts++;



	if (solved_boards.count(b)) return solved_boards[b];
	bool legal_move = b.move(proposed_move);
	if (!legal_move) {
		return ILLEGAL_MOVE;
	}

	if (b.wins()) {
		solved_boards[b] = 1;
		return turns;
	}

	vector<size_t> moves;
	for (size_t move = 0; move < NUM_COLS; move++) {
		 int score = move_score(b, move, -(turns + 1))  ;
		 if (score == ILLEGAL_MOVE) continue; // illegal move
		 // if (!(score % 2)) {
		 // 	solved_boards[b] = score;
		 // 	return score;
		 // }
		 moves.push_back(score);
	}
	// if (!moves.size()) {
	// 	solved_boards[b] = 0;
	// 	return 0;
	// }
	int outcome = 0;
	size_t curr_wins = -1;
	size_t other_wins = -1;
	for (size_t move : moves) {
		if (!(move % 2) && move < curr_wins) curr_wins = move;
		else if ((move % 2) && move < other_wins) other_wins = move;
	}
	if (curr_wins != (size_t) -1) {
		outcome = curr_wins;
		return curr_wins;
	}
	else if (other_wins != (size_t) -1) {
		outcome = other_wins;
	}
	solved_boards[b] = outcome;
	return outcome;
}


void print_usage() {
	cout << "Usage:" << endl;
	cout << "\t./solve moves" << endl << endl;
	cout << "\t where <moves> is a string representing each move as a (zero-indexed)" << endl;
	cout << "\t column." << endl;
	exit(1);
}

int main(int argc, char *argv[]) {
	if (argc != 2) print_usage();
	vector<size_t> moves;
	for (char c : (string) argv[1]) {
		moves.push_back(atoi(&c));
	}
	Board b;
	b.process_moves(moves);
	cout << b << endl;
	for (size_t i = 0; i < NUM_COLS; i++) {
		cout << i << " " << move_score(b, i) << endl;
	}
	cout << solved_boards.size() << endl;
	cout << counts << endl;
}

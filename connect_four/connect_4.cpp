#include "board.hpp"
#include <map>

using namespace std;

map<Board, int> solved_boards;


int move_score(Board b, size_t proposed_move) {
	if (solved_boards.count(b)) return solved_boards[b];
	bool legal_move = b.move(proposed_move);
	if (!legal_move) {
		return -2;
		// solved_boards[make_pair(b, b.is_black_turn())] = 0;

	}
	int winner = b.board_wins();
	if (winner) {
		solved_boards[b] = winner;
		return winner;
	}

	set<size_t> moves;
	for (size_t move = 0; move < NUM_COLS; move++) {
		Board bb = b;
		 int score = move_score(bb, move);
		 if (score == -2) continue; // illegal move
		 if (score == -1 && b.is_black_turn()) {
		 	solved_boards[b] = -1;
		 	return -1;
		 }
		 if (score == 1 && !b.is_black_turn()) {
		 	solved_boards[b] = 1;
		 	return 1;
		 }
		 moves.insert(score);
	}
	if (!moves.size()) return 0;
	bool white_can_win = moves.find(1) != moves.end();
	bool black_can_win = moves.find(-1) != moves.end();
	bool can_draw = moves.find(0) != moves.end();
	int outcome = 0;
	if (b.is_black_turn()) {
		if (black_can_win) outcome = -1;
		else if (can_draw) outcome = 0;
		else if (white_can_win) outcome = 1;
	}
	else {
		if (white_can_win) outcome = 1;
		else if (can_draw) outcome = 0;
		else if (black_can_win) outcome = -1;
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
}

import string
import sys
from copy import deepcopy as copy
from queue import PriorityQueue
import numpy as np

# DICT = "/usr/share/dict/web2"
DICT = "/usr/local/share/dict/9C.txt"  # 9th collegiate from http://wiki.puzzlers.org/pub/wordlists/9C.txt
DROP_MARKER = ";"
TOP_MARKER = "-"
BLOCK_MARKER = "0"
SPECIAL_LETTERS = "jqxz"


class Spelltower():

    def __init__(self, letters, dictionary=DICT):
        legal_characters = string.ascii_letters + BLOCK_MARKER
        self.n_rows = 12
        self.n_cols = 9
        self._empty_col = np.full(self.n_rows, TOP_MARKER, dtype=np.string_)

        # silently ignore illegal characters
        letters = [x for x in letters.lower() if x in legal_characters]
        assert(len(letters) == self.n_rows * self.n_cols)
        self.game = np.array(letters).reshape(self.n_rows, self.n_cols)
        self._read_dict(dictionary)

    def _read_dict(self, dictionary):
        lowercase = set(string.ascii_lowercase)
        with open(dictionary) as d:
            self.dictionary = set(w for w in d.read().split() if len(w) >= 3 and set(w) <= lowercase)
        self.prefixes = set()
        for word in self.dictionary:
            for end_pos in range(1, len(word) - 1):
                self.prefixes.add(word[:end_pos])

    def print_game(self, path):
        print("START")
        print(self.game)
        game_state = copy(self.game)
        for word in path:
            game_state, _ = self._play_word(game_state, word)
            print(word)
            print(game_state)

    def solve(self):
        paths_in_progress = PriorityQueue()
        paths_in_progress.put((0, ([], copy(self.game))))
        complete_paths = []
        best_path = None
        best_score = 0
        while not paths_in_progress.empty():
            curr_score, (path, game_state) = paths_in_progress.get()
            words = self._solve_step(game_state)
            if not words:
                complete_paths.append((curr_score, path))
                if curr_score < best_score:
                    best_score = curr_score
                    best_path = path
                    print(curr_score)
                    self.print_game(path)
                # print("completed:", len(complete_paths))
                continue
            for word in words:
                new_game_state, score = self._play_word(game_state, word)
                new_path = copy(path)
                new_path.append(copy(word))
                paths_in_progress.put((curr_score - score, (new_path, new_game_state)))
        return best_path

    def _is_word(self, word):
        return word in self.dictionary

    def _is_prefix(self, prefix):
        return prefix in self.prefixes

    def _get_words_from_pos(self, game_state, prefix, hist):
        curr_pos = hist[-1]
        words = []
        if self._is_word(prefix):
            words.append((prefix, hist))
        if not self._is_prefix(prefix):
            return words

        for row_delta in [-1, 0, 1]:
            for col_delta in [-1, 0, 1]:
                new_pos = (curr_pos[0] + row_delta, curr_pos[1] + col_delta)
                if new_pos[0] < 0 or new_pos[0] >= self.n_rows:
                    continue
                if new_pos[1] < 0 or new_pos[1] >= self.n_cols:
                    continue
                if new_pos in hist:
                    continue
                new_letter = game_state[new_pos[0]][new_pos[1]]
                new_hist = copy(hist)
                new_hist.append(new_pos)
                words += self._get_words_from_pos(game_state, prefix + new_letter, new_hist)
        return words

    def _solve_step(self, game_state):
        words = []
        for start_row in range(self.n_rows):
            for start_col in range(self.n_cols):
                words_from_pos = self._get_words_from_pos(
                    game_state, game_state[start_row][start_col], [(start_row, start_col)]
                )
                words += words_from_pos

        return words

    def _gravitate(self, game_state):
        for col in range(self.n_cols):
            col_values = game_state[:, col]
            game_state[:, col] = np.concatenate(
                (self._empty_col, col_values[col_values != DROP_MARKER])
            )[-self.n_rows:]
        return game_state

    def _play_word(self, game_state, word):
        word_string, word_path = word
        bonus_length = len(word_string) >= 5
        game_state = copy(game_state)
        # TODO figure out how actual scoring system works

        score_heuristic = 0
        for pos in word_path:
            if game_state[pos] in SPECIAL_LETTERS:
                game_state[pos, :] = DROP_MARKER
                score_heuristic += 1

            game_state[pos] = DROP_MARKER

            for neighbor_delta in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
                new_pos = tuple(np.add(pos, neighbor_delta))
                if new_pos[0] < 0 or new_pos[0] >= self.n_rows:
                    continue
                if new_pos[1] < 0 or new_pos[1] >= self.n_cols:
                    continue
                # if bonus or adjacent character is a void, delete the
                # adjacent character
                if bonus_length or (game_state[new_pos] == BLOCK_MARKER):
                    game_state[new_pos] = DROP_MARKER
                    score_heuristic += 1

        game_state = self._gravitate(game_state)
        return game_state, score_heuristic

    def __str__(self):
        """Pretty-print game state"""
        return str(self.game)

if __name__ == "__main__":
    game = []
    infile = sys.argv[1]
    game = []
    with open(infile, "r") as infd:
        data = infd.read()
    s = Spelltower(data)
    words = s.solve()
    print(len(words))

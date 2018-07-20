import string
import sys
from copy import deepcopy as copy
from queue import PriorityQueue
import numpy as np

# DICT = "/usr/share/dict/web2"

# public domain enable dict from http://wiki.puzzlers.org/pub/wordlists/enable1.txt
DICT = "/usr/local/share/dict/enable1.txt"
DROP_MARKER = "-"
BLOCK_MARKER = "0"
SPECIAL_LETTERS = "jqxz"

# don't know the real letter values; use this as a heuristic
SCRABBLE_LETTER_VALUES = {
    'a': 1, 'b': 3, 'c': 3, 'd': 2, 'e': 1, 'f': 4,
    'g': 2, 'h': 4, 'i': 1, 'j': 8, 'k': 5, 'l': 1,
    'm': 3, 'n': 1, 'o': 1, 'p': 3, 'q': 10, 'r': 1,
    's': 1, 't': 1, 'u': 1, 'v': 4, 'w': 4, 'x': 8,
    'y': 4, 'z': 10
}


class Spelltower():

    def __init__(self, letters, dictionary=DICT):
        legal_characters = string.ascii_letters + BLOCK_MARKER
        self.n_rows = 12
        self.n_cols = 9
        self._empty_col = np.full(self.n_rows, DROP_MARKER, dtype=np.string_)
        self._empty_row = np.full(self.n_cols, DROP_MARKER, dtype=np.string_)

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
        score = sum(self._score(p) for p in path)
        print("\n\nSTART (score={})".format(score))
        game_state = copy(self.game)
        for word in path:
            new_game_state, _ = self._play_word(game_state, word)
            print(word[0])
            for pos in word[1]:
                game_state[pos] = game_state[pos].upper()
            print(game_state)
            game_state = copy(new_game_state)
        print("END\n\n")

    def _score(self, word):
        # TODO figure out how actual scoring system works
        word_string, word_path = word
        score_heuristic = sum([SCRABBLE_LETTER_VALUES[c] for c in word_string])
        score_heuristic *= len(word_string)
        return score_heuristic

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
                    self.print_game(path)
                continue
            seen_on_path = set(p[0] for p in path)
            for word in words:
                if word[0] in seen_on_path:
                    continue  # can't play the same word twice

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

    def _play_word(self, game_state2, word):
        word_string, word_path = word
        bonus_length = len(word_string) >= 5
        game_state = copy(game_state2)

        for i, pos in enumerate(word_path):

            if word_string[i] in SPECIAL_LETTERS:
                game_state[pos[0], :] = self._empty_row

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

        game_state = self._gravitate(game_state)
        return game_state, self._score(word)

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
    best_path = s.solve()
    print(best_path)

# XWord Info and Cruciverb word lists needed

import numpy as np
from collections import defaultdict
import string
import z3

WORDS = ["CAT", "ADO", "NON", "CAN", "TON"]
DICT = "mini_dict.txt"
DICT = "/usr/local/share/dict/enable1.txt"
DICT = "/usr/share/dict/propernames"
# WORDS = ["TUNA", "MUSIC", "CAN", "HI"]

class Crossword():
    def __init__(self, grid_path, dict_path):
        with open(grid_path, "r") as g:
            self.grid = np.array([list(c) for c in g.read().split()])
            self.grid = np.pad(self.grid, 1, 'constant', constant_values="-")

        self._make_lexicon(dict_path)
        # self._get_constraints()
        # self._constraints_to_z3()

    def _make_lexicon(self, dict_path):
        with open(dict_path, "r") as d:
            words = d.readlines()
        self.words = [w.upper().strip() for w in words]
        self.words = [w for w in self.words if set(w).issubset(set(string.ascii_uppercase))]
        self.words_by_length = defaultdict(list)
        for word in self.words:
            self.words_by_length[len(word)].append(word)

        # bitarrays[len][i][letter](n) is a bitarray that is 1 in the nth position
        # if word n of length len has letter in the ith position.
        bitarrays = {}
        for key in self.words_by_length:
            self.words_by_length[key].sort()
            bitarrays[key] = {i: {
                c: 0 for c in string.ascii_uppercase
            } for i in range(key)}
        for l, words in self.words_by_length.items():
            for word_index, word in enumerate(words):
                for char_index, char in enumerate(word):
                    bitarrays[l][char_index][char] |= (1 << word_index)
        # print(bitarrays)
        packed = 0
        for c in string.ascii_uppercase:
            packed |= bitarrays[4][3][c]
        for l in bitarrays:
            for char_index in bitarrays[l]:
                for char in bitarrays[l][char_index]:
                    bitarrays[l][char_index][char] = z3.BitVecVal(bitarrays[l][char_index][char], len(self.words_by_length[l]))
        self.lexicon_bitarrays = bitarrays

        # together = z3.BitVec("together", len(self.words_by_length[4]))
        # for c in string.ascii_uppercase:
        #     together |= self.lexicon_bitarrays[4][3][c]
        #     # print(z3.simplify(z3.BVRedAnd(together)))
        # print(z3.simplify(z3.BVRedAnd(self.lexicon_bitarrays[4][3]["A"]))) # should be 0
        # print(z3.simplify(z3.BVRedAnd(together))) # should be 1


    def _get_constraints(self):
        clue_num = 0
        clue_assignments = np.empty_like(self.grid, dtype=complex)
        acrosses = defaultdict(list)
        downs = defaultdict(list)

        # restrictions {(a, b): (c, d)} means that a-across intersects with d-down
        # at the cth index in across and dth index in b-down.
        restrictions = {}

        # if the grid has any prefilled letters, save these restrictions.
        across_letter_restrictions = {}
        down_letter_restrictions = {}
        for row in range(self.grid.shape[0]):
            for col in range(self.grid.shape[1]):
                if self.grid[row][col] == "-":
                    clue_assignments[row][col] = 0 + 0j
                else:
                    prev_across = clue_assignments[row][col - 1].real
                    prev_down = clue_assignments[row - 1][col].imag
                    if prev_across == 0 or prev_down == 0:
                        clue_num += 1
                    new_across = int(clue_num if prev_across == 0 else prev_across)
                    new_down = int(clue_num if prev_down == 0 else prev_down)
                    clue_assignments[row][col] = complex(new_across, new_down)
                    acrosses[new_across].append((row, col))
                    downs[new_down].append((row, col))
                    restrictions[(new_across, new_down)] = (len(acrosses[new_across]) - 1, len(downs[new_down]) - 1)
                    if self.grid[row][col] != "?":
                        pass
                        # across_letter_restrictions[new_across] = self.grid[row][col]
                        # down_letter_restrictions[new_down] = self.grid[row][col]

        # ignore 1-letter words
        self.acrosses = {k: v for k, v in acrosses.items() if len(v) > 1}
        self.downs = {k: v for k, v in downs.items() if len(v) > 1}
        self.restrictions = {k: v for k, v in restrictions.items() if k[0] in self.acrosses and k[1] in self.downs}
        # self.across_letter_restrictions = {k: v for k, v in across_letter_restrictions if k in self.acrosses}
        # self.down_letter_restrictions = {k: v for k, v in down_letter_restrictions if k in self.downs}

    def _constraints_to_z3(self):
        word_constants = {z3.StringVal(word) for word in self.words}
        across_variables = {k: (z3.String("{}-across".format(k)), v) for k, v in self.acrosses.items()}
        down_variables = {k: (z3.String("{}-down".format(k)), v) for k, v in self.downs.items()}
        print("here")
        s = z3.Solver()
        for variable, info in list(across_variables.values()) + list(down_variables.values()):
            # constrain every variable to be a valid word of the correct length
            s.add(z3.Or([variable == word for word in self.words_by_length[len(info)]])) # [w for w in word_constants if z3.Length(w) == len(info)]]))
            # s.add(z3.IsMember(variable, self.words_set))
            # s.add(lambda x: True)
            # s.add(z3.Contains(z3.StringVal(self.words), variable))#  in word_constants)
            print(variable)
            # of the correct length
            # s.add(z3.Length(variable) == len(info))

        print(self.acrosses)
        for restriction, indices in self.restrictions.items():
            across = across_variables[restriction[0]][0]
            down = down_variables[restriction[1]][0]
            across_ix, down_ix = indices
            s.add(z3.SubString(across, across_ix, 1) == z3.SubString(down, down_ix, 1))

        print("here")
        # constrain every variable to be the correct length
        print(s)
        print(s.check())
        print(s.model())

    def __str__(self):
        black_square = "▩"
        white_square = "▣"
        st = ""
        for row in self.grid[1:-1]:  # padded
            for elem in row[1:-1]:
                if elem == "-":
                    st += black_square
                elif elem == "?":
                    st += white_square
                else:
                    st += elem.upper()
                st += " "
            st += "\n"
        st += "\n"
        return st

if __name__ == "__main__":
    c = Crossword("grid.txt", DICT)
    print(c)

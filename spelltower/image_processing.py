from PIL import Image
import numpy as np
import glob

N_ROWS = 12
N_COLS = 9
LETTERS_DIR = "letter_images"

LETTER_DIM = 119

# not constant border size for some reason
GRID_X = [14, 136, 258, 380, 503, 625, 747, 870, 992]
GRID_Y = [639, 761, 884, 1006, 1128, 1251, 1373, 1495, 1617, 1740, 1862, 1984]


def screenshot_to_board(path):
    # don't do anything clever, just compare directly with the correct image.
    # assumes screenshot from iphone x.
    img = np.array(Image.open(path))
    platonic_ideals = load_letters()
    board = []
    i = 0
    for row in GRID_Y:
        for col in GRID_X:
            letter_image = img[row:row + LETTER_DIM, col:col + LETTER_DIM]
            nn = nearest_neighbor(letter_image, platonic_ideals)
            board.append(nn)
            i += 1
    board = ''.join(board)
    return board


def nearest_neighbor(letter_image, platonic_ideals):
    best_letter = None
    best_value = None
    letter_image = np.array(Image.fromarray(letter_image).convert('L'), dtype=int)
    for letter, ideal in platonic_ideals.items():
        distance = np.linalg.norm(ideal - letter_image)
        if best_value is None or distance < best_value:
            best_value = distance
            best_letter = letter
    return best_letter


def load_letters():
    letters = {}
    for img_path in glob.glob("{}/*.png".format(LETTERS_DIR)):
        img = np.array(Image.open(img_path).convert('L'), dtype=int)
        letter = img_path[-5]
        letters[letter] = img
    return letters


if __name__ == "__main__":
    print(screenshot_to_board("example2.png"))

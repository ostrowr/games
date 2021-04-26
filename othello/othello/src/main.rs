use std::fmt;

const WHITE: usize = 0;
const BLACK: usize = 1;

/*
  0 1 2 3 4 5 6 7
0
1
2
3
4
5
6
7
*/

const ROWS: [u64; 8] = [
    255,        // 0b11111111;
    65280,      // 0b11111111 << 8
    16711680,   // 0b11111111 << 16
    4278190080, // ...
    1095216660480,
    280375465082880,
    71776119061217280,
    18374686479671623680,
];

const COLS: [u64; 8] = [
    72340172838076673, // 0b0000000100000001000000010000000100000001000000010000000100000001
    144680345676153346,
    289360691352306692,
    578721382704613384,
    1157442765409226768,
    2314885530818453536,
    4629771061636907072,
    9259542123273814144,
];

const WHITE_START: u64 = 68853694464;
const BLACK_START: u64 = 34628173824;

// for every position, compute all possible rays to the edge of the board.
// index by number of trailing zeroes.

fn main() {
    let mut game = Othello::new();
    println!("{}", game);

    let rays = precompute_all_rays();

    println!("{:#?}", rays);

    let mv = Position::from_coordinates(0, 7);

    // for p in mv.neighborhood() {
    //     println!("{}", p)
    // }
}

#[derive(Clone, Copy, Debug)]
struct Position {
    val: u64, // To check: is u64 with trailing_zeroes faster, or u8 with left-shift?
}

impl Position {
    // convert a human-readable move (row, col) to an efficient
    // move that can be ANDed with the board representation
    fn from_coordinates(row: usize, col: usize) -> Position {
        // TODO check if we can strip an assert at runtime
        Position {
            val: ROWS[row] & COLS[col],
        }
    }

    fn to_coordinates(&self) -> (u32, u32) {
        let tz = self.val.trailing_zeros();
        let col = tz % 8;
        let row = tz / 8;
        (row, col)
    }

    fn neighborhood(&self) -> Vec<Position> {
        // should precompute all neighborhoods
        let (row, col) = self.to_coordinates();
        let rows = match row {
            0 => vec![0, 1],
            7 => vec![6, 7],
            o => vec![o - 1, o, o + 1],
        };
        let cols = match col {
            0 => vec![0, 1],
            7 => vec![6, 7],
            o => vec![o - 1, o, o + 1],
        };

        let mut v: Vec<Position> = Vec::new();

        for r in &rows {
            for c in &cols {
                v.push(Position::from_coordinates(*r as usize, *c as usize))
            }
        }
        v
    }
}

fn precompute_all_rays() -> Vec<Vec<Vec<Position>>> {
    (0..64)
        .map(|i| precompute_rays(Position { val: 1 << i }))
        .collect()
}

fn precompute_rays(from: Position) -> Vec<Vec<Position>> {
    println!("{}", from);
    let mut rays: Vec<Vec<Position>> = Vec::new();
    for row_delta in -1..2 {
        for col_delta in -1..2 {
            if row_delta != 0 || col_delta != 0 {
                rays.push(compute_ray_in_direction(from, row_delta, col_delta))
            }
        }
    }
    rays
}

fn compute_ray_in_direction(from: Position, row_delta: i32, col_delta: i32) -> Vec<Position> {
    let mut ray: Vec<Position> = Vec::new();
    let mut curr = from;

    loop {
        let (row, col) = curr.to_coordinates();
        let new_row = row as i32 + row_delta;
        let new_col = col as i32 + col_delta;
        if new_row < 0 || new_row > 7 || new_col < 0 || new_col > 7 {
            break ray;
        }
        curr = Position::from_coordinates(new_row as usize, new_col as usize);
        ray.push(curr);
    }
}

struct Othello {
    // white is 0, black is 1
    board: [u64; 2],
    turn: usize,
    // For each position on the board,
}

impl Othello {
    fn new() -> Othello {
        Othello {
            board: [WHITE_START, BLACK_START],
            turn: 0,
        }
    }

    fn get_rays(from: Position) -> &'static Vec<Vec<Position>> {
        let mut cached_rays: Option<&Vec<Vec<Vec<Position>>>> = None;

        let func = |pos: Position| -> &Vec<Vec<Position>> {
            let rays = cached_rays.unwrap_or_else(|| {
                let precomputed_rays = precompute_all_rays();
                cached_rays = Some(&precomputed_rays);
                &precomputed_rays
            });

            &rays[pos.val.trailing_zeros() as usize]
        };

        func(from)
    }

    fn possible_moves(&self) -> u64 {
        0
    }

    fn available_squares(&self) -> u64 {
        !(self.board[0] | self.board[1])
    }

    fn do_move(&mut self, position: Position) {
        self.board[self.turn] |= position.val;
        // do flips
        self.turn = (self.turn + 1) % 2;
    }
}

impl fmt::Display for Othello {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pretty_board = ["O"; 64];
        for i in 0..64 {
            let pos = 1 << i;
            if self.board[WHITE] & pos != 0 {
                pretty_board[i] = "W";
            } else if self.board[BLACK] & pos != 0 {
                pretty_board[i] = "B";
            }
        }

        let mut out = String::new();
        for chunk in pretty_board.chunks_exact(8) {
            out.push_str(&chunk.join(" ")[..]);
            out.push('\n');
        }

        write!(f, "{}", out)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (row, col) = self.to_coordinates();

        write!(f, "(row: {}, col: {})", row, col)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_and_from_coordinates() {
        for i in 0..8 {
            for j in 0..8 {
                let mv = Position::from_coordinates(i, j);
                let (row, col) = mv.to_coordinates();
                assert_eq!(row, i as u32);
                assert_eq!(col, j as u32);
            }
        }
    }
}

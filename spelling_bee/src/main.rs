use itertools::Itertools;
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

/***********************************************************************************************************************
Rules of spelling bee:
----------------------

Words must include the center letter.
Words must contain at least four letters.
Letters can be used more than once.
Our word list does not include words that are offensive, obscure, hyphenated or proper nouns.
Four-letter words are worth one point each.
Longer words earn one point per letter. A six-letter word is worth six points.
Each puzzle includes at least one “pangram,” which uses every letter at least once.
    A pangram is worth an additional seven points.

Note: there are never any `s`es in the puzzle.
***********************************************************************************************************************/

// Given a word, return its sorted set of unique characters and its score
/**
 *
 * ```
 * let word = "hello";
 * let (sorted, score) = normalize(word);
 * assert_eq!(sorted, "ehlo");
 * assert_eq!(score, 5);
 *
 * let pangram = "abcdefgg";
 * let (sorted, score) = normalize(pangram);
 * assert_eq!(sorted, "abcdefg");
 * assert_eq!(score, 8 + 7); // 8 for length, 7 for pangram bonus
 * ```
 */
fn normalize(word: &str) -> (String, usize) {
    let mut chars = word.chars().collect::<Vec<char>>();
    chars.sort();
    chars.dedup();

    let sorted = chars.into_iter().collect::<String>();

    if word.len() == 4 {
        return (sorted, 1);
    }
    if sorted.len() > 7 {
        return ("".to_string(), 0); // not possible to use more than 7 unique letters
    }
    let pangram_bonus = if sorted.len() == 7 { 7 } else { 0 };
    (sorted, word.len() + pangram_bonus)
}

const LEGAL_LETTERS: [char; 25] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
    /*'s',*/ 't', 'u', 'v', 'w', 'x', 'y', 'z',
];
/**
 * Preprocess a word list into a hashmap of letter -> sorted remaining letters -> total score of all words created
 * by all words that contain the letter and all of the remaining letters.
 */
fn preprocess(filename: &str) -> HashMap<char, HashMap<String, (usize, Vec<String>)>> {
    let word_scores: HashMap<String, (usize, Vec<String>)> = read_lines(filename)
        .unwrap()
        .flatten()
        .filter_map(|word| {
            // words must be at least 4 characters long
            if word.len() <= 3 {
                return None;
            }
            if !word.chars().all(|c| LEGAL_LETTERS.contains(&c)) {
                return None;
            }

            let (normalized, score) = normalize(&word);
            if word.is_empty() {
                return None;
            }
            Some((normalized, score, word))
        })
        .fold(HashMap::new(), |mut map, (normalized, score, original)| {
            map.entry(normalized)
                .and_modify(|prev| {
                    prev.0 += score;
                    prev.1.push(original.clone());
                })
                .or_insert((score, vec![original]));
            map
        });

    let by_letter: HashMap<char, HashMap<String, (usize, Vec<String>)>> = LEGAL_LETTERS
        .iter()
        .map(|c| {
            let words = word_scores
                .iter()
                .filter(|(word, _)| word.contains(*c))
                .map(|(word, score)| (word.replace(*c, ""), score.clone()))
                .collect();
            (*c, words)
        })
        .collect();

    by_letter
}

fn score_board(
    preprocessed: &HashMap<char, HashMap<String, (usize, Vec<String>)>>,
    center_letter: &char,
    other_letters: &str, // must be sorted
    print_words: bool,
) -> usize {
    debug_assert!(other_letters.chars().all(|c| c.is_ascii_lowercase()));
    debug_assert!(other_letters
        .chars()
        .collect::<Vec<char>>()
        .windows(2)
        .all(|pair| pair[0] < pair[1]));
    let word_scores = preprocessed.get(center_letter).unwrap();
    let total_score = other_letters
        .chars()
        .combinations(3)
        .chain(other_letters.chars().combinations(4))
        .chain(other_letters.chars().combinations(5))
        .chain(other_letters.chars().combinations(6))
        .map(|letters| {
            let normalized = letters.iter().collect::<String>();
            let default = (0, vec![]);
            let (score, words) = word_scores.get(&normalized).unwrap_or(&default);
            if print_words {
                // todo actually collect these and print them in sorted order
                println!(
                    "\tUsing {}{}: {}",
                    center_letter.to_uppercase(),
                    normalized,
                    words.join(", ")
                );
            }
            *score
        })
        .sum();

    total_score
}

fn main() {
    // hashmap between normalized words and the total score for all words that normalize to the same key
    let preprocessed = preprocess("enable.txt");

    let scores = LEGAL_LETTERS.iter().flat_map(|center_letter| {
        eprintln!("Processing center letter: {}", center_letter);
        LEGAL_LETTERS
            .iter()
            .filter(|c| c != &center_letter)
            .combinations(6)
            .filter_map(|letters| {
                let letters: String = letters.iter().copied().collect();
                let score = score_board(&preprocessed, center_letter, &letters, false);
                if score == 0 {
                    return None;
                }
                Some((center_letter, letters, score))
            })
            .collect::<Vec<_>>()
    });

    scores
        .sorted_by_key(|x| -(x.2 as i32))
        .take(10)
        .for_each(|(center, letters, score)| {
            println!("{} {} {}", center, letters, score);
            score_board(&preprocessed, center, &letters, true);
        });
}

// The output is wrapped in a Result to allow matching on errors.
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

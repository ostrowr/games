use itertools::Itertools;
use std::cmp;
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

/**
 * Preprocess a word list into a hashmap of letter -> sorted remaining letters -> total score of all words created
 * by all words that contain the letter and all of the remaining letters.
 */
fn preprocess(filename: &str) -> HashMap<char, HashMap<String, usize>> {
    let word_scores: HashMap<String, usize> = read_lines(filename)
        .unwrap()
        .flatten()
        .filter_map(|word| {
            // words must be at least 4 characters long
            if word.len() <= 3 {
                return None;
            }
            if !word.chars().all(|c| c.is_ascii_lowercase()) {
                return None;
            }

            let (word, score) = normalize(&word);
            if word.is_empty() {
                return None;
            }
            Some((word, score))
        })
        .fold(HashMap::new(), |mut map, (word, score)| {
            map.entry(word)
                .and_modify(|prev_score| *prev_score += score)
                .or_insert(score);
            map
        });

    let by_letter: HashMap<char, HashMap<String, usize>> = ('a'..='z')
        .filter(|c| c != &'s')
        .map(|c| {
            let words = word_scores
                .iter()
                .filter(|(word, _)| word.contains(c))
                .map(|(word, score)| (word.replace(c, ""), *score))
                .collect();
            (c, words)
        })
        .collect();

    by_letter
}

fn score_board(
    preprocessed: &HashMap<char, HashMap<String, usize>>,
    center_letter: &char,
    other_letters: &str,
) -> usize {
    let word_scores = preprocessed.get(center_letter).unwrap();
    let total_score = other_letters
        .chars()
        .combinations(3)
        .chain(other_letters.chars().combinations(4))
        .chain(other_letters.chars().combinations(5))
        .chain(other_letters.chars().combinations(6))
        .map(|letters| letters.iter().collect::<String>())
        .map(|word| word_scores.get(&word).unwrap_or(&0))
        .sum();

    total_score
}

fn main() {
    // hashmap between normalized words and the total score for all words that normalize to the same key
    let preprocessed = preprocess("/usr/share/dict/words");

    let scores = ('a'..='z').filter(|c| c != &'s').flat_map(|center_letter| {
        println!("Processing center letter: {}", center_letter);
        ('a'..='z')
            .filter(|c| c != &'s')
            .filter(|c| c != &center_letter)
            .combinations(6)
            .map(|letters| {
                let letters: String = letters.iter().collect();
                let score = score_board(&preprocessed, &center_letter, &letters);
                (center_letter, letters, score)
            })
            .collect::<Vec<_>>()
    });

    let top_10 = scores
        .sorted_by_key(|x| -(x.2 as i32))
        .take(10)
        .collect::<Vec<_>>();
    println!("{:?}", top_10);
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

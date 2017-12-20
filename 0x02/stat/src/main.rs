use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;
use std::collections::HashMap;
use std::env::args;

// ADVANGES: full unicode support (e.g. arabic/thai or difficult languages) + proper stripping out
// of non-unicode-word special characters
// ISSUES:
// 1) speed. lookups into word breaks are performed w/ trees instead of hash tables
// 2) chinese cannot be split with the standard unicode UAX 29 algorithm. a separate dictionary for
//    chinese words must be implemented, which is outside the scope of this project
extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

fn main()
{
    // OPEN FILE
    let filename = match args().nth(1)
    {
        Some(x) => x,
        _ => {eprintln!("ERROR: please give me a filename!"); return}
    };
    let file = match File::open(&filename)
    {
        Ok(file) => BufReader::new(file),
        Err(e) => {eprintln!("ERROR: could not open {} due to \"{}\"", filename, e); return}
    };

    // GENERATE DATA STRUCTURES
    let mut d_words = HashMap::new();
    for line in file.lines()
    {
        let line = match line
        {
            Ok(line) => line.to_lowercase(),
            _ => {eprintln!("ERROR: file not UTF8");return}
        };
        //for word in line.split_whitespace() // does not handle special languages, but is faster
        for word in UnicodeSegmentation::unicode_words(&line[..])
        {
            *d_words.entry(word.to_owned()).or_insert(0) += 1;
        }
    }

    let mut d_lengths = HashMap::new();
    for (key,value) in d_words.iter()
    {
        *d_lengths.entry(key.len() as u64).or_insert(0) += value;
    }

    // GENERATE STATISTICS
    // 1. total #words
    // 2. avg word len
    // 3. top lengths and #words
    // 4. top 10
    let tot:u64 = match d_lengths.values().sum()
    {
        0 => { eprintln!("NOTE: file is empty, program will abort!"); return; }
        x => x
    };
    let avg = d_lengths.iter().fold(0, |prev,(value, weight)| prev + value*weight) / tot;
    let stat= 
    {
        let mut stat:Vec<_> = d_lengths.iter().collect();
        stat.sort();
        stat
    };
    let popular:Vec<_>=
    {
        let mut popular: Vec<_> = d_words.iter().collect();
        popular.sort_by(|a,b| b.1.cmp(a.1));
        popular
    };
    let popular:Vec<_> = popular.iter().take(10).collect();

    // OUTPUT
    let max_width : usize= match popular.iter().map(|t| t.0.len()).max() 
    {
        Some(n) if n<=30 => n,
        _ => 10
    };
    println!(">>> Total Words: {}", tot);
    println!(">>> Average Length: {}", avg);
    println!(">>> Top Lengths:");
    for (len, num) in stat
    {
        println!("  {:3} chars => {:5} times", len, num);
    };
    println!(">>> Top Words:");
    for (i,&&(word, num)) in popular.iter().enumerate()
    {
        println!("  {:#>2}. {:width$} => {:5} times", i+1, word, num, width=max_width);
    };
}

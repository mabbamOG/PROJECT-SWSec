// NEED TO CONSIDER SPECIAL CHARS + ELLIPSIS!!!
use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;
use std::collections::HashMap;
use std::env::args;

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
            Ok(line) => line,
            _ => {eprintln!("ERROR: file not UTF8");return}
        };
        for word in line.split_whitespace().map(|s| s.to_lowercase())
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
    let avg = 
    {
        let lenwsum = d_lengths.iter().fold(0, |prev,(value, weight)| prev + value*weight);
        let weightsum :u64  = d_lengths.values().sum();
        lenwsum / weightsum
    };
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
    println!("Total Words: {}", tot);
    println!("\nAverage Length: {}", avg);
    println!("\nTop Lengths:");
    for (len, num) in stat
    {
        println!("{:3} chars => {:5}", len, num);
    };
    println!("\nTop Words:");
    for (i,&(word, num)) in (1..).zip(popular)
    {
        println!("{:2}. {:10} => {:5}", i, word, num);
    };
}

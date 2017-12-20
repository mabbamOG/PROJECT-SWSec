use std::io::{self, Write};
mod sortedcontainer;
use sortedcontainer::SortedContainer;

#[derive(Debug)]
enum Command {
    Insert { age: i32, name: String },
    Erase { age: i32, name: String },
    Contains { age: i32, name: String },
    Print,
    Exit,
    Error(String),
}

fn parse_command(input: String) -> Command {
    let command_items: Vec<&str> = input.split_whitespace().collect();

    // FIX: crash during [0] access
    if command_items.first().is_none() {
        return Command::Error("Invalid command.".to_string());
    }
    match (command_items[0], command_items.len()) {
        ("p", 1) => Command::Print,
        ("x", 1) => Command::Exit,
        ("i", 3) => {
            if let Ok(age) = command_items[1].parse::<i32>() {
                // FIX: check for age
                if age >= 0 {
                    Command::Insert {
                        age: age,
                        name: command_items[2].to_string(),
                    }
                } else {
                    Command::Error("Invalid input.".to_string())
                }
            } else {
                Command::Error("unable to parse int (age).".to_string())
            }
        }
        ("e", 3) => {
            if let Ok(age) = command_items[1].parse::<i32>() {
                Command::Erase {
                    age: age,
                    name: command_items[2].to_string(),
                }
            } else {
                Command::Error("unable to parse int (age).".to_string())
            }
        }
        ("c", 3) => {
            if let Ok(age) = command_items[1].parse::<i32>() {
                Command::Contains {
                    age: age,
                    name: command_items[2].to_string(),
                }
            } else {
                Command::Error("unable to parse int (age).".to_string())
            }
        }

        (_, _) => Command::Error("invalid command.".to_string()),
    }
}

fn main() {
    let mut sc = SortedContainer::new();
    loop {
        let mut input = String::new();
        print!("> ");
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                match parse_command(input) {
                    Command::Insert { age, name } => sc.insert(age, name),
                    Command::Erase { age, name } => sc.erase(age, name),
                    Command::Contains { age, name } => {
                        match sc.contains(age, name) {
                            true => println!("y"),
                            false => println!("n"),
                        }
                    }
                    Command::Print => {
                        sc.print();
                    }
                    Command::Exit => {
                        println!("Exiting...");
                        break;
                    }
                    Command::Error(error) => {
                        println!("Error: {}", error);
                    }
                }
            }
            Err(error) => println!("Error: {}", error),
        }
    }
}

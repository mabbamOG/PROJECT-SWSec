#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
use std::io::{self, Write};
use std::cmp::Ordering;
use std::ops::Deref;
use std::fmt::Display;

#[derive(Debug)]
enum Command 
{
    Insert{age: i32, name: String},
    Erase{age: i32, name: String},
    Contains{age: i32, name: String},
    Print,
    Exit,
    Error(String)
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
struct Node
{
    age: i32, // must come before name in the definition!
    name: String,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>
}
#[derive(Debug, PartialEq, Eq)]
struct SortedContainer
{
    root: Option<Box<Node>>
}
impl Display for Node
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result 
    {
        self.fmt_rec(1, f)
    }
}
impl Display for SortedContainer
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result 
    {
        match self.root 
        {
            None => write!(f, " (nil)"), // TODO writeln?
            Some(ref node) => write!(f, "{}", node)
        }
    }
}
impl Node // TODO make PRIVATE
{
    fn new(age: i32, name: String) -> Self
    {
        Node{age, name, left:None, right:None}
    }
    fn fmt_rec(&self, count: usize, f: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        write!(f, "{:width$}", "", width=count)?;
        writeln!(f, "{} {}", self.age, self.name)?;
        match self.left
        {
            None => { write!(f, "{:width$}", "", width=count+1)?; writeln!(f, "(nil)") }
            Some(ref node) => node.fmt_rec(count + 1, f)
        }?;
        match self.right
        {
            None => { write!(f, "{:width$}", "", width=count+1)?; writeln!(f, "(nil)") }
            Some(ref node) => node.fmt_rec(count + 1, f)
        }

    }
    fn contains(&self, other: &Node) -> bool
    {
        match other.cmp(self)
        {
            Ordering::Equal => true,
            Ordering::Less => match self.left
            {
                None => false,
                Some(ref n) => n.contains(&other)
            }
            Ordering::Greater => match self.right
            {
                None => false,
                Some(ref n) => n.contains(&other)
            }
        }
    }
    fn insert(&mut self, other: Node)
    {
        match other.cmp(&*self)
        {
            Ordering::Equal => return,
            Ordering::Less => match self.left
            {
                None => self.left = Some(Box::new(other)),
                Some(ref mut node) => node.insert(other)
            }
            Ordering::Greater => match self.right
            {
                None => self.right = Some(Box::new(other)),
                Some(ref mut node) => node.insert(other)
            }
        };
    }
}
impl SortedContainer
{
    fn new() -> Self
    {
        SortedContainer{root: None }
    }
    fn contains(&self, age:i32, name:String) -> bool
    {
        match self.root
        {
            None => false,
            Some(ref root) => { root.contains(&Node::new(age, name)) }
        }
    }
    fn insert(&mut self, age:i32, name:String)
    {
        let tmp = Node::new(age, name);
        match self.root
        {
            None => self.root = Some(Box::new(tmp)),
            Some(ref mut node) => node.insert(tmp)
        };
    }
}
/*
fn index(this: &mut Option<Box<Node>>, other: &Node) -> Option<Box<Node>>
{
    match self
    {
        None => self,
        Some(ref node) => match node.cmp(other)
        {
            Ordering::Equal => self,
            Ordering::Less => index(node.left),
            Ordering::Greater => index(node.right)
        }
    }
}
*/
/*
enum Tree
{
    Nil,
    // Leaf ...
    Node { name: String, age: i32, left: Box<Tree>, right: Box<Tree> }
}
*/

fn parse_command(input: String) -> Command 
{
    let command_items: Vec<&str> = input.split_whitespace().collect();
    match (command_items[0], command_items.len()) 
    { // TODO CRASH?
        ("p", 1) => Command::Print,
        ("x", 1) => Command::Exit,
        ("i", 3) => 
        {
            if let Ok(age) = command_items[1].parse::<i32>() 
            {
                Command::Insert{age: age, name: command_items[2].to_string()}
            } else 
            {
                Command::Error("unable to parse int (age).".to_string())
            }
        },
        ("e", 3) => 
        {
            if let Ok(age) = command_items[1].parse::<i32>() 
            {
                Command::Erase{age: age, name: command_items[2].to_string()}
            } else {
                Command::Error("unable to parse int (age).".to_string())
            }
        },
        ("c", 3) => 
        {
            if let Ok(age) = command_items[1].parse::<i32>() 
            {
                Command::Contains{age: age, name: command_items[2].to_string()}
            } else 
            {
                Command::Error("unable to parse int (age).".to_string())
            }
        },

        (_, _) => Command::Error("invalid command.".to_string())
    }
}

fn main() 
{
    test();
    let mut sc = SortedContainer::new();
    loop 
    {
        let mut input = String::new();
        print!("> ");
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut input) 
        {
            Ok(_) => 
            {
                match parse_command(input) 
                {
                    Command::Insert{age, name} => 
                    {
                        sc.insert(age, name)
                    },
                    Command::Erase{age, name} => 
                    {
                        unimplemented!();
                    },
                    Command::Contains{age, name} => 
                    {
                        match sc.contains(age, name)
                        {
                            true => println!("y"),
                            false =>  println!("n")
                        }
                    }
                    Command::Print => 
                    {
                        println!("{}", sc);
                    },
                    Command::Exit => 
                    {
                        println!("Exiting...");
                        break;
                    },
                    Command::Error(error) => 
                    {
                        println!("Error: {}", error);
                    }
                }
            }
            Err(error) => println!("Error: {}", error),
        }
    }
}
fn test()
{
    test_node_derive();
    test_node_method();
    test_tree();
}
fn test_node_derive()
{
    // EQ????? TODO
    //let n1 = Node {name:"Bob".to_string(), age:32, left:None, right:None};
    //let n2 = Node {name:"Alice".to_string(), age:22, left:None, right:None};
    //let n3 = Node {name:"Mark".to_string(), age:32, left:None, right:None};
    //let n4 = Node {name:"Bob".to_string(), age:32, left:None, right:None};
    //let n5 : Node= std::default::Default::default();
    //println!("{:?}", n1);
    //println!("{:?} == {:?}? {}", n1, n2, n1==n2);
    //println!("{:?} < {:?}? {}",n1 ,n3, n1<n3);
    //println!("{:?}", n5);
    // INIT
    //let ins_copy = Node{age:32, name:"Alice".to_string(), left: None, right: None};
    /*
    let mut n = Node{age:22, name:"Bob".to_string(), left: None, right: None};
    {
        let ins = Node{age:32, name:"Alice".to_string(), left: None, right: None};
        n.left = Some(Box::new(ins));
    }
    //assert!(*n.left.unwrap().deref() == ins);
    //ins.name = "Cindy".to_string();
    //assert!(*n.left.unwrap().deref() != ins);
    //println!("{:?}", ins);
    println!("{:?}", n);
    */

    //println!("{:?}", n);
    //println!("{:?}", n);

    // CMP: OBVIOUS
    let n1 = Node {name:"B".to_string(), age:22, left:None, right:None};
    let n2 = Node {name:"C".to_string(), age:23, left:None, right:None};
    assert!(n1<n2);
    // CMP: NAME
    let n1 = Node {name:"B".to_string(), age:22, left:None, right:None};
    let n2 = Node {name:"C".to_string(), age:22, left:None, right:None};
    assert!(n1<n2);
    // CMP: NUMBER
    let n1 = Node {name:"B".to_string(), age:22, left:None, right:None};
    let n2 = Node {name:"B".to_string(), age:23, left:None, right:None};
    assert!(n1<n2);
    // CMP: NAME VS NUMBER
    let n1 = Node {name:"C".to_string(), age:22, left:None, right:None};
    let n2 = Node {name:"B".to_string(), age:23, left:None, right:None};
    assert!(n1<n2);
    // TODO DEFAULT
    // TODO DISPLAY
}
fn test_node_method()
{
    // CONTAINS
    let n = Node{age:22, name:"Bob".to_string(), left: None, right: None};
    let n2 = Node{age:22, name:"Bob".to_string(), left: None, right: None};
    let n3 = Node{age:22, name:"Alice".to_string(), left: None, right: None};
    assert_eq!(n.contains(&n2), true);
    assert_eq!(n.contains(&n3), false);
    // INSERT
    let mut sc = SortedContainer::new();
    sc.insert(22, "Bob".to_string());
    sc.insert(22, "Amarena".to_string());
    sc.insert(22, "Crozza".to_string());
    sc.insert(23, "Amarena".to_string());

    let expected = SortedContainer{root: 
        Some(Box::new(Node{age:22, name:"Bob".to_string(), 
            left: Some(Box::new(Node::new(22, "Amarena".to_string()))),
            right: Some(Box::new(Node{age:22, name: "Crozza".to_string(),
                    left: None,
                    right: Some(Box::new(Node::new(23, "Amarena".to_string())))
                }))
        }))
    };
    assert_eq!(sc, expected);

    // PRINT
    let mut sc = SortedContainer::new();
    sc.insert(22, "Bob".to_string());
    sc.insert(22, "Amarena".to_string());
    sc.insert(22, "Crozza".to_string());
    sc.insert(23, "Amarena".to_string());
    let expected = " 22 Bob\n  22 Amarena\n   (nil)\n   (nil)\n  22 Crozza\n   (nil)\n   23 Amarena\n    (nil)\n    (nil)\n";
    assert_eq!(format!("{}", sc), expected);
    
    // TODO DELETE
}
fn test_tree()
{
    let t = SortedContainer::new();
    assert_eq!(t.root, None);
    let n = Node{age:22, name:"Bob".to_string(), left:None, right:None};
}

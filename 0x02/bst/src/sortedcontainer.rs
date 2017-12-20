use std;
use std::fmt::Display;
use std::cmp::Ordering;

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
struct Node {
    age: i32,
    name: String,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SortedContainer {
    root: Option<Box<Node>>,
}
impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_rec(1, f)
    }
}
impl Display for SortedContainer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.root {
            None => writeln!(f, " (nil)"),
            Some(ref node) => write!(f, "{}", node),
        }
    }
}
impl Node {
    fn new(age: i32, name: String) -> Self {
        Node {
            age,
            name,
            left: None,
            right: None,
        }
    }

    /// This function provides the same sort of equality found in the original C program
    fn shallow_cmp(&self, other: &Node) -> Ordering {
        match self.age.cmp(&other.age) {
            Ordering::Equal => self.name.cmp(&other.name),
            x => x,
        }
    }

    /// This function is useful for implementing the Display trait
    fn fmt_rec(&self, count: usize, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:width$}", "", width = count)?;
        writeln!(f, "{} {}", self.age, self.name)?;
        match self.left {
            None => {
                write!(f, "{:width$}", "", width = count + 1)?;
                writeln!(f, "(nil)")
            }
            Some(ref node) => node.fmt_rec(count + 1, f),
        }?;
        match self.right {
            None => {
                write!(f, "{:width$}", "", width = count + 1)?;
                writeln!(f, "(nil)")
            }
            Some(ref node) => node.fmt_rec(count + 1, f),
        }

    }
    fn contains(&self, other: &Node) -> bool {
        match other.shallow_cmp(self) {
            Ordering::Equal => true,
            Ordering::Less => {
                match self.left {
                    None => false,
                    Some(ref node) => node.contains(other),
                }
            }
            Ordering::Greater => {
                match self.right {
                    None => false,
                    Some(ref node) => node.contains(other),
                }
            }
        }
    }
    fn insert(&mut self, other: Node) {
        match other.shallow_cmp(self) {
            Ordering::Equal => return,
            Ordering::Less => {
                match self.left {
                    None => self.left = Some(Box::new(other)),
                    Some(ref mut node) => node.insert(other),
                }
            }
            Ordering::Greater => {
                match self.right {
                    None => self.right = Some(Box::new(other)),
                    Some(ref mut node) => node.insert(other),
                }
            }
        };
    }
    /// This function places a link in the rightmost empty spot in the Tree
    fn place_right(&mut self, link: Option<Box<Node>>) {
        match self.right {
            None => self.right = link,
            Some(ref mut node) => node.place_right(link),
        }
    }
    /// This function returns an Option<> of a "link" (Option<Box<Node>>)
    /// This is because the parent needs to modify its own fields
    /// depending on a link given by the son
    /// The option implies whether a link needs to be placed to self.left
    /// If the son returns None, then no action needs to be taken.
    fn erase(&mut self, other: Node) -> Option<Option<Box<Node>>> {
        match other.shallow_cmp(self) {
            Ordering::Equal => {
                // juast return the correct link to place to father's left
                match (self.left.take(), self.right.take()) {
                    (None, None) => Some(None),
                    (link, None) => Some(link),
                    (None, link) => Some(link),
                    (Some(mut x), y) => {
                        // in this case, also move the right link under node
                        x.place_right(y);
                        Some(Some(x))
                    }
                }
            }
            Ordering::Less => {
                match self.left.take() {
                    None => None,
                    Some(mut node) => {
                        match node.erase(other) {
                            None => None,
                            Some(link) => {
                                // take action, then you're done
                                self.left = link;
                                None
                            }
                        }
                    }
                }
            }
            Ordering::Greater => {
                match self.right.take() {
                    None => None,
                    Some(mut node) => {
                        match node.erase(other) {
                            None => None,
                            Some(link) => {
                                // take action, then you're done
                                self.right = link;
                                None
                            }
                        }
                    }
                }
            }
        }
    }
}
impl SortedContainer {
    pub fn new() -> Self {
        SortedContainer { root: None }
    }
    pub fn print(&self) {
        println!("{}", self);
    }
    pub fn contains(&self, age: i32, name: String) -> bool {
        match self.root {
            None => false,
            Some(ref node) => node.contains(&Node::new(age, name)),
        }
    }
    pub fn insert(&mut self, age: i32, name: String) {
        let tmp = Node::new(age, name);
        match self.root {
            None => self.root = Some(Box::new(tmp)),
            Some(ref mut node) => node.insert(tmp),
        };
    }
    pub fn erase(&mut self, age: i32, name: String) {
        match self.root.take() {
            None => return,
            Some(mut node) => {
                match node.erase(Node::new(age, name)) {
                    // the son requested that we take action
                    Some(link) => self.root = link,

                    // we do nothing, aka we reverse take()
                    None => self.root = Some(node),
                }
            }
        };
    }
}

#[cfg(test)]
mod tests_node {
    use super::*;


    #[test]
    fn new() {
        let n = Node::new(22, "BOB".to_string());
        assert_eq!(n.age, 22);
        assert_eq!(n.name, "BOB");
    }

    #[test]
    fn eq() {
        let n1 = Node::new(22, "BOB".to_string());
        let n2 = Node::new(22, "BOB".to_string());
        let n3 = Node::new(22, "bob".to_string());
        let n4 = Node {
            age: 22,
            name: "BOB".to_string(),
            left: Some(Box::new(Node::new(1, "One".to_string()))),
            right: None,
        };
        assert_eq!(n1, n2);
        assert_ne!(n1, n3);
        assert_ne!(n1, n4);
    }

    #[test]
    fn cmp() {
        // simple
        let n1 = Node::new(22, "B".to_string());
        let n2 = Node::new(23, "C".to_string());
        assert!(n1 < n2);

        // name
        let n1 = Node::new(22, "B".to_string());
        let n2 = Node::new(22, "C".to_string());
        assert!(n1 < n2);

        // number
        let n1 = Node::new(22, "B".to_string());
        let n2 = Node::new(23, "B".to_string());
        assert!(n1 < n2);

        // name vs number
        let n1 = Node::new(22, "C".to_string());
        let n2 = Node::new(23, "B".to_string());
        assert!(n1 < n2);

        // deep cmp
        let n1 = Node::new(22, "BOB".to_string());
        let n2 = Node {
            age: 22,
            name: "BOB".to_string(),
            left: Some(Box::new(Node::new(22, "BOB".to_string()))),
            right: None,
        };
        assert!(n1 < n2);

        // shallow cmp
        let n1 = Node::new(22, "BOB".to_string());
        let n2 = Node {
            age: 22,
            name: "BOB".to_string(),
            left: Some(Box::new(Node::new(22, "BOB".to_string()))),
            right: None,
        };
        assert_eq!(n1.shallow_cmp(&n2), Ordering::Equal);
    }

    #[test]
    fn place_right() {
        // simple
        let mut n = Node::new(22, "BOB".to_string());
        let opt = Some(Box::new(Node::new(22, "ACE".to_string())));
        n.place_right(opt);
        let expected = Node {
            age: 22,
            name: "BOB".to_string(),
            left: None,
            right: Some(Box::new(Node::new(22, "ACE".to_string()))),
        };
        assert_eq!(n, expected);

        // more complex
        let mut n = Node::new(22, "BOB".to_string());
        n.insert(Node::new(22, "ACE".to_string()));
        n.insert(Node::new(23, "ACE".to_string()));
        let opt = Some(Box::new(Node::new(24, "OPERA".to_string())));
        n.place_right(opt);
        let expected = Node {
            age: 22,
            name: "BOB".to_string(),
            left: Some(Box::new(Node::new(22, "ACE".to_string()))),
            right: Some(Box::new(Node {
                age: 23,
                name: "ACE".to_string(),
                left: None,
                right: Some(Box::new(Node::new(24, "OPERA".to_string()))),
            })),
        };
        assert_eq!(n, expected);
    }

}
#[cfg(test)]
mod tests_sc {
    use super::*;
    #[test]
    fn new() {
        let sc = SortedContainer::new();
        assert!(sc.root.is_none());

        let mut sc = SortedContainer { root: Some(Box::new(Node::new(22, "Bob".to_string()))) };
        assert!(sc.root.is_some());
        let n = sc.root.take().unwrap();
        assert_eq!(n.age, 22);
        assert_eq!(n.name, "Bob");
    }

    #[test]
    fn eq() {
        let sc1 = SortedContainer::new();
        let sc2 = SortedContainer::new();
        let sc3 = SortedContainer { root: Some(Box::new(Node::new(22, "BOB".to_string()))) };
        let sc4 = SortedContainer {
            root: Some(Box::new(Node {
                age: 22,
                name: "BOB".to_string(),
                left: Some(Box::new(Node::new(22, "BOB".to_string()))),
                right: None,
            })),
        };
        let sc5 = SortedContainer {
            root: Some(Box::new(Node {
                age: 22,
                name: "BOB".to_string(),
                left: None,
                right: Some(Box::new(Node::new(23, "BOB".to_string()))),
            })),
        };
        let sc6 = SortedContainer {
            root: Some(Box::new(Node {
                age: 22,
                name: "BOB".to_string(),
                left: Some(Box::new(Node::new(23, "BOB".to_string()))),
                right: Some(Box::new(Node::new(23, "BOB".to_string()))),
            })),
        };
        assert_eq!(sc1, sc2);
        assert_ne!(sc1, sc3);
        assert_ne!(sc3, sc4);
        assert_ne!(sc3, sc5);
        assert_ne!(sc4, sc5);
        assert_ne!(sc4, sc6);
        assert_ne!(sc5, sc6);
    }

    #[test]
    fn display() {
        // simple
        let sc = SortedContainer::new();
        let expected = " (nil)\n";
        assert_eq!(format!("{}", sc), expected);

        // complex
        let mut sc = SortedContainer::new();
        sc.insert(22, "Bob".to_string());
        sc.insert(22, "Amarena".to_string());
        sc.insert(22, "Crozza".to_string());
        sc.insert(23, "Amarena".to_string());
        let expected = " 22 Bob\n  22 Amarena\n   (nil)\n   (nil)\n".to_string() +
            "  22 Crozza\n   (nil)\n   23 Amarena\n    (nil)\n    (nil)\n";
        assert_eq!(format!("{}", sc), expected);
    }

    #[test]
    fn contains() {
        let sc = SortedContainer {
            root: Some(Box::new(Node {
                age: 22,
                name: "Bob".to_string(),
                left: Some(Box::new(Node::new(22, "Amarena".to_string()))),
                right: Some(Box::new(Node {
                    age: 22,
                    name: "Crozza".to_string(),
                    left: None,
                    right: Some(Box::new(Node::new(23, "Amarena".to_string()))),
                })),
            })),
        };
        assert!(sc.contains(22, "Bob".to_string()));
        assert!(!sc.contains(22, "bob".to_string()));
        assert!(!sc.contains(21, "Bob".to_string()));
        assert!(sc.contains(22, "Amarena".to_string()));
        assert!(sc.contains(22, "Crozza".to_string()));
        assert!(sc.contains(23, "Amarena".to_string()));
    }

    #[test]
    fn insert() {
        let mut sc = SortedContainer::new();
        sc.insert(22, "Bob".to_string());
        sc.insert(22, "Amarena".to_string());
        sc.insert(22, "Crozza".to_string());
        sc.insert(23, "Amarena".to_string());
        let expected = SortedContainer {
            root: Some(Box::new(Node {
                age: 22,
                name: "Bob".to_string(),
                left: Some(Box::new(Node::new(22, "Amarena".to_string()))),
                right: Some(Box::new(Node {
                    age: 22,
                    name: "Crozza".to_string(),
                    left: None,
                    right: Some(Box::new(Node::new(23, "Amarena".to_string()))),
                })),
            })),
        };
        assert_eq!(sc, expected);
    }

    #[test]
    fn erase() {
        // both leaves null
        let mut sc = SortedContainer::new();
        sc.insert(22, "BOB".to_string());
        sc.erase(22, "BOB".to_string());
        assert_eq!(sc, SortedContainer { root: None });

        // remove leaf left
        let mut sc = SortedContainer::new();
        sc.insert(22, "BOB".to_string());
        sc.insert(22, "ACE".to_string());
        sc.erase(22, "ACE".to_string());
        assert_eq!(
            sc,
            SortedContainer { root: Some(Box::new(Node::new(22, "BOB".to_string()))) }
        );

        // remove leaf right
        let mut sc = SortedContainer::new();
        sc.insert(22, "BOB".to_string());
        sc.insert(23, "ACE".to_string());
        sc.erase(23, "ACE".to_string());
        assert_eq!(
            sc,
            SortedContainer { root: Some(Box::new(Node::new(22, "BOB".to_string()))) }
        );

        // right is null
        let mut sc = SortedContainer::new();
        sc.insert(22, "BOB".to_string());
        sc.insert(22, "ACE".to_string());
        sc.erase(22, "BOB".to_string());
        assert_eq!(
            sc,
            SortedContainer { root: Some(Box::new(Node::new(22, "ACE".to_string()))) }
        );

        // left is null
        let mut sc = SortedContainer::new();
        sc.insert(22, "BOB".to_string());
        sc.insert(23, "ACE".to_string());
        sc.erase(22, "BOB".to_string());
        assert_eq!(
            sc,
            SortedContainer { root: Some(Box::new(Node::new(23, "ACE".to_string()))) }
        );

        // both leaves full
        let mut sc = SortedContainer::new();
        sc.insert(22, "BOB".to_string());
        sc.insert(22, "ACE".to_string());
        sc.insert(23, "ACE".to_string());
        sc.erase(22, "BOB".to_string());
        let mut expected = SortedContainer::new();
        expected.insert(22, "ACE".to_string());
        expected.insert(23, "ACE".to_string());
        assert_eq!(sc, expected);


        // complex
        let mut sc = SortedContainer::new();
        sc.insert(10, "Ten".to_string());
        sc.insert(5, "Five".to_string());
        sc.insert(6, "Six".to_string());
        sc.insert(7, "Seven".to_string());
        sc.insert(15, "Fifteen".to_string());
        sc.erase(10, "Ten".to_string());
        let mut expected = SortedContainer::new();
        expected.insert(5, "Five".to_string());
        expected.insert(6, "Six".to_string());
        expected.insert(7, "Seven".to_string());
        expected.insert(15, "Fifteen".to_string());
        assert_eq!(sc, expected);

        // more complex
        let mut sc = SortedContainer::new();
        sc.insert(10, "Ten".to_string());
        sc.insert(5, "Five".to_string());
        sc.insert(3, "Three".to_string());
        sc.insert(15, "Fifteen".to_string());
        sc.insert(6, "Six".to_string());
        sc.insert(7, "Seven".to_string());
        sc.insert(8, "Eight".to_string());
        sc.insert(9, "Nine".to_string());
        sc.erase(10, "Ten".to_string());
        let mut expected = SortedContainer::new();
        expected.insert(5, "Five".to_string());
        expected.insert(3, "Three".to_string());
        expected.insert(6, "Six".to_string());
        expected.insert(7, "Seven".to_string());
        expected.insert(8, "Eight".to_string());
        expected.insert(9, "Nine".to_string());
        expected.insert(15, "Fifteen".to_string());
        assert_eq!(sc, expected);
    }
}

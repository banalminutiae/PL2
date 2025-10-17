use std::io;
mod token;
mod lexer;

fn main() {
    let mut src = String::new();
    let _ = io::stdin().read_line(&mut src);

    println!("{}", src);
}

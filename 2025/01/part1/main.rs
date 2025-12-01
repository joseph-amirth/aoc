use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let input: Vec<(char, usize)> = reader
        .lines()
        .map(Result::unwrap)
        .map(|line| {
            (
                line.chars().nth(0).unwrap(),
                (&line[1..]).parse::<usize>().unwrap(),
            )
        })
        .collect();

    let mut answer = 0;

    let mut cur: i32 = 50;
    for (dir, times) in input {
        if dir == 'L' {
            cur -= times as i32;
        } else if dir == 'R' {
            cur += times as i32;
        } else {
            unreachable!("dir should only be 'L' or 'R'");
        }
        if cur % 100 == 0 {
            answer += 1;
        }
    }

    println!("{}", answer);
    Ok(())
}

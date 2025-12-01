use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let input: Vec<(char, i32)> = reader
        .lines()
        .map(Result::unwrap)
        .map(|line| {
            (
                line.chars().nth(0).unwrap(),
                (&line[1..]).parse::<i32>().unwrap(),
            )
        })
        .collect();

    let mut answer: i32 = 0;

    let mut cur: i32 = 50;
    for (dir, times) in input {
        let after = if dir == 'L' {
            cur - times
        } else if dir == 'R' {
            cur + times
        } else {
            unreachable!("dir should only be 'L' or 'R'")
        };

        if after < cur {
            answer += ((cur as f32 / 100.).ceil()) as i32 - (after as f32 / 100.0).ceil() as i32;
        }
        if cur < after {
            answer += (after as f32 / 100.).floor() as i32 - (cur as f32 / 100.0).floor() as i32;
        }

        cur = after;
    }

    println!("{}", answer);
    Ok(())
}

use std::{
    cmp::Reverse,
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let mut answer = 0;

    for line in reader.lines().map(Result::unwrap) {
        let len: usize = line.len();

        let (first_pos, first): (usize, usize) = line
            .chars()
            .map(|c| c.to_digit(10).unwrap() as usize)
            .enumerate()
            .take(len - 1)
            .max_by_key(|(pos, digit)| (*digit, Reverse(*pos)))
            .unwrap();

        let second: usize = line
            .chars()
            .skip(first_pos + 1)
            .map(|c| c.to_digit(10).unwrap() as usize)
            .max()
            .unwrap();

        answer += first * 10 + second;
    }

    println!("{}", answer);

    Ok(())
}

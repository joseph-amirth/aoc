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

        let mut number: i64 = 0;
        let mut skip = 0;

        for i in 0..12 {
            let (pos, digit): (usize, usize) = line
                .chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .enumerate()
                .take(len - (12 - i - 1))
                .skip(skip)
                .max_by_key(|(pos, digit)| (*digit, Reverse(*pos)))
                .unwrap();
            number = 10 * number + (digit as i64);
            skip = pos + 1;
        }

        answer += number;
    }

    println!("{}", answer);

    Ok(())
}

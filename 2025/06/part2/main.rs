use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let lines = reader
        .lines()
        .map(Result::unwrap)
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut answer: u64 = 0;

    let num_lines = lines.len();

    let last_line = lines.last().unwrap();
    for i in 0..last_line.len() {
        if last_line[i] == ' ' {
            continue;
        }

        let mut pos = i;
        for j in i + 1..last_line.len() {
            if last_line[j] != ' ' {
                pos = j;
                break;
            }
        }

        let j = if pos == i {
            last_line.len() - 1
        } else {
            pos - 2
        };

        let mut result: u64 = if last_line[i] == '+' { 0 } else { 1 };

        for k in i..=j {
            let mut num: u64 = 0;
            for line in lines.iter().take(num_lines - 1) {
                if line[k] != ' ' {
                    num = 10 * num + (line[k].to_digit(10).unwrap() as u64);
                }
            }
            result = if last_line[i] == '+' {
                result + num
            } else {
                result * num
            };
        }

        answer += result;
    }

    println!("{}", answer);

    Ok(())
}

use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let lines = reader.lines().map(Result::unwrap).collect::<Vec<_>>();

    let num_lines = lines.len();

    let lists = lines
        .iter()
        .take(num_lines - 1)
        .map(|line| {
            line.split(' ')
                .filter(|item| !item.is_empty())
                .map(|item| item.parse::<usize>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let operators = lines
        .iter()
        .skip(num_lines - 1)
        .next()
        .unwrap()
        .split(' ')
        .filter(|item| !item.is_empty())
        .collect::<Vec<_>>();

    let mut answer = 0;

    let n = operators.len();

    for i in 0..n {
        match operators[i] {
            "+" => {
                let mut result = 0;
                for list in &lists {
                    result += list[i];
                }
                answer += result;
            }
            "*" => {
                let mut result = 1;
                for list in &lists {
                    result *= list[i];
                }
                answer += result;
            }
            _ => panic!("Unexpected operator"),
        }
    }

    println!("{}", answer);

    Ok(())
}

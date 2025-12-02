use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let line = reader.lines().next().unwrap()?;

    let ranges: Vec<(usize, usize)> = line
        .split(',')
        .map(|range| {
            let mut it = range.split('-');
            let l = it.next().unwrap().parse::<usize>().unwrap();
            let r = it.next().unwrap().parse::<usize>().unwrap();
            (l, r)
        })
        .collect();

    let mut answer = 0;

    for num in 1..100_000 {
        let num_str = num.to_string();
        let id_str = num_str.clone() + num_str.as_str();
        let id = id_str.parse::<usize>().unwrap();
        for (l, r) in &ranges {
            if *l <= id && id <= *r {
                answer += id;
            }
        }
    }

    println!("{}", answer);

    Ok(())
}

use std::{
    collections::VecDeque,
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let grid = reader
        .lines()
        .map(Result::unwrap)
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let m = grid.len();
    let n = grid.first().unwrap().len();

    let mut visited = vec![vec![false; n]; m];
    let mut queue = VecDeque::new();

    let start_pos = grid[0].iter().position(|ch| *ch == 'S').unwrap();
    visited[0][start_pos] = true;
    queue.push_back((0, start_pos));

    while !queue.is_empty() {
        let (i, j) = queue.pop_front().unwrap();

        if i + 1 == m {
            continue;
        }

        if grid[i + 1][j] == '.' && !visited[i + 1][j] {
            visited[i + 1][j] = true;
            queue.push_back((i + 1, j));
        }

        if grid[i + 1][j] == '^' {
            visited[i + 1][j] = true;
            if j > 0 && grid[i + 1][j - 1] == '.' && !visited[i + 1][j - 1] {
                visited[i + 1][j - 1] = true;
                queue.push_back((i + 1, j - 1));
            }
            if j + 1 < n && grid[i + 1][j + 1] == '.' && !visited[i + 1][j + 1] {
                visited[i + 1][j + 1] = true;
                queue.push_back((i + 1, j + 1));
            }
        }
    }

    let mut answer = 0;

    for i in 0..m {
        for j in 0..n {
            if grid[i][j] == '^' && visited[i][j] {
                answer += 1;
            }
        }
    }

    println!("{}", answer);

    Ok(())
}

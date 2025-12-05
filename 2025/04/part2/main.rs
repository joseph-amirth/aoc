use std::{
    collections::VecDeque,
    fs::File,
    io::{self, BufRead, BufReader},
};

type Grid = Vec<Vec<char>>;

fn neighbors(grid: &Grid, i: usize, j: usize) -> Vec<(usize, usize)> {
    const DIRS: [i32; 3] = [-1, 0, 1];

    let m = grid.len();
    let n = grid.first().unwrap().len();

    let mut neighbors = Vec::new();

    for di in DIRS {
        for dj in DIRS {
            if di == 0 && dj == 0 {
                continue;
            }

            let ni = (i as i32) + di;
            let nj = (j as i32) + dj;

            if ni < 0 || nj < 0 {
                continue;
            }

            let ni = ni as usize;
            let nj = nj as usize;
            if ni < m && nj < n && grid[ni][nj] == '@' {
                neighbors.push((ni, nj));
            }
        }
    }

    neighbors
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let grid: Grid = reader
        .lines()
        .map(Result::unwrap)
        .map(|line| line.chars().collect())
        .collect();

    let m = grid.len();
    let n = grid.first().unwrap().len();

    let mut grid = grid;
    let mut queue = VecDeque::new();

    for i in 0..m {
        for j in 0..n {
            if grid[i][j] == '@' && neighbors(&grid, i, j).len() < 4 {
                grid[i][j] = '.';
                queue.push_back((i, j));
            }
        }
    }

    let mut answer = 0;

    while !queue.is_empty() {
        let (i, j) = queue.pop_front().unwrap();

        answer += 1;

        for (ni, nj) in neighbors(&grid, i, j) {
            if neighbors(&grid, ni, nj).len() < 4 {
                grid[ni][nj] = '.';
                queue.push_back((ni, nj));
            }
        }
    }

    println!("{}", answer);

    Ok(())
}

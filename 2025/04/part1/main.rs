use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let grid: Vec<Vec<char>> = reader
        .lines()
        .map(Result::unwrap)
        .map(|line| line.chars().collect())
        .collect();

    let m = grid.len();
    let n = grid.first().unwrap().len();

    let mut answer = 0;

    for i in 0..m {
        for j in 0..n {
            if grid[i][j] == '.' {
                continue;
            }

            let mut cnt_neighbors = 0;

            const DIRS: [i32; 3] = [-1, 0, 1];
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
                        cnt_neighbors += 1;
                    }
                }
            }

            if cnt_neighbors < 4 {
                answer += 1;
            }
        }
    }

    println!("{}", answer);

    Ok(())
}

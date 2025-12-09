use std::{
    collections::VecDeque,
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let num_edges = 1000;

    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let points = reader
        .lines()
        .map(Result::unwrap)
        .map(|line| {
            line.split(',')
                .map(|token| token.parse::<usize>().unwrap())
                .collect::<Vec<_>>()
        })
        .map(|tokens| {
            assert_eq!(tokens.len(), 3);
            (tokens[0], tokens[1], tokens[2])
        })
        .collect::<Vec<_>>();

    let n = points.len();

    let adj = {
        let mut edges = Vec::new();
        for i in 0..n {
            let p1 = &points[i];
            for j in 0..i {
                let p2 = &points[j];

                let dx = p1.0 as i64 - p2.0 as i64;
                let dy = p1.1 as i64 - p2.1 as i64;
                let dz = p1.2 as i64 - p2.2 as i64;
                let distance = (dx * dx) as usize + (dy * dy) as usize + (dz * dz) as usize;
                edges.push((i, j, distance));
            }
        }
        edges.sort_by_key(|(_, _, distance)| *distance);
        edges.truncate(num_edges);

        let mut adj = vec![vec![]; n];

        for (u, v, _) in edges {
            adj[u].push(v);
            adj[v].push(u);
        }

        adj
    };

    let mut sizes = Vec::new();

    let mut visited = vec![false; n];

    for s in 0..n {
        if !visited[s] {
            let mut size = 0;

            let mut queue = VecDeque::new();

            visited[s] = true;
            queue.push_back(s);

            while !queue.is_empty() {
                let u = queue.pop_front().unwrap();

                size += 1;

                for v in &adj[u] {
                    if !visited[*v] {
                        visited[*v] = true;
                        queue.push_back(*v);
                    }
                }
            }

            sizes.push(size);
        }
    }

    sizes.sort_by(|a, b| b.cmp(a));

    let answer = sizes[0] * sizes[1] * sizes[2];
    println!("{}", answer);

    Ok(())
}

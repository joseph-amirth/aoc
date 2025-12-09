use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    mem::swap,
};

struct UnionFind {
    parent: Vec<usize>,
    rank: Vec<usize>,
}

impl UnionFind {
    pub fn new(n: usize) -> Self {
        Self {
            parent: (0..n).collect(),
            rank: vec![1; n],
        }
    }

    pub fn find(&mut self, u: usize) -> usize {
        if self.parent[u] == u {
            u
        } else {
            self.parent[u] = self.find(self.parent[u]);
            self.parent[u]
        }
    }

    pub fn merge(&mut self, u: usize, v: usize) -> bool {
        let mut u = self.find(u);
        let mut v = self.find(v);
        if u == v {
            return false;
        }
        if self.rank[u] < self.rank[v] {
            swap(&mut u, &mut v);
        }
        self.rank[u] += self.rank[v];
        self.parent[v] = u;
        return true;
    }
}

fn main() -> io::Result<()> {
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

    let edges = {
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
        edges
    };

    let mut union_find = UnionFind::new(n);
    let mut comps = n;

    for (u, v, _) in edges {
        if union_find.merge(u, v) {
            comps -= 1;
            if comps == 1 {
                let answer = points[u].0 * points[v].0;
                println!("{}", answer);
                return Ok(());
            }
        }
    }

    unreachable!()
}

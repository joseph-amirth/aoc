use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let lines = reader.lines().map(Result::unwrap).collect::<Vec<_>>();

    let (id_ranges, _) = {
        let mut iter = lines.split(|item| item == "");
        let id_ranges = iter.next().unwrap();
        let ids = iter.next().unwrap();
        assert_eq!(iter.next(), None);
        (id_ranges, ids)
    };

    let mut id_ranges = id_ranges
        .into_iter()
        .map(|range_str| {
            let mut iter = range_str.split('-');
            let l = iter.next().unwrap().parse::<usize>().unwrap();
            let r = iter.next().unwrap().parse::<usize>().unwrap();
            assert_eq!(iter.next(), None);
            (l, r)
        })
        .collect::<Vec<_>>();

    let n = id_ranges.len();

    fn intersects((l1, r1): &(usize, usize), (l2, r2): &(usize, usize)) -> bool {
        !(r2 < l1 || r1 < l2)
    }

    for _ in 0..n {
        for i in 0..n {
            for j in 0..n {
                if intersects(&id_ranges[i], &id_ranges[j]) {
                    let (l1, r1) = id_ranges[i];
                    let (l2, r2) = &mut id_ranges[j];
                    *l2 = (*l2).min(l1);
                    *r2 = (*r2).max(r1);
                }
            }
        }
    }

    id_ranges.sort();
    id_ranges.dedup();

    let answer = id_ranges.into_iter().map(|(l, r)| r - l + 1).sum::<usize>();
    println!("{}", answer);

    Ok(())
}

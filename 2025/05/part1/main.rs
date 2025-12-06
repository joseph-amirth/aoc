use std::{
    fs::File,
    io::{self, BufRead, BufReader},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let lines = reader.lines().map(Result::unwrap).collect::<Vec<_>>();

    let (id_ranges, ids) = {
        let mut iter = lines.split(|item| item == "");
        let id_ranges = iter.next().unwrap();
        let ids = iter.next().unwrap();
        assert_eq!(iter.next(), None);
        (id_ranges, ids)
    };

    let id_ranges = id_ranges
        .into_iter()
        .map(|range_str| {
            let mut iter = range_str.split('-');
            let l = iter.next().unwrap().parse::<usize>().unwrap();
            let r = iter.next().unwrap().parse::<usize>().unwrap();
            assert_eq!(iter.next(), None);
            (l, r)
        })
        .collect::<Vec<_>>();

    let ids = ids
        .into_iter()
        .map(|id_str| id_str.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    let mut answer = 0;

    for id in ids {
        let mut fresh = false;
        for (l, r) in &id_ranges {
            fresh |= *l <= id && id <= *r;
        }
        answer += fresh as usize;
    }

    println!("{}", answer);

    Ok(())
}

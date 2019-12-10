use std::collections::HashSet;
use std::fs;
use std::iter::FromIterator;
type Point = (isize, isize);
type PointLengths = HashSet<(isize, isize, usize)>;
type Points = Vec<(isize, isize)>;
pub fn main() {
    let contents =
        fs::read_to_string("./src/day3/input").expect("Something went wrong reading the file");
    let wires: Vec<Vec<&str>> = contents
        .lines()
        .map(|line| line.split(",").collect())
        .collect();
    let wire1 = &wires[0];
    let wire2 = &wires[1];
    let points1 = get_points(wire1);
    let points2 = get_points(wire2);
    let dist = find_closest_intersection_dist(points1, points2);
    println!("dist: {}", dist);
}

fn get_points(wire: &Vec<&str>) -> PointLengths {
    let mut curr_x = 0;
    let mut curr_y = 0;
    let mut curr_len = 0;
    let mut res = vec![];
    for path in wire {
        let (dir, amount) = path.split_at(1);
        let amount: usize = amount.parse().unwrap();
        match dir {
            "R" => {
                for _ in 0..amount {
                    curr_x += 1;
                    curr_len += 1;
                    res.push((curr_x, curr_y, curr_len));
                }
            }
            "L" => {
                for _ in 0..amount {
                    curr_x -= 1;
                    curr_len += 1;
                    res.push((curr_x, curr_y, curr_len));
                }
            }
            "U" => {
                for _ in 0..amount {
                    curr_y += 1;
                    curr_len += 1;
                    res.push((curr_x, curr_y, curr_len));
                }
            }
            "D" => {
                for _ in 0..amount {
                    curr_y -= 1;
                    curr_len += 1;
                    res.push((curr_x, curr_y, curr_len));
                }
            }
            _ => panic!("Bad dir"),
        }
    }
    HashSet::from_iter(res)
}

fn get_intersections(p1: PointLengths, p2: PointLengths) -> Points {
    let p1: HashSet<Point> = HashSet::from_iter(p1.iter().map(|(x, y, _)| (*x, *y)));
    let p2: HashSet<Point> = HashSet::from_iter(p2.iter().map(|(x, y, _)| (*x, *y)));
    p1.intersection(&p2).map(|x| *x).collect()
}

fn find_length_to_point(point_list: PointLengths, p: Point) -> usize {
    let (_, _, len) = point_list.iter().find(|(x, y, _)| (*x, *y) == p).unwrap();
    *len
}

fn find_closest_intersection_dist(p1: PointLengths, p2: PointLengths) -> usize {
    // p1.intersection(&p2)
    //     .map(|(x, y)| x.abs() + y.abs())
    //     .min()
    //     .unwrap()
    let a = get_intersections(p1.clone(), p2.clone());
    println!("a: {:?}", a);
    a.iter()
        .map(|p| find_length_to_point(p1.clone(), *p) + find_length_to_point(p2.clone(), *p))
        .min()
        .unwrap()
}

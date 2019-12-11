use fraction::GenericFraction;
// use num::abs;
use std::collections::HashSet;
type MapList = Vec<Vec<char>>;
type Points = HashSet<Point>;
type SlopeFrac = GenericFraction<usize>;
type Slope = (bool, bool, SlopeFrac);
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: isize,
    y: isize,
}
impl Point {
    pub fn new(x: usize, y: usize) -> Self {
        Point {
            x: x as isize,
            y: y as isize,
        }
    }
    pub fn get_slope(&self, other: &Point) -> Slope {
        let x_is_pos = other.x >= self.x;
        let y_is_pos = other.y >= self.y;
        let (x, y) = (
            other.x as isize - self.x as isize,
            other.y as isize - self.y as isize,
        );
        let (x, y) = (x.abs() as usize, y.abs() as usize);
        (x_is_pos, y_is_pos, SlopeFrac::new(x, y))
    }

    pub fn add_slope(&self, slope: &Slope, width: usize, height: usize) -> Option<Point> {
        use fraction::GenericFraction::*;
        let (x_slope, y_slope) = match slope {
            (x_is_pos, y_is_pos, Rational(_, ratio)) => {
                let (mut x, mut y) = (*ratio.numer() as isize, *ratio.denom() as isize);
                if !x_is_pos {
                    x *= -1;
                }
                if !y_is_pos {
                    y *= -1;
                }
                (x, y)
            }
            (true, _, Infinity(_)) => (1, 0),
            (false, _, Infinity(_)) => (-1, 0),
            (_, _, NaN) => panic!("ass"),
        };
        let (x, y) = (self.x + x_slope, self.y + y_slope);
        if x as usize > width || y as usize > height || x < 0 || y < 0 {
            return None;
        }
        Some(Point { x, y })
    }
}

#[derive(Debug)]
struct Map {
    // map: Vec<Vec<char>>,
    height: usize,
    width: usize,
    points: Points,
}
pub fn main(contents: &str) {
    let map = Map::new(contents);
    let best = map.get_best_point();
    println!("best point {:?}", best);
    println!("count: {}", map.get_count_for_point(&best.unwrap()));
    // println!("map: {:?}", map);
}

impl Map {
    pub fn new(map_str: &str) -> Self {
        let map: MapList = map_str.lines().map(|line| line.chars().collect()).collect();
        let mut points = HashSet::new();
        for (y, line) in map.iter().enumerate() {
            for (x, p) in line.iter().enumerate() {
                if *p == '#' {
                    points.insert(Point::new(x, y));
                }
            }
        }
        Map {
            width: map[0].len(),
            height: map.len(),
            points,
        }
    }

    pub fn get_best_point(&self) -> Option<&Point> {
        let mut max = 0;
        let mut max_point = None;
        for p in self.points.iter() {
            let val = self.get_count_for_point(p);
            if val > max {
                max = val;
                max_point = Some(p);
            }
        }
        max_point
    }

    pub fn get_count_for_point(&self, point: &Point) -> usize {
        let mut can_see: Points = HashSet::new();
        let mut not_see: Points = HashSet::new();
        not_see.insert(*point);
        for p in self.points.iter() {
            if not_see.contains(p) {
                continue;
            }
            let slope = point.get_slope(p);
            let mut new_point_opt = point.add_slope(&slope, self.width, self.height);
            let mut blocked = false;
            while new_point_opt.is_some() {
                let new_point = new_point_opt.unwrap();
                if self.points.contains(&new_point) && !not_see.contains(&new_point) && !blocked {
                    blocked = true;
                    can_see.insert(new_point);
                } else if self.points.contains(&new_point) && blocked {
                    not_see.insert(new_point);
                }
                new_point_opt = new_point.add_slope(&slope, self.width, self.height);
            }
        }
        can_see.len()
    }
}

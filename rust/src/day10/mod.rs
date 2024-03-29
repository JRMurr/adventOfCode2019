use fraction::GenericFraction;
// use num::abs;
use std::collections::HashMap;
use std::collections::HashSet;
// use std::iter::FromIterator;
type MapList = Vec<Vec<char>>;
type Points = HashSet<Point>;
type Angle = f64;

// NOTE: slope is x/y aka run over rise, but it still works lol
type SlopeFrac = GenericFraction<usize>;
type Slope = (bool, bool, SlopeFrac);
// impl FromIterator<Slope> for Vec<Slope> {
//     fn from_iter<I: IntoIterator<Item = Slope>>(iter: I) -> Self {
//         let mut c = Vec::new();

//         for i in iter {
//             c.add(i);
//         }

//         c
//     }
// }

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

    pub fn get_dist(&self, other: &Point) -> usize {
        let (x, y) = (other.x - self.x, other.y - self.y);
        // (x.powi(2) + y.powi(2)).sqrt()
        x.abs() as usize + y.abs() as usize
    }

    // pub fn add_slope(&self, slope: &Slope, width: usize, height: usize) -> Option<Point> {
    //     use fraction::GenericFraction::*;
    //     let (x_slope, y_slope) = match slope {
    //         (x_is_pos, y_is_pos, Rational(_, ratio)) => {
    //             let (mut x, mut y) = (*ratio.numer() as isize, *ratio.denom() as isize);
    //             if !x_is_pos {
    //                 x *= -1;
    //             }
    //             if !y_is_pos {
    //                 y *= -1;
    //             }
    //             (x, y)
    //         }
    //         (true, _, Infinity(_)) => (1, 0),
    //         (false, _, Infinity(_)) => (-1, 0),
    //         (_, _, NaN) => panic!("ass"),
    //     };
    //     let (x, y) = (self.x + x_slope, self.y + y_slope);
    //     if x as usize > width || y as usize > height || x < 0 || y < 0 {
    //         return None;
    //     }
    //     Some(Point { x, y })
    // }
}

fn angle_of_slope(slope: &Slope) -> Angle {
    use fraction::GenericFraction::*;
    let (x_is_pos, y_is_pos, slope) = slope;
    match slope {
        Rational(_, ratio) => {
            let (x, y) = (*ratio.numer() as f64, *ratio.denom() as f64);
            // if x == 0f64 {
            //     if !*y_is_pos {
            //         // its above
            //         0f64
            //     } else {
            //         180f64
            //     }
            // } else {
            //     let angle = (x / y).atan().to_degrees();
            //     let angle = match (x_is_pos, y_is_pos) {
            //         (true, false) => angle,
            //         (true, true) => angle + 90f64,
            //         (false, true) => angle + 180f64,
            //         (false, false) => angle + 270f64,
            //     };
            //     angle % 360f64
            // }
            let angle = (x / y).atan().to_degrees();
            let angle = match (x_is_pos, y_is_pos) {
                (true, false) => angle,
                (true, true) => angle + 90f64,
                (false, true) => angle + 180f64,
                (false, false) => angle + 270f64,
            };
            angle % 360f64
        }
        Infinity(_) => {
            if *x_is_pos {
                90f64
            } else {
                270f64
            }
        }
        _ => panic!("ass in convert angle"),
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
    // println!("count: {}", map.get_count_for_point(&best.unwrap()));
    let destroyed = map.destroy_2(best.unwrap());
    // let destroyed = map.destroy_roids(&Point { x: 11, y: 13 });

    println!("destroyed: {:?}", destroyed[199]);
    // println!("best point {:?}", best);
    // println!("map: {:?}", map);
    // for (x, y) in vec![
    //     // (0, 2),
    //     // (1, 3),
    //     // (1, 2),
    //     // (2, 3),
    //     // (1, 1),
    //     // (3, 2),
    //     // (4, 2),
    //     // (3, 1),
    //     (1, 1),
    //     (1, -1),
    //     (-1, -1),
    //     (-1, 1),
    //     (0, 1),
    //     (1, 0),
    //     (0, -1),
    //     (-1, 0),
    // ] {
    //     let angle = angle_of_slope(&(
    //         x > 0,
    //         y > 0,
    //         SlopeFrac::new((x as isize).abs() as usize, (y as isize).abs() as usize),
    //     ));
    //     println!("angle: {}, from point: {:?}", angle, (x, y));
    // }
}

impl Map {
    pub fn new(map_str: &str) -> Self {
        let map: MapList = map_str.lines().map(|line| line.chars().collect()).collect();
        let mut points = HashSet::new();
        for (y, line) in map.iter().enumerate() {
            for (x, p) in line.iter().enumerate() {
                if *p == '#' || *p == 'X' {
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
        let mut slopes = HashSet::new();
        for p in self.points.iter() {
            if p == point {
                continue;
            }
            let slope = point.get_slope(p);
            slopes.insert(slope);
        }
        slopes.len()
    }

    // fn get_closest(
    //     &self,
    //     point: &Point,
    //     min_angle: f64,
    //     destroyed: &Vec<Point>,
    // ) -> Option<(&Point, f64)> {
    //     let mut min_slope = 400f64; //above 360 so its cool
    //     let mut res = None;
    //     for p in self.points.iter() {
    //         if destroyed.contains(p) || p == point {
    //             continue;
    //         }
    //         let slope = point.get_slope(p);
    //         let angle = angle_of_slope(&slope);
    //         if angle > min_angle {
    //             if angle < min_slope {
    //                 min_slope = angle;
    //                 res = Some((p, angle));
    //             } else if angle == min_slope {
    //                 let (other, _) = res.unwrap();
    //                 let other_dist = point.get_dist(other);
    //                 let this_dist = point.get_dist(p);
    //                 if this_dist < other_dist {
    //                     res = Some((p, angle));
    //                 }
    //             }
    //         }
    //     }
    //     res
    // }

    // fn destroy_roids(&self, start: &Point) -> Vec<Point> {
    //     let mut destroyed: Vec<Point> = vec![];
    //     // angle will be >=0 so smallest to start
    //     let mut min_angle = -1f64;
    //     // sub 1 since the start will not be destroyed
    //     while destroyed.len() < self.points.len() - 1 {
    //         let new_point = self.get_closest(start, min_angle, &destroyed);
    //         match new_point {
    //             Some((p, angle)) => {
    //                 destroyed.push(*p);
    //                 min_angle = angle;
    //             }
    //             None => {
    //                 // do a rotation
    //                 min_angle = -1f64;
    //             }
    //         }
    //     }
    //     destroyed
    // }

    fn destroy_2(&self, start: &Point) -> Vec<&Point> {
        let mut destroyed: Vec<&Point> = vec![];
        let mut slope_map = self.get_slopes_rel_to_point(start);
        let tmp = slope_map.clone();
        let mut angles: Vec<&Slope> = tmp.keys().collect();
        angles.sort_by(|a, b| angle_of_slope(a).partial_cmp(&angle_of_slope(b)).unwrap());
        while destroyed.len() < self.points.len() - 1 {
            for angle in &angles {
                println!("angle: {:.10}\tslope:{:?}", angle_of_slope(angle), angle);
                slope_map.entry(**angle).and_modify(|points| {
                    if !points.is_empty() {
                        let roid = points.remove(0);
                        destroyed.push(roid);
                    }
                });
            }
            println!("------------")
        }
        destroyed
    }

    fn get_slopes_rel_to_point(&self, point: &Point) -> HashMap<Slope, Vec<&Point>> {
        let mut ret = HashMap::new();
        for p in self.points.iter() {
            if p == point {
                continue;
            }
            let slope = match point.get_slope(p) {
                (true, true, (fraction::GenericFraction::Infinity(_))) => {
                    (true, true, SlopeFrac::new(0 as usize, 1 as usize))
                }
                other => other,
            };
            let entry = ret.entry(slope).or_insert_with(|| vec![]);
            entry.push(p);
        }
        for points in ret.values_mut() {
            points.sort_by_cached_key(|p| point.get_dist(p));
        }
        ret
    }

    // fn destroy_roids2(&self, start: &Point) {

    // }
}

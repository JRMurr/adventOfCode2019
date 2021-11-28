mod intcode;
use intcode::{IntCode, LangVal};
use std::collections::HashMap;
pub fn main(contents: &str) {
    let prog: Vec<LangVal> = contents
        .split(",")
        .map(|x| x.trim().parse().unwrap())
        .collect();
    // println!("out: {:?}", IntCode::run_with_input(&prog, &mut vec![2]));
    let brain = IntCode::new(prog);
    let mut robot = Robot::new(brain);
    robot.run();
    // println!("size: {}", robot.point_colors.len());
    robot.get_output();
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    pub fn new(x: isize, y: isize) -> Self {
        Point { x, y }
    }

    pub fn move_dir(self, dir: &Direction) -> Point {
        use Direction::*;
        let (x, y) = match dir {
            Up => (self.x, self.y + 1),
            Down => (self.x, self.y - 1),
            Right => (self.x + 1, self.y),
            Left => (self.x - 1, self.y),
        };
        Point { x, y }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Color {
    Black,
    White,
}
impl Color {
    pub fn to_int(self) -> LangVal {
        use Color::*;
        match self {
            Black => 0,
            White => 1,
        }
    }
    pub fn from_int(val: LangVal) -> Color {
        use Color::*;
        match val {
            0 => Black,
            1 => White,
            _ => panic!("bad color"),
        }
    }
}
enum Direction {
    Up,
    Down,
    Left,
    Right,
}
impl Direction {
    pub fn rotate_left(&self) -> Direction {
        use Direction::*;
        match self {
            Up => Left,
            Down => Right,
            Left => Down,
            Right => Up,
        }
    }

    pub fn rotate_right(&self) -> Direction {
        use Direction::*;
        match self {
            Up => Right,
            Down => Left,
            Left => Up,
            Right => Down,
        }
    }
}
type PointColors = HashMap<Point, Color>;
struct Robot {
    brain: IntCode,
    curr_point: Point,
    curr_direction: Direction,
    point_colors: PointColors,
}

impl Robot {
    pub fn new(brain: IntCode) -> Robot {
        let curr_point: Point = Point::new(0, 0);
        let curr_direction = Direction::Up;
        let point_colors: PointColors = HashMap::new();
        Robot {
            brain,
            curr_point,
            curr_direction,
            point_colors,
        }
    }

    pub fn run(&mut self) {
        self.brain.add_input(Color::White.to_int());
        let mut pc = Some(0);
        loop {
            match pc {
                Some(new_pc) => {
                    pc = self.run_brain_for_two_outputs(new_pc);
                    if pc.is_some() {
                        self.process_output();
                    }
                }
                None => return,
            }
        }
    }

    fn run_brain_for_two_outputs(&mut self, pc: usize) -> Option<usize> {
        let pc = self.brain.run_prog(pc);
        match pc {
            Some(new_pc) => self.brain.run_prog(new_pc),
            None => None,
        }
    }

    fn process_output(&mut self) {
        let to_paint = Color::from_int(self.brain.out_buf.remove(0));
        let rotate_dir = self.brain.out_buf.remove(0);
        self.set_color(self.curr_point, to_paint);
        self.curr_direction = match rotate_dir {
            0 => self.curr_direction.rotate_left(),
            1 => self.curr_direction.rotate_right(),
            _ => panic!("bad rotate"),
        };
        self.curr_point = self.curr_point.move_dir(&self.curr_direction);
        self.brain
            .add_input(self.get_color(self.curr_point).to_int());
    }

    fn set_color(&mut self, p: Point, c: Color) {
        self.point_colors.insert(p, c);
    }

    fn get_color(&self, p: Point) -> Color {
        match self.point_colors.get(&p) {
            Some(v) => *v,
            None => Color::Black, //default to black
        }
    }

    fn get_output(&self) {
        let keys: Vec<(isize, isize)> = self.point_colors.keys().map(|p| (p.x, p.y)).collect();
        let (min_x, _) = keys.iter().min_by_key(|(x, _)| x).unwrap();
        let (_, min_y) = keys.iter().min_by_key(|(_, y)| y).unwrap();
        let (max_x, _) = keys.iter().max_by_key(|(x, _)| x).unwrap();
        let (_, max_y) = keys.iter().max_by_key(|(_, y)| y).unwrap();

        // println!(
        //     "max_y: {}, min_y {}, add: {}",
        //     max_y.abs(),
        //     min_y.abs(),
        //     max_y.abs() + min_y.abs()
        // );
        let height = (max_y.abs() + min_y.abs()) as usize;
        let width = (max_x.abs() + min_x.abs()) as usize;
        let mut picture: Vec<Vec<char>> = vec![];
        for _ in 0..height + 1 {
            // make row all black
            picture.push(vec!['.'; width + 1]);
        }
        for (point, color) in self.point_colors.iter() {
            // make it positive
            if *color == Color::White {
                let (x, y) = (
                    (point.x + min_x.abs()) as usize,
                    (point.y + min_y.abs()) as usize,
                );
                // println!("was: ({}, {})\tnow: ({}, {})", point.x, point.y, x, y);
                picture[y][x] = '#';
            }
        }
        println!("-------");
        for line in picture.iter().rev() {
            for c in line {
                print!("{}", c);
            }
            println!("");
        }
        println!("-------");
    }
}

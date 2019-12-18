mod intcode;
use intcode::{IntCode, LangVal};
use std::collections::HashMap;

pub fn main(contents: &str) {
    let prog: Vec<LangVal> = contents
        .split(",")
        .map(|x| x.trim().parse().unwrap())
        .collect();
    let int_code = IntCode::new(prog);
    let mut arcade = Arcade::new(int_code);
    arcade.run();
    println!("blocks: {}", arcade.get_num_blocks());
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Block,
    HPaddle,
    Ball,
}
impl Tile {
    pub fn from_int(val: LangVal) -> Tile {
        use Tile::*;
        match val {
            0 => Empty,
            1 => Wall,
            2 => Block,
            3 => HPaddle,
            4 => Ball,
            _ => panic!("bad tile"),
        }
    }

    pub fn to_char(self) -> char {
        use Tile::*;
        match self {
            Empty => ' ',
            Wall => '|',
            Block => '▓',
            HPaddle => '-',
            Ball => 'O',
        }
    }
}

enum JoyStick {
    Neutral = 0,
    Left = -1,
    Right = 1,
}

struct Arcade {
    int_code: IntCode,
    tiles: HashMap<(LangVal, LangVal), Tile>,
    score: LangVal,
}
impl Arcade {
    pub fn new(int_code: IntCode) -> Arcade {
        Arcade {
            int_code,
            tiles: HashMap::new(),
            score: 0,
        }
    }

    pub fn run(&mut self) {
        // set to free play
        self.int_code.set_loc(0, 2);
        let mut pc = Some(0);
        loop {
            match pc {
                Some(new_pc) => {
                    pc = self.int_code.run_till_n_outputs(new_pc, 3);
                    if pc.is_some() {
                        self.process_output();
                    }
                }
                None => return,
            }
        }
    }

    fn process_output(&mut self) {
        let x = self.int_code.out_buf.remove(0);
        let y = self.int_code.out_buf.remove(0);
        let val = self.int_code.out_buf.remove(0);
        match (x, y) {
            (-1, 0) => self.score = val,
            _ => self
                .tiles
                .insert((x, y), Tile::from_int(val))
                .map_or((), |_| ()),
        }
        self.display();
    }

    fn display(&self) {
        let keys: Vec<(LangVal, LangVal)> = self.tiles.keys().map(|(x, y)| (*x, *y)).collect();
        let (min_x, _) = keys.iter().min_by_key(|(x, _)| x).unwrap();
        let (_, min_y) = keys.iter().min_by_key(|(_, y)| y).unwrap();
        let (max_x, _) = keys.iter().max_by_key(|(x, _)| x).unwrap();
        let (_, max_y) = keys.iter().max_by_key(|(_, y)| y).unwrap();
        let height = (max_y.abs() + min_y.abs()) as usize;
        let width = (max_x.abs() + min_x.abs()) as usize;
        let mut picture: Vec<Vec<char>> = vec![];
        for _ in 0..height + 1 {
            // make rows all empty
            picture.push(vec![Tile::Empty.to_char(); width + 1]);
        }
        for (point, tile) in self.tiles.iter() {
            let (x, y) = point;
            picture[*y as usize][*x as usize] = tile.to_char();
        }
        for line in picture.iter() {
            for c in line {
                print!("{}", c);
            }
            println!("");
        }
    }

    fn get_num_blocks(&self) -> usize {
        self.tiles.values().filter(|x| **x == Tile::Block).count()
    }
}

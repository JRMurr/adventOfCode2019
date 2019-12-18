mod intcode;
use intcode::{IntCode, LangVal};
use std::collections::HashMap;

pub fn main(contents: &str) {
    let prog: Vec<LangVal> = contents
        .split(",")
        .map(|x| x.trim().parse().unwrap())
        .collect();
    let int_code = IntCode::new(prog, true);
    let mut arcade = Arcade::new(int_code);
    arcade.run();
    // println!("blocks: {}", arcade.get_num_blocks());
    println!("\n\nfinal score: {}", arcade.score.unwrap())
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}
impl Tile {
    pub fn from_int(val: LangVal) -> Tile {
        use Tile::*;
        match val {
            0 => Empty,
            1 => Wall,
            2 => Block,
            3 => Paddle,
            4 => Ball,
            _ => panic!("bad tile"),
        }
    }

    pub fn to_char(self) -> char {
        use Tile::*;
        match self {
            Empty => ' ',
            Wall => '█',
            Block => '▓',
            Paddle => '─',
            Ball => 'O',
        }
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum JoyStick {
    Neutral,
    Left,
    Right,
}
impl JoyStick {
    pub fn to_lang_val(self) -> LangVal {
        use JoyStick::*;
        match self {
            Neutral => 0,
            Left => -1,
            Right => 1,
        }
    }
}

struct Arcade {
    int_code: IntCode,
    tiles: HashMap<(LangVal, LangVal), Tile>,
    score: Option<LangVal>,
    paddle_pos: (LangVal, LangVal),
}
impl Arcade {
    pub fn new(int_code: IntCode) -> Arcade {
        Arcade {
            int_code,
            tiles: HashMap::new(),
            score: None,
            paddle_pos: (0, 0),
        }
    }

    pub fn run(&mut self) {
        // set to free play
        self.int_code.set_loc(0, 2);
        let mut pc = Some((0, false, 0));
        loop {
            self.process_output();
            match pc {
                Some((index, false, remaining)) if remaining != 0 => {
                    pc = self.int_code.run_till_n_outputs(index, remaining);
                }
                Some((index, false, _)) => {
                    pc = self.int_code.run_till_n_outputs(index, 3);
                }
                Some((index, true, remaining)) => {
                    self.process_output();
                    match self.get_joystick_input_auto() {
                        Some(joy_input) => {
                            println!("dir: {:?}", joy_input);
                            self.int_code.add_input(joy_input.to_lang_val());
                        }
                        _ => (),
                    }
                    pc = self.int_code.run_till_n_outputs(index, remaining);
                }
                None => return,
            }
        }
    }

    fn process_output(&mut self) {
        while self.int_code.out_buf.len() >= 3 {
            let x = self.int_code.out_buf.remove(0);
            let y = self.int_code.out_buf.remove(0);
            let val = self.int_code.out_buf.remove(0);
            match (x, y) {
                (-1, 0) => {
                    self.score = Some(val);
                }
                _ => {
                    self.tiles.insert((x, y), Tile::from_int(val));
                }
            }
        }
    }

    fn get_joystick_input(&mut self) -> Option<JoyStick> {
        use std::io;
        if self.score.is_none() {
            return None;
        }
        self.display();
        let mut input = String::new();

        match io::stdin().read_line(&mut input) {
            Ok(_) => match input.as_str().trim() {
                "d" => Some(JoyStick::Right),
                "a" => Some(JoyStick::Left),
                _ => Some(JoyStick::Neutral),
            },
            _ => panic!("no input given"),
        }
    }

    fn get_joystick_input_auto(&mut self) -> Option<JoyStick> {
        if self.score.is_none() {
            return None;
        }
        self.display();
        let mut ball_and_paddle_iter = self.tiles.iter().filter_map(|(&pos, &tile)| match tile {
            Tile::Ball => Some((pos, tile)),
            Tile::Paddle => Some((pos, tile)),
            _ => None,
        });
        let ball_pos = ball_and_paddle_iter
            .find(|(_, tile)| *tile == Tile::Ball)
            .map(|(pos, _)| pos);
        let paddle_pos = ball_and_paddle_iter
            .find(|(_, tile)| *tile == Tile::Paddle)
            .map(|(pos, _)| pos);
        println!("ball: {:?}\tpaddle:{:?}", ball_pos, paddle_pos);
        Some(match (paddle_pos, ball_pos) {
            (Some(paddle_x), Some(ball_x)) if ball_x > paddle_x => JoyStick::Right,
            (Some(paddle_x), Some(ball_x)) if ball_x < paddle_x => JoyStick::Left,
            _ => JoyStick::Neutral,
        })
    }

    fn display(&mut self) {
        self.process_output();
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
        for (pos, tile) in self.tiles.iter() {
            let (x, y) = pos;
            picture[*y as usize][*x as usize] = tile.to_char();
        }
        if self.score.is_some() {
            println!("score: {}", self.score.unwrap())
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

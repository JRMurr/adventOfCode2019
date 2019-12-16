use itertools::Itertools;
use regex::Regex;
use std::fmt;
pub fn main(contents: &str) {
    let mut moon_sys = MoonSystem::new(contents);
    // moon_sys.run_steps(1000);
    // println!("moons: \n{}", moon_sys);
    // println!("energy: {}", moon_sys.get_energy());
    println!(
        "----------------------\n\n\nsteps: {}",
        moon_sys.run_till_equal()
    );
}
lazy_static! {
    static ref RE: Regex = Regex::new(r"<x=(.*),\s*y=(.*),\s*z=(.*)>").unwrap();
}
#[derive(Debug, Clone, Copy, PartialEq)]
struct Position {
    x: isize,
    y: isize,
    z: isize,
}

impl Position {
    pub fn get_energy(self) -> usize {
        (self.x.abs() + self.y.abs() + self.z.abs()) as usize
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vel=<x= {}, y= {}, z= {}>", self.x, self.y, self.z)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Velocity {
    x: isize,
    y: isize,
    z: isize,
}

impl Velocity {
    pub fn get_energy(self) -> usize {
        (self.x.abs() + self.y.abs() + self.z.abs()) as usize
    }
}

impl fmt::Display for Velocity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vel=<x= {}, y= {}, z= {}>", self.x, self.y, self.z)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Moon {
    pub pos: Position,
    pub vel: Velocity,
}

impl fmt::Display for Moon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {}", self.pos, self.vel)
    }
}

impl Moon {
    pub fn new(initial: &str) -> Moon {
        let caps = RE.captures(initial).unwrap();
        let positions: Vec<isize> = caps
            .iter()
            .skip(1) // first is the whole capture
            .map(|var| var.unwrap().as_str().parse::<isize>().unwrap())
            .collect();
        let pos = Position {
            x: positions[0],
            y: positions[1],
            z: positions[2],
        };
        let vel = Velocity { x: 0, y: 0, z: 0 };
        Moon { pos, vel }
    }

    pub fn apply_velocity(self) -> Moon {
        let mut new_moon = self.clone();
        new_moon.pos.x += self.vel.x;
        new_moon.pos.y += self.vel.y;
        new_moon.pos.z += self.vel.z;
        new_moon
    }

    pub fn get_kinetic_energy(self) -> usize {
        self.vel.get_energy()
    }

    pub fn get_potential_energy(self) -> usize {
        self.pos.get_energy()
    }

    pub fn get_energy(self) -> usize {
        self.get_kinetic_energy() * self.get_potential_energy()
    }
}

#[derive(Debug)]
struct MoonSystem {
    moons: Vec<Moon>,
}

impl fmt::Display for MoonSystem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let format_str = self.moons.iter().format("\n");
        write!(f, "{}", format_str)
    }
}

impl MoonSystem {
    pub fn new(contents: &str) -> MoonSystem {
        let moons: Vec<Moon> = contents.lines().map(|line| Moon::new(line)).collect();
        MoonSystem { moons }
    }

    pub fn run_steps(&mut self, n: usize) {
        for _ in 0..n {
            self.single_step();
        }
    }

    pub fn run_till_equal(&mut self) -> usize {
        let mut states = Vec::new();
        let mut count = 0;
        loop {
            let curr_state = self.moons.to_vec();
            states.push(curr_state);
            self.single_step();
            for old_state in states.clone() {
                if old_state.iter().eq(self.moons.iter()) {
                    return count;
                }
            }
            println!("count: {}", count);
            count += 1;
        }
    }

    fn get_gravity_change(field1: isize, field2: isize) -> (isize, isize) {
        if field1 > field2 {
            (-1, 1)
        } else if field1 < field2 {
            (1, -1)
        } else {
            (0, 0)
        }
    }

    fn single_step(&mut self) {
        // get indices for each pair of moons
        let pair_indices = (0..self.moons.len()).combinations(2);
        for pair in pair_indices {
            let moon1 = &self.moons[pair[0]];
            let moon2 = &self.moons[pair[1]];

            let (x1, x2) = MoonSystem::get_gravity_change(moon1.pos.x, moon2.pos.x);
            let (y1, y2) = MoonSystem::get_gravity_change(moon1.pos.y, moon2.pos.y);
            let (z1, z2) = MoonSystem::get_gravity_change(moon1.pos.z, moon2.pos.z);

            self.moons[pair[0]].vel.x += x1;
            self.moons[pair[0]].vel.y += y1;
            self.moons[pair[0]].vel.z += z1;

            self.moons[pair[1]].vel.x += x2;
            self.moons[pair[1]].vel.y += y2;
            self.moons[pair[1]].vel.z += z2;
        }
        self.moons = self.moons.iter().map(|x| x.apply_velocity()).collect();
    }

    pub fn get_energy(self) -> usize {
        self.moons
            .iter()
            .fold(0, |acc, moon| acc + moon.get_energy())
    }
}

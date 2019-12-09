type Layer = Vec<u32>;
type Layers = Vec<Layer>;
// type Image = Vec<Vec<u32>>;
const WIDTH: usize = 25;
const HEIGHT: usize = 6;
use std::str;
pub fn main(contents: &str) {
    let layers = get_layers(contents);
    // let target = find_layer(&layers);
    // println!("out: {}", parse_layer(target));
    get_image(&layers);
}

fn get_layers(contents: &str) -> Layers {
    contents
        .chars()
        .collect::<Vec<char>>()
        .chunks(WIDTH * HEIGHT)
        .map(|layer| {
            layer
                .iter()
                .map(|digit| {
                    digit
                        .to_digit(10)
                        .unwrap_or_else(|| panic!("died on ({})", digit))
                })
                .collect()
        })
        .collect()
}

fn get_pixel(layers: &Layers, index: usize) -> u32 {
    for layer in layers {
        let pixel = layer[index];
        if pixel == 0 || pixel == 1 {
            return pixel;
        }
    }
    2
}

fn get_image(layers: &Layers) {
    let mut image: Vec<u32> = vec![];
    for i in 0..(WIDTH * HEIGHT) {
        image.push(get_pixel(layers, i));
    }
    println!("-----");
    let image: Vec<Vec<u32>> = image.chunks(WIDTH).map(|line| line.to_vec()).collect();
    for line in image {
        for s in line {
            if s == 1 {
                print!("#");
            } else if s == 0 {
                print!(" ");
            }
        }
        println!("");
    }
    println!("-----");
}

// part 1
// fn get_num_digit(layer: &Layer, digit: u32) -> usize {
//     layer.iter().filter(|x| **x == digit).count()
// }

// fn find_layer(layers: &Layers) -> &Layer {
//     layers
//         .iter()
//         .min_by(|x, y| get_num_digit(x, 0).cmp(&get_num_digit(y, 0)))
//         .unwrap()
// }

// fn parse_layer(layer: &Layer) -> usize {
//     println!("target: {:?}", layer);
//     get_num_digit(layer, 1) * get_num_digit(layer, 2)
// }

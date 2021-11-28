mod graph;
use graph::{Graph, NodeIndex};
pub fn main(contents: String) {
    let orbits: Vec<(String, String)> = contents
        .lines()
        .map(|line| line.split(")").collect())
        .map(|split: Vec<&str>| (split[0].trim().to_string(), split[1].trim().to_string()))
        .collect();
    let (orbits, com_index) = init_graph(orbits);
    println!("oribits: {:?}", orbits);
    // println!("com: {}", com_index);
    // println!("counts: {}", get_counts(&orbits, com_index, 0));
    // println!("parent: {}", get_deepest_parent(&orbits, com_index, 2, 4));

    let you_idx = orbits.get_by_data("YOU".to_owned()).unwrap();
    let san_idx = orbits.get_by_data("SAN".to_owned()).unwrap();
    println!(
        "t counts: {}",
        get_transfer_count(&orbits, com_index, you_idx, san_idx),
    );
}

fn init_graph(orbits: Vec<(String, String)>) -> (Graph, NodeIndex) {
    let mut graph = Graph::new();
    let mut com_index = 0;
    for (planet, orbiter) in orbits {
        let p_index = graph.add_node(planet.clone());
        let o_index = graph.add_node(orbiter);
        graph.add_edge(p_index, o_index);
        if planet == "COM" {
            com_index = p_index;
        }
    }
    (graph, com_index)
}

fn get_counts<'a>(orbits: &'a Graph, start_index: NodeIndex, depth: usize) -> usize {
    let mut count = depth;
    println!("node: {:?}", orbits.get_node(start_index));
    for succ_idx in orbits.successors(start_index) {
        count += get_counts(orbits, succ_idx, depth + 1);
    }
    count
}

fn is_succ<'a>(orbits: &'a Graph, start_index: NodeIndex, target_index: NodeIndex) -> bool {
    for succ_idx in orbits.successors(start_index) {
        if succ_idx == target_index {
            return true;
        }
        if is_succ(orbits, succ_idx, target_index) {
            return true;
        }
    }
    false
}

// returns start_index if there is no deeper parent
fn get_deepest_parent<'a>(
    orbits: &'a Graph,
    start_index: NodeIndex,
    t1_index: NodeIndex,
    t2_index: NodeIndex,
) -> NodeIndex {
    for succ_idx in orbits.successors(start_index) {
        let found_t1 = is_succ(orbits, succ_idx, t1_index);
        let found_t2 = is_succ(orbits, succ_idx, t2_index);
        if found_t1 && found_t2 {
            return get_deepest_parent(orbits, succ_idx, t1_index, t2_index);
        }
    }
    start_index
}

fn get_count_to_node<'a>(
    orbits: &'a Graph,
    root_idx: NodeIndex,
    target_idx: NodeIndex,
    curr_depth: usize,
) -> Option<usize> {
    for succ_idx in orbits.successors(root_idx) {
        if succ_idx == target_idx {
            return Some(curr_depth);
        }
        let res = get_count_to_node(orbits, succ_idx, target_idx, curr_depth + 1);
        if res.is_some() {
            return res;
        }
    }
    None
}

fn get_transfer_count<'a>(
    orbits: &'a Graph,
    root_idx: NodeIndex,
    t1_index: NodeIndex,
    t2_index: NodeIndex,
) -> usize {
    let parent_idx = get_deepest_parent(orbits, root_idx, t1_index, t2_index);
    println!("Parent: {:?}", orbits.get_node(parent_idx).unwrap());
    let t1_count = get_count_to_node(orbits, parent_idx, t1_index, 0);
    let t2_count = get_count_to_node(orbits, parent_idx, t2_index, 0);
    println!("counts: {:?}", (t1_count, t2_count));
    match (t1_count, t2_count) {
        (Some(x), Some(y)) => x + y,
        _ => panic!("ass"),
    }
}

mod graph;
use graph::{Graph, NodeIndex};
pub fn main(contents: String) {
    let orbits: Vec<(String, String)> = contents
        .lines()
        .map(|line| line.split(")").collect())
        .map(|split: Vec<&str>| (split[0].trim().to_string(), split[1].trim().to_string()))
        .collect();
    let (orbits, com_index) = init_graph(orbits);
    // println!("oribits: {:?}", orbits);
    // println!("com: {}", com_index);
    println!("counts: {}", get_counts(&orbits, com_index, 0));
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

fn find_common_parent<'a>(orbits: &'a Graph, start_index: NodeIndex) {
    for succ_idx in orbits.successors(start_index) {
        find_common_parent(orbits, succ_idx);
    }
}

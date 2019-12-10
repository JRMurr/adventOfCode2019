#[derive(Clone, Debug)]
pub struct Graph {
    nodes: Vec<NodeData>,
    edges: Vec<EdgeData>,
}
pub type NodeIndex = usize;

#[derive(Clone, Debug)]
pub struct NodeData {
    data: String,
    first_outgoing_edge: Option<EdgeIndex>,
}
pub type EdgeIndex = usize;

#[derive(Clone, Debug)]
pub struct EdgeData {
    target: NodeIndex,
    next_outgoing_edge: Option<EdgeIndex>,
}

impl Graph {
    pub fn new() -> Graph {
        Graph {
            nodes: vec![],
            edges: vec![],
        }
    }

    pub fn get_node(&self, index: NodeIndex) -> Option<NodeData> {
        if index < self.nodes.len() {
            return Some(self.nodes[index].clone());
        }
        None
    }

    pub fn get_by_data(&self, data: String) -> Option<NodeIndex> {
        for (index, node) in self.nodes.iter().enumerate() {
            if node.data == data {
                return Some(index);
            }
        }
        None
    }

    pub fn add_node(&mut self, data: String) -> NodeIndex {
        for (index, node) in self.nodes.iter().enumerate() {
            if node.data == data {
                return index;
            }
        }
        let index = self.nodes.len();
        self.nodes.push(NodeData {
            data,
            first_outgoing_edge: None,
        });
        index
    }

    pub fn add_edge(&mut self, source: NodeIndex, target: NodeIndex) {
        let edge_index = self.edges.len();
        let node_data = &mut self.nodes[source];
        self.edges.push(EdgeData {
            target: target,
            next_outgoing_edge: node_data.first_outgoing_edge,
        });
        node_data.first_outgoing_edge = Some(edge_index);
    }

    pub fn successors(&self, source: NodeIndex) -> Successors {
        let first_outgoing_edge = self.nodes[source].first_outgoing_edge;
        Successors {
            graph: self,
            current_edge_index: first_outgoing_edge,
        }
    }
}
pub struct Successors<'graph> {
    graph: &'graph Graph,
    current_edge_index: Option<EdgeIndex>,
}

impl<'graph> Iterator for Successors<'graph> {
    type Item = NodeIndex;
    fn next(&mut self) -> Option<NodeIndex> {
        match self.current_edge_index {
            None => None,
            Some(edge_num) => {
                let edge = &self.graph.edges[edge_num];
                self.current_edge_index = edge.next_outgoing_edge;
                Some(edge.target)
            }
        }
    }
}
